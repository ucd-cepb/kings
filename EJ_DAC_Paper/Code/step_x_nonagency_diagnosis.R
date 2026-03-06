# step_x_nonagency_diagnosis.R
# Diagnostic analysis: characterizing non-agency nodes and their differential
# connections to DAC vs non-DAC community places.
#
# Addresses Mark Lubell's concern (Slack):
#   "When you remove the non-agency nodes, how many of each type of community
#    do you take out of the network? I'm guessing DACs may be more likely to be
#    connected to non-agency/geographic nodes."
#
# Key questions:
#   1. What entity types exist in the tagged full network, and which are non-agency?
#   2. What types of nodes are DAC vs non-DAC places connected to (in/out edges)?
#   3. How many edges do DAC vs non-DAC places lose when non-agency nodes are removed?
#   4. Do DAC vs non-DAC places differ in their rates of becoming isolates?
#
# Outputs (to EJ_DAC_Paper/Out/):
#   CSV: diag_node_type_counts.csv        -- entity type inventory across all networks
#   CSV: diag_neighbor_agency_split.csv   -- % edges to agency/non-agency by DAC status
#   CSV: diag_nonagency_neighbor_types.csv-- non-agency neighbor types by DAC status (in/out)
#   CSV: diag_agency_neighbor_types.csv   -- agency neighbor types by DAC status (in/out)
#   CSV: diag_degree_loss_summary.csv     -- mean degree in tagged vs agency by DAC status
#   CSV: diag_isolate_status.csv          -- isolate rates by DAC status in each network
#   PNG: diag_p1_agency_edge_share.png    -- stacked bar: agency vs non-agency share by DAC
#   PNG: diag_p2_nonagency_types.png      -- non-agency neighbor type breakdown
#   PNG: diag_p3_degree_comparison.png    -- mean degree in tagged vs agency by DAC status
#   PNG: diag_p4_degree_loss.png          -- mean edges lost (tagged minus agency) by DAC
#   PNG: diag_p5_all_neighbor_types.png   -- full entity type breakdown for in/out edges
#   PNG: diag_p6_inout_loss.png           -- in vs out degree loss side by side

library(dotenv)
library(igraph)
library(tidyverse)
library(ggpubr)
library(scales)
library(RColorBrewer)

load_dot_env()

# ============================================================================
# CONFIGURATION
# ============================================================================

agency_types <- c("Local_GSA", "Other_GSA", "District", "Local_Gov",
                  "State_Gov", "Federal_Gov", "City", "County",
                  "Group", "NGO", "Company", "Person")

tagged_dir <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_tagged")
agency_dir <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_agency")
out_dir    <- "EJ_DAC_Paper/Out/"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ============================================================================
# HELPER: load place-level vertex attributes from a network directory
# ============================================================================

load_place_nodes <- function(data_dir) {
  extract_list <- list.files(data_dir)
  gsp_ids      <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
  result <- data.frame()

  for (g in seq_along(gsp_ids)) {
    net   <- readRDS(paste0(data_dir, "/", extract_list[g]))
    nodes <- tibble(igraph::as_data_frame(net, what = "vertices")) %>%
      mutate(
        across(any_of(c("deg", "in", "out", "in_w", "out_w", "deg_w",
                        "leader_dist", "leader_dist_nona", "reachable",
                        "GSA", "is_place")), as.numeric),
        gsp_id   = gsp_ids[g],
        is_place = ifelse(is.na(GEOID20), 0, 1),
        MHI_log  = suppressWarnings(log(as.numeric(MHI))),
        POP_log  = suppressWarnings(log(as.numeric(POP)))
      ) %>%
      filter(is_place == 1)
    result <- bind_rows(result, nodes)
  }
  result
}

# ============================================================================
# SECTION 1: Node inventory — entity type counts across all tagged networks
# ============================================================================

cat("Loading node inventory from tagged networks...\n")

extract_list_tagged <- list.files(tagged_dir)
gsp_ids_tagged      <- gsub("^0+", "", gsub("\\.RDS", "", extract_list_tagged))

all_nodes_tagged <- data.frame()

for (g in seq_along(gsp_ids_tagged)) {
  net   <- readRDS(paste0(tagged_dir, "/", extract_list_tagged[g]))
  nodes <- tibble(igraph::as_data_frame(net, what = "vertices")) %>%
    mutate(
      gsp_id   = gsp_ids_tagged[g],
      is_place = ifelse(is.na(GEOID20), 0, 1)
    )
  all_nodes_tagged <- bind_rows(all_nodes_tagged, nodes)
}

node_type_summary <- all_nodes_tagged %>%
  mutate(
    entity_type_clean = ifelse(is.na(entity_type), "Unclassified", entity_type),
    is_agency_label   = ifelse(entity_type_clean %in% agency_types, "Agency", "Non-Agency")
  ) %>%
  count(entity_type_clean, is_agency_label, name = "n_node_appearances") %>%
  arrange(is_agency_label, desc(n_node_appearances))

cat("\n=== NODE APPEARANCES BY ENTITY TYPE (all tagged networks) ===\n")
print(node_type_summary, n = Inf)
write_csv(node_type_summary, paste0(out_dir, "diag_node_type_counts.csv"))

# ============================================================================
# SECTION 2: Neighbor analysis — what types of nodes are DAC/non-DAC places
#             connected to? (using tagged full network edgelists)
# ============================================================================

cat("\nBuilding neighbor-type dataset from tagged networks...\n")

place_neighbor_types <- data.frame()

for (g in seq_along(gsp_ids_tagged)) {
  net <- readRDS(paste0(tagged_dir, "/", extract_list_tagged[g]))

  vdf <- igraph::as_data_frame(net, what = "vertices") %>%
    mutate(is_place = ifelse(is.na(GEOID20), 0, 1),
           entity_type = ifelse(is.na(entity_type), "Unclassified", entity_type))

  edf <- igraph::as_data_frame(net, what = "edges") %>%
    mutate(gsp_id = gsp_ids_tagged[g])

  if (nrow(edf) == 0) next

  # Edges where a PLACE is the SOURCE (outgoing edge from a place)
  src_place <- edf %>%
    left_join(vdf %>% select(name, DAC, is_place) %>%
                rename(src_DAC = DAC, src_is_place = is_place),
              by = c("from" = "name")) %>%
    left_join(vdf %>% select(name, entity_type) %>%
                rename(tgt_type = entity_type),
              by = c("to" = "name")) %>%
    filter(src_is_place == 1) %>%
    transmute(
      gsp_id,
      place_name   = from,
      DAC          = src_DAC,
      neighbor     = to,
      entity_type  = tgt_type,
      direction    = "out"
    )

  # Edges where a PLACE is the TARGET (incoming edge to a place)
  tgt_place <- edf %>%
    left_join(vdf %>% select(name, DAC, is_place) %>%
                rename(tgt_DAC = DAC, tgt_is_place = is_place),
              by = c("to" = "name")) %>%
    left_join(vdf %>% select(name, entity_type) %>%
                rename(src_type = entity_type),
              by = c("from" = "name")) %>%
    filter(tgt_is_place == 1) %>%
    transmute(
      gsp_id,
      place_name   = to,
      DAC          = tgt_DAC,
      neighbor     = from,
      entity_type  = src_type,
      direction    = "in"
    )

  place_neighbor_types <- bind_rows(place_neighbor_types, src_place, tgt_place)

  if (g %% 20 == 0) cat("  ... processed", g, "of", length(gsp_ids_tagged), "networks\n")
}

place_neighbor_types <- place_neighbor_types %>%
  mutate(
    entity_type = ifelse(is.na(entity_type), "Unclassified", entity_type),
    is_agency   = entity_type %in% agency_types,
    DAC_label   = case_when(DAC == 1 ~ "DAC", DAC == 0 ~ "non-DAC", TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(DAC_label))

cat("  Total place-neighbor edge records:", nrow(place_neighbor_types), "\n")

# ============================================================================
# SECTION 3: Agency vs non-agency edge split by DAC status and direction
# ============================================================================

# Overall agency share
neighbor_agency_summary <- place_neighbor_types %>%
  group_by(DAC_label, direction, is_agency) %>%
  summarise(n_edges = n(), .groups = "drop") %>%
  group_by(DAC_label, direction) %>%
  mutate(
    total_edges   = sum(n_edges),
    pct           = n_edges / total_edges * 100,
    agency_label  = ifelse(is_agency, "Agency", "Non-Agency")
  ) %>%
  ungroup()

cat("\n=== AGENCY vs NON-AGENCY EDGE SHARE BY DAC STATUS AND DIRECTION ===\n")
print(neighbor_agency_summary %>% select(DAC_label, direction, agency_label, n_edges, total_edges, pct))
write_csv(neighbor_agency_summary, paste0(out_dir, "diag_neighbor_agency_split.csv"))

# ============================================================================
# SECTION 4: Non-agency neighbor types by DAC status and direction
# ============================================================================

nonagency_type_summary <- place_neighbor_types %>%
  filter(!is_agency) %>%
  group_by(DAC_label, direction, entity_type) %>%
  summarise(n_edges = n(), .groups = "drop") %>%
  group_by(DAC_label, direction) %>%
  mutate(pct = n_edges / sum(n_edges) * 100) %>%
  ungroup() %>%
  arrange(DAC_label, direction, desc(n_edges))

cat("\n=== TOP NON-AGENCY NEIGHBOR TYPES BY DAC STATUS AND DIRECTION ===\n")
print(nonagency_type_summary %>% group_by(DAC_label, direction) %>% slice_head(n = 6), n = Inf)
write_csv(nonagency_type_summary, paste0(out_dir, "diag_nonagency_neighbor_types.csv"))

# Agency neighbor type breakdown (excluding City-to-City / place-to-place)
agency_type_summary <- place_neighbor_types %>%
  filter(is_agency, !(entity_type %in% c("City", "County"))) %>%
  group_by(DAC_label, direction, entity_type) %>%
  summarise(n_edges = n(), .groups = "drop") %>%
  group_by(DAC_label, direction) %>%
  mutate(pct = n_edges / sum(n_edges) * 100) %>%
  ungroup() %>%
  arrange(DAC_label, direction, desc(n_edges))

cat("\n=== AGENCY NEIGHBOR TYPES (excl. City/County) BY DAC STATUS ===\n")
print(agency_type_summary, n = Inf)
write_csv(agency_type_summary, paste0(out_dir, "diag_agency_neighbor_types.csv"))

# ============================================================================
# SECTION 5: Degree loss — compare tagged vs agency network metrics for places
# ============================================================================

cat("\nLoading place nodes from tagged and agency networks...\n")
tagged_places <- load_place_nodes(tagged_dir)
agency_places <- load_place_nodes(agency_dir)
cat("  Tagged place nodes:", nrow(tagged_places), "\n")
cat("  Agency place nodes:", nrow(agency_places), "\n")

degree_comparison <- tagged_places %>%
  select(name, gsp_id, DAC, incorporated, MHI, MHI_log, POP, POP_log,
         in_w, out_w, deg_w, leader_dist, leader_dist_nona, reachable) %>%
  rename_with(~ paste0(.x, "_tagged"),
              c(in_w, out_w, deg_w, leader_dist, leader_dist_nona, reachable)) %>%
  left_join(
    agency_places %>%
      select(name, gsp_id, in_w, out_w, deg_w, leader_dist, leader_dist_nona, reachable) %>%
      rename_with(~ paste0(.x, "_agency"),
                  c(in_w, out_w, deg_w, leader_dist, leader_dist_nona, reachable)),
    by = c("name", "gsp_id")
  ) %>%
  mutate(
    DAC_label          = case_when(DAC == 1 ~ "DAC", DAC == 0 ~ "non-DAC", TRUE ~ NA_character_),
    in_w_agency        = replace_na(in_w_agency,  0),
    out_w_agency       = replace_na(out_w_agency, 0),
    deg_w_agency       = replace_na(deg_w_agency, 0),
    reachable_agency   = replace_na(reachable_agency, 0),
    in_w_loss          = in_w_tagged  - in_w_agency,
    out_w_loss         = out_w_tagged - out_w_agency,
    deg_w_loss         = deg_w_tagged - deg_w_agency,
    pct_in_retained    = ifelse(in_w_tagged  > 0, in_w_agency  / in_w_tagged,  NA),
    pct_out_retained   = ifelse(out_w_tagged > 0, out_w_agency / out_w_tagged, NA),
    became_isolate     = (deg_w_tagged > 0) & (deg_w_agency == 0)
  ) %>%
  filter(!is.na(DAC_label))

# Summary by DAC status
degree_loss_summary <- degree_comparison %>%
  group_by(DAC_label) %>%
  summarise(
    n_places              = n(),
    # Indegree
    mean_in_tagged        = mean(in_w_tagged,       na.rm = TRUE),
    mean_in_agency        = mean(in_w_agency,        na.rm = TRUE),
    mean_in_loss          = mean(in_w_loss,          na.rm = TRUE),
    pct_in_loss           = mean_in_loss / mean_in_tagged * 100,
    # Outdegree
    mean_out_tagged       = mean(out_w_tagged,       na.rm = TRUE),
    mean_out_agency       = mean(out_w_agency,       na.rm = TRUE),
    mean_out_loss         = mean(out_w_loss,         na.rm = TRUE),
    pct_out_loss          = mean_out_loss / mean_out_tagged * 100,
    # Total degree
    mean_deg_tagged       = mean(deg_w_tagged,       na.rm = TRUE),
    mean_deg_agency       = mean(deg_w_agency,       na.rm = TRUE),
    mean_deg_loss         = mean(deg_w_loss,         na.rm = TRUE),
    # Leader distance
    mean_ld_nona_tagged   = mean(leader_dist_nona_tagged, na.rm = TRUE),
    mean_ld_nona_agency   = mean(leader_dist_nona_agency, na.rm = TRUE),
    # Reachability
    pct_reachable_tagged  = mean(reachable_tagged,   na.rm = TRUE) * 100,
    pct_reachable_agency  = mean(reachable_agency,   na.rm = TRUE) * 100,
    # Became isolate
    n_became_isolate      = sum(became_isolate,      na.rm = TRUE),
    pct_became_isolate    = n_became_isolate / n_places * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n=== DEGREE LOSS SUMMARY: TAGGED vs AGENCY NETWORKS ===\n")
print(as.data.frame(t(degree_loss_summary)), row.names = TRUE)
write_csv(degree_loss_summary, paste0(out_dir, "diag_degree_loss_summary.csv"))

# ============================================================================
# SECTION 6: Isolate analysis — who becomes isolated when non-agency nodes removed?
# ============================================================================

isolate_status <- degree_comparison %>%
  mutate(
    is_isolate_tagged = (deg_w_tagged == 0),
    is_isolate_agency = (deg_w_agency == 0)
  ) %>%
  group_by(DAC_label) %>%
  summarise(
    n_total             = n(),
    n_isolate_tagged    = sum(is_isolate_tagged, na.rm = TRUE),
    n_isolate_agency    = sum(is_isolate_agency, na.rm = TRUE),
    pct_isolate_tagged  = n_isolate_tagged / n_total * 100,
    pct_isolate_agency  = n_isolate_agency / n_total * 100,
    n_newly_isolated    = sum(became_isolate,    na.rm = TRUE),
    pct_newly_isolated  = n_newly_isolated / n_total * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

cat("\n=== ISOLATE STATUS BY DAC STATUS ===\n")
print(isolate_status)
write_csv(isolate_status, paste0(out_dir, "diag_isolate_status.csv"))

# ============================================================================
# VISUALIZATIONS
# ============================================================================

dac_colors <- c("DAC" = "#d73027", "non-DAC" = "#2166ac")

# --- PLOT 1: Agency vs Non-Agency edge share by DAC status and direction ---

p1_data <- neighbor_agency_summary %>%
  mutate(
    direction_lab = ifelse(direction == "in", "Incoming edges\n(indegree)", "Outgoing edges\n(outdegree)")
  )

p1 <- ggplot(p1_data, aes(x = DAC_label, y = pct, fill = agency_label)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  facet_wrap(~ direction_lab) +
  scale_fill_manual(values = c("Agency" = "#4393c3", "Non-Agency" = "#d6604d"),
                    name = "Neighbor type") +
  labs(
    title    = "Share of Edges to Agency vs Non-Agency Nodes",
    subtitle = "For DAC and non-DAC places in the full tagged network",
    x = NULL, y = "Percent of edges"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(paste0(out_dir, "diag_p1_agency_edge_share.png"), p1, width = 7, height = 5)

# --- PLOT 2: Non-agency neighbor types by DAC status and direction ---

top_nonagency <- nonagency_type_summary %>%
  group_by(entity_type) %>%
  summarise(total = sum(n_edges), .groups = "drop") %>%
  slice_max(total, n = 10) %>%
  pull(entity_type)

p2 <- nonagency_type_summary %>%
  filter(entity_type %in% top_nonagency) %>%
  mutate(
    entity_type   = factor(entity_type, levels = rev(top_nonagency)),
    direction_lab = ifelse(direction == "in", "Incoming (indegree)", "Outgoing (outdegree)")
  ) %>%
  ggplot(aes(x = entity_type, y = n_edges, fill = DAC_label)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = dac_colors, name = NULL) +
  coord_flip() +
  facet_wrap(~ direction_lab) +
  labs(
    title    = "Non-Agency Neighbor Types: DAC vs non-DAC Places",
    subtitle = "Count of edges connecting places to non-agency node types (top 10)",
    x = NULL, y = "Number of edges"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(paste0(out_dir, "diag_p2_nonagency_types.png"), p2, width = 10, height = 5)

# --- PLOT 3: Mean degree comparison (tagged vs agency) by DAC status ---

p3_data <- degree_loss_summary %>%
  select(DAC_label, mean_in_tagged, mean_in_agency, mean_out_tagged, mean_out_agency) %>%
  pivot_longer(-DAC_label, names_to = "metric", values_to = "value") %>%
  mutate(
    network = ifelse(grepl("tagged", metric), "Full (Tagged)", "Agency Only"),
    measure = ifelse(grepl("^mean_in", metric), "Indegree", "Outdegree")
  )

p3 <- ggplot(p3_data,
             aes(x = network, y = value, fill = DAC_label,
                 group = interaction(DAC_label, network))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = round(value, 2)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.5) +
  facet_wrap(~ measure) +
  scale_fill_manual(values = dac_colors, name = NULL) +
  labs(
    title    = "Mean Degree: Full Network vs Agency-Only Network",
    subtitle = "By DAC status — shows connectivity lost when non-agency nodes are removed",
    x = NULL, y = "Mean weighted degree"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(paste0(out_dir, "diag_p3_degree_comparison.png"), p3, width = 8, height = 5)

# --- PLOT 4: Degree loss magnitude by DAC status ---

p4_data <- degree_loss_summary %>%
  select(DAC_label, mean_in_loss, mean_out_loss, pct_in_loss, pct_out_loss) %>%
  pivot_longer(-DAC_label, names_to = "metric", values_to = "value") %>%
  mutate(
    type    = ifelse(grepl("^mean", metric), "Absolute loss (edges)", "Percent loss (%)"),
    measure = ifelse(grepl("in_", metric), "Indegree", "Outdegree")
  )

p4 <- ggplot(p4_data, aes(x = DAC_label, y = value, fill = DAC_label)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = ifelse(type == "Percent loss (%)",
                               paste0(round(value, 1), "%"),
                               round(value, 2))),
            vjust = -0.5, size = 3.5) +
  facet_grid(type ~ measure, scales = "free_y") +
  scale_fill_manual(values = dac_colors, name = NULL) +
  labs(
    title    = "Degree Lost When Removing Non-Agency Nodes",
    subtitle = "Tagged network minus agency-only network, by DAC status",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(paste0(out_dir, "diag_p4_degree_loss.png"), p4, width = 7, height = 6)

# --- PLOT 5: Full entity type breakdown (in + out combined) for DAC vs non-DAC ---

all_type_summary <- place_neighbor_types %>%
  group_by(DAC_label, entity_type) %>%
  summarise(n_edges = n(), .groups = "drop") %>%
  group_by(DAC_label) %>%
  mutate(pct = n_edges / sum(n_edges) * 100) %>%
  ungroup()

top_types_all <- all_type_summary %>%
  group_by(entity_type) %>%
  summarise(total = sum(n_edges), .groups = "drop") %>%
  slice_max(total, n = 12) %>%
  pull(entity_type)

p5_data <- all_type_summary %>%
  filter(entity_type %in% top_types_all) %>%
  mutate(
    is_agency_flag = entity_type %in% agency_types,
    entity_type    = factor(entity_type,
                            levels = all_type_summary %>%
                              group_by(entity_type) %>%
                              summarise(total = sum(n_edges), .groups = "drop") %>%
                              filter(entity_type %in% top_types_all) %>%
                              arrange(total) %>%
                              pull(entity_type))
  )

p5 <- ggplot(p5_data, aes(x = entity_type, y = pct, fill = DAC_label)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(0.7), hjust = -0.1, size = 2.8) +
  coord_flip() +
  scale_fill_manual(values = dac_colors, name = NULL) +
  labs(
    title    = "Neighbor Entity Type Distribution: DAC vs non-DAC Places",
    subtitle = "% of all edges (in + out) to each entity type (top 12 shown)",
    x = NULL, y = "% of edges"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(paste0(out_dir, "diag_p5_all_neighbor_types.png"), p5, width = 9, height = 6)

# --- PLOT 6: Leader distance comparison ---

p6_data <- degree_loss_summary %>%
  select(DAC_label, mean_ld_nona_tagged, mean_ld_nona_agency) %>%
  pivot_longer(-DAC_label, names_to = "network", values_to = "mean_ld") %>%
  mutate(
    network_lab = ifelse(grepl("tagged", network), "Full (Tagged)", "Agency Only")
  )

p6 <- ggplot(p6_data, aes(x = network_lab, y = mean_ld, fill = DAC_label,
                           group = interaction(DAC_label, network_lab))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = round(mean_ld, 2)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = dac_colors, name = NULL) +
  labs(
    title    = "Mean Leader Distance: Full vs Agency-Only Network",
    subtitle = "By DAC status — ceiling applied for unreachable nodes",
    x = NULL, y = "Mean leader distance (w/ ceiling)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(paste0(out_dir, "diag_p6_leader_dist_comparison.png"), p6, width = 6, height = 5)

# --- PANEL SUMMARY: key plots combined ---

summary_panel <- ggarrange(p1, p4,
                           nrow = 1, labels = "AUTO", common.legend = FALSE)
ggsave(paste0(out_dir, "diag_panel_summary.png"), summary_panel, width = 13, height = 5)

full_panel <- ggarrange(p1, p3, p4, p2,
                        nrow = 2, ncol = 2, labels = "AUTO", common.legend = FALSE)
ggsave(paste0(out_dir, "diag_panel_full.png"), full_panel, width = 14, height = 10)

# ============================================================================
# PRINT FINAL SUMMARY TO CONSOLE
# ============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("DIAGNOSTIC SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("-- DEGREE LOSS WHEN NON-AGENCY NODES REMOVED --\n")
degree_loss_summary %>%
  select(DAC_label, n_places,
         mean_in_tagged, mean_in_agency, mean_in_loss, pct_in_loss,
         mean_out_tagged, mean_out_agency, mean_out_loss, pct_out_loss) %>%
  print()

cat("\n-- ISOLATE RATES --\n")
print(isolate_status)

cat("\n-- AGENCY EDGE SHARE (in-edges) --\n")
neighbor_agency_summary %>%
  filter(direction == "in") %>%
  select(DAC_label, agency_label, n_edges, total_edges, pct) %>%
  print()

cat("\n-- ALL OUTPUTS SAVED TO:", out_dir, "--\n")
