# generate_before_after_pngs.R
# Generate before/after network PNGs for one example GSP:
#   - Tagged (all entity types) vs Agency (social actors only)
# Output: EJ_DAC_Paper/Out/example_network_tagged.png
#         EJ_DAC_Paper/Out/example_network_agency.png

library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)

load_dot_env()

# --- Pick a GSP to visualize ---
# Choose a mid-sized network with a good mix of agency and non-agency nodes
example_gsp <- "0042.RDS"

# --- Load networks ---
tagged_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/processed_networks_tagged/")
agency_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/processed_networks_agency/")

net_tagged <- readRDS(paste0(tagged_fp, example_gsp))
net_agency <- readRDS(paste0(agency_fp, example_gsp))

# --- plot_graph (adapted from Network_Structure_Paper/Code/1a_generate_networks_functions.R) ---

plot_graph <- function(igraph_obj, title = NULL) {
  node_type_levels <- c(
    "Local_GSA", "Other_GSA", "District",
    "Local_Gov", "State_Gov", "Federal_Gov",
    "City", "County",
    "Basin", "Natural_Feature", "Geographic_Unit",
    "Infrastructure", "Water_Project",
    "Group", "NGO", "Company",
    "Technical", "Reference",
    "Legal", "Person", "Nonsense"
  )
  node_type_colors <- c(
    Local_GSA = "#20b2aa", Other_GSA = "#20b2aa",
    District = "#122382",
    Local_Gov = "#6baed6", State_Gov = "#2171b5", Federal_Gov = "#08306b",
    City = "#ff9800", County = "#e65100",
    Basin = "#2ca02c", Natural_Feature = "#98df8a", Geographic_Unit = "#b2df8a",
    Infrastructure = "#8e24aa", Water_Project = "#ce93d8",
    Group = "#8B0000", NGO = "#E57373", Company = "#FFCDD2",
    Technical = "#607d8b", Reference = "#b0bec5", Legal = "#757575",
    Person = "#ffd700",
    Nonsense = "#000000"
  )
  node_type_shapes <- c(
    Local_GSA = 24, Other_GSA = 25, District = 25,
    Local_Gov = 22, State_Gov = 22, Federal_Gov = 22,
    City = 21, County = 21,
    Basin = 21, Natural_Feature = 21, Geographic_Unit = 21,
    Infrastructure = 24, Water_Project = 24,
    Group = 23, NGO = 23, Company = 23,
    Technical = 25, Reference = 25,
    Legal = 8, Person = 21, Nonsense = 4
  )

  vdf <- igraph::as_data_frame(igraph_obj, what = "vertices")
  vdf$entity_type <- factor(vdf$entity_type, levels = node_type_levels)
  ig <- igraph::graph_from_data_frame(
    igraph::as_data_frame(igraph_obj, what = "edges"),
    vertices = vdf,
    directed = TRUE
  )

  p <- ggraph::ggraph(ig, layout = "fr") +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(
      aes(color = entity_type, fill = entity_type,
          shape = entity_type, size = degree),
      stroke = 1.2
    ) +
    scale_color_manual(values = node_type_colors, breaks = node_type_levels,
                       name = "Node Type") +
    scale_fill_manual(values = node_type_colors, breaks = node_type_levels,
                      name = "Node Type") +
    scale_shape_manual(values = node_type_shapes, breaks = node_type_levels,
                       name = "Node Type") +
    guides(
      color = guide_legend(override.aes = list(size = 5)),
      fill  = guide_legend(override.aes = list(size = 5)),
      shape = guide_legend(override.aes = list(size = 5))
    ) +
    theme_void()

  if (!is.null(title)) p <- p + ggtitle(title)
  return(p)
}

# --- Generate plots with reproducible layout ---
gsp_label <- gsub("^0+|.RDS", "", example_gsp)

set.seed(42)
p_tagged <- plot_graph(net_tagged, title = paste0("GSP ", gsp_label, ": All Entity Types"))

set.seed(42)
p_agency <- plot_graph(net_agency, title = paste0("GSP ", gsp_label, ": Agency Nodes Only"))

# --- Save ---
ggsave(p_tagged,
       filename = "EJ_DAC_Paper/Out/example_network_tagged.png",
       width = 12, height = 12, dpi = 300)

ggsave(p_agency,
       filename = "EJ_DAC_Paper/Out/example_network_agency.png",
       width = 12, height = 12, dpi = 300)

cat("Saved example_network_tagged.png and example_network_agency.png\n")

# --- Print summary stats ---
cat("\nTagged network:\n")
cat("  Nodes:", vcount(net_tagged), "\n")
cat("  Edges:", ecount(net_tagged), "\n")

cat("\nAgency network:\n")
cat("  Nodes:", vcount(net_agency), "\n")
cat("  Edges:", ecount(net_agency), "\n")
