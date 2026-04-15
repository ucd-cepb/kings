library(scales)
library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)
library(ggpubr)
library(skimr)
library(plotly)
library(RColorBrewer)

# ============================================================================
# CONFIGURATION - Must match the mode used in step_3 and step_4
# ============================================================================

network_mode <- "tagged"   # Options: "original", "tagged", "agency"

# ============================================================================
load_dot_env()

# --- Directory mapping ---
data_dirs <- list(
   original = "/EJ_Paper/cleaned_extracts_DACified",
   tagged   = "/EJ_Paper/cleaned_extracts_tagged",
   agency   = "/EJ_Paper/cleaned_extracts_agency"
)

# --- Output file suffix ---
mode_suffix <- if (network_mode == "original") "" else paste0("_", network_mode)

network_fp <- paste0(Sys.getenv("BOX_PATH"), data_dirs[[network_mode]])
extract_list <- list.files(network_fp)

place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

all_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   
   gsp_id <- paste0("gsp_",gsp_ids[g])
   
   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   
   all_nodes <- rbind(all_nodes, nodes)
   
   place_nodes <- nodes %>%
      mutate(across(any_of(c('deg', 'pr', 'pr_w', 'alpha', 'in', 'out', 'eig', 'gsp_id')),
                    as.numeric)) %>%
      mutate(
         MHI_log = log(MHI),
         POP_log = log(POP),
         is_place = ifelse(is.na(GEOID20), 0, 1)
      ) %>%
      filter(is_place == 1)
   
   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}


all_places <- bind_rows(place_existance) %>% 
   mutate(DAC = as.factor(DAC),
          incorporated = as.factor(incorporated),
          exists = as.factor(exists),
          MHI_log = log(MHI),
          POP_log = log(POP)
   )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data summary table 
# table 2 in section 3.1

# most common nodes in network (not used )
all_nodes %>%
   group_by(name, entity_type) %>% 
   summarize(count = sum(num_appearances)) %>%
   select(name, entity_type, count) %>%
   ungroup() %>% 
   unique() %>% 
   arrange(desc(count))

# prevelence of node types
node_types <- all_nodes %>%
   select(name, entity_type) %>%
   unique() %>% 
   count(entity_type) %>%
   mutate(pct = n / sum(n) * 100) %>%
   select(entity_type, pct) %>%
   arrange(desc(pct)) 

# same node shows up multiple times with different node types
all_nodes %>% 
   group_by(name) %>% 
   summarize(num_types = n_distinct(entity_type)) %>% 
   filter(num_types > 2) %>% 
   arrange(desc(num_types))

all_nodes %>% 
   filter(name == 'district_board_meetings') %>% 
   select(name, num_appearances, entity_type, AI_TAGGED)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data summary table 
# table 2=3 in section 3.3

data_summary <- all_place_nodes %>% 
   select( MHI, POP, DAC, incorporated, per_latino, deg_w, leader_dist_nona) %>%
   rename("MHI" = MHI,
          "Population" = POP,
          "DAC" = DAC,
          "% Latino" = per_latino,
          "Incorporated" = incorporated,
          "Degree" = deg_w,
          "Leader Distance" = leader_dist_nona) %>%
   skim() %>% tibble() %>% 
   select(-c(skim_type, n_missing, complete_rate, numeric.hist,
             numeric.p0,  numeric.p25, numeric.p75, numeric.p100
             )) %>%
   rename("Variable" = skim_variable,
          "Mean" = numeric.mean,
          "SD" = numeric.sd,
          "Median" = numeric.p50) %>% 
   mutate(across(where(is.numeric), ~ round(.x, 2))) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT - FIGURE 2 in RESULTS

create_model_plot <- function(data, family, x_var, y_var, x_label, y_label) {
   # Fit the model
   formula_str <- as.formula(paste(y_var, "~", x_var, "+ incorporated + POP_log + per_latino"))
   model <- glm(formula_str, family = family, data = data)
   
   # Create the prediction plot
   plot <- plot_model(model, type = 'pred',
                      terms = c(x_var, 'incorporated'),
                      title = " ",
                      legend.title = 'Incorporated',
                      colors=c('darkorange', 'forestgreen'),
                      axis.title = c(x_label, y_label)) +
      geom_vline(xintercept = c(10.78932, 11.30958), linetype = "dashed", color = "black") +
      annotate('text', x=10.5, y=0.33, label='DAC')+
      annotate('text', x=11.8, y=0.33, label='non-DAC')+
      theme_minimal() +
      scale_x_continuous(
         name = "ln(MHI)",
         sec.axis = sec_axis(
            trans = ~ exp(.),
            name = "MHI",
            breaks = exp(seq(10, 12, by = 1)),
            labels = label_comma() # Use commas for non-scientific formatting
         )
      )
   
   return(plot)
}

# Calling the function for each combination with customized axis labels and axis limits
plots <- list(
   create_model_plot(x_var = "MHI_log", 
                     y_var = "exists", 
                     data = all_places, 
                     family = 'binomial',
                     x_label = "ln(MHI)", 
                     y_label = "Exists (Binary)"), 
   
   create_model_plot(x_var = "MHI_log", 
                     y_var = "deg_w", 
                     data = all_place_nodes, 
                     family = 'poisson',
                     x_label = "ln(MHI)", 
                     y_label = "Degree"),
   
   create_model_plot(x_var = "MHI_log", 
                     y_var = "leader_dist_nona", 
                     data = all_place_nodes, 
                     family = 'poisson',
                     x_label = "ln(MHI)", 
                     y_label = "Leader Distance (Min)"))


# Print plots
for (plot in plots) {
   print(plot)
}


results_plot <- ggarrange(plots[[1]], plots[[2]], plots[[3]], 
                          nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom",
                          labels = "AUTO"); results_plot

ggsave('EJ_DAC_Paper/Out/results_plot.png', results_plot, width = 10, height = 4)
