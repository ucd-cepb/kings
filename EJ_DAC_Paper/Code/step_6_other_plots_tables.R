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

setwd("/Users/ajguerra/Documents/projects/kings")
load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_2026")
extract_list <- list.files(network_fp)

place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))

   gsp_id <- paste0("gsp_",gsp_ids[g])

   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))

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
   formula_str <- as.formula(paste(y_var, "~", x_var, "+ incorporated + POP_log + per_latino"))
   model <- glm(formula_str, family = family, data = data)

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
            labels = label_comma()
         )
      )

   return(plot)
}

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


for (plot in plots) {
   print(plot)
}


results_plot <- ggarrange(plots[[1]], plots[[2]], plots[[3]],
                          nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom",
                          labels = "AUTO"); results_plot

ggsave('EJ_DAC_Paper/Out/results_plot.png', results_plot, width = 10, height = 4)
