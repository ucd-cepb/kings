# step_6_other_plots_tables.R
# Tables and figures from the final sample and models:
#   - Table 2: descriptives of the network-node sample (places named in their plan, with DAC data)
#   - sample flow: pairs -> with DAC data -> in network (H1 = 1; H2 / H3 sample) -> path to a GSA, by incorporation
#   - model-implied quantities by DAC x incorporated (fixed effects only, averaged over each model's sample)
#   - Figure 2: model-implied inclusion, degree, and distance to leadership over ln(MHI), by incorporation
# Run from the project root (kings.Rproj).

library(tidyverse)
library(glmmTMB)
library(ordinal)
library(skimr)
library(scales)
library(ggpubr)

data_dir <- "EJ_DAC_Paper/Data/"
mod_dir <- "EJ_DAC_Paper/Out/mods/"
out_dir <- "EJ_DAC_Paper/Out/"

community_plan <- read_csv(paste0(data_dir, "community_plan.csv"))

all_places <- community_plan %>%
   # DAC data not available: no ACS income for the place
   filter(!is.na(DAC)) %>%
   # centered as in step_5a-c
   mutate(GSP_ID = as.factor(GSP_ID),
          MHI_log = log(MHI) - mean(log(MHI)),
          POP_log = log(POP),
          pages_log = log(pages))

# network nodes only
place_nodes <- all_places %>% filter(included == 1)

mhi_center <- mean(log(all_places$MHI))

incl_mods <- readRDS(paste0(mod_dir, "h1_inclusion.RDS"))
deg_mods <- readRDS(paste0(mod_dir, "h2_degree.RDS"))
lead_mods <- readRDS(paste0(mod_dir, "h3_leader.RDS"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data summary table
# table 2 in section 3.3

data_summary <- place_nodes %>%
   select(MHI, POP, DAC, incorporated, per_latino, pages, degree, reach) %>%
   rename("MHI" = MHI,
          "Population" = POP,
          "DAC" = DAC,
          "Incorporated" = incorporated,
          "% Latino" = per_latino,
          "Plan pages" = pages,
          "Degree" = degree,
          "Path to a GSA" = reach) %>%
   skim() %>% tibble() %>%
   select(-c(skim_type, n_missing, complete_rate, numeric.hist,
             numeric.p0,  numeric.p25, numeric.p75, numeric.p100
             )) %>%
   rename("Variable" = skim_variable,
          "Mean" = numeric.mean,
          "SD" = numeric.sd,
          "Median" = numeric.p50) %>%
   mutate(across(where(is.numeric), ~ round(.x, 2)))

leader_summary <- place_nodes %>%
   count(incorporated, leader_cat) %>%
   group_by(incorporated) %>%
   mutate(share = round(n / sum(n), 3))

data_summary
leader_summary

write_csv(data_summary, paste0(out_dir, "data_summary.csv"))
write_csv(leader_summary, paste0(out_dir, "table2_leader_cat.csv"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sample flow

sample_flow <- community_plan %>%
   group_by(incorporated) %>%
   summarise(pairs = n(),
             with_dac_data = sum(!is.na(DAC)),
             in_network = sum(included == 1 & !is.na(DAC)),
             any_edge = sum(any_edge == 1 & !is.na(DAC), na.rm = TRUE),
             path_to_gsa = sum(reach == 1 & !is.na(DAC), na.rm = TRUE)) %>%
   bind_rows(summarise(., across(everything(), sum)) %>% mutate(incorporated = NA)) %>%
   mutate(incorporated = case_when(incorporated == 1 ~ "incorporated",
                                   incorporated == 0 ~ "unincorporated",
                                   TRUE ~ "total"))

sample_flow

write_csv(sample_flow, paste0(out_dir, "sample_flow.csv"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model-implied quantities (fixed effects only, averaged over each model's sample)

# H1 and H2: mean predicted response
mean_response <- function(model, new_data) {
   mean(predict(model, new_data, type = "response", re.form = NA))
}

# H3: mean probability of each leader_cat (clmm has no predict method)
ordinal_probs <- function(model, new_data) {
   X <- model.matrix(reformulate(names(model$beta)), new_data)[, names(model$beta), drop = FALSE]
   eta <- drop(X %*% model$beta)
   p_at_or_below <- sapply(model$Theta, function(theta) plogis(theta - eta))
   p_cat <- cbind(p_at_or_below, 1) - cbind(0, p_at_or_below)
   colnames(p_cat) <- levels(model$model$leader_cat)
   colMeans(p_cat)
}

cells <- expand_grid(DAC = c(0, 1), incorporated = c(0, 1))

cell_preds <- map2_dfr(cells$DAC, cells$incorporated, function(dac, inc) {
   tibble(DAC = dac,
          incorporated = inc,
          included = mean_response(incl_mods[["(1)"]],
                                   all_places %>% mutate(DAC = dac, incorporated = inc)),
          degree = mean_response(deg_mods[["(1)"]],
                                 place_nodes %>% mutate(DAC = dac, incorporated = inc)),
          !!!ordinal_probs(lead_mods[["(1)"]],
                           place_nodes %>% mutate(DAC = dac, incorporated = inc)))
})

cell_preds

write_csv(cell_preds, paste0(out_dir, "predictions_dac_incorporated.csv"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT - FIGURE 2 in RESULTS
# ln(MHI) models (2); lines differ in slope through the ln(MHI) x incorporated term
# panel C: "far from a GSA" = leader_cat 3+ or unreachable (the two farthest categories)

mhi_grid <- expand_grid(MHI_log = seq(min(all_places$MHI_log), max(all_places$MHI_log), length.out = 50),
                        incorporated = c(0, 1))

mhi_preds <- map2_dfr(mhi_grid$MHI_log, mhi_grid$incorporated, function(mhi, inc) {
   lead_probs <- ordinal_probs(lead_mods[["(2)"]],
                               place_nodes %>% mutate(MHI_log = mhi, incorporated = inc))

   tibble(MHI_log = mhi + mhi_center,
          incorporated = as.factor(inc),
          included = mean_response(incl_mods[["(2)"]],
                                   all_places %>% mutate(MHI_log = mhi, incorporated = inc)),
          degree = mean_response(deg_mods[["(2)"]],
                                 place_nodes %>% mutate(MHI_log = mhi, incorporated = inc)),
          far = lead_probs[["3+"]] + lead_probs[["unreachable"]])
})

# mean ln(MHI) of DAC and non-DAC places
dac_lines <- all_places %>%
   group_by(DAC) %>%
   summarise(MHI_log = mean(log(MHI)))

create_model_plot <- function(y_var, y_label) {
   plot <- ggplot(mhi_preds, aes(x = MHI_log, y = .data[[y_var]], color = incorporated)) +
      geom_line(linewidth = 1) +
      scale_color_manual(name = 'Incorporated',
                         values = c('darkorange', 'forestgreen'),
                         labels = c('0', '1')) +
      geom_vline(xintercept = dac_lines$MHI_log, linetype = "dashed", color = "black") +
      annotate('text', x = dac_lines$MHI_log[dac_lines$DAC == 1], y = -Inf, hjust = 1.1, vjust = -0.5, label = 'DAC') +
      annotate('text', x = dac_lines$MHI_log[dac_lines$DAC == 0], y = -Inf, hjust = -0.1, vjust = -0.5, label = 'non-DAC') +
      labs(y = y_label) +
      theme_minimal() +
      scale_x_continuous(
         name = "ln(MHI)",
         sec.axis = sec_axis(
            transform = ~ exp(.),
            name = "MHI",
            breaks = c(25000, 50000, 100000, 200000),
            labels = label_comma()
         )
      )

   return(plot)
}

plots <- list(
   create_model_plot(y_var = "included",
                     y_label = "Included (Prob.)"),

   create_model_plot(y_var = "degree",
                     y_label = "Degree (Expected)"),

   create_model_plot(y_var = "far",
                     y_label = "Far from a GSA (Prob.)"))


for (plot in plots) {
   print(plot)
}


results_plot <- ggarrange(plots[[1]], plots[[2]], plots[[3]],
                          nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom",
                          labels = "AUTO"); results_plot

ggsave(paste0(out_dir, 'results_plot.png'), results_plot, width = 10, height = 4)
