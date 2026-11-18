# step_6_other_plots_tables.R
# Tables and figures from the final sample and models:
#   - Table 2: descriptives of the network-node sample (places named in their plan, with DAC data)
#   - sample flow: pairs -> with DAC data -> in network (H1 = 1; H2 / H3 sample) -> path to a GSA, by incorporation
#   - model-implied quantities by DAC x incorporated (fixed effects only, averaged over each model's sample)
#   - model tables for H1-H3 in one Word file (for Google Docs)
#   - Figure 2: model-implied inclusion, degree, and distance to leadership over ln(MHI), by incorporation
# Run from the project root (kings.Rproj).

library(tidyverse)
library(glmmTMB)
library(ordinal)
library(skimr)
library(scales)
library(ggpubr)

source("EJ_DAC_Paper/Code/utils/model_tables.R")

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
# model tables (utils/model_tables.R), one Word file for Google Docs

save_as_docx("H1. Inclusion in the plan network (mixed logit)" = make_table(incl_mods),
             "H2. Degree (mixed negative binomial)" = make_table(deg_mods),
             "H3. Distance to leadership (mixed ordinal logit)" = make_table(lead_mods),
             path = paste0(mod_dir, "model_tables.docx"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model-implied quantities (fixed effects only, averaged over each model's sample)

# fixed-effect coefficients and their vcov (clmm: thresholds, then betas)
fixed_coefs <- function(model) {
   if (inherits(model, "clmm")) {
      b <- c(model$alpha, model$beta)
      list(b = b,
           V = vcov(model)[names(b), names(b)])
   } else {
      list(b = fixef(model)$cond,
           V = vcov(model)$cond)
   }
}

# B: one row of coefficients per draw (default: the point estimates)

# H1 and H2: mean predicted response, one value per row of B
mean_response <- function(model, new_data, B = t(fixed_coefs(model)$b)) {
   X <- model.matrix(reformulate(colnames(B)[-1]), new_data)[, colnames(B), drop = FALSE]
   colMeans(family(model)$linkinv(X %*% t(B)))
}

# H3: mean probability of each leader_cat, one row per row of B (clmm has no predict method)
ordinal_probs <- function(model, new_data, B = t(fixed_coefs(model)$b)) {
   X <- model.matrix(reformulate(names(model$beta)), new_data)[, names(model$beta), drop = FALSE]
   eta <- X %*% t(B[, names(model$beta), drop = FALSE])
   p_at_or_below <- sapply(names(model$alpha), function(theta) {
      colMeans(plogis(sweep(-eta, 2, B[, theta], "+")))
   })
   p_at_or_below <- matrix(p_at_or_below, nrow = nrow(B))
   p_cat <- cbind(p_at_or_below, 1) - cbind(0, p_at_or_below)
   colnames(p_cat) <- levels(model$model$leader_cat)
   p_cat
}

cells <- expand_grid(DAC = c(0, 1), incorporated = c(0, 1))

cell_preds <- map2_dfr(cells$DAC, cells$incorporated, function(dac, inc) {
   tibble(DAC = dac,
          incorporated = inc,
          included = mean_response(incl_mods[["(1)"]],
                                   all_places %>% mutate(DAC = dac, incorporated = inc)),
          degree = mean_response(deg_mods[["(1)"]],
                                 place_nodes %>% mutate(DAC = dac, incorporated = inc)),
          as_tibble(ordinal_probs(lead_mods[["(1)"]],
                                  place_nodes %>% mutate(DAC = dac, incorporated = inc))))
})

cell_preds

write_csv(cell_preds, paste0(out_dir, "predictions_dac_incorporated.csv"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT - FIGURE 2 in RESULTS
# ln(MHI) models (2); lines differ in slope through the ln(MHI) x incorporated term
# panel C: "far from a GSA" = leader_cat 3+ or unreachable (the two farthest categories)

# 5th-95th percentile of ln(MHI): predictions past the tails rest on few places
mhi_range <- quantile(all_places$MHI_log, c(0.05, 0.95))

mhi_grid <- expand_grid(MHI_log = seq(mhi_range[[1]], mhi_range[[2]], length.out = 50),
                        incorporated = c(0, 1))

# model (2) coefficients: point estimates, and draws from their sampling distribution
# (fixed effects only; the draws carry coefficient uncertainty into the predictions)
mods_2 <- list(incl = incl_mods[["(2)"]],
               deg = deg_mods[["(2)"]],
               lead = lead_mods[["(2)"]])

point_coefs <- map(mods_2, ~ t(fixed_coefs(.x)$b))

set.seed(20261110)
draw_coefs <- map(mods_2, function(model) {
   coefs <- fixed_coefs(model)
   MASS::mvrnorm(1000, coefs$b, coefs$V)
})

# model (2) predictions at one centered ln(MHI) value and incorporation status,
# one row per row of coefficients
mhi_predictions <- function(mhi, inc, coefs = point_coefs) {
   lead_probs <- ordinal_probs(mods_2$lead,
                               place_nodes %>% mutate(MHI_log = mhi, incorporated = inc),
                               coefs$lead)

   tibble(MHI_log = mhi + mhi_center,
          incorporated = as.factor(inc),
          included = mean_response(mods_2$incl,
                                   all_places %>% mutate(MHI_log = mhi, incorporated = inc),
                                   coefs$incl),
          degree = mean_response(mods_2$deg,
                                 place_nodes %>% mutate(MHI_log = mhi, incorporated = inc),
                                 coefs$deg),
          unreachable = lead_probs[, "unreachable"],
          far = lead_probs[, "3+"] + lead_probs[, "unreachable"])
}

# 95% intervals: 2.5th and 97.5th percentiles of the predictions over the draws
mhi_intervals <- function(mhi, inc) {
   mhi_predictions(mhi, inc, draw_coefs) %>%
      group_by(MHI_log, incorporated) %>%
      summarise(across(c(included, degree, far),
                       list(low = ~ quantile(.x, 0.025),
                            high = ~ quantile(.x, 0.975))),
                .groups = "drop")
}

mhi_preds <- map2_dfr(mhi_grid$MHI_log, mhi_grid$incorporated, mhi_predictions) %>%
   left_join(map2_dfr(mhi_grid$MHI_log, mhi_grid$incorporated, mhi_intervals),
             by = c("MHI_log", "incorporated"))

# mean ln(MHI) of DAC and non-DAC places
dac_lines <- all_places %>%
   group_by(DAC) %>%
   summarise(MHI_log = mean(log(MHI)))

create_model_plot <- function(y_var, y_label) {
   plot <- ggplot(mhi_preds, aes(x = MHI_log, y = .data[[y_var]],
                                 color = incorporated, fill = incorporated)) +
      geom_ribbon(aes(ymin = .data[[paste0(y_var, "_low")]],
                      ymax = .data[[paste0(y_var, "_high")]]),
                  alpha = 0.2, color = NA) +
      geom_line(linewidth = 1) +
      scale_color_manual(name = NULL,
                         values = c('darkorange', 'forestgreen'),
                         labels = c('Unincorporated', 'Incorporated'),
                         aesthetics = c("color", "fill")) +
      geom_vline(xintercept = dac_lines$MHI_log, linetype = "dashed", color = "black") +
      annotate('text', x = dac_lines$MHI_log[dac_lines$DAC == 1], y = Inf,
               hjust = 1.1, vjust = 1.5, label = 'DAC') +
      annotate('text', x = dac_lines$MHI_log[dac_lines$DAC == 0], y = Inf,
               hjust = -0.1, vjust = 1.5, label = 'non-DAC') +
      labs(y = y_label) +
      theme_minimal() +
      scale_x_continuous(
         name = "ln(MHI)",
         breaks = seq(10.4, 11.8, by = 0.2),
         sec.axis = sec_axis(
            transform = ~ exp(.),
            name = "MHI",
            breaks = c(35000, 50000, 70000, 100000),
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NUMBERS REPORTED IN THE TEXT
# With the interaction, each main coefficient is the effect at the other variable = 0:
#   incorporated = effect for non-DAC places (models 1) or at mean ln(MHI) (models 2)
#   DAC / ln(MHI) = effect for unincorporated places
# Effects at other values are sums of coefficients, with SEs from the model vcov.
#   - incorporated for non-DAC and DAC places (models 1)
#   - incorporated at the mean ln(MHI) of non-DAC and DAC places (models 2)
#   - doubling MHI, for unincorporated and incorporated places (models 2)
#     (doubling is about the gap between DAC and non-DAC mean MHI)
# ratio: odds ratio (H1, H3) or incidence rate ratio (H2); H3 ratio < 1 = closer to a GSA

# effect = sum(weights x coefficients), e.g. c(incorporated = 1, "DAC:incorporated" = 1)
lincom <- function(model, weights) {
   coefs <- fixed_coefs(model)
   stopifnot(all(names(weights) %in% names(coefs$b)))

   w <- setNames(rep(0, length(coefs$b)), names(coefs$b))
   w[names(weights)] <- weights
   estimate <- sum(w * coefs$b)
   se <- sqrt(drop(t(w) %*% coefs$V %*% w))

   tibble(estimate = estimate,
          se = se,
          p = 2 * pnorm(-abs(estimate / se)),
          ratio = exp(estimate),
          ratio_low = exp(estimate - 1.96 * se),
          ratio_high = exp(estimate + 1.96 * se))
}

# centered ln(MHI) at the DAC and non-DAC means
mhi_dac <- dac_lines$MHI_log[dac_lines$DAC == 1] - mhi_center
mhi_nondac <- dac_lines$MHI_log[dac_lines$DAC == 0] - mhi_center

hypo_mods <- list("H1 inclusion" = incl_mods,
                  "H2 degree" = deg_mods,
                  "H3 leader distance" = lead_mods)

text_effects <- map_dfr(names(hypo_mods), function(h) {
   mods <- hypo_mods[[h]]

   bind_rows(
      lincom(mods[["(1)"]], c(incorporated = 1)) %>%
         mutate(effect = "incorporated", at = "non-DAC"),
      lincom(mods[["(1)"]], c(incorporated = 1, "DAC:incorporated" = 1)) %>%
         mutate(effect = "incorporated", at = "DAC"),
      lincom(mods[["(2)"]], c(incorporated = 1, "MHI_log:incorporated" = mhi_nondac)) %>%
         mutate(effect = "incorporated", at = "non-DAC mean MHI"),
      lincom(mods[["(2)"]], c(incorporated = 1, "MHI_log:incorporated" = mhi_dac)) %>%
         mutate(effect = "incorporated", at = "DAC mean MHI"),
      lincom(mods[["(2)"]], c(MHI_log = log(2))) %>%
         mutate(effect = "MHI doubled", at = "unincorporated"),
      lincom(mods[["(2)"]], c(MHI_log = log(2), "MHI_log:incorporated" = log(2))) %>%
         mutate(effect = "MHI doubled", at = "incorporated")
   ) %>%
      mutate(hypothesis = h)
}) %>%
   select(hypothesis, effect, at, everything()) %>%
   mutate(across(where(is.numeric), ~ round(.x, 3)))

# the same contrasts on the outcome scale (model 2, fixed effects only, no uncertainty)
#   included = Pr(named in plan), degree = expected ties, unreachable / far = Pr(no path / 3+ or no path)
text_levels <- expand_grid(at = c("DAC mean MHI", "non-DAC mean MHI"),
                           incorporated = c(0, 1)) %>%
   mutate(mhi = ifelse(at == "DAC mean MHI", mhi_dac, mhi_nondac)) %>%
   mutate(preds = map2(mhi, incorporated, mhi_predictions)) %>%
   select(at, preds) %>%
   unnest(preds) %>%
   mutate(MHI = round(exp(MHI_log)),
          across(c(included, degree, unreachable, far), ~ round(.x, 3))) %>%
   select(at, MHI, incorporated, included, degree, unreachable, far)

text_effects
text_levels

write_csv(text_effects, paste0(out_dir, "text_effects.csv"))
write_csv(text_levels, paste0(out_dir, "text_levels.csv"))
