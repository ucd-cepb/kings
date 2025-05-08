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

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list <- list.files(network_fp)
place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   
   gsp_id <- paste0("gsp_",gsp_ids[g])
   
   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   place_nodes <- nodes %>% 
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
# table 2 in section 3.3

all_place_nodes %>% 
   select( MHI, POP, DAC, incorporated, per_latino, in_w, out_w, leader_dist_min_w_nona) %>%
   rename("MHI" = MHI,
          "Population" = POP,
          "DAC" = DAC,
          "% Latino" = per_latino,
          "Incorporated" = incorporated,
          "Indegree" = in_w,
          "Outdegree" = out_w,
          "Leader Distance" = leader_dist_min_w_nona) %>%
   skim() %>% tibble() %>% 
   select(-c(skim_type, n_missing, complete_rate, numeric.hist,
             numeric.p0,  numeric.p25, numeric.p75, numeric.p100
             )) %>%
   rename("Variable" = skim_variable,
          "Mean" = numeric.mean,
          "SD" = numeric.sd,
          "Median" = numeric.p50) %>% 
   mutate(across(where(is.numeric), ~ round(.x, 3))) %>%   
   write_csv("EJ_DAC_Paper/Out/data_summary.csv")


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
      theme_minimal() +
      scale_x_continuous(
         name = "log(MHI)",
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
                     x_label = "log(MHI)", 
                     y_label = "Exists (Binary)"), 
   
   create_model_plot(x_var = "MHI_log", 
                     y_var = "in_w", 
                     data = all_place_nodes, 
                     family = 'poisson',
                     x_label = "log(MHI)", 
                     y_label = "Indegree"),
   
   create_model_plot(x_var = "MHI_log", 
                     y_var = "out_w", 
                     data = all_place_nodes, 
                     family = 'poisson',
                     x_label = "log(MHI)", 
                     y_label = "Outdegree"),
   
   create_model_plot(x_var = "MHI_log", 
                     y_var = "leader_dist_min_w_nona", 
                     data = all_place_nodes, 
                     family = 'poisson',
                     x_label = "log(MHI)", 
                     y_label = "Leader Distance (Min)"))


# Print plots
for (plot in plots) {
   print(plot)
}


results_plot <- ggarrange(plots[[1]], plots[[4]], plots[[2]], plots[[3]], 
                          nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom",
                          labels = "AUTO"); results_plot

ggsave('EJ_DAC_Paper/Out/results_plot.png', results_plot, width = 7, height = 6)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OLD CODE for plots

# SETUP
{
   # dimension plots
   
   group_means <- all_place_nodes %>% 
      group_by(incorporated, DAC) %>%
      summarize(in_w = mean(in_w),
                out_w = mean(out_w),
                leader_dist = mean(leader_dist_min_w_nona)) %>% 
      ungroup() %>% 
      mutate(d_lab = case_when(DAC == 0 ~ 'non-DAC',
                               DAC == 1 ~ 'DAC',
                               TRUE ~ 'NA'),
             i_lab = case_when(incorporated == 0 ~ 'Unincorporated',
                               incorporated == 1 ~ 'Incorporated',
                               TRUE ~ 'NA'),
             color = paste0('Mean ', i_lab, ' ', d_lab),
             y_val = as.numeric(factor(color)))
   
   # Define the palette
   names <- c(sort(unique(apn_graph_1$color)), sort(unique(group_means$color)))
   palette <- c(brewer.pal(n = 4, name = "Pastel1"), brewer.pal(n = 4, name = "Set1"))
   named_palette <- setNames(palette, names)
   
   apn_graph_1 <- all_place_nodes %>%
      mutate(d_lab = case_when(DAC == 0 ~ 'non-DAC',
                               DAC == 1 ~ 'DAC',
                               TRUE ~ 'NA'),
             i_lab = case_when(incorporated == 0 ~ 'Unincorporated',
                               incorporated == 1 ~ 'Incorporated',
                               TRUE ~ 'NA'),
             color = paste0(i_lab, ' ', d_lab),
             y_val = as.numeric(factor(color))) 
   
   all_places <- bind_rows(place_existance) %>% 
      mutate(DAC = as.factor(DAC),
             incorporated = as.factor(incorporated),
             exists = as.factor(exists))
   
   exists_mod_3 <- glm(exists~DAC+incorporated+POP+per_latino, 
                       family = binomial,
                       data = all_places)
   
   ap_graph <- all_places %>% 
      drop_na(c(DAC, incorporated, POP, per_latino)) %>% 
      mutate(exists_prob = predict(exists_mod_3, type = 'response'),
             d_lab = case_when(DAC == 0 ~ 'non-DAC',
                               DAC == 1 ~ 'DAC',
                               TRUE ~ 'NA'),
             i_lab = case_when(incorporated == 0 ~ 'Unincorporated',
                               incorporated == 1 ~ 'Incorporated',
                               TRUE ~ 'NA'),
             color = paste0(i_lab, ' ', d_lab),
             y_val = as.numeric(factor(color)))
   
   ap_group_means <- ap_graph %>% 
      group_by(incorporated, DAC) %>%
      summarize(exists_prob = mean(exists_prob)) %>% 
      ungroup() %>% 
      mutate(d_lab = case_when(DAC == 0 ~ 'non-DAC',
                               DAC == 1 ~ 'DAC',
                               TRUE ~ 'NA'),
             i_lab = case_when(incorporated == 0 ~ 'Unincorporated',
                               incorporated == 1 ~ 'Incorporated',
                               TRUE ~ 'NA'),
             color = paste0('Mean ', i_lab, ' ', d_lab),
             y_val = as.numeric(factor(color)))}

# DOTPLOT (LINE GRAPH)
{
   
   exists_p <- ggplot() +
      geom_point(data=ap_graph, aes(x = exists_prob, y = y_val, color = color)) +
      geom_point(data=ap_group_means, aes(x = exists_prob, y = y_val, color = color, size=5), show.legend = c(size=FALSE)) +
      scale_color_manual(name = 'Color', values = named_palette, breaks=names) +
      xlab('Probability of Existence') +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom"); exists_p
   
   ggsave('EJ_DAC_Paper/Out/exists_p.png', exists_p, width = 8, height = 4)
   
   in_p <- ggplot() +
      geom_point(data=apn_graph_1, aes(x = in_w, y = y_val, color = color)) +
      geom_point(data=group_means, aes(x = in_w, y = y_val, color = color, size=5), show.legend = c(size=FALSE)) +
      scale_color_manual(name = 'Color', values = named_palette, breaks=names) +
      xlab('Weighted Indegree') +
      xlim(c(0,20))+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom"); in_p
   
   ggsave('EJ_DAC_Paper/Out/in_p.png', in_p, width = 8, height = 4)
   
   out_p <- ggplot() +
      geom_point(data=apn_graph_1, aes(x = out_w, y = y_val, color = color)) +
      geom_point(data=group_means, aes(x = out_w, y = y_val, color = color, size=5,), show.legend = c(size=FALSE)) +
      scale_color_manual(values = named_palette, breaks=names) +
      xlab('Weighted Outdegree') +
      xlim(c(0,20))+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom"); out_p
   
   ggsave('EJ_DAC_Paper/Out/out_p.png', out_p, width = 8, height = 4)
   
   lead_p <- ggplot() +
      geom_point(data=apn_graph_1, aes(x = leader_dist_min_w_nona, y = y_val, color = color)) +
      geom_point(data=group_means, aes(x = leader_dist, y = y_val, color = color, size=5), show.legend = c(size=FALSE)) +
      scale_color_manual(values = named_palette, breaks=names) +
      xlab('Leader Distance')+
      xlim(c(0,20))+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom"); lead_p
   
   ggsave('EJ_DAC_Paper/Out/lead_p.png', lead_p, width = 8, height = 4)
   
   dims_p <- ggarrange(exists_p, in_p, out_p, lead_p, 
                       nrow = 4, 
                       common.legend = TRUE, 
                       legend = "bottom",
                       labels = "AUTO"); dims_p
   
   ggsave('EJ_DAC_Paper/Out/dims_p.png', dims_p, width = 9.2, height = 9.5)
   
}

# OLD HISTOGRAM
{
   in_hist <- ggplot() +
      geom_density(data=apn_graph_1, 
                   aes(x = in_w, color = color, fill = color),
                   alpha = 1,
                   show.legend=c(color=FALSE)
      ) +
      geom_vline(data = group_means, 
                 aes(xintercept = in_w, color = color), 
                 linetype = "dashed",
                 show.legend = FALSE
      ) +
      xlab('Weighted Indegree') +
      ylab('Density')+
      xlim(c(0, 10)) +
      scale_color_manual(name = 'Color',values = named_palette, breaks = names) +
      scale_fill_manual(name = 'Color', values = named_palette, breaks = names) +
      theme_minimal() ; in_hist
   
   out_hist <- ggplot() +
      geom_density(data=apn_graph_1, 
                   aes(x = out_w, color = color, fill = color),
                   alpha = 1, 
                   show.legend=c(color=FALSE)
      ) +
      geom_vline(data = group_means, 
                 aes(xintercept = out_w, color = color), 
                 linetype = "dashed", 
                 show.legend = FALSE) +
      xlab('Weighted Outdegree') +
      ylab('Density')+
      xlim(c(0, 10)) +
      scale_color_manual(name = 'Color',values = named_palette, breaks = names) +
      scale_fill_manual(name = 'Color', values = named_palette, breaks = names) +
      theme_minimal() ; out_hist
   
   
   lead_hist <- ggplot() +
      geom_density(data=apn_graph_1, 
                   aes(x = leader_dist_min_w_nona, color = color, fill = color),
                   alpha = 1,
                   show.legend=c(color=FALSE)
      ) +
      geom_vline(data = group_means, 
                 aes(xintercept = leader_dist, color = color), 
                 linetype = "dashed",
                 show.legend = FALSE
      ) +
      xlab('Leader Distance') +
      ylab('Density')+
      xlim(c(0, 20)) +
      scale_color_manual(name = 'Color', values = named_palette, breaks = names) +
      scale_fill_manual(name = 'Color',values = named_palette, breaks = names) +
      theme_minimal() ; lead_hist
   
   dims_hist <- ggarrange(in_hist, out_hist, lead_hist, 
                          nrow = 3, 
                          common.legend = TRUE, 
                          legend = "bottom",
                          labels = "AUTO"); dims_hist
   
   ggsave('EJ_DAC_Paper/Out/dims_hist.png', dims_hist, width = 7, height = 5)
   
}

# 3D PLOT
{
   # 3d plot
   
   # Define the palette
   
   apn_graph_1 <- all_place_nodes %>%
      mutate(d_lab = case_when(DAC == 0 ~ 'non-DAC',
                               DAC == 1 ~ 'DAC',
                               TRUE ~ 'NA'),
             i_lab = case_when(incorporated == 0 ~ 'Unincorporated',
                               incorporated == 1 ~ 'Incorporated',
                               TRUE ~ 'NA'),
             color = paste0(i_lab, ' ', d_lab),
             y_val = as.numeric(factor(color))) 
   
   names <- sort(unique(apn_graph_1$color))
   palette <- c(brewer.pal(n = 6, name = "Paired"))[c(1,2,5,6)]
   named_palette <- setNames(palette, names)
   
   
   # Create the 3D scatter plot with pastel colors for individual points and dark colors for means
   p <- plot_ly() %>%
      add_markers(data = apn_graph_1,
                  x = ~in_w, 
                  y = ~out_w, 
                  z = ~leader_dist_min_w_nona,
                  marker = list(size = 5,
                                opacity = 0.5),
                  color = ~color,
                  colors = named_palette
      ) %>%
      # Set axis labels
      layout(scene = list(
         xaxis = list(title = 'Indegree', range = c(0, 20)),
         yaxis = list(title = 'Outdegree', range = c(0, 20)),
         zaxis = list(title = 'Leader Distance', range = c(0,20))
      ), 
      legend = list(orientation = 'h', x = 0, y = 0)
      )
   
   p}