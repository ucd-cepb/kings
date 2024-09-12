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

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   
   gsp_id <- paste0("gsp_",gsp_ids[g])
   
   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   place_nodes <- nodes %>% 
      mutate(
         MHI_std = MHI/10000,
         POP_std = POP/1000000,
         is_place = ifelse(is.na(GEOID20), 0, 1)
      ) %>% 
      filter(is_place == 1) 
   
   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}
# data summary table

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

# DOTPLOT (LINE GRAPH)
{
in_p <- ggplot() +
   geom_point(data=apn_graph_1, aes(x = in_w, y = y_val, color = color)) +
   geom_point(data=group_means, aes(x = in_w, y = y_val, color = color, size=5), show.legend = c(size=FALSE)) +
   scale_color_manual(name = 'Color', values = named_palette, breaks=names) +
   xlab('Weighted Indegree') +
   xlim(c(0,20))+
   theme_minimal() +
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank()); in_p

out_p <- ggplot() +
   geom_point(data=apn_graph_1, aes(x = out_w, y = y_val, color = color)) +
   geom_point(data=group_means, aes(x = out_w, y = y_val, color = color, size=5,), show.legend = c(size=FALSE)) +
   scale_color_manual(values = named_palette, breaks=names) +
   xlab('Weighted Outdegree') +
   xlim(c(0,20))+
   theme_minimal() +
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank()); out_p

lead_p <- ggplot() +
   geom_point(data=apn_graph_1, aes(x = leader_dist_min_w_nona, y = y_val, color = color)) +
   geom_point(data=group_means, aes(x = leader_dist, y = y_val, color = color, size=5), show.legend = c(size=FALSE)) +
   scale_color_manual(values = named_palette, breaks=names) +
   xlab('Leader Distance')+
   xlim(c(0,20))+
   theme_minimal() +
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank()); lead_p

dims_p <- ggarrange(in_p, out_p, lead_p, 
                    nrow = 3, 
                    common.legend = TRUE, 
                    legend = "bottom",
                    labels = "AUTO"); dims_p
ggsave('EJ_DAC_Paper/Out/dims_p.png', dims_p, width = 8.2, height = 7)

   }

# HISTOGRAM
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

# OLD DOTPLOT (no background dots)
{
   
palette2 <-  brewer.pal(n = 4, name = "Set1")
   
in_p_old <- ggplot(group_means, aes(x=in_w, y=1, color=color))+
   geom_point()+
   scale_color_manual(values = palette2) +
   xlab('Weighted Indegree')+
   theme_minimal()+
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank())

out_p_old <- ggplot(group_means, aes(x=out_w, y=1, color=color))+
   geom_point()+
   scale_color_manual(values = palette2) +
   xlab('Weighted Outdegree')+
   theme_minimal()+
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank())

lead_p_old <- ggplot(group_means, aes(x=leader_dist, y=1, color=color))+
   geom_point()+
   scale_color_manual(values = palette2) +
   xlab('Leader Distance')+
   theme_minimal()+
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank())

dims_p_old <- ggarrange(in_p_old, out_p_old, lead_p_old, 
          nrow = 3, 
          common.legend = TRUE, 
          legend = "bottom",
          labels = "AUTO") 
}

palette <- c(brewer.pal(n = 4, name = "Pastel1"), brewer.pal(n = 4, name = "Set1"))

# Create the 3D scatter plot with pastel colors for individual points and dark colors for means
p <- plot_ly() %>%
   add_markers(data = apn_graph_1,
               x = ~in_w, 
               y = ~out_w, 
               z = ~leader_dist_min_w_nona,
               marker = list(size = 8,
                             opacity = 0.7),
               color = ~color,
               colors = named_palette
               ) %>%
   add_markers(inherit=FALSE,
               data = group_means,
               x = ~in_w, y = ~out_w, z = ~leader_dist,
               marker = list(size = 10, 
                             opacity = 1),
               color=~color
               ) %>%
   # Set axis labels
   layout(scene = list(
      xaxis = list(title = 'Indegree', range = c(0, 20)),
      yaxis = list(title = 'Outdegree', range = c(0, 20)),
      zaxis = list(title = 'Leader Distance', range = c(0,20))
   ), 
   legend = list(orientation = 'h', x = 0, y = 0)
   )

p

# interaction plot for in_w, out_w

apn_graph_2 <- all_place_nodes %>% 
   mutate(incorporated = as.factor(incorporated))

int_in <- glm(`in_w` ~ MHI_std*incorporated+
                 POP_std+
                 per_latino,
              family=poisson,
              data = apn_graph_2)


int_plot_in <- plot_model(int_in, type = 'int', 
                          terms = c('incorporated', 'MHI'),
                          title=" ",
                          legend.title = 'Incorporated',
                          axis.title = c('MHI (1,000 $)', 'Indegree'),
                          axis.lim=list(c(0,10),
                                        c(0,10)))+
   geom_vline(xintercept = c(4.85, 8.16), linetype = "dashed", color = "black") +
   theme_minimal(); int_plot_in

int_out <- glm(`out_w` ~ MHI_std*incorporated+
                  POP_std+
                  per_latino,
               family=poisson,
               data = apn_graph_2)

int_plot_out <- plot_model(int_out, type = 'int', 
                           terms = c('incorporated', 'MHI'),
                           title=" ",
                           legend.title = 'Incorporated',
                           axis.title = c('MHI (1,000 $)', 'Outdegree'),
                           axis.lim=list(c(0,10),
                                         c(0,10)))+
   geom_vline(xintercept = c(4.85, 8.16), linetype = "dashed", color = "black") +
   theme_minimal(); int_plot_out

int_plot <- ggarrange(int_plot_in, int_plot_out, 
                      ncol = 2, common.legend = TRUE, legend = "bottom",
                      labels = "AUTO"); int_plot

ggsave('EJ_DAC_Paper/Out/int_plot.png', int_plot, width = 7, height = 4)