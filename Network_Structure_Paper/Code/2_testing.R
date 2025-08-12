
library(dotenv)
library(sf)
library(tidyverse)
library(knitr)
library(broom)
library(statnet)
library(ca)
library(skimr)
library(dotenv)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), 
                     "/network_structure_by_plan/networks_fully_labeled")
extract_list = list.files(network_fp)

i <- 1
net_i <- readRDS(paste0(network_fp, "/", extract_list[i]))
net_i_df_edges <- network::as.data.frame.network(net_i, unit='edges')
net_i_df_verts <- network::as.data.frame.network(net_i, unit='vertices')

net_i_igraph <- graph <- igraph::graph_from_data_frame(net_i_df_edges,
                                                       vertices = net_i_df_verts)






gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
# gsp_ids <- setdiff(gsp_ids, "112")

gsp_shp <- paste0(Sys.getenv("BOX_PATH"), 
                  "/EJ_Paper/dac_shapefiles/gsp/gsp.shp")
gsp <- st_read(gsp_shp)

gsp_meta <- read_csv(paste0(Sys.getenv("BOX_PATH"), 
                            "/Structural_Topic_Model_Paper/gsp_ids_with_metadata.csv")) %>% 
   select(c(GSP.ID, priority, gwsum, Agr_Share_Of_GDP_scaled, 
            local_govs_per_10k_people_log_scaled, urbangw_af_log_scaled)) %>%
   mutate(
      priority = case_when(
         priority == "High" ~ 3,
         priority == "Medium" ~ 2,
         priority == "Low" ~ 1,
         priority == "Very Low" ~ 0),
      # working definition for GSPs predicted to be defensive
      pred_def = ifelse(gwsum >= 2 &
                           priority >= 2 &
                           Agr_Share_Of_GDP_scaled > median(Agr_Share_Of_GDP_scaled, 
                                                            na.rm = TRUE),
                        1, 0)
   ) %>% 
   filter(GSP.ID %in% gsp_ids)

gsp_plot <- gsp %>%
   left_join(gsp_meta, by = c("GSP_ID" = "GSP.ID"))

ggplot(data = gsp_plot) +
   geom_sf(aes(fill = pred_def)) +
   scale_fill_viridis_c() + 
   theme_minimal()


### NETWORK STATISTICS

decay_param <- 0.3

df <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   
   # set org_type of NA to 'other' and split into 'org' or 'GSA'
   net %v% 'org_type' <- ifelse(is.na(net %v% 'org_type'), 'not_org', 'org')
   net %v% 'org_type' <- ifelse(net %v% 'GSA' == 1, 'GSA', net %v% 'org_type')
   
   gsp_id <- as.numeric(gsp_ids[g])
   estats <- summary(net ~ 
                     edges+
                        
                     # hypo 1, in/out between GSA and non-GSA
                     nodemix('GSA') +
                     nodemix('org_type')+
                        
                     # hypo 2, transitivitiy 
                     
                     triangles()+
                     ttriple()+
                     ctriple()+
                        
                     triangles('org_type', diff=TRUE)+
                     ttriple('org_type', diff=TRUE)+
                     ctriple('org_type', diff=TRUE)+
                     
                     # hypo 3, mutual
                     mutual() +
                     mutual(same='GSA')+
                     mutual(same='org_type') 
   )
   
   rowx <- pivot_wider(cbind(gsp_id, tidy(estats)),
                       id_cols = gsp_id,
                       names_from = names,
                       values_from = x)
   
   mstats <- data.frame(n = network.size(net),
                        m = network.edgecount(net),
                        net_density = gden(net),
                        net_diameter = max(ifelse(is.infinite(geodist(net)$gdist), 
                                                  0, 
                                                  geodist(net)$gdist)),
                        net_reciprocity = grecip(net)
   )
   
   rowx <- bind_cols(rowx, mstats) 
   
   df <- bind_rows(df, rowx)
}

df <- tibble(df) 

dfx <- df %>% 
   #make all na values 0
   # mutate(across(everything(), ~replace_na(., 0))) %>% 
   left_join(gsp_meta, by = c("gsp_id" = "GSP.ID"))


### GROUPED SUMMARY STATISTICS BASED ON PREDICTED DEFENSIVE STATUS

skim(dfx %>% group_by(pred_def))

# Function to create a ggplot for a given column
calculate_summary_and_test <- function(data, column) {
   summary_stats <- data %>%
      group_by(pred_def) %>%
      summarise(
         mean = mean(.data[[column]], na.rm = TRUE),
         se = sd(.data[[column]], na.rm = TRUE) / sqrt(n())
      )
   
   t_test_result <- t.test(data[[column]] ~ data$pred_def, na.rm = TRUE)
   
   
   list(summary_stats = summary_stats, p_value = t_test_result$p.value)
}

# Function to create a ggplot for a given column and add t-test p-value in subtitle
plot_column <- function(column_name) {
   results <- calculate_summary_and_test(dfx, column_name)
   
   p_value_text <- ifelse(results$p_value < 0.05, 
                          paste("p-value:", 
                                signif(results$p_value, 3), 
                                "(Significant)"), 
                          paste("p-value:", 
                                signif(results$p_value, 3), 
                                "(Not Significant)"))
   
   ggplot(dfx, aes_string(x = 'pred_def', y = column_name, fill = 'pred_def')) +
      stat_summary(fun = mean, geom = "bar", position = "dodge") +
      stat_summary(fun.data = mean_se, 
                   geom = "errorbar", 
                   width = 0.2, 
                   position = position_dodge(0.9)) +
      labs(
         y = column_name, 
         title = paste("Mean of", column_name, "by pred_def"),
         subtitle = p_value_text
      ) +
      theme_minimal()
}

plot_cols <- setdiff(colnames(dfx), 
                     c("gsp_id", "pred_def", 
                       colnames(gsp_meta),
                       colnames(mstats)
                       ) )

lapply(plot_cols, plot_column)

plots <- lapply(plot_cols, plot_column)

for (p in plots) {
   # save image
   ggsave(paste0("Network_Structure_Paper/Out/struc_stats/", p$labels$y, ".png"), 
          p, 
          width = 16,
          height = 9)
}


### CORRELATION MATRIX

dfx %>%
   drop_na() %>% 
   select(-c(gsp_id)) %>% 
   cor() %>%
   corrplot::corrplot('shade',
                      type = 'lower')


### PCA

pca_df <- df %>%
   filter(gsp_id != 112) %>%
   scale() %>% 
   princomp()

summary(pca_df)

gsp_ids_temp <- gsp_ids[which(gsp_ids != 112)]

plotdf <- as.data.frame(pca_df$scores[,1:2]) %>% 
   tibble() %>% 
   rename(PC1 = `Comp.1`, 
          PC2 = `Comp.2`) %>% 
   mutate(gsp_id = as.numeric(gsp_ids_temp)) %>%
   left_join(gsp_meta, by = c("gsp_id" = "GSP.ID"))

ggplot(plotdf, aes(x = PC1, y = PC2, color = pred_def)) +
   geom_point() +
   theme_minimal()

