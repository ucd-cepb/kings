library(dotenv)
library(terra)
library(tidyverse)
library(knitr)
library(broom)
library(statnet)
library(ca)
library(skimr)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/network_structure_by_plan/cleaned_extracts")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

gsp_shp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/dac_shapefiles/gsp/gsp.shp")
gsp <- st_read(gsp_shp)

gsp_meta <- read_csv(paste0(Sys.getenv("BOX_PATH"), "/Structural_Topic_Model_Paper/gsp_ids_with_metadata.csv")) %>% 
   select(c(GSP.ID, priority, gwsum, Agr_Share_Of_GDP_scaled, 
            local_govs_per_10k_people_log_scaled, urbangw_af_log_scaled)) %>%
   mutate(
      priority = case_when(
         priority == "High" ~ 3,
         priority == "Medium" ~ 2,
         priority == "Low" ~ 1,
         priority == "Very Low" ~ 0),
      pred_def = ifelse(gwsum >= 2 &
                           priority >= 2 &
                           Agr_Share_Of_GDP_scaled > 0, 
                        1, 0),
   )

gsp_plot <- gsp %>% 
   left_join(gsp_meta, by = c("GSP_ID" = "GSP.ID"))

ggplot(data = gsp_plot) +
   geom_sf(aes(fill = pred_def)) +
   scale_fill_viridis_c() +  # Optional: use a color scale like viridis
   theme_minimal() 

df <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- as.numeric(gsp_ids[g])
   estats <- summary(net ~ 
                        # edges+
                        # mutual+
                        # nodematch('GSA')+
                        gwesp(0.5, fixed=TRUE)+
                        nodemix('GSA')
   )
   rowx <- pivot_wider(cbind(gsp_id, tidy(estats)),
                       id_cols = gsp_id,
                       names_from = names,
                       values_from = x)
   mstats <- data.frame(n = network.size(net),
                        m = network.edgecount(net),
                        net_density = gden(net),
                        net_diameter = max(ifelse(is.infinite(geodist(net)$gdist), 0, geodist(net)$gdist)),
                        net_reciprocity = grecip(net)
   )
   rowx <- bind_cols(rowx, mstats)
   df <- bind_rows(df, rowx)
}

dfx <- df %>% 
   as_tibble() %>% 
   #make all na values 0
   mutate(across(everything(), ~replace_na(., 0))) 

### CORRELATION MATRIX

dfx %>%
   left_join(gsp_meta, by = c("gsp_id" = "GSP.ID")) %>% 
   select(-c(gsp_id)) %>% 
   cor() %>%
   corrplot::corrplot()


### PCA OPTION

pca_df <- dfx %>%
   select(-gsp_id) %>% 
   scale() %>% 
   princomp()

summary(pca_df)
summary(pca_df)$loadings

plotdf <- as.data.frame(pca_df$scores[,1:2]) %>% 
   tibble() %>% 
   rename(PC1 = `Comp.1`, 
          PC2 = `Comp.2`) %>% 
   mutate(gsp_id = as.numeric(gsp_ids)) %>%
   left_join(gsp_meta, by = c("gsp_id" = "GSP.ID"))

ggplot(plotdf, aes(x = PC1, y = PC2, color = gwsum)) +
   geom_point() +
   theme_minimal()

ggplot(plotdf, aes(x = PC1, y = PC2, color = priority == 3 )) +
   geom_point() +
   scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
   theme_minimal()

