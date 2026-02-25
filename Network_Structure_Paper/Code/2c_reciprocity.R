library(dotenv)
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

gsp_meta <- read_csv(paste0(Sys.getenv("BOX_PATH"), "/Structural_Topic_Model_Paper/gsp_ids_with_metadata.csv")) %>% 
   select(c(GSP.ID, priority, gwsum, Agr_Share_Of_GDP_scaled, 
            local_govs_per_10k_people_log_scaled, urbangw_af_log_scaled)) %>%
   mutate(
      priority = case_when(
         priority == "High" ~ 3,
         priority == "Medium" ~ 2,
         priority == "Low" ~ 1,
         priority == "Very Low" ~ 0
      ),
      # priority_High = if_else(priority == 3, 
      #                         1, 0),
      # priority_Medium = if_else(priority == 2, 
      #                           1, 0),
      # priority_Low = if_else(priority == 1, 
      #                        1, 0),
      # priority_Very_Low = if_else(priority == 0, 
      #                             1, 0),
      # gwsum_0 = if_else(gwsum == 0, 1, 0),
      # gwsum_1 = if_else(gwsum == 1, 1, 0),
      # gwsum_2 = if_else(gwsum == 2, 1, 0),
      # gwsum_3 = if_else(gwsum == 3, 1, 0)
   )

df <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- as.numeric(gsp_ids[g])
   estats <- summary(net ~ edges+
                        mutual+
                        nodematch('GSA')+
                        # nodefactor('org_type')+
                        gwidegree(attr='GSA', fixed=TRUE, decay=0.3)+
                        gwodegree(attr='GSA', fixed=TRUE, decay=0.3)
   )
   rowx <- pivot_wider(cbind(gsp_id, tidy(estats)),
                       id_cols = gsp_id,
                       names_from = names,
                       values_from = x)
   mstats <- data.frame(net_density = gden(net),
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

ggplot(plotdf, aes(x = PC1, y = PC2, color = gwsum >=2)) +
   geom_point() +
   scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
   theme_minimal()
