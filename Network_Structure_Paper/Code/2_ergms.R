library(dotenv)
library(tidyverse)
library(knitr)
library(broom)
library(statnet)
library(ca)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/network_structure_by_plan/cleaned_extracts")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

nets <- list()
stats <- list()
df <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   net %v% "org_type" <- ifelse(is.na(net %v% "org_type"), "notorg", net %v% "org_type")
   gsp_id <- gsp_ids[g]
   nets[[gsp_id]] <- net
   estats <- summary(net ~ edges+mutual+triangle+nodefactor('org_type'))
   stats[[gsp_id]] <- estats
   rowx <- pivot_wider(cbind(gsp_id, tidy(estats)), 
                       id_cols = gsp_id, 
                       names_from = names, 
                       values_from = x)
   mstats <- data.frame(net_density = gden(net),
                        net_diameter = max(ifelse(is.infinite(geodist(net)$gdist), 0, geodist(net)$gdist)),
                        net_components = components(net, connected = 'weak'),
                        net_core = kcores(net),
                        net_reciprocity = grecip(net)
                        )
   rowx <- bind_cols(rowx, mstats)
   df <- bind_rows(df, rowx) 
}

df <- tibble(df) %>% 
   mutate(across(everything(), replace_na, 0),
          across(everything(), as.numeric)) %>% 
   select(-nodefactor.org_type.notorg)

summary(princomp(scale(df)))
plot(ca(df))
df

