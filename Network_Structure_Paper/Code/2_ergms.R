library(ggraph)
library(dotenv)
library(igraph)
library(intergraph)
library(tidyverse)
library(migraph)
library(randomForest)
library(knitr)
library(broom)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/network_structure_by_plan/cleaned_extracts")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

nets <- list()
ergms <- list()
df <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- gsp_ids[g]
   nets[[gsp_id]] <- net
   
   
   # ergmx <- ergm(net ~ edges+nodecov('degree'))
   # ergms[[gsp_id]] <- ergmx
   # rowx <- data.frame(gsp_id = gsp_id,
   #                   gsa_n = sum(net %v% 'GSA'),
   #                   ecoef = ergmx$coefficients[[2]],
   #                   sig = summary(ergmx)$coefficients[2,5] < 0.05)
   # df <- rbind(df, rowx)
}

head(nets)
