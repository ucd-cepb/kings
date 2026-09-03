library(data.table)
library(lsa)
library(Matrix)
refs = readRDS('Network_Innovation_Paper/data_products/gsp_solr_OA_matches.rds')
#first_year <- 1980
cutoff_score <- 11
refs2 <- refs[{score > cutoff_score | title.gsp == title.oa} & !is.na(source.id.oa),]
refs3 <- refs2[,.(basename(openalex.ID),basename(GSP.File))]
saveRDS(refs3,'Network_Innovation_Paper/data_products/gsp_reference_pairs.rds')
