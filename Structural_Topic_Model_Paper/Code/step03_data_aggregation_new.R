dac_corr_check=F #Toggle this to run dac corr tests

packs <- c('stm','tm','SnowballC','tidytext','data.table',
           'tidyverse','sf','pbapply','geometry','Rtsne','rsvd',
           'stringi','stringr','scico','boxr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

source(filekey[filekey$var_name=="create_lang_meta_function",]$filepath)
source(filekey[filekey$var_name=="create_spat_meta_function",]$filepath)
source(filekey[filekey$var_name=="dac_svi_analysis_function",]$filepath)
source(filekey[filekey$var_name=="local_predictors_script",]$filepath)

if(dac_corr_check==T){
   #correlation check to determine which spatial metadata to use
   results_tract <- dac_svi_analysis("tract")
   results_place_pop <- dac_svi_analysis("place","pop")
   results_place_area <- dac_svi_analysis("place","area")
}

#Create Meta####
gsp_text_with_lang <- create_lang_meta(run_repair = F)

#retrieves the latest save of gsp_text_with_lang
#generated in create_lang_meta, which allows create_lang_meta() to be skipped

gsp_text_with_lang <- readRDS(filekey[filekey$var_name=="gsp_docs_lang",]$filepath)

type = "pop"
#or type = "area"
scope = "blockgroup"
gsp_dac <- create_dac_meta(type, scope, overwrite = T)
#rows = num docs; cols = metadata types
gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))

#filtering NA admin = proxy for GSPs whose texts have not been processed
gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))

#gsp_text_with_meta_mini <- unique(gsp_text_with_meta[,c(7,14,16,19:26)])
gsp_text_with_meta$GSP.ID <- as.numeric(gsp_text_with_meta$gsp_id)
gsp_local$GSP.ID <- as.numeric(gsp_local$GSP.ID)
library(raster)
library(rgdal)
maxdryspell <- raster::raster(filekey[filekey$var_name=="max_dry_spell_data",]$filepath)
st_transform(gsp_local, raster::projection(maxdryspell))

gsp_local$maxdryspell <- as.numeric(raster::extract(maxdryspell$cddm_year_ens32avg_rcp45_2040, 
                                         gsp_local, fun=mean, na.rm=T, sp=F))
gsp_local <- st_drop_geometry(gsp_local)

gsp_text_with_meta <- left_join(gsp_text_with_meta, gsp_local)


gsp_text_with_meta$urbangw_af_log_scaled <- scale(log(gsp_text_with_meta$urbangw_af))
gsp_text_with_meta$percent_dac_by_pop_scaled <- scale(gsp_text_with_meta$percent_dac_by_pop)
gsp_text_with_meta$fract_of_area_in_habitat_log_scaled <- scale(log(gsp_text_with_meta$fract_of_area_in_habitat))
gsp_text_with_meta$maxdryspell_scaled <- scale(gsp_text_with_meta$maxdryspell)
gsp_text_with_meta$Agr_Share_Of_GDP_scaled <- scale(gsp_text_with_meta$Agr_Share_Of_GDP)
gsp_text_with_meta$Republican_Vote_Share_scaled <- scale(gsp_text_with_meta$Republican_Vote_Share)
gsp_text_with_meta$Perc_Bach_Degree_Over25_scaled <- scale(gsp_text_with_meta$Perc_Bach_Degree_Over25)
gsp_text_with_meta$local_govs_per_10k_people_log_scaled <- scale(log(gsp_text_with_meta$local_govs_per_10k_people))

saveRDS(gsp_text_with_meta, file = filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)

gsp_text_lean <- gsp_text_with_meta[,c("text","gsp_id","is_comment","is_reference","page_num",
                                                 "admin","basin_plan","sust_criteria","monitoring_networks",
                                                 "projects_mgmt_actions","urbangw_af_log_scaled",
                                                 "percent_dac_by_pop_scaled",
                                                 "fract_of_area_in_habitat_log_scaled",
                                                 "maxdryspell_scaled",
                                                 "Agr_Share_Of_GDP_scaled",
                                                 "Republican_Vote_Share_scaled",
                                                 "Perc_Bach_Degree_Over25_scaled",
                                                 "local_govs_per_10k_people_log_scaled",
                                                 "mult_gsas",
                                                 "gwsum",
                                                 "approval")]

saveRDS(gsp_text_lean, file = filekey[filekey$var_name=="gsp_docs_lean",]$filepath)
