dac_corr_check=F #Toggle this to run dac corr tests

packs <- c('stm','tm','SnowballC','tidytext','data.table',
           'tidyverse','sf','pbapply','geometry','Rtsne','rsvd',
           'stringi','stringr','scico','boxr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

source('code/stm_workflow/utils/create_lang_meta.R')
source('code/stm_workflow/utils/create_spat_meta.R')
source('code/stm_workflow/utils/generate_proper_names.R')
#source('code/functions/compare_models.R')
source('code/stm_workflow/utils/dac_svi_analysis.R')
source('code/stm_workflow/utils/local_predictors.R')

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

gsp_text_with_lang <- readRDS("data/output_large_files/gsp_docs_w_lang")

type = "pop"
#or type = "area"
scope = "blockgroup"
gsp_dac <- create_dac_meta(type, scope, overwrite = T)
options(timeout=600)
#rows = num docs; cols = metadata types
gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))

#filtering NA admin = proxy for GSPs whose texts have not been processed
gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))

#gsp_text_with_meta_mini <- unique(gsp_text_with_meta[,c(7,14,16,19:26)])
gsp_text_with_meta$GSP.ID <- as.numeric(gsp_text_with_meta$gsp_id)
gsp_local$GSP.ID <- as.numeric(gsp_local$GSP.ID)
library(raster)
library(rgdal)
maxdryspell <- raster::raster("data/spatial_raw_large_files/cddm_year_ens32avg_rcp45_2040.tif")
st_transform(gsp_local, raster::projection(maxdryspell))

gsp_local$maxdryspell <- as.numeric(raster::extract(maxdryspell$cddm_year_ens32avg_rcp45_2040, 
                                         gsp_local, fun=mean, na.rm=T, sp=F))
gsp_local <- st_drop_geometry(gsp_local)

gsp_text_with_meta <- left_join(gsp_text_with_meta, gsp_local)

#converting gw severity to binary metrics
gsp_text_with_meta$subsidence <- ifelse(gsp_text_with_meta$subsidence>0,1,0)
gsp_text_with_meta$saltintrusion <-ifelse(gsp_text_with_meta$saltintrusion>0,1,0)
gsp_text_with_meta$declininggw <- ifelse(gsp_text_with_meta$declininggw>0,1,0)
gsp_text_with_meta$gwsum <- gsp_text_with_meta$declininggw + 
   gsp_text_with_meta$saltintrusion + 
   gsp_text_with_meta$subsidence
saveRDS(gsp_text_with_meta, file = "data/output_large_files/gsp_docs_w_meta")

gsp_mini <- unique(gsp_text_with_meta[,c(7,25,26,27)])
table(gsp_mini[,2:4])
      