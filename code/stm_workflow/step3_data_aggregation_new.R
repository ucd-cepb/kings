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

gsp_text_with_lang <- readRDS("data_output/gsp_docs_w_lang")

type = "pop"
#or type = "area"
scope = "blockgroup"
gsp_dac <- create_dac_meta(type, scope, box_sync = T,overwrite = T)
options(timeout=600)
#rows = num docs; cols = metadata types
gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))

#filtering NA admin = proxy for GSPs whose texts have not been processed
gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))

saveRDS(gsp_text_with_meta, file = "data_output/gsp_docs_w_meta")



      