dac_corr_check=F #Toggle this to run dac corr tests

packs <- c('stm','tm','SnowballC','tidytext','data.table',
           'tidyverse','sf','pbapply','geometry','Rtsne','rsvd',
           'stringi','stringr','scico','boxr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

source('code/functions/lex_clean.R')
source('code/functions/create_lang_meta.R')
source('code/functions/create_spat_meta.R')
source('code/functions/generate_proper_names.R')
#source('code/functions/compare_models.R')
source('code/functions/visualize_topics.R')
source('code/functions/dac_svi_analysis.R')
source('code/functions/summarize_pws_by_gsp.R')

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
gsp_pws <- summarize_pws_by_gsp() %>% st_drop_geometry() %>% 
   select(gsp_id, service_count,hviol_avg_res,prop_service_gw_source)
#rows = num docs; cols = metadata types
gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))
gsp_text_with_meta <- full_join(gsp_text_with_meta, gsp_pws, by = "gsp_id")

#gsp_text_with_meta <- gsp_text_with_meta %>% mutate(
 #                          hviol_avg_res = ifelse(is.na(hviol_avg_res),0,hviol_avg_res),
  #                         service_count = ifelse(is.na(service_count),0,service_count),
   #                        prop_service_gw_source = 
    #                          ifelse(is.na(prop_service_gw_source),1,prop_service_gw_source)) 
                       
#filtering NA admin = proxy for GSPs whose texts have not been processed
gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))

saveRDS(gsp_text_with_meta, file = "data_output/gsp_docs_w_meta")



      