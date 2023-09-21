
   
   model <- readRDS(list.files(path = "data/output_large_files/mdl", pattern = "model", full.names = T)[length(
      list.files(path = "data/output_large_files/mdl", pattern = "model", full.names = T))])
   
   
   inputs <- readRDS(list.files(path = "data/temp_large_files", pattern = "slam", full.names = T)[length(
      list.files(path = "data/temp_large_files", pattern = "slam", full.names = T))])
   
   gsp_text_with_meta <- readRDS(
      list.files(path = "data/output_large_files",pattern = "docs_lean",full.names = T)[
         length(list.files(path = "data/output_large_files",pattern = "docs_lean",full.names = T))
      ])
   
   topic_indicators <- list(ej = c("disadvantaged community", "disadvantaged communities",
                                   "^dac$", "^dacs$",
                                   "^community$","engagement","outreach","environmental_justice"),
                            dw = c("contaminat", "safe", "public_supply", "drinking_water",
                                   "porter-cologne", "fluoride", "water_quality"),
                            cc = c("climate change","projection","projections"),
                            gde = c("groundwater-dependent ecosystem",
                                    "groundwater dependent ecosystem",
                                    "groundwater-dependent ecosystems",
                                    "groundwater dependent ecosystems",
                                    "^gde$","^gdes$","habitat","species","^spp$","vegetation"))
   
   
   source("code/stm_workflow/utils/visualize_topics_grouped.R")
      visualize_topics_grouped(model, 
                       inputs, 
                       text_col = gsp_text_with_meta$text[gsp_text_with_meta$is_comment == F &
                                                  gsp_text_with_meta$is_reference == F],
                       topic_indicators,
                       scatter = T,
                       effects=T)
      
   