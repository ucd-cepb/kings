
   
   gsp_model_saved <- readRDS(list.files(path = "data_output/mdl", pattern = "model", full.names = T)[length(
      list.files(path = "data_output/mdl", pattern = "model", full.names = T))])
   
   
   gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
      list.files(path = "data_temp", pattern = "slam", full.names = T))])
   
   gsp_text_with_meta <- readRDS(
      list.files(path = "data_output",pattern = "docs_w_meta",full.names = T)[
         length(list.files(path = "data_output",pattern = "docs_w_meta",full.names = T))
      ])
   
   topic_indicators <- list(ej = c("disadvantaged community", "disadvantaged communities",
                                   "^community$","engagement","outreach","environmental_justice"),
                            dw = c("drinking water", "water quality","safe","^well$","^wells$"),
                            cc = c("climate change","projection","projections"),
                            gde = c("groundwater-dependent ecosystem",
                                    "groundwater dependent ecosystem",
                                    "groundwater-dependent ecosystems",
                                    "groundwater dependent ecosystems",
                                    "^gde$","^gdes$","habitat","species"))
   
   

      visualize_topics(gsp_model_saved, 
                       gsp_out, 
                       gsp_text_with_meta$text[gsp_text_with_meta$is_comment == F &
                                                  gsp_text_with_meta$is_reference == F],
                       topic_indicators)
      
   