#retrieves gsp_text_with_meta (lean version)
gsp_text_with_meta <- readRDS(
   list.files(path = "data/output_large_files",pattern = "docs_lean",full.names = T)[
      length(list.files(path = "data/output_large_files",pattern = "docs_lean",full.names = T))
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

#Clean Lex ####
gsp_out <- lex_clean(gsp_text_with_meta, 
                     topic_indicators = unlist(topic_indicators,use.names=F))


saveRDS(gsp_out, file = paste0("data/temp_large_files/","gsp_slam_",format(Sys.time(), "%Y%m%d-%H:%M")))

