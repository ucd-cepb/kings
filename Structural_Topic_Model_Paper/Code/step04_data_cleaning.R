#retrieves gsp_text_with_meta (lean version)
filekey <- read.csv("filekey.csv")

gsp_text_with_meta <- readRDS(filekey[filekey$var_name=="gsp_docs_lean",]$filepath)

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
source("Structural_Topic_Model_Paper/Code/utils/lex_clean.R")
gsp_out <- lex_clean(gsp_text_with_meta, 
                     topic_indicators = unlist(topic_indicators,use.names=F))


saveRDS(gsp_out, file = paste0(filekey[filekey$var_name=="gsp_out_files",]$filepath,format(Sys.time(), "%Y%m%d-%H:%M")))

