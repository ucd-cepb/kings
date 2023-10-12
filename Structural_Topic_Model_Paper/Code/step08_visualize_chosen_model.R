filekey <- read.csv("filekey.csv")

modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

model <- readRDS(list.files(path = modelpath, pattern = "model", full.names = T)[
   length(list.files(path = modelpath, pattern = "model", full.names = T))])

   

inputsfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
inputsfilenamesplits <- unlist(strsplit(inputsfilename,split="/"))
inputspath <- paste(inputsfilenamesplits[1:(length(inputsfilenamesplits)-1)],collapse = "/")
inputspattern <- inputsfilenamesplits[length(inputsfilenamesplits)]

inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[
   length(list.files(path = inputspath, pattern = inputspattern, full.names = T))])

leanfilename <- filekey[filekey$var_name=="gsp_docs_lean",]$filepath
leanfilenamesplits <- unlist(strsplit(leanfilename,split="/"))
leanpath <- paste(leanfilenamesplits[1:(length(leanfilenamesplits)-1)],collapse = "/")
leanpattern <- leanfilenamesplits[length(leanfilenamesplits)]

gsp_text_with_meta <- readRDS(list.files(path = leanpath, pattern = leanpattern, full.names = T)[
   length(list.files(path = leanpath, pattern = leanpattern, full.names = T))])


   
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
   
   
   source(filekey[filekey$var_name=="visualize_topics_grouped_function",]$filepath)
      visualize_topics_grouped(model, 
                       inputs, 
                       text_col = gsp_text_with_meta$text[gsp_text_with_meta$is_comment == F &
                                                  gsp_text_with_meta$is_reference == F],
                       topic_indicators,
                       scatter = T,
                       effects=T)
      
   