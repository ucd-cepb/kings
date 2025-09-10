filekey <- read.csv("filekey.csv")

modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

finfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
version_select <- which(finfo$mtime==max(finfo$mtime))

model <- readRDS(rownames(finfo)[version_select])

inputsfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
inputsfilenamesplits <- unlist(strsplit(inputsfilename,split="/"))
inputspath <- paste(inputsfilenamesplits[1:(length(inputsfilenamesplits)-1)],collapse = "/")
inputspattern <- inputsfilenamesplits[length(inputsfilenamesplits)]

inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[
   length(list.files(path = inputspath, pattern = inputspattern, full.names = T))])

leanfilename <- filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath
leanfilenamesplits <- unlist(strsplit(leanfilename,split="/"))
leanpath <- paste(leanfilenamesplits[1:(length(leanfilenamesplits)-1)],collapse = "/")
leanpattern <- leanfilenamesplits[length(leanfilenamesplits)]

gsp_text_with_meta <- readRDS(list.files(path = leanpath, pattern = leanpattern, full.names = T)[
   length(list.files(path = leanpath, pattern = leanpattern, full.names = T))])

library(dplyr)
metadata <- inputs$meta
meta_with_text <- left_join(metadata, gsp_text_with_meta)






summary(meta_with_text$admin)
summary(meta_with_text$basin_plan)
summary(meta_with_text$sust_criteria)
summary(meta_with_text$monitoring_networks)
summary(meta_with_text$projects_mgmt_actions)
summary(meta_with_text$exante_collab)
summary(meta_with_text$mult_gsas)
table(meta_with_text$priority_category)
set.seed(30)
summary(meta_with_text$basin_population)
summary(meta_with_text$percent_dac_by_pop)
summary(meta_with_text$exceedance)
summary(meta_with_text$DSCI)
set.seed(30)
summary(meta_with_text$fract_of_area_in_habitat)
summary(meta_with_text$Agr_Share_Of_GDP)
summary(meta_with_text$Republican_Vote_Share)
