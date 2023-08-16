
generate_phrases <- FALSE
draw_edges <- T
directed <- T

library(reticulate)
library(spacyr)
library(magrittr)
library(dplyr)
library(tidytext)
library(quanteda)
library(pbapply)
library(stringr)
library(network)
library(pbapply)
library(data.table)
library(textNet)

spacy_initialize(model = "en_core_web_lg")
flist <- list.files('parsed_documents/',full.names = T)
doc <- readRDS(flist[1])
doc <- doc[doc$doc_id %in% sample(doc$doc_id,10),]
saveRDS(doc,file = '../textNet/data/sample_10p.RDS')


spacy_finalize()






