library(data.table)
library(stringr)
library(dplyr)

filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)

#ergm on num_GSPs_in + terms from JPART paper + entity_scope * verb_tense

