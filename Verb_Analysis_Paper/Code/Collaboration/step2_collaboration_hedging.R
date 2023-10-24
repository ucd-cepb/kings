library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)


#Hedges ####
#eg do we see hedges disproportionately attached to collaboration verbs, signalling the uncertainty of the collaboration relationships?
#see step1 results

#TODO
#Hedging and Node Attributes ####
#do the most central nodes have more hedged edges attached to them than is typical in the network?
#this may require an ergm.

