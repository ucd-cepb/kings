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

#notes from meeting with Tyler
#what is the role of plans?
#symbolic? signalling? is it a roadmap? 
#if symbolic or signalling, important to say what other people should do
#if its a roadmap you might want to hedge your actions
#PAR would be a good place for the collaboration paper
#PAR has a "takeaways for practitioners" section
#next step = make an outline. Tag Tyler at rationale of why this matters, 
#Elise first cut of what we did
hedged ~ centrality


