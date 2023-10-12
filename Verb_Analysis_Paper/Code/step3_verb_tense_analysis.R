library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)


#Section 3: Verb Class and Verb Tense/Hedges ####
#which actions have already been done and which are yet to be done?
#eg do we see evidence for social verbs/collaboration happening in the past, present, and future to equal degrees?
#eg do we see transformation verbs (eg signalling landscape transformation and gov structural modifications) happening in the past, present, or future most often?
#eg do we see hedges disproportionately attached to transformation verbs, signalling the uncertainty of the actual project actions?
#which types of verbs are the most likely to be hedged, proportionately to their total percentage of use?

#Section 4: Verb Tense and Node Attributes ####
#do the most central nodes have more past tense, present tense, or future tense edges attached to them than is typical in the network?
#this may require an ergm.

