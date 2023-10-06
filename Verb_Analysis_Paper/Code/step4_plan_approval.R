library(data.table)
library(stringr)
library(dplyr)

filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)


#P(approval) ~ entity_type (actors) , network structure attr random effect (interactions), 
#verb classes (interactions), mult_gsas attr random effect (structures)

#present results