library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)

#Verb Classes ~ Basin Attributes esp coordinating ~ Mult_GSAs and ?? ~ DACs and transformation ~ approval ####
#VERB CLASSES AND AG
#summary(glm(type_64 ~ Agr_Share_Of_GDP_scaled + Republican_Vote_Share_scaled, data = edgelist_w_meta, family = "binomial"))
#class 64 ~ "allow_verbs"
#should remove word "include" and "forbid" type words

#VERB CLASSES AND DW
summary(glm(type_76 ~ Agr_Share_Of_GDP_scaled + urbangw_af_log_scaled + fract_of_area_in_habitat_log_scaled +
               maxdryspell_scaled + gwsum, data = edgelist_w_meta, family = "binomial"))
#class 76 limit_verbs #water restraint to protect drinking water
#hypoth confirmed for ag and gwsum, not for urban, not for habitat, not for maxdryspell
summary(glm(type_90 ~ Agr_Share_Of_GDP_scaled + urbangw_af_log_scaled + fract_of_area_in_habitat_log_scaled +
               maxdryspell_scaled + gwsum, data = edgelist_w_meta, family = "binomial"))
#class 90 exceed_verbs
#hypoth confirmed for maxdryspell, not for gwsum, habitat, urban, or ag

#VERB CLASSES AND GDEs
#class 26 verbs_of_creation_and_transformation (mostly "develop")
#class 76 limit_verbs #water restraint to protect drinking water
#(see above)
#class 90 exceed_verbs
#(see above)


