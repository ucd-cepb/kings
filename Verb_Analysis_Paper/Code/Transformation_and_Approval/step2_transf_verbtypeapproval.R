library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)

#VERB CLASSES AND approval
#class 61 ~ "try_verbs"
summary(glm(type_61 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#unapproved plans "try"/"attempt" more!

#class 14 learn_verbs #learning
summary(glm(type_14 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#no clear trend

#class 26 verbs_of_creation_and_transformation #landscape transformation
summary(glm(type_26 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#no clear trend

#class 30 verbs_of_perception #psych elements (main verb is meet. would need to remove "meet")

#class 31 psych-verbs-verbs_of_psychological_state #psych elements (main verbs are support, approve, affect, recharge)
summary(glm(type_31 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#approved plans have this most. maybe because of discussion of "recharge"?

#class 33 judgment_verbs #evaluation (main verbs = report, approve, recommend)
summary(glm(type_33 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#no clear trend

#class 34 verbs_of_assessment #thoroughness (main verbs = monitor, estimate, follow)
summary(glm(type_34 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 35 verbs_of_searching #thoroughness (main verb = monitor)
summary(glm(type_35 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed! Weird -- do rejected plans discuss monitoring more?

#class 45 verbs_of_change_of_state #transformation (main verbs = set, collect, operate, reduce)
summary(glm(type_45 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 54 measure_verbs #thoroughness (main verbs = estimate, contain, hold, serve, take, assess)
summary(glm(type_54 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 87 verbs_of_focusing_and_comprehending #thoroughness; main verb is "follow"
summary(glm(type_87 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

