library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)
#need at least one of the nodes in each edge to be not GPE
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$source_entity_type!="GPE" | 
                                      edgelist_w_meta$target_entity_type!="GPE",]

edgelist_w_meta$is_collab <- edgelist_w_meta$type_13 == T |
                              edgelist_w_meta$type_14 == T |
                              edgelist_w_meta$type_22 == T |
                              edgelist_w_meta$type_36 == T |
                              edgelist_w_meta$type_37 == T |
                              edgelist_w_meta$type_58 == T |
                              edgelist_w_meta$type_70 == T |
                              edgelist_w_meta$type_71 == T |
                              edgelist_w_meta$type_72 == T |
                              edgelist_w_meta$type_73 == T |
                              edgelist_w_meta$type_107 == T 


edgelist_w_meta$is_transf <- 
   edgelist_w_meta$type_26 == T |
   edgelist_w_meta$type_33 == T |
   edgelist_w_meta$type_34 == T |
   edgelist_w_meta$type_35 == T |
   edgelist_w_meta$type_45 == T |
   edgelist_w_meta$type_54 == T |
   edgelist_w_meta$type_87 == T 

#only keep the six main verb tenses
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c(
   "VB","VBD","VBG","VBN","VBP","VBZ"),]
#only keep verbs tagged in verbnet
edgelist_w_meta <- edgelist_w_meta[unlist(lapply(seq_along(edgelist_w_meta$type_id), function(k) !is.null(edgelist_w_meta$type_id[[k]]))),]

portiontransf <- sum(edgelist_w_meta$is_transf)/length(edgelist_w_meta$is_collab)
portionnotcollab <- sum(!edgelist_w_meta$is_collab)/length(edgelist_w_meta$is_collab)

portiontransf / portionnotcollab
byplan <- edgelist_w_meta %>% group_by(gsp_id) %>% summarize(pct_collab = mean(is_collab), is_transf = mean(is_transf))
cor(byplan$pct_collab, byplan$is_transf, method = "pearson")

#H1 confirmed?
cor(byplan$pct_collab, byplan$is_transf, method = "pearson") > -1*portiontransf / portionnotcollab


edgelist_w_meta$head_verb_tense <- as.factor(edgelist_w_meta$head_verb_tense)

edgelist_w_meta <- edgelist_w_meta %>% mutate(verbtype = 
                     ifelse(is_collab==T, ifelse(is_transf==F,"Collaborative Processes","both"), 
                                    ifelse(is_transf==T, "Actionable Outcomes","neither")))
edgelist_w_meta$head_verb_tense <- as.character(edgelist_w_meta$head_verb_tense)
edgelist_w_meta <- edgelist_w_meta %>% mutate(verb_tense = ifelse(
   is_future==T,"Future",head_verb_tense))
edgelist_w_meta$verb_tense <- factor(edgelist_w_meta$verb_tense,
                  levels = c("VBN","VBD","VBG","VBZ","VBP","VB","Future"))
edgelist_w_meta$verbtype <- factor(edgelist_w_meta$verbtype,
                              levels = c("Actionable Outcomes","both","neither","Collaborative Processes"))

teamsandtasks <- edgelist_w_meta[!edgelist_w_meta$verbtype %in% c("both"),]


#H2
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

edgelist_w_meta$verbtype <- relevel(edgelist_w_meta$verbtype, ref = "neither")
multinomH2 <- multinom(verbtype ~ mult_gsas, data = edgelist_w_meta)
summary(multinomH2)
collabonly <- summary(glm(is_collab ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#VERB CLASSES AND mult_gsas
#class 13 verbs_of_change_of_possession
summary(glm(type_13 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 14 learn_verbs
summary(glm(type_14 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#class 16 verbs_of_concealment
summary(glm(type_16 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed

#class 22 verbs_of_combining_and_attaching
summary(glm(type_22 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#class 23 verbs_of_separating_and_disassembling
summary(glm(type_23 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed, although most common word is "vary" and "divide"

#class 36 verbs_of_social_interaction, eg "communicate", "collaborate", "correspond", "cooperate"
summary(glm(type_36 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 37 verbs_of_communication
summary(glm(type_37 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 58 verbs_of_urging_and_begging
summary(glm(type_58 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed! Main verb is "require"


#class 70 rely_verbs
summary(glm(type_70 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 71 conspire_verbs (main verb is collaborate. also includes "partner" but collaborate is covered elsewhere)

#class 72 help_verbs
summary(glm(type_72 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 73 cooperate_verbs
summary(glm(type_73 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 107 involve_verbs #main verb is include
summary(glm(type_107 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#H3 and H4
#Verb Tense ####
#which actions have already been done and which are yet to be done?
#eg do we see evidence for social verbs/collaboration happening in the past, 
#present, and future to equal degrees?
summary(glm(is_collab ~ head_verb_tense + has_hedge + is_future, 
            data = edgelist_w_meta, family = "binomial"))

edgelist_w_meta$verb_tense <- relevel(edgelist_w_meta$verb_tense, ref = "VB")
edgelist_w_meta$verbtype <- relevel(edgelist_w_meta$verbtype, ref = "neither")
multinomH3 <- multinom(verb_tense ~ verbtype, data = edgelist_w_meta)
summary(multinomH3)

#compared to the base form, past tense and past participle verbs are
#less likely to be collaboration verbs, 
#gerund or present participle verbs are somewhat less likely to be
#collaboration verbs, and singular present verbs are
#less likely to be less collaboration verbs
#hedged verbs are less likely to be collaboration verbs
#future verbs are neither more nor less likely to be collaboration verbs

#In other words, past tense or singular present makes it less likely to be collaboration.
#Hedging makes it
#less likely to be a collaboration verb so collaboration verbs are
#relatively certain compared to other verbs

#Verb Tense ####
#which actions have already been done and which are yet to be done?
#eg do we see transformation verbs (eg signalling landscape transformation and gov structural modifications) happening in the past, present, or future most often?
summary(glm(is_transf ~ head_verb_tense + has_hedge + is_future, data = edgelist_w_meta, family = "binomial"))
#compared to the base verb form, past tense and past participle are
#more likely to be transformational verbs, which is interesting considering
#some of these categories presumably apply to monitoring efforts and 
#ongoing actions needed to bring water goals into being by 2040.
#singular present verbs are less likely to be transformation verbs,
#especially the non-third person one. (Does this mean the actions are being
#outsourced to third parties more, compared to the collaboration results
#where the third-person present was less common, signalling less collaboration where a
#third party is the main actor?)
#hedging increases the probability of one of these verbs, indicating
#that the actual plan actions are more likely to be uncertain.
#future tense indicators also increase the probability of these actions.
#in contrast to the collaboration set, the base verb is one of the
#least common verb tenses for the transformation collection.



library(ggplot2)



teamsandtasks <- teamsandtasks %>% mutate(verb_tense_simpl = dplyr::case_when(
   verb_tense =="VBN" | 
      verb_tense =="VBD" ~ "Past",
   verb_tense =="VB" | 
      verb_tense =="VBG" | 
      verb_tense =="VBZ" | 
      verb_tense =="VBP" ~ "Present and Base",
   verb_tense =="Future" ~ "Future"
   
))
teamsandtasks <- teamsandtasks %>% mutate(verbtype_new = dplyr::case_when(
   verbtype=="Teamwork" ~ "Collaborative Process",
   verbtype=="Tasks" ~ "Actionable Outcomes",
   verbtype=="neither" ~ "neither"
))
teamsandtasks$verbtype <- teamsandtasks$verbtype_new
teamsandtasks$verb_tense <- teamsandtasks$verb_tense_simpl

teamsandtasks$verb_tense <- factor(teamsandtasks$verb_tense, levels = c("Past","Present and Base","Future"))

ggplot(teamsandtasks, aes(x = verbtype, fill = verb_tense)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "C")

ggplot(teamsandtasks, aes(x = has_hedge, fill = verbtype)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "D")

ggplot(teamsandtasks, aes(x = approval, fill = verbtype)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "E")+
   scale_x_discrete(labels=c('Approved', 'Rejected', 'Review in Progress'))

#H5

taskonly <- edgelist_w_meta[edgelist_w_meta$is_transf==T,]
#approval based on task hedging
summary(glm(factor(approval, levels=c("Incomplete","Review In Progress", "Approved")
) ~ has_hedge, data = taskonly, family = "binomial"))

#Is hedging in general related to approval?
summary(glm(has_hedge ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#if a sentence is from a plan under review,
#it is less likely to be hedged, compared to an approved plan

#in other words, under-review plans have more decisive language. 

taskonly$approval <- ifelse(taskonly$approval=="Incomplete","Rejected",taskonly$approval)
ggplot(taskonly, aes(x = has_hedge, fill = approval)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "E")


#H6
#VERB CLASSES AND approval
cor(edgelist_w_meta$Agr_Share_Of_GDP_scaled, 
    as.numeric(factor(edgelist_w_meta$verb_tense, levels = c("VBN","VBD","VB","VBG","VBP","VBZ","Future"))), method = "pearson")
#basin priority ~ certainty (past)

summary(glm(is_transf ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))

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


#TODO
#Verb Tense and Node Attributes ####
#do the most central nodes have more past tense, present tense, or future tense edges attached to them than is typical in the network?
#this may require an ergm.

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
#hedged ~ centrality










