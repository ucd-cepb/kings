library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)
pagewise <- readRDS(filekey[filekey$var_name=="gsp_docs_lean",]$filepath)
pages <- readRDS(filekey[filekey$var_name=="gsp_docs_lang",]$filepath)
pages <- split(pages, pages$gsp_id)
keep_pages <- lapply(pages, function(i) i$is_comment==FALSE & i$is_reference==FALSE)
keep_pages <- unlist(keep_pages)
pagewise <- pagewise[keep_pages,]
types <- colnames(edgelist_w_meta)[
   stringr::str_detect(colnames(edgelist_w_meta),
                       "type_\\d")]
edgelist_w_meta$type_multi <- rowSums(edgelist_w_meta[,..types])>1
#whichtypetemp <- lapply(1:1000, 
#                        function(q) {ifelse(edgelist_w_meta$type_multi[q] == T, "multi",
#                                            types[edgelist_w_meta[,..types][q]==T])})
#for(i in 2:554){
#   print(i)
#   whichtypetemp <- append(whichtypetemp, 
#                           lapply(((1000*(i-1))+1):(1000*i), 
#                                  function(q) {ifelse(edgelist_w_meta$type_multi[q] == T, "multi",
#                                                      types[edgelist_w_meta[,..types][q]==T])}))
#}
#whichtypetemp <- 
#edgelist_w_meta$whichtype <- unlist(whichtypetemp)

top_past_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c("VBD","VBN"),]$head_verb_lemma
                        ),decreasing = T)
paste0(head(names(top_past_verbs), 20), collapse = ", ")

top_present_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c("VBP","VBZ","VBG"),]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_present_verbs), 20), collapse = ", ")

top_future_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$is_future == T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_future_verbs), 20), collapse = ", ")

top_hedge_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$has_hedge == T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_hedge_verbs), 20), collapse = ", ")



top_admin_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$admin==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_admin_verbs), 20), collapse = ", ")

top_basin_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$basin_plan==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_basin_verbs), 20), collapse = ", ")

top_sust_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$sust_criteria==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_sust_verbs), 20), collapse = ", ")

top_proj_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$projects_mgmt_actions==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_proj_verbs), 20), collapse = ", ")

top_monit_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$monitoring_networks==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_monit_verbs), 20), collapse = ", ")

top_collab_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$is_collab==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_collab_verbs), 20), collapse = ", ")

top_transf_verbs <- sort(table(edgelist_w_meta[edgelist_w_meta$is_transf==T,]$head_verb_lemma
),decreasing = T)
paste0(head(names(top_transf_verbs), 20), collapse = ", ")

typesummary <- edgelist_w_meta |> group_by(mult_gsas) |> 
   summarise_at(vars(type_9:type_114), sum, na.rm = T)
typesummary <- tidyr::pivot_longer(typesummary, cols = -mult_gsas, names_to = "verb_type", values_to = "count")
typesummary <- typesummary |> group_by(mult_gsas) |> mutate(
   percentage = count/sum(count))
   
typesummary <- edgelist_w_meta |> group_by(mult_gsas) |> 
   summarise_at(vars(c(is_transf, is_collab)), sum, na.rm = T)
typesummary <- tidyr::pivot_longer(typesummary, cols = -mult_gsas, names_to = "verb_type", values_to = "count")
typesummary <- typesummary |> group_by(mult_gsas) |> mutate(
   percentage = count/sum(count)
)
library(ggplot2)
ggplot(typesummary, aes(x = mult_gsas)) +
   geom_col(aes(fill = verb_type, y = percentage))

verbtypesumm <- as.data.frame(table(edgelist_w_meta$mult_gsas, edgelist_w_meta$is_transf, edgelist_w_meta$is_collab))
colnames(verbtypesumm) <- c("mult_gsas","is_transf","is_collab","count")

verbtypesumm$verb_type <- case_when(
   verbtypesumm$is_collab ==T & verbtypesumm$is_transf ==T ~ "collab_and_action",
   verbtypesumm$is_collab ==T & !verbtypesumm$is_transf ==T ~ "collab_only",
   !verbtypesumm$is_collab ==T & verbtypesumm$is_transf ==T ~ "action_only",
   !verbtypesumm$is_collab ==T & !verbtypesumm$is_transf ==T ~ "neither"
) 
verbtypesumm <- verbtypesumm |> group_by(mult_gsas) |> mutate(
   percentage = count/sum(count)
)
ggplot(verbtypesumm, aes(x = mult_gsas)) +
   geom_col(aes(fill = verb_type, y = percentage)) + scale_fill_viridis_d()

edgelist_w_meta$verb_type <- case_when(
   edgelist_w_meta$is_collab ==T & edgelist_w_meta$is_transf ==T ~ "collab_and_action",
   edgelist_w_meta$is_collab ==T & !edgelist_w_meta$is_transf ==T ~ "collab_only",
   !edgelist_w_meta$is_collab ==T & edgelist_w_meta$is_transf ==T ~ "action_only",
   !edgelist_w_meta$is_collab ==T & !edgelist_w_meta$is_transf ==T ~ "neither"
) 
typesummary <- as.data.frame(table(edgelist_w_meta$primarysection, edgelist_w_meta$verb_type)
)


colnames(typesummary) <- c("primarysection","verb_type","count")

typesummary <- typesummary |> group_by(primarysection) |> mutate(
   proportion = count/sum(count)
)
typesummary$verb_type <- factor(typesummary$verb_type, levels = c(
   "action_only","neither","collab_and_action","collab_only"
))
typesummary$primarysection <- factor(typesummary$primarysection, levels = c(
   "admin","basin_plan","sust_criteria","projects_mgmt_actions","monitoring_networks","multi","other"
))
ggplot(typesummary, aes(x = primarysection)) +
   geom_col(aes(fill = verb_type, y = proportion)) + scale_fill_viridis_d()

#present/future only
presfuture <- edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c("VB","VBP","VBZ","VBG"),]
typesummary <- as.data.frame(table(presfuture$primarysection, presfuture$verb_type)
)


colnames(typesummary) <- c("primarysection","verb_type","count")

typesummary <- typesummary |> group_by(primarysection) |> mutate(
   proportion = count/sum(count)
)
typesummary$verb_type <- factor(typesummary$verb_type, levels = c(
   "action_only","neither","collab_and_action","collab_only"
))
typesummary$primarysection <- factor(typesummary$primarysection, levels = c(
   "admin","basin_plan","sust_criteria","projects_mgmt_actions","monitoring_networks","multi","other"
))
ggplot(typesummary, aes(x = primarysection)) +
   geom_col(aes(fill = verb_type, y = proportion)) + scale_fill_viridis_d()

actiononly <- edgelist_w_meta[edgelist_w_meta$is_transf ==T,]
typesummary <- as.data.frame(table(actiononly$mult_gsas, actiononly$has_hedge)
)
colnames(typesummary) <- c("mult_gsas","has_hedge","count")

typesummary <- typesummary |> group_by(mult_gsas) |> mutate(
   proportion = count/sum(count)
)


m <- aov(has_hedge ~ as.factor(mult_gsas),
                      data = actiononly)
m <- oneway.test(has_hedge ~ mult_gsas, data = actiononly, 
            var.equal = T)
TukeyHSD(m)

ggplot(typesummary, aes(x = mult_gsas)) +
   geom_col(aes(fill = has_hedge, y = proportion)) + scale_fill_viridis_d()


#renumber pages to match network page nums, which don't include comment/reference pages
pagewise$original_page_num <- pagewise$page_num
pagewise$page_num <- ave(1:nrow(pagewise), pagewise$gsp_id, FUN = seq_along)
pagewise <- pagewise[,c("gsp_id", "page_num","admin","basin_plan",
                        "sust_criteria","monitoring_networks",
                        "projects_mgmt_actions", "is_comment", "is_reference", "original_page_num")]

edgelist_w_meta$page_num <- stringr::str_remove(edgelist_w_meta$doc_sent_verb,".*pdf")
edgelist_w_meta$page_num <- as.numeric(stringr::str_remove(edgelist_w_meta$page_num, "_.*"))

edgelist_w_meta <- dplyr::left_join(edgelist_w_meta, pagewise)

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
   edgelist_w_meta$type_9 == T |
   edgelist_w_meta$type_10 == T |
   edgelist_w_meta$type_26 == T |
   edgelist_w_meta$type_33 == T |
   edgelist_w_meta$type_34 == T |
   edgelist_w_meta$type_35 == T |
   edgelist_w_meta$type_45 == T |
   edgelist_w_meta$type_54 == T |
   edgelist_w_meta$type_63 == T 


#only keep the six main verb tenses
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c(
   "VB","VBD","VBG","VBN","VBP","VBZ"),]
#only keep verbs tagged in verbnet
edgelist_w_meta <- edgelist_w_meta[unlist(lapply(seq_along(edgelist_w_meta$type_id), function(k) !is.null(edgelist_w_meta$type_id[[k]]))),]

edgelist_w_meta$head_verb_tense <- as.factor(edgelist_w_meta$head_verb_tense)

edgelist_w_meta <- edgelist_w_meta %>% mutate(verbtype = 
                                                 ifelse(is_collab==T, ifelse(is_transf==F,"Collab","both"), 
                                                        ifelse(is_transf==T, "Outcome","neither")))
edgelist_w_meta$head_verb_tense <- as.character(edgelist_w_meta$head_verb_tense)
edgelist_w_meta <- edgelist_w_meta %>% mutate(verb_tense = ifelse(
   is_future==T,"Future",head_verb_tense))
#ordered so VB will be the reference level
edgelist_w_meta$verb_tense <- factor(edgelist_w_meta$verb_tense,
                                     levels = c("VB","VBN","VBD","VBG","VBZ","VBP","Future"))
#so "neither" will be the reference level
edgelist_w_meta$verbtype <- factor(edgelist_w_meta$verbtype,
                                   levels = c("neither","Outcome","both","Collab"))

#teamsandtasks <- edgelist_w_meta[!edgelist_w_meta$verbtype %in% c("both"),]

portiontransf <- sum(edgelist_w_meta$is_transf)/length(edgelist_w_meta$is_transf)
portionnotcollab <- sum(!edgelist_w_meta$is_collab)/length(edgelist_w_meta$is_collab)

#portiontransf / portionnotcollab
byplan <- edgelist_w_meta %>% filter(verb_tense %in% c(
   "VB","VBG","VBP","VBZ", "Future"
)) %>% group_by(gsp_id) %>% summarize(pct_collab = mean(is_collab), 
                                                             num_collab = sum(is_collab), 
                                                             pct_transf = mean(is_transf), 
                                                             num_transf = sum(is_transf),
                                                             num_total_hedge = sum(has_hedge),
                                                             num_transf_hedge = sum(is_transf & has_hedge),
                                                             num_collab_hedge = sum(is_collab & has_hedge))
byplan$pct_transf_hedge <- byplan$num_transf_hedge / byplan$num_transf
minimeta <- edgelist_w_meta[!duplicated(edgelist_w_meta$gsp_id),c("gsp_id", "mult_gsas", "approval",
                                                                  "Republican_Vote_Share_scaled",
                                                                  "Agr_Share_Of_GDP_scaled",
                                                                  "percent_dac_by_pop_scaled")]

byplan <- left_join(byplan, minimeta)
byplan$approval <- factor(byplan$approval, ordered = T, levels = c("Incomplete", 
                                                                   "Review In Progress", 
                                                                   "Approved"))
cor(byplan$pct_collab, byplan$pct_transf, method = "pearson")

#### Part 0: Where are the verbs?
summary(glm(is_collab ~ verb_tense + admin +
               basin_plan + 
               sust_criteria + 
               monitoring_networks +
               projects_mgmt_actions, data = edgelist_w_meta, family = "binomial"))
summary(glm(is_transf ~ verb_tense + admin +
               basin_plan + 
               sust_criteria + 
               monitoring_networks +
               projects_mgmt_actions, data = edgelist_w_meta, family = "binomial"))

library(ggplot2)
edgelist_w_meta$primarysection <- case_when(
   edgelist_w_meta$admin + 
          edgelist_w_meta$basin_plan + 
          edgelist_w_meta$sust_criteria + 
          edgelist_w_meta$monitoring_networks + 
          edgelist_w_meta$projects_mgmt_actions > 1 ~ "multi",
   edgelist_w_meta$admin == T ~ "admin",
   edgelist_w_meta$basin_plan == T ~ "basin_plan",
   edgelist_w_meta$sust_criteria == T ~ "sust_criteria",
   edgelist_w_meta$monitoring_networks == T ~ "monitoring_networks",
   edgelist_w_meta$projects_mgmt_actions == T ~ "projects_mgmt_actions",
   edgelist_w_meta$admin + 
      edgelist_w_meta$basin_plan + 
      edgelist_w_meta$sust_criteria + 
      edgelist_w_meta$monitoring_networks + 
      edgelist_w_meta$projects_mgmt_actions == 0 ~ "other")
   

ggplot(edgelist_w_meta) + geom_bar(aes(x = verb_tense, fill = primarysection))
#Verb-level summary of verb type ~ plan section, tense
#verb-level summary of hedging ~ plan section, tense
#### Part 1: When do we see less commitment? ###
#### tense is control - wouldn't expect hedging on past tense verbs
#hedging ~ verb type and plan section and tense 



   
#### Part 1: plan-level analysis 
#model 1: count of collab verbs ~ mult gsas, dac, etc (have version that includes plan section)
summary(m1 <- glm(num_collab ~ mult_gsas + percent_dac_by_pop_scaled, family="poisson", data=byplan))
#model 2: count of action verbs ~ mult gsas, dac, etc
summary(m2 <- glm(num_transf ~ mult_gsas + percent_dac_by_pop_scaled, family="poisson", data=byplan))

#### Part 2: plan-level features 
#   (Do we see more collab, do we see more action with mult gsas?)
#Does capacity --> certainty?
#model 3: count (or proportion) of collab verbs that are hedged ~ plan-level features (GSA, DAC)
summary(m3 <- glm(num_transf_hedge ~ mult_gsas + percent_dac_by_pop_scaled, family="poisson", data=byplan))

#model 4: count (or proportion) of action verbs that are hedged ~ plan-level features
summary(m4 <- glm(num_collab_hedge ~ mult_gsas + percent_dac_by_pop_scaled, family="poisson", data=byplan))



bysection <- edgelist_w_meta %>% filter(verb_tense %in% c(
   "VB","VBG","VBP","VBZ", "Future"
)) %>% group_by(gsp_id, primarysection) %>% summarize(pct_collab = mean(is_collab), 
                                                             num_collab = sum(is_collab), 
                                                             pct_transf = mean(is_transf), 
                                                             num_transf = sum(is_transf),
                                                             num_total_hedge = sum(has_hedge),
                                                             num_transf_hedge = sum(is_transf & has_hedge),
                                                             num_collab_hedge = sum(is_collab & has_hedge))
bysection <- left_join(bysection, minimeta)
bysection$approval <- factor(bysection$approval, ordered = T, levels = c("Incomplete", 
                                                                   "Review In Progress", 
                                                                   "Approved"))


#### Part 1: plan-level analysis 
#model 1: count of collab verbs ~ mult gsas, dac, etc (have version that includes plan section)
summary(m1 <- glm(num_collab ~ mult_gsas + percent_dac_by_pop_scaled + primarysection, family="poisson", data=bysection))
#model 2: count of action verbs ~ mult gsas, dac, etc
summary(m2 <- glm(num_transf ~ mult_gsas + percent_dac_by_pop_scaled + primarysection, family="poisson", data=bysection))

#### Part 2: plan-level features 
#   (Do we see more collab, do we see more action with mult gsas?)
#Does capacity --> certainty?
#model 3: count (or proportion) of collab verbs that are hedged ~ plan-level features (GSA, DAC)
summary(m3 <- glm(num_transf_hedge ~ mult_gsas + percent_dac_by_pop_scaled + primarysection, family="poisson", data=bysection))

#model 4: count (or proportion) of action verbs that are hedged ~ plan-level features
summary(m4 <- glm(num_collab_hedge ~ mult_gsas + percent_dac_by_pop_scaled + primarysection, family="poisson", data=bysection))



#### Part 3: consequences
#plan approval ~ hedging (of non-past verbs)
modelpart3 <- nnet::multinom(formula = factor(approval, ordered = F) ~ mult_gsas + 
                                percent_dac_by_pop_scaled + pct_transf_hedge + 
                              pct_transf + pct_collab, data = byplan)
modelpart3 <- nnet::multinom(formula = factor(approval, ordered = F) ~ mult_gsas + 
                                percent_dac_by_pop_scaled + num_transf_hedge + 
                                num_transf + num_collab, data = byplan)
summary(modelpart3)
plot(modelpart3)

tt <- broom::tidy(modelpart3,conf.int=TRUE)
tt <- dplyr::filter(tt, !term %in% c("(Intercept)"))#,"mult_gsasTRUE","percent_dac_by_pop_scaled"))
ggplot(tt, aes(x=estimate, y=term, color = y.level, xmin=conf.low, xmax=conf.high)) + 
   geom_pointrange(alpha = 0.4) + theme_bw() +
   geom_vline(xintercept=0, linetype=2)


summary(m4 <- glm(approval ~ mult_gsas + percent_dac_by_pop_scaled + primarysection, family="poisson", data=bysection))

#### Part 2: 
####H1 confirmed?####
alpha <- cor(byplan$pct_collab, byplan$pct_transf, method = "pearson") #> -1*portiontransf / portionnotcollab

set.seed(431)
N <- 1000
bootstrap <- vector(mode = "list", length = N)
corrs <- vector(mode = "numeric", length = N)
for(i in 1:N){
   bootstrap[[i]] <- sample(byplan$pct_transf, length(byplan$pct_transf), replace = T)
   corrs[i] <- cor(byplan$pct_collab, bootstrap[[i]], method = "pearson")
}
boxplot(corrs)
stripchart(alpha,              # Data
           pch = 19,          # Pch symbols
           col = "blue",           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)

####H2A####
set.seed(936834)
summary(glm(is_collab ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 9 putting
summary(glm(type_9 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 10 removing
summary(glm(type_10 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 76 limit verbs
summary(glm(type_76 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 63 enforce
summary(glm(type_63 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 111 conduct
summary(glm(type_111 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))


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
summary(glm(type_71 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 72 help_verbs
summary(glm(type_72 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 73 cooperate_verbs
summary(glm(type_73 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 107 involve_verbs #main verb is include
summary(glm(type_107 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#aov test h2a

res_aov_collab <- aov(pct_collab ~ as.factor(mult_gsas),
                      data = byplan
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov_collab$residuals)

# QQ-plot
library(car)
qqPlot(res_aov_collab$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov_collab$residuals) 
#since p>0.05, we aren't certain that residuals are abnormally distributed
#we proceed with anova or t-test

#time to test homogeneity

colnames(byplan)[colnames(byplan)=="pct_collab"] <- "prop_collab"

boxplot(prop_collab ~ mult_gsas,
        data = byplan
)

leveneTest(prop_collab ~ mult_gsas,
           data = byplan
)#is homogeneous
plot(res_aov_collab, which = 3)#red line is close to horizontal. homogeneity met.

#this means we can run a regular anova, ie res_aov or equivalently:
oneway.test(prop_collab ~ mult_gsas, data = byplan, 
            var.equal = T)
#the two groups are different!
TukeyHSD(res_aov_collab)
#mult_gsas have 0.02 (20%) more collaborative verbs compared to single_gsas.


####H2B####
set.seed(2370923)
summary(glm(is_transf ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#VERB CLASSES AND mult_gsas
#class 26 creation and transformation
summary(glm(type_26 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 33 judgement_verbs
summary(glm(type_33 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#confirmed

#class 34 verbs_of_assessment
summary(glm(type_34 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 35 verbs_of_searching
summary(glm(type_35 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#confirmed

#class 45 verbs_of_change of state
summary(glm(type_45 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 54 measure verbs
summary(glm(type_54 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 87 verbs_of_focusing and comprehending
summary(glm(type_87 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed


#aov test h2b
res_aov_transf <- aov(pct_transf ~ as.factor(mult_gsas),
                      data = byplan
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov_transf$residuals)

# QQ-plot
library(car)
qqPlot(res_aov_transf$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov_transf$residuals) 
#since p>0.05, we aren't certain that residuals are abnormally distributed
#we proceed with anova or t-test

#time to test homogeneity

colnames(byplan)[colnames(byplan)=="pct_transf"] <- "prop_transf"

boxplot(prop_transf ~ mult_gsas,
        data = byplan
)

leveneTest(prop_transf ~ mult_gsas,
           data = byplan
)#is homogeneous
plot(res_aov_transf, which = 3)#red line is close to horizontal. homogeneity met.

#this means we can run a regular anova, ie res_aov or equivalently:
oneway.test(prop_transf ~ mult_gsas, data = byplan, 
            var.equal = T)
#the two groups are not different
TukeyHSD(res_aov_transf)
#mult_gsas do not have a sig difference in prop transf verbs 



####H3A, H3B, H4A, H4B ####


#TODO make anovas for H3-H4

model34 <- MASS::polr(approval ~ pct_transf_hedge + pct_transf + pct_collab + mult_gsas, data = byplan,
           Hess = T)
summary(model34)

model34b <- nnet::multinom(formula = factor(approval, ordered = F) ~ pct_transf_hedge + 
                              pct_transf + pct_collab + mult_gsas, data = byplan)
summary(model34b)

#### H5 ####
#is there a significant difference in percentage of verbs that are base form between
#collab and transformative?

basetest <- edgelist_w_meta[edgelist_w_meta$verbtype %in% c("Outcome", "Collab"),c("verbtype", "verb_tense")]
basetest$is_vb <- ifelse(basetest$verb_tense=="VB", T, F)
basetest$verb_tense <- NULL
prop.test(c(sum(basetest[basetest$verbtype=="Collab",]$is_vb),
            sum(basetest[basetest$verbtype=="Outcome",]$is_vb)), 
          c(nrow(basetest[basetest$verbtype=="Collab",]), 
            nrow(basetest[basetest$verbtype=="Outcome",])), p = NULL, alternative = "two.sided", correct = TRUE)

#there is no difference in the proportion of base form verbs between outcome and collab verbs, so we can
#interpret the bayesian plot as comparing the proportion of collab verbs vs outcome verbs for each verb tense

library(brms)
library(tidybayes)
library(tidyverse)

edgelist_w_meta_sample <- edgelist_w_meta %>%
   group_by(verbtype, gsp_id) %>%
   summarize(VB = sum(verb_tense == 'VB'),
             VBN = sum(verb_tense == 'VBN'),
             VBD = sum(verb_tense == 'VBD'),
             VBP = sum(verb_tense == 'VBP'),
             VBZ = sum(verb_tense == 'VBZ'),
             VBG = sum(verb_tense == 'VBG'),
             Future = sum(verb_tense == 'Future')) %>%
   mutate(cell_size = VB+VBN+VBD+VBP+VBZ+VBG+Future)



edgelist_w_meta_sample$cell_counts = with(edgelist_w_meta_sample, 
                                          cbind(VB,VBN,VBD,VBP,VBZ,VBG,Future))
colnames(edgelist_w_meta_sample$cell_counts) <- c(
   "VB","VBN","VBD","VBP","VBZ","VBG","Future")
edgelist_w_meta_sample <- edgelist_w_meta_sample %>% select(-VB,-VBN,-VBD,-VBP,-VBZ,-VBG,-Future)

dat_tibble <- tibble(
   # verb_tense = factor(edgelist_w_meta_sample$verb_tense,levels=c(
   #   "VBN","VBD","VB","VBP","VBZ","VBG","Future")
   #),
   cell_size = edgelist_w_meta_sample$cell_size,
   cell_counts = edgelist_w_meta_sample$cell_counts,
   verbtype = edgelist_w_meta_sample$verbtype,
   gsp_id = as.factor(edgelist_w_meta_sample$gsp_id)#,
   #has_hedge = as.numeric(edgelist_w_meta_sample$has_hedge)
)

set.seed(13)
options(mc.cores=parallel::detectCores())

priors <- c(
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBN'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar = 'muVBD'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBP'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBZ'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBG'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muFuture'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBN'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBD'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBP'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBZ'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBG'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muFuture')
)

#cell_counts is count of each verb tense
#trials(cell_size) needs to be the row
formula <- brmsformula(
   cell_counts | trials(cell_size) ~ verbtype + (1|gsp_id))#random intercept by plan id.
#instead of treating 117 different levels, we think of them as random draws from a single variance
model <- brm(formula, dat_tibble, multinomial(), priors, control = 
                list(adapt_delta = 0.95), chains = 4, iter = 5000, seed = 12, save_pars = save_pars(all = T))
#check output warning for convergence

saveRDS(model, "data/Verb_Analysis_Paper/brmsH5model.RDS")

#look at coefficient stability
myplot <- plot(model)
length(myplot)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part1.png", myplot[[1]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part2.png", myplot[[2]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part3.png", myplot[[3]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part4.png", myplot[[4]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part5.png", myplot[[5]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part6.png", myplot[[6]], width=7, height=4, units = "in", dpi=700)

#look at posterior distribution of each coefficient
summ <- as.data.frame(posterior_summary(model))
splits <- str_split(rownames(summ), "_")
summ$metric <- sapply(splits, function(i) i[1])
summ$tense <- str_extract(sapply(splits, function(i) i[2]), "VB[A-Z]*|Future")
summ$type <- str_remove(sapply(splits, function(i) i[3]), "verbtype")
summ$tensetype <- paste0(summ$tense, summ$type)
summ$zeroes <- 0
summ <- summ[summ$metric == "b" & summ$type != "Intercept",]
myplot <- ggplot(summ, aes(x = Estimate, y = type, color = type)) +  geom_point()  +
   facet_wrap(vars(summ$tense), drop = T, nrow = 6, ncol = 1) + geom_vline(aes(xintercept = zeroes), lty = 2) +
   geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5)) 
ggsave("data/Verb_Analysis_Paper/figures/H5coeffs.png", myplot, width=7, height=4, units = "in", dpi=700)

summ <- as.data.frame(posterior_summary(model))
splits <- str_split(rownames(summ), "_")
summ$metric <- sapply(splits, function(i) i[1])
summ$tense <- str_extract(sapply(splits, function(i) i[2]), "VB[A-Z]*|Future")
summ$type <- str_remove(sapply(splits, function(i) i[3]), "verbtype")
summ$tensetype <- paste0(summ$tense, summ$type)
summ$zeroes <- 0
summ <- summ[summ$metric == "b" & summ$type == "Intercept",]
myplot <- ggplot(summ, aes(x = Estimate, y = type, color = type)) +  geom_point()  +
   facet_wrap(vars(summ$tense), drop = T, nrow = 6, ncol = 1) +
   geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5)) 
myplot
ggsave("data/Verb_Analysis_Paper/figures/H5prob.png", myplot, width=7, height=4, units = "in", dpi=700)



#### H6 ####

summary(glm(has_hedge ~ verbtype, data = edgelist_w_meta, family = "binomial"))

#### scraps ####




byplan <- byplan %>% filter(verbtype %in% c("Tasks","Teamwork"))
library(GGally)
byplanmini <- byplan %>% ungroup() 
byplanmini <- byplanmini[,c(#"approval",
   "Agr_Share_Of_GDP_scaled",
   "percent_dac_by_pop_scaled",
   "Republican_Vote_Share_scaled",
   #"Perc_Bach_Degree_Over25_scaled",
   "pct_transf_hedge","pct_collab","pct_transf")]
byplanmini$approval <- factor(byplanmini$approval, levels = c("Incomplete","Review In Progress", "Approved"))
colnames(byplanmini)[colnames(byplanmini)=="pct_tasks"] <- "prop_outcomes"
colnames(byplanmini)[colnames(byplanmini)=="pct_tasks_hedged"] <- "prop_outcomes_hedged"
colnames(byplanmini)[colnames(byplanmini)=="pct_collab"] <- "prop_collab"


library(GGally)
pairs <- byplanmini %>% ggpairs()
pairs
library(ggplot2)
fit_approval <- nnet::multinom(formula = as.factor(approval) ~ pct_tasks_hedged + 
                                  pct_collab + pct_tasks,
                               data = byplan, model = F)

fdf <- broom::tidy(fit_approval, exponentiate = FALSE, conf.int = TRUE)
View(fdf)

simple <- nnet::multinom(formula = as.factor(approval) ~ pct_tasks_hedged ,
                         data = byplan, model = F)
summary(simple)
colnames(byplan)[colnames(byplan)=="pct_tasks_hedged"] <- "prop_outcomes_hedged"
byplan$approval <- ifelse(byplan$approval=="Incomplete","Rejected",byplan$approval)
boxplot(prop_outcomes_hedged ~ approval,
        data = byplan
)


res_aov <- aov(pct_tasks_hedged ~ as.factor(approval),
               data = byplan
)
ggplot(aes(x=approval,y=pct_tasks_hedged), data = byplan)+
   geom_boxplot(fill="#666666") + theme_bw()
TukeyHSD(res_aov)
oneway.test(pct_multi ~ verbtype, data = byplan, 
            var.equal = T)
res_aov <- aov(pct_multi ~ as.factor(verbtype),
               data = byplan
)
boxplot(pct_multi ~ verbtype,
        data = byplan
)
TukeyHSD(res_aov)

edgelist_w_meta$doc_sent <- unlist(lapply(1:length(mysplits), function(i) paste(mysplits[[i]][1:length(mysplits[[i]])-1],collapse="_")))
bysentence <- edgelist_w_meta %>% group_by(doc_sent, gsp_id) %>% summarize(has_collab = max(is_collab), has_transf = max(is_transf))
res_aov <- aov(has_collab ~ as.factor(has_transf),
               data = bysentence
)
res_aov
boxplot(has_collab ~ has_transf,
        data = bysentence
)
TukeyHSD(res_aov)
mean(bysentence$has_transf)
mean(bysentence[bysentence$has_collab==1,bysentence$has_transf])

get_variables(model)
#this works!
m13.4hedge <- ulam(
   alist(
      has_hedge ~ dbinom( 1, p) ,
      logit(p) <- a2[verbtype] + g2[gsp_id],
      # adaptive priors
      a2[verbtype] ~ dnorm( a_bar , sigma_a ),
      g2[gsp_id] ~ dnorm( 0 , sigma_g ),
      # hyper-priors
      a_bar ~ dnorm( 0 , 1 ),
      sigma_a ~ dexp(0.75),
      sigma_g ~ dexp(0.75)
   ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )

#less efficient!
m13.4hedge_reparam <- ulam(
   alist(
      has_hedge ~ dbinom( 1, p) ,
      logit(p) <- a_bar + z[verbtype]*sigma_a + x[gsp_id]*sigma_g,
      # adaptive priors
      z[verbtype] ~ dnorm( 0 , 1 ),
      x[gsp_id] ~ dnorm( 0 , 1 ),
      # hyper-priors
      a_bar ~ dnorm( 0 , 1 ),
      sigma_a ~ dexp(1),
      sigma_g ~ dexp(1)
   ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )


