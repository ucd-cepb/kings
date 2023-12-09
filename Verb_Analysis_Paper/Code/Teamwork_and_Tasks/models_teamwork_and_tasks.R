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

edgelist_w_meta$head_verb_tense <- as.factor(edgelist_w_meta$head_verb_tense)

edgelist_w_meta <- edgelist_w_meta %>% mutate(verbtype = 
                                                 ifelse(is_collab==T, ifelse(is_transf==F,"Teamwork","both"), 
                                                        ifelse(is_transf==T, "Tasks","neither")))
edgelist_w_meta$head_verb_tense <- as.character(edgelist_w_meta$head_verb_tense)
edgelist_w_meta <- edgelist_w_meta %>% mutate(verb_tense = ifelse(
   is_future==T,"Future",head_verb_tense))
edgelist_w_meta$verb_tense <- factor(edgelist_w_meta$verb_tense,
                                     levels = c("VBN","VBD","VBG","VBZ","VBP","VB","Future"))
edgelist_w_meta$verbtype <- factor(edgelist_w_meta$verbtype,
                                   levels = c("Tasks","both","neither","Teamwork"))

teamsandtasks <- edgelist_w_meta[!edgelist_w_meta$verbtype %in% c("both"),]


portiontransf <- sum(edgelist_w_meta$is_transf)/length(edgelist_w_meta$is_collab)
portionnotcollab <- sum(!edgelist_w_meta$is_collab)/length(edgelist_w_meta$is_collab)

portiontransf / portionnotcollab
byplan <- edgelist_w_meta %>% group_by(gsp_id) %>% summarize(pct_collab = mean(is_collab), pct_transf = mean(is_transf))

byplan <- left_join(byplan, unique(edgelist_w_meta[,c("gsp_id","mult_gsas")]))
byplan


#insert bootstrapping for H1



#H2

res_aov <- aov(pct_collab ~ as.factor(mult_gsas),
               data = byplan
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov$residuals) 
#since p>0.05, we can assume residuals are normally distributed
#it's ok to use anova or t-test

#time to test homogeneity
boxplot(pct_collab ~ mult_gsas,
        data = byplan
)

leveneTest(pct_collab ~ mult_gsas,
           data = byplan
)#is homogeneous
plot(res_aov, which = 3)#red line is close to horizontal. homogeneity met.

#this means we can run a regular anova, ie res_aov or equivalently:
oneway.test(pct_collab ~ mult_gsas, data = byplan, 
            var.equal = T)
#the two groups are different!
TukeyHSD(res_aov)
#mult_gsas have 0.02 more percent collaborative verbs compared to single_gsas.


edgelist_w_meta_max <- edgelist_w_meta

#edgelist_w_meta <- head(edgelist_w_meta, 300)

#H3
edgelist_w_meta$verb_tense_id = factor(edgelist_w_meta$verb_tense,levels=c(
      "VBN","VBD","VB","VBP","VBZ","VBG","Future")
   )
partial.pooling.fit_hedge <- nnet::multinom(formula = has_hedge ~  verbtype +  gsp_id,
                                            data = edgelist_w_meta, model = T)
partial.pooling.fit_tense <- nnet::multinom(formula = verb_tense_id ~  verbtype +  gsp_id,
                            data = edgelist_w_meta, model = T)

#TODO how to interpret and see results

hedgedf <- as.data.frame(summary(partial.pooling.fit_hedge))
tensedf <- as.data.frame(summary(partial.pooling.fit_tense))

#TODO make anovas for H3-H4, then model for H5 and H6
library(brms)
library(tidybayes)
library(tidyverse)

edgelist_w_meta_sample <- edgelist_w_meta %>%
   group_by(verbtype, gsp_id) %>%
   summarize(VBN = sum(verb_tense == 'VBN'),
             VBD = sum(verb_tense == 'VBD'),
             VB = sum(verb_tense == 'VB'),
             VBP = sum(verb_tense == 'VBP'),
             VBZ = sum(verb_tense == 'VBZ'),
             VBG = sum(verb_tense == 'VBG'),
             Future = sum(verb_tense == 'Future')) %>%
   mutate(cell_size = VBN+VBD+VB+VBP+VBZ+VBG+Future)


edgelist_w_meta_sample$cell_counts = with(edgelist_w_meta_sample, 
                     cbind(VBN,VBD,VB,VBP,VBZ,VBG,Future))
colnames(edgelist_w_meta_sample$cell_counts) <- c(
   "VBN","VBD","VB","VBP","VBZ","VBG","Future")
edgelist_w_meta_sample <- edgelist_w_meta_sample %>% select(-VBN,-VBD,-VB,-VBP,-VBZ,-VBG,-Future)

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
options(mc.cores=parallel:detectCores())

priors <- c(
   prior('student_t(5,0,1)',class = 'Intercept',dpar = 'muVBD'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVB'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBP'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBZ'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBG'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muFuture'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBD'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVB'),
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
                list(adapt_delta = 0.95), chains = 1, iter = 100, seed = 12)

#check to set convergence
#look at coefficient stability
#look at posterior distribution of each coefficient

#start with frequentist
#SAS might have this ability
#glmmix

partial.pooling.fit_tense <- nnet::multinom(formula = verb_tense_id ~  verbtype +  (1|gsp_id),
                                            data = edgelist_w_meta_sample, model = T)


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

