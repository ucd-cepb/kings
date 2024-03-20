

library(stm)
library(data.table)
filekey <- read.csv("filekey.csv")
#### this code was taken from step08 ###
inputsfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
inputsfilenamesplits <- unlist(strsplit(inputsfilename,split="/"))
inputspath <- paste(inputsfilenamesplits[1:(length(inputsfilenamesplits)-1)],collapse = "/")

inputspattern <- inputsfilenamesplits[length(inputsfilenamesplits)]

fl <- list.files(path = inputspath, pattern = inputspattern, full.names = T)
which_file <- which.max(file.info(fl)$mtime)

inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[which_file])


modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

minfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
which_file <- which.max(minfo$mtime)
model <- readRDS(list.files(path = modelpath, pattern = "model", full.names = T)[which_file])

# findTopic doens;'t handle regex
# do it ourselves
frex_scores <- labelTopics(model,n = 50)$frex
topic_indicators <- list(ej = c("disadvantaged community", "disadvantaged communities",
                                "^dac$", "^dacs$",
                                "^community$","engagement","outreach","environmental_justice"),
                         dw = c("contaminat", "safe", "public_supply", "drinking_water",
                                "porter-cologne", "fluoride", "water_quality"),
                         cc = c("climate change","projection","projections"),
                         gde = c("groundwater-dependent ecosystem",
                                 "groundwater dependent ecosystem",
                                 "groundwater-dependent ecosystems",
                                 "groundwater dependent ecosystems",
                                 "^gde$","^gdes$","habitat","species","^spp$","vegetation"))

### this returns a vector, not a matrix
### but we can redo as a matrix, filling by row, to create boolean
topic_ids <- lapply(topic_indicators,function(indicator){
word_match_matrix <- matrix(grepl(paste(indicator,collapse = '|'),frex_scores),
          byrow = F,ncol = ncol(frex_scores))
topic_nums <- which(rowSums(word_match_matrix)>0)
topic_nums})



source('Structural_Topic_Model_Paper/Code/utils/estimateEffectDEV.R')


problem_measures = list('ej' = 'percent_dac_by_pop_scaled',
                        'gde' = 'fract_of_area_in_habitat_log_scaled',
                        'cc' = 'maxdryspell_scaled',
                        'dw' = 'urbangw_af_log_scaled')


#### agr interactions ####
foci <- names(problem_measures)
agr_interaction_estimates = lapply(foci,function(x){
   print(x)
   vr <- as.formula(paste("~",as.name(problem_measures[[x]]),collapse = " "))
   fr <- update.formula(vr,topic_ids[[x]] ~ . * Agr_Share_Of_GDP_scaled)
   m <- estimateEffectDEV(fr, metadata = model$settings$covariates$X,group = T,
               stmobj = model)
   m
   })

#### rep vote share interactions ####
rep_interaction_estimates = lapply(foci,function(x){
   print(x)
   vr <- as.formula(paste("~",as.name(problem_measures[[x]]),collapse = " "))
   fr <- update.formula(vr,topic_ids[[x]] ~ . * Republican_Vote_Share_scaled)
   m <- estimateEffectDEV(fr, metadata = model$settings$covariates$X,group = T,
                          stmobj = model)
   m
})

getConfint = function(est,moderator = moderator,
                      moderator.value = moderator.value,cv = cv){
   d = data.table(mean = as.data.table(est$means),
                  t(as.data.table(est$ci)),
                  x = est$x)
   d$moderator = moderator;d$covariate = cv;d$moderator.value = moderator.value
   setnames(d,c('mean.V1','V1','V2'),c('mean','upper','lower'))}


mapply(function(x,y,z,m){
   est <- plot.estimateEffect(x,
                              model = model,
                              method = 'continuous',
                              covariate = y,
                              moderator.value = z,
                              moderator = m)
   getConfint(est = est,mv = m,cv = y)
}, x = rep_interaction_estimates, y = problem_measures, z = 1,m = 'Republican_Vote_Share_scaled')






est


as.formula(tt)

fm <- as.formula(topic_ids[[x]] ~ as.name(problem_measures[[x]]))
              fm
estimateEffectDEV(formula(topic_ids[[x]] ~ as.name(problem_measures[[x]])),
                  metadata = model$settings$covariates$X,group = T,
                  stmobj = model)

lapply(foci,function(x){estimateEffectDEV(formula(paste(as.vectortopic_ids[[x]],"~",problem_measures[[x]],collapse = " ")),
                                          metadata = model$settings$covariates$X,group = T,
                                          stmobj = model)})

ej_interaction0 = estimateEffectDEV(formula = topic_ids$ej ~ Agr_Share_Of_GDP_scaled * (urbangw_af_log_scaled+
                                            percent_dac_by_pop_scaled +
                                           fract_of_area_in_habitat_log_scaled+
                                          maxdryspell_scaled),
                           metadata = model$settings$covariates$X,group = T,
                           stmobj = model)
ej_interaction1 = estimateEffectDEV(formula = topic_ids$ej ~ Agr_Share_Of_GDP_scaled * percent_dac_by_pop_scaled,
                                    metadata = model$settings$covariates$X,group = T,
                                    stmobj = model)

par(mfrow=c(1,2))
plot(ej_interaction0, "percent_dac_by_pop_scaled", method="continuous",
    # cov.value1=1, cov.value2=-1, 
     xlim=c(-2,2), moderator="Agr_Share_Of_GDP_scaled", moderator.value=c(1))
plot(ej_interaction1, "percent_dac_by_pop_scaled", method="continuous",
     #cov.value1=1, cov.value2=-1, 
     xlim=c(-2,2), moderator="Agr_Share_Of_GDP_scaled", moderator.value=c(1))


plot(prep, "treatment", method="pointestimate",
     cov.value1=1, cov.value2=0, xlim=c(-1,1), moderator="binaryvar",
     moderator.value=0)


colnames(model$settings$covariates$X)
summary(test)
* urbangw_af_log_scaled + 
   Republican_Vote_Share_scaled * percent_dac_by_pop_scaled + 
   Republican_Vote_Share_scaled * fract_of_area_in_habitat_log_scaled + 
   Republican_Vote_Share_scaled * maxdryspell_scaled

dim(model$settings$covariates$X)
apply(topic_labels,2,function(x) grepl(paste(topic_indicators$ej,collapse = '|'),x))


grepl(paste(topic_indicators$ej,collapse = '|'),)

#### par(mfrow = c(1,2))
par(mfrow = c(1,2))
tt1 = plot.estimateEffect(test,covariate = 'maxdryspell_scaled',topics = 7,method = 'continuous',
                    moderator = 'Republican_Vote_Share_scaled',npoints = 20,
                    moderator.value = -2,printlegend = F,main = 'drought * low Rep. vote share')
tt2 = plot.estimateEffect(test,covariate = 'maxdryspell_scaled',topics = 7,method = 'continuous',
                    main = 'drought * high Rep. vote share',
                    moderator = 'Republican_Vote_Share_scaled',npoints = 20,
                    moderator.value = 2,printlegend = F)

library(ggplot2)
extractPoints = function(x,x_variable = NULL,conditional_variable = NULL){
   ### x = output from plot.estimateEffect
   ### x_variable = string for name of plot x-axis variable
   ### conditional_variable = string for condition level of moderator variable (.e.g, "high republican vote share")
   cidt = data.table(t(as.data.table(x$ci)))
   setnames(cidt,new = c('lower','upper'))
   cidt = data.table(mean = unlist(x$means),cidt)
   if(!is.null(x_variable)){cidt[[x_variable]] <- tt$x}else{cidt$x <- tt$x}
   if(!is.null(conditional_variable)){cidt$conditional_variable<- conditional_variable}
   return(cidt)
}

df = rbind(
      extractPoints(x = tt1,x_variable = 'maxdryspell_scaled',conditional_variable = 'low Rep. vote share'),
   extractPoints(x = tt2,x_variable = 'maxdryspell_scaled',conditional_variable = 'high Rep. vote share'),
   use.names = T,fill = T)

ggplot(df) + 
   geom_ribbon(fill = NA,aes(x = maxdryspell_scaled,
                             ymin = lower,
                             ymax = upper,col = conditional_variable),lty = 2)+
   geom_path(aes(x = maxdryspell_scaled,y = mean,col = conditional_variable),lty = 1)+
   theme_bw()


tt3 = plot.estimateEffect(test,covariate = 'maxdryspell_scaled',topics = 24,method = 'continuous',
                          moderator = 'Republican_Vote_Share_scaled',npoints = 20,
                          moderator.value = -2,printlegend = F,main = 'drought * low Rep. vote share')
tt4 = plot.estimateEffect(test,covariate = 'maxdryspell_scaled',topics = 24,method = 'continuous',
                          main = 'drought * high Rep. vote share',
                          moderator = 'Republican_Vote_Share_scaled',npoints = 20,
                          moderator.value = 2,printlegend = F)

df2 = rbind(
   extractPoints(x = tt3,x_variable = 'maxdryspell_scaled',conditional_variable = 'low Rep. vote share'),
   extractPoints(x = tt4,x_variable = 'maxdryspell_scaled',conditional_variable = 'high Rep. vote share'),
   use.names = T,fill = T)

ggplot(df2) + 
   geom_ribbon(fill = NA,aes(x = maxdryspell_scaled,
                             ymin = lower,
                             ymax = upper,col = conditional_variable),lty = 2)+
   geom_path(aes(x = maxdryspell_scaled,y = mean,col = conditional_variable),lty = 1)+
   theme_bw()

#####

metadata <- inputs$meta
prev <- as.data.frame(model$theta)

topic_prev <- cbind(prev, metadata)

ntopics <- model$settings$dim$K

tps <- aggregate(as.matrix(topic_prev[,1:ntopics]), data.frame(topic_prev$gsp_id), mean)
rownames(tps) <- tps[,1]
tps <- tps[,2:31]

saveRDS(tps, filekey[filekey$var_name=="topic_prevalence",]$filepath)
