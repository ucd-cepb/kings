

library(stm)
library(data.table)
library(stringr)
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
topic_indicators <- lapply(topic_indicators,str_replace_all,'\\s','_')

### this returns a vector, not a matrix
### but we can redo as a matrix, filling by row, to create boolean
topic_ids <- lapply(topic_indicators,function(indicator){
word_match_matrix <- matrix(grepl(paste(indicator,collapse = '|'),frex_scores),
          byrow = F,ncol = ncol(frex_scores))
topic_nums <- which(rowSums(word_match_matrix)>0)
topic_nums})

source('Structural_Topic_Model_Paper/Code/utils/estimateEffectDEV.R')

problem_measures = list('ej' = 'percent_dac_by_pop_scaled',
                        'dw' = 'well_MCL_exceedance_count_by_log_pop_scaled',
                        'cc' = 'maxdryspell_scaled',
                        'gde' = 'fract_of_area_in_habitat_log_scaled'
                        )

foci <- names(problem_measures)
problem_estimates <- vector(mode = 'list',length = length(foci))
for(x in foci){
   print(x)
   problem <- problem_measures[[x]]
   topic_vec <- topic_ids[[x]]
   print(paste('topic',topic_vec))
   #vr <- as.formula(paste("~",paste0('s(',as.name(problem),',4)'),collapse = " "))
   vr <- as.formula(paste("~",as.name(problem),collapse = " "))
   fr <- update.formula(vr,topic_vec ~ . )
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = model$settings$covariates$X,group = T,
                             stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = model$settings$covariates$X,stmobj = model)
   }
   problem_estimates[[match(x,foci)]] <- m
}
names(problem_estimates) <- paste(foci)

#### agr interactions ####
agr_interaction_estimates <- vector(mode = 'list',length = length(foci))
for(x in foci){
   print(x)
   problem <- problem_measures[[x]]
   topic_vec <- topic_ids[[x]]
   print(paste('topic',topic_vec))
   #vr <- as.formula(paste("~",paste0('s(',as.name(problem),',4)'),collapse = " "))
   vr <- as.formula(paste("~",as.name(problem),collapse = " "))
   fr <- update.formula(vr,topic_vec ~ . * Agr_Share_Of_GDP_scaled)
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = model$settings$covariates$X,group = T,
               stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = model$settings$covariates$X,stmobj = model)
   }
   agr_interaction_estimates[[match(x,foci)]] <- m
}
names(agr_interaction_estimates) <- paste(foci,'agr',sep = '_')


repvote_interaction_estimates <- vector(mode = 'list',length = length(foci))
for(x in foci){
   print(x)
   problem <- problem_measures[[x]]
   topic_vec <- topic_ids[[x]]
   print(paste('topic',topic_vec))
   #vr <- as.formula(paste("~",paste0('s(',as.name(problem),',4)'),collapse = " "))
   vr <- as.formula(paste("~",as.name(problem),collapse = " "))
   fr <- update.formula(vr,topic_vec ~ . * Republican_Vote_Share_scaled)
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = model$settings$covariates$X,group = T,
                             stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = model$settings$covariates$X,stmobj = model)
   }
   repvote_interaction_estimates[[match(x,foci)]] <- m
}
names(repvote_interaction_estimates) <- paste(foci,'rep',sep = '_')


interaction_estimates <- c(problem_estimates,agr_interaction_estimates,repvote_interaction_estimates)
saveRDS(object = interaction_estimates,file = 'data/Structural_Topic_Model_Paper/interaction_estimates.RDS')
inter_grid <- expand.grid(moderator = c('Republican_Vote_Share_scaled','Agr_Share_Of_GDP_scaled'),
                          moderator.value = c(-1,1),
                          covariate = unlist(problem_measures))
inter_grid$foci <- rep(foci,each = 4)
inter_grid$model.match <- paste0(inter_grid$foci,ifelse(grepl('Agr',inter_grid$moderator),'_agr','_rep'))

# getConfint = function(est,moderator = moderator,
#                       moderator.value = moderator.value,
#                       covariate = covariate){
#    d = data.table(mean = as.data.table(est$means),
#                   t(as.data.table(est$ci)),
#                   x = est$x)
#    d$moderator = moderator;d$covariate = covariate;d$moderator.value = moderator.value
#    setnames(d,c('mean.V1','V1','V2'),c('mean','upper','lower'))}


#inter_grid <- inter_grid[inter_grid$covariate=='maxdryspell_scaled',]

confint_list <- lapply(1:nrow(inter_grid),function(i){
   print(i)
   # temp_est <- plot.estimateEffect(
   #                   x = interaction_estimates[[inter_grid$model.match[i]]],
   #                     model = model,
   #                     method = 'continuous',
   #                     moderator = as.character(inter_grid$moderator[i]),
   #                     moderator.value = inter_grid$moderator.value[i],
   #                     covariate = as.character(inter_grid$covariate[i]))
   # 
   temp_est <- extract.estimateEffectDEV( x = interaction_estimates[[inter_grid$model.match[i]]],
                              model = model,
                              method = 'continuous',
                              moderator = as.character(inter_grid$moderator[i]),
                              moderator.value = inter_grid$moderator.value[i],
                              covariate = as.character(inter_grid$covariate[i]))
   temp_est$model <- inter_grid$model.match[i]
   temp_est
   })


confint_dt <- rbindlist(confint_list,use.names = T,fill = T)
confint_dt$foci <- inter_grid$foci[match(confint_dt$covariate,inter_grid$covariate)]


library(tidyverse)
library(ggthemes)
confint_dt$foci <- toupper(confint_dt$foci)
saveRDS(object = confint_dt,'data/Structural_Topic_Model_Paper/confint_dt.RDS')



gg_cc <- ggplot(data = confint_dt[foci == 'cc',]) + 
   facet_wrap(~moderator,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Rep. vote share',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated marginal effect') + 
   theme(#legend.position = c(0.8,0.2),
         legend.position = 'bottom') +
   ggtitle('Policy priorites moderated by Rep. vote share')
ggsave(plot = gg_repvote,filename = 'Structural_Topic_Model_Paper/output/repvote_interaction.png',dpi = 450,width = 7,height = 7,units = 'in')


gg_agr <- ggplot(data = confint_dt[moderator == "Agr_Share_Of_GDP_scaled" ,]) + 
   facet_wrap(~foci,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Agr. share of GDP',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated marginal effect') + 
   theme(#legend.position = c(0.8,0.2),
      legend.position = 'bottom') +
   ggtitle('Policy priorites moderated by agric. industry')
ggsave(plot = gg_agr,filename = 'Structural_Topic_Model_Paper/output/agr_interaction.png',dpi = 450,width = 7,height = 7,units = 'in')



topic_ids
mapply(function(moderator,covariate,moderator.value,model.match) {
                        plot.estimateEffect(x = interaction_estimates[[model.match]],
                           model = model,
                           method = 'continuous',
                           moderator = moderator,
                           moderator.value = as.numeric(moderator.value),
                           covariate = covariate)},
       moderator = inter_grid$moderator,moderator.value = inter_grid$moderator.value,
       covariate = inter_grid$covariate,model.match = inter_grid$model.match)


                           ))

test = mapply(function(x,y,z,m){
   est <- plot.estimateEffect(x,
                              model = model,
                              method = 'continuous',
                              covariate = y,
                              moderator.value = z,
                              moderator = m)
   getConfint(est = est,moderator.value = m,cv = y)
}, x = repvote_interaction_estimates, y = problem_measures, z = 1,m = 'Republican_Vote_Share_scaled')






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
