

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
grab_this_inputs = list.files(path = inputspath, pattern = inputspattern, full.names = T)[which_file]
print(grab_this_inputs)
inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[which_file])


modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

minfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
which_file <- which.max(minfo$mtime)
grab_this_model = list.files(path = modelpath, pattern = "model", full.names = T)[which_file]
print(grab_this_model)
model <- readRDS(grab_this_model)

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
                        'dw' = 'log_well_MCL_exceedance_count_by_log_pop_scaled',
                        'cc' = 'dsci_scaled',
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


linear_confints <- rbindlist(lapply(foci,function(f) {
dt <- extract.estimateEffectDEV(interaction_estimates[[f]],
                     npoints = 10,
                    method = 'continuous',
                    model = model,covariate = problem_measures[[f]])
dt$problem <- f
dt}))

linear_confints$problem <- toupper(linear_confints$problem)
gg_problem_severity <- ggplot(linear_confints) + facet_wrap(~problem,scale = 'free') + theme_bw() + 
   ggtitle('Focus on problem as problem severity increases') +
   geom_path(aes(x = covariate.value,y =estimate)) + 
   scale_y_continuous(name = 'estimated topic proportion')+
   scale_x_continuous(name = 'problem severity measure') +
   geom_ribbon(aes(x = covariate.value,max = ci.upper,min = ci.lower),
               col = 'black',fill = NA,lty = 2)
ggsave(plot = gg_problem_severity,filename = 'Structural_Topic_Model_Paper/output/problem_severity.png',dpi = 450,width = 7,height = 7,units = 'in')



gg_cc <- ggplot(data = confint_dt[foci == 'CC',]) + 
   facet_wrap(~moderator,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Moderator',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated climate topic %') + 
   theme(#legend.position = c(0.8,0.2),
         legend.position = 'bottom') +
   ggtitle('Climate focus moderated by local political economy')
ggsave(plot = gg_cc,filename = 'Structural_Topic_Model_Paper/output/climate_interaction.png',dpi = 450,width = 7,height = 3.5,units = 'in')

gg_gde <- ggplot(data = confint_dt[foci == 'GDE',]) + 
   facet_wrap(~moderator,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Moderator',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated GDE topic %') + 
   theme(#legend.position = c(0.8,0.2),
      legend.position = 'bottom') +
   ggtitle('GDE focus moderated by local political economy')
ggsave(plot = gg_gde,filename = 'Structural_Topic_Model_Paper/output/gde_interaction.png',dpi = 450,width = 7,height = 3.5,units = 'in')


gg_ej <- ggplot(data = confint_dt[foci == 'EJ',]) + 
   facet_wrap(~moderator,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Moderator',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated EJ topic %') + 
   theme(#legend.position = c(0.8,0.2),
      legend.position = 'bottom') +
   ggtitle('EJ focus moderated by local political economy')
ggsave(plot = gg_ej,filename = 'Structural_Topic_Model_Paper/output/ej_interaction.png',dpi = 450,width = 7,height = 3.5,units = 'in')

gg_dw <- ggplot(data = confint_dt[foci == 'DW',]) + 
   facet_wrap(~moderator,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Moderator',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated DW topic %') + 
   theme(#legend.position = c(0.8,0.2),
      legend.position = 'bottom') +
   ggtitle('DW focus moderated by local political economy')
ggsave(plot = gg_dw,filename = 'Structural_Topic_Model_Paper/output/dw_interaction.png',dpi = 450,width = 7,height = 3.5,units = 'in')


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

gg_rep <- ggplot(data = confint_dt[moderator == "Republican_Vote_Share_scaled" ,]) + 
   facet_wrap(~foci,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Republican_Vote_Share',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated marginal effect') + 
   theme(#legend.position = c(0.8,0.2),
      legend.position = 'bottom') +
   ggtitle('Policy priorites moderated by Republican vote share')
ggsave(plot = gg_rep,filename = 'Structural_Topic_Model_Paper/output/rep_interaction.png',dpi = 450,width = 7,height = 7,units = 'in')


