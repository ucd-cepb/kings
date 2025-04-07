

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
input_file = list.files(path = inputspath, pattern = inputspattern, full.names = T)[which_file]
print(input_file)
inputs <- readRDS(input_file)

modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

minfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
which_file <- which.max(minfo$mtime)
model_file <- list.files(path = modelpath, pattern = "model", full.names = T)[which_file]
print(model_file)
model <- readRDS(model_file)

base_form <- ~ admin + 
   basin_plan +
   sust_criteria +
   monitoring_networks + 
   projects_mgmt_actions + 
   mult_gsas +
   exante_collab +
   priority_category +
   basin_population_log_scaled +
   Republican_Vote_Share_scaled + 
   Agr_Share_Of_GDP_scaled

# 
# linear_terms <- update.formula(base_form,~. + )
# 
# interaction_combos = expand.grid(moderator = as.character(c('Agr_Share_Of_GDP_scaled',
#                'Republican_Vote_Share_scaled')),
#            covariate = as.character(c('log_well_MCL_exceedance_count_by_log_pop_scaled',
#                  'percent_dac_by_pop_scaled',
#                  'fract_of_area_in_habitat_log_scaled',
#                  'dsci_scaled')),stringsAsFactors = F)
# 
# paste(as.vector(interaction_combos[1,]),collapse = '*')
# 
# inter_forms = lapply(1:nrow(interaction_combos),function(i) {
# update.formula(base_form,as.formula(paste('~ . + ',paste(interaction_combos[i,],collapse = '*'))))
# })
# 
# all_forms = c(base_form,inter_forms)

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
   vr <- as.formula(paste("~",base_form,'+',as.name(problem),collapse = " "))
   fr <- update.formula(vr,topic_vec ~ . )
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = inputs$meta,group = T,
                             stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = inputs$meta,stmobj = model)
   }
   problem_estimates[[match(x,foci)]] <- m
}
names(problem_estimates) <- paste(foci)

# 
pol_measures = list('politics' = 'Republican_Vote_Share_scaled',
                    'econ_interest' = 'Agr_Share_Of_GDP_scaled')
# pol_estimates <- vector(mode = 'list',length = length(foci))
# for(x in foci){
#    print(x)
#    pol1 <- pol_measures[1]
#    pol2 <- pol_measures[2]
#    topic_vec <- topic_ids[[x]]
#    print(paste('topic',topic_vec))
#    #vr <- as.formula(paste("~",paste0('s(',as.name(problem),',4)'),collapse = " "))
#    vr <- as.formula(paste("~",base_form,'+',pol1,'+',pol2,collapse = " "))
#    fr <- update.formula(vr,topic_vec ~ . )
#    if(length(topic_vec)>1){
#       m <- estimateEffectDEV(fr, metadata = inputs$meta,group = T,
#                              stmobj = model)
#    }else{
#       m <- estimateEffect(fr, metadata = inputs$meta,stmobj = model)
#    }
#    pol_estimates[[match(x,foci)]] <- m
# }
# names(pol_estimates) <- paste0('pol_',foci)
# 

#### agr interactions ####
agr_interaction_estimates <- vector(mode = 'list',length = length(foci))
for(x in foci){
   print(x)
   problem <- problem_measures[[x]]
   topic_vec <- topic_ids[[x]]
   print(paste('topic',topic_vec))
   vr = formula(sprintf('~%s + %s*Agr_Share_Of_GDP_scaled', as.character(base_form),problem)[2])
   fr <- update.formula(vr,topic_vec ~ . )
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = inputs$meta,group = T,
                             stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = inputs$meta,stmobj = model)
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
   vr = formula(sprintf('~%s + %s*Republican_Vote_Share_scaled', as.character(base_form),problem)[2])
   fr <- update.formula(vr,topic_vec ~ . )
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = inputs$meta,group = T,
                             stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = inputs$meta,stmobj = model)
   }
   repvote_interaction_estimates[[match(x,foci)]] <- m
}
names(repvote_interaction_estimates) <- paste(foci,'rep',sep = '_')


interaction_estimates <- c(problem_estimates,#pol_estimates,
                           agr_interaction_estimates,
                           repvote_interaction_estimates)
saveRDS(object = interaction_estimates,file = 'data/Structural_Topic_Model_Paper/interaction_estimates_RnR.RDS')

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



model_summaries = lapply(interaction_estimates,summary)
model_tabs = lapply(seq_along(model_summaries),function(x) data.frame(model_summaries[[x]]$tables[[1]],
                                                                      coef = rownames(model_summaries[[x]]$tables[[1]]),
                                                                      model = names(model_summaries)[x]))

model_coefs <- rbindlist(model_tabs)
library(gtsummary)

model_coefs$Estimate <- round(model_coefs$Estimate,4)
model_coefs$SE <- round(model_coefs$Std..Error,4)
model_coefs$p.value <- round(model_coefs$Pr...t..,4)


library(texreg)
linear_mods = c('ej','dw','cc','gde')
agr_mods = paste0(c('ej','dw','cc','gde'),'_agr')
rep_mods = paste0(c('ej','dw','cc','gde'),'_rep')
tr <- lapply(linear_mods, function(x) {
   d <- model_coefs[model_coefs$model == x, ]
   t <- createTexreg(coef.names = as.character(d$coef),
                     coef = d$Estimate,
                     se = d$Std..Error,
                     pvalues = d$p.value,
                     model.name = x)
   return(t)
})
htmlreg(tr,single.row = T,caption.above = 'Base model estimates',
        file = 'Structural_Topic_Model_Paper/output/table_base_coef_appendixRnR.html')

tr <- lapply(agr_mods, function(x) {
   d <- model_coefs[model_coefs$model == x, ]
   t <- createTexreg(coef.names = as.character(d$coef),
                     coef = d$Estimate,
                     se = d$Std..Error,
                     pvalues = d$p.value,
                     model.name = x)
   return(t)
})
htmlreg(tr,single.row = T,caption.above = 'Problem severity x agr. share of GDP',
        file = 'Structural_Topic_Model_Paper/output/table_agr_coef_appendixRnR.html')


tr <- lapply(rep_mods, function(x) {
   d <- model_coefs[model_coefs$model == x, ]
   t <- createTexreg(coef.names = as.character(d$coef),
                     coef = d$Estimate,
                     se = d$Std..Error,
                     pvalues = d$p.value,
                     model.name = x)
   return(t)
})
htmlreg(tr,single.row = T,caption.above = 'Problem severity x Rep. vote share',
        file = 'Structural_Topic_Model_Paper/output/table_grep_coef_appendixRnR.html')



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
   temp_est <- extract.estimateEffect( x = interaction_estimates[[inter_grid$model.match[i]]],
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
saveRDS(object = confint_dt,'data/Structural_Topic_Model_Paper/confint_dt_RnR.RDS')


linear_confints <- rbindlist(lapply(foci,function(f) {
   dt <- extract.estimateEffect(interaction_estimates[[f]],
                                npoints = 10,
                                method = 'continuous',
                                model = model,covariate = problem_measures[[f]])
   dt$problem <- f
   dt}))

linear_confints$problem <- toupper(linear_confints$problem)

# New facet label names for problem variable
prob.labs <- c("Climate Change", "Drinking Water", 
               "Environmental Justice", 
               "Groundwater Dependent Ecosystems")
names(prob.labs) <- c("CC", "DW", "EJ", "GDE")

ps_text <- data.frame(
   label = c("Hydroclimate Pressure", "Drinking Water Contamination", 
             "% DAC Population", "GDE Habitat"),
   problem   = c("CC", "DW", "EJ", "GDE"),
   x     = c(-1.7, -2.3, -0.8, -1.2),
   y     = c(0.02, 0.10, 0.060, -0.02)
)

gg_problem_severity <- ggplot(linear_confints) + 
   facet_wrap(~problem,scale = 'free',
              labeller = labeller(problem = prob.labs)) + 
   theme_bw() + 
   ggtitle('Change in topic prevalence as problem severity increases') +
   geom_path(aes(x = covariate.value,y =estimate)) + 
   scale_y_continuous(name = 'estimated topic proportion')+
   scale_x_continuous(name = 'problem severity measure') +
   geom_ribbon(aes(x = covariate.value,max = ci.upper,min = ci.lower),
               col = 'black',fill = NA,lty = 2)
saveRDS(gg_problem_severity, "gg_problem_severity_RnR")
gg_problem_severity <- readRDS("gg_problem_severity_RnR")
gg_problem_severity <- gg_problem_severity + ggtitle('Change in topic prevalence as problem severity increases') +
   scale_y_continuous(name = 'estimated topic prevalence') +
   theme(panel.spacing.y = unit(1.8, "lines")) +
   coord_cartesian(clip = 'off') +
   scale_x_continuous(name = '') + geom_text(
      data    = ps_text,
      mapping = aes(x = x, y = y, label = label),
      hjust   = "inward",
      vjust   = 4
   )
ggsave(plot = gg_problem_severity,filename = 'Structural_Topic_Model_Paper/output/problem_severityRnR_withtext.png',dpi = 450,width = 7,height = 7,units = 'in')


linear_pol_confints <- rbindlist(lapply(foci,function(f) {
   f_pol = paste0('pol_',f)
   dt <- extract.estimateEffect(interaction_estimates[[f]],
                                npoints = 10,
                                method = 'continuous',
                                model = model,
                                covariate = unlist(pol_measures)[1])
   dt2 <- extract.estimateEffect(interaction_estimates[[f]],
                                 npoints = 10,
                                 method = 'continuous',
                                 model = model,
                                 covariate = unlist(pol_measures)[2])
   dt <- rbind(dt,dt2)
   dt$problem <- f
   dt}))

linear_pol_confints$problem <- toupper(linear_pol_confints$problem)



(gg_pol <- ggplot(linear_pol_confints,aes(col = covariate,group = covariate)) + 
      facet_wrap(~problem,scale = 'free_y',
                 labeller = labeller(problem = prob.labs)) + 
      theme_bw() + 
      ggtitle('Focus on topic given local political economy') +
      geom_path(aes(x = covariate.value,y =estimate)) + 
      scale_y_continuous(name = 'estimated topic proportion')+
      scale_x_continuous(name = 'local political economy measure') +
      scale_color_manual(values =  colorblind_pal()(8)[c(3,7)],
                         labels = c('Agr. share of GDP','Republican vote share')) + 
      theme(legend.position = 'bottom',legend.title = element_blank()) + 
      geom_ribbon(aes(x = covariate.value,
                      max = ci.upper,min = ci.lower,col = covariate),fill = NA,lty = 2) +
      NULL)
saveRDS(gg_pol, "gg_pol_RnR")
ggsave(plot = gg_pol,filename = 'Structural_Topic_Model_Paper/output/political_econ_linearRnR.png',dpi = 450,width = 7,height = 7,units = 'in')
gg_pol <- readRDS("gg_pol_RnR")
gg_pol <- gg_pol + 
   scale_y_continuous(name = 'estimated topic prevalence')+
   scale_color_manual(values =  colorblind_pal()(8)[c(3,7)],
                      labels = c('Agr. Share of Basin GDP',
                                 'Republican Vote Share')) + 
   ggtitle('Change in topic prevalence given local political economy') 
ggsave(plot = gg_pol,filename = 'Structural_Topic_Model_Paper/output/political_econ_linearRnR.png',dpi = 450,width = 7,height = 7,units = 'in')


# #### current climate problem ###
# x <- 'cc'
# problem <- 'gwsum'
# topic_vec <- topic_ids[[x]]
# print(paste('topic',topic_vec))
# #vr <- as.formula(paste("~",paste0('s(',as.name(problem),',4)'),collapse = " "))
# vr <- as.formula(paste("~",as.name(problem),collapse = " "))
# fr <- update.formula(vr,topic_vec ~ . )
# m <- estimateEffectDEV(fr, metadata = model$settings$covariates$X,group = T,
#                        stmobj = model)
# 
# ex <- extract.estimateEffectDEV(m,
#                                 model = model,covariate = 'gwsum')
# 
# gg_gwsum <- ggplot(data = ex) + geom_errorbar(aes(ymin = ci.lower,
#                                                   ymax = ci.upper,
#                                                   x = covariate.value),width = 0.15)+ 
#    geom_point(aes(x = covariate.value,y = estimate)) +
#    theme_bw() +
#    scale_y_continuous(name = 'estimated climate topic proportion')+
#    scale_x_continuous(name = 'Priority points (intrusion + dry wells + subsidence') +
#    ggtitle('Plan climate focus by basin priority level (current climate pressures)')
# 
# ggsave(plot = gg_gwsum,
#        filename = 'Structural_Topic_Model_Paper/output/current_climate_pressure.png',
#        dpi = 450,width = 7,height = 6.5,units = 'in')


(gg_cc <- ggplot(data = confint_dt[foci == 'CC',]) + 
      facet_wrap(~moderator,scale = 'free_x') + theme_bw() + 
      geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
      geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
      scale_color_colorblind(name = 'Moderator',
                             labels = c('-1 SD','+1 SD')) +
      scale_x_continuous('Hydroclimate Pressure') + 
      scale_y_continuous(name = 'estimated climate topic proportion') + 
      theme(#legend.position = c(0.8,0.2),
         legend.position = 'bottom') +
      ggtitle('Climate topic prevalence moderated by local political economy'))
saveRDS(gg_cc, "cc_plot_RnR")

gg_cc <- readRDS("cc_plot_RnR")
mod.labs <- c("Agr. Share of Basin GDP", "Republican Vote Share")
names(mod.labs) <- c("Agr_Share_Of_GDP_scaled",
                     "Republican_Vote_Share_scaled")
gg_cc <- gg_cc + facet_wrap(~moderator,scale = 'free_x',
                            labeller = labeller(moderator = mod.labs)) +
   scale_y_continuous(name = 'estimated climate topic prevalence') + 
   scale_x_continuous('Hydroclimate Pressure') + 
   ggtitle('Climate topic prevalence moderated by local political economy')
ggsave(plot = gg_cc,filename = 'Structural_Topic_Model_Paper/output/climate_interactionRnR.png',dpi = 450,width = 7,height = 3.5,units = 'in')



(gg_gde <- ggplot(data = confint_dt[foci == 'GDE',]) + 
      facet_wrap(~moderator,scale = 'free_x',
                 labeller = labeller(moderator = mod.labs)) + theme_bw() + 
      geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
      geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
      scale_color_colorblind(name = 'Moderator',
                             labels = c('-1 SD','+1 SD')) +
      scale_x_continuous('ecosystem need measure (scaled)') + 
      scale_y_continuous(name = 'estimated GDE topic %') + 
      theme(#legend.position = c(0.8,0.2),
         legend.position = 'bottom') +
      ggtitle('GDE focus moderated by local political economy'))
saveRDS(gg_gde, "gde_plot_RnR")

gg_gde <- readRDS("gde_plot_RnR")
mod.labs <- c("Agr. Share of Basin GDP", "Republican Vote Share")
names(mod.labs) <- c("Agr_Share_Of_GDP_scaled",
                     "Republican_Vote_Share_scaled")
gg_gde <- gg_gde + facet_wrap(~moderator,scale = 'free_x',
                            labeller = labeller(moderator = mod.labs)) +
   scale_y_continuous(name = 'estimated GDE topic prevalence') + 
   scale_x_continuous('GDE Habitat') + 
   ggtitle('GDE topic prevalence moderated by local political economy')
ggsave(plot = gg_gde,filename = 'Structural_Topic_Model_Paper/output/gde_interactionRnR.png',dpi = 450,width = 7,height = 3.5,units = 'in')


(gg_ej <- ggplot(data = confint_dt[foci == 'EJ',]) + 
      facet_wrap(~moderator,scale = 'free_x') + theme_bw() + 
      geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
      geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
      scale_color_colorblind(name = 'Moderator',
                             labels = c('-1 SD','+1 SD')) +
      scale_x_continuous('ratio of residents in economically disadvantaged area (scaled)') + 
      scale_y_continuous(name = 'estimated EJ topic %') + 
      theme(#legend.position = c(0.8,0.2),
         legend.position = 'bottom') +
      ggtitle('EJ focus moderated by local political economy'))
saveRDS(gg_ej, "ej_plot_RnR")

gg_ej <- readRDS("ej_plot_RnR")
mod.labs <- c("Agr. Share of Basin GDP", "Republican Vote Share")
names(mod.labs) <- c("Agr_Share_Of_GDP_scaled",
                     "Republican_Vote_Share_scaled")
gg_ej <- gg_ej + facet_wrap(~moderator,scale = 'free_x',
                              labeller = labeller(moderator = mod.labs)) +
   scale_y_continuous(name = 'estimated EJ topic prevalence') + 
   scale_x_continuous('% DAC Population') + 
   ggtitle('EJ topic prevalence moderated by local political economy')


ggsave(plot = gg_ej,filename = 'Structural_Topic_Model_Paper/output/ej_interactionRnR.png',dpi = 450,width = 7,height = 3.5,units = 'in')

(gg_dw <- ggplot(data = confint_dt[foci == 'DW',]) + 
      facet_wrap(~moderator,scale = 'free_x') + theme_bw() + 
      geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
      geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
      scale_color_colorblind(name = 'Moderator',
                             labels = c('-1 SD','+1 SD')) +
      scale_x_continuous('drinking water contamination measure (scaled)') + 
      scale_y_continuous(name = 'estimated DW topic %') + 
      theme(#legend.position = c(0.8,0.2),
         legend.position = 'bottom') +
      ggtitle('Drinking water focus moderated by local political economy'))
saveRDS(gg_dw, "dw_plot_RnR")

gg_dw <- readRDS("dw_plot_RnR")
mod.labs <- c("Agr. Share of Basin GDP", "Republican Vote Share")
names(mod.labs) <- c("Agr_Share_Of_GDP_scaled",
                     "Republican_Vote_Share_scaled")
gg_dw <- gg_dw + facet_wrap(~moderator,scale = 'free_x',
                              labeller = labeller(moderator = mod.labs)) +
   scale_y_continuous(name = 'estimated DW topic prevalence') + 
   scale_x_continuous('Drinking Water Contamination') + 
   ggtitle('Drinking water topic prevalence moderated by local political economy')


ggsave(plot = gg_dw,filename = 'Structural_Topic_Model_Paper/output/dw_interactionRnR.png',dpi = 450,width = 7,height = 3.5,units = 'in')


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
ggsave(plot = gg_agr,filename = 'Structural_Topic_Model_Paper/output/agr_interactionRnR.png',dpi = 450,width = 7,height = 7,units = 'in')

gg_rep <- ggplot(data = confint_dt[moderator == "Republican_Vote_Share_scaled" ,]) + 
   facet_wrap(~foci,scale = 'free') + theme_bw() + 
   geom_path(aes(x = covariate.value,y = estimate,col = as.factor(moderator.value))) +
   geom_ribbon(aes(x = covariate.value,max= ci.upper,min = ci.lower,col = as.factor(moderator.value)),lty = 2,fill = NA) + 
   scale_color_colorblind(name = 'Republican vote share',
                          labels = c('-1 SD','+1 SD')) +
   scale_x_continuous('problem measure (scaled)') + 
   scale_y_continuous(name = 'estimated marginal effect') + 
   theme(#legend.position = c(0.8,0.2),
      legend.position = 'bottom') +
   ggtitle('Policy priorites moderated by Republican vote share')
ggsave(plot = gg_rep,filename = 'Structural_Topic_Model_Paper/output/rep_interactionRnR.png',dpi = 450,width = 7,height = 7,units = 'in')



#### Making giant plot
#### agr and rep interactions ####
agr_rep_interaction_estimates <- vector(mode = 'list',length = length(foci))
for(x in foci){
   print(x)
   problem <- problem_measures[[x]]
   topic_vec <- topic_ids[[x]]
   print(paste('topic',topic_vec))
   vr = formula(sprintf('~%s + %s*Agr_Share_Of_GDP_scaled + %s*Republican_Vote_Share_scaled', as.character(base_form),problem, problem)[2])
   fr <- update.formula(vr,topic_vec ~ . )
   if(length(topic_vec)>1){
      m <- estimateEffectDEV(fr, metadata = inputs$meta,group = T,
                             stmobj = model)
   }else{
      m <- estimateEffect(fr, metadata = inputs$meta,stmobj = model)
   }
   agr_rep_interaction_estimates[[match(x,foci)]] <- m
}
names(agr_rep_interaction_estimates) <- paste(foci,'agr_rep',sep = '_')


saveRDS(object = agr_rep_interaction_estimates,file = 'data/Structural_Topic_Model_Paper/agr_rep_interaction_estimates_estimates_RnR.RDS')

agr_rep_summaries = lapply(agr_rep_interaction_estimates,summary)
agr_rep_tabs = lapply(seq_along(agr_rep_summaries),function(x) data.frame(agr_rep_summaries[[x]]$tables[[1]],
                                                                      coef = rownames(agr_rep_summaries[[x]]$tables[[1]]),
                                                                      model = names(agr_rep_summaries)[x]))

agr_rep_coefs <- rbindlist(agr_rep_tabs)
library(gtsummary)

agr_rep_coefs$Estimate <- round(agr_rep_coefs$Estimate,4)
agr_rep_coefs$SE <- round(agr_rep_coefs$Std..Error,4)
agr_rep_coefs$p.value <- round(agr_rep_coefs$Pr...t..,4)


library(texreg)
agr_rep_mods = c('ej_agr_rep','dw_agr_rep','cc_agr_rep','gde_agr_rep')

tr <- lapply(agr_rep_mods, function(x) {
   d <- agr_rep_coefs[agr_rep_coefs$model == x, ]
   t <- createTexreg(coef.names = as.character(d$coef),
                     coef = d$Estimate,
                     se = d$Std..Error,
                     pvalues = d$p.value,
                     model.name = x)
   return(t)
})
htmlreg(tr,single.row = T,caption.above = 'Problem severity x agr. share of GDP x Rep. vote share',
        file = 'Structural_Topic_Model_Paper/output/table_agr_rep_coef_appendixRnR.html')



