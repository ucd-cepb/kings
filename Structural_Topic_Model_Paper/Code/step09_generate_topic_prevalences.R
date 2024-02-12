filekey <- read.csv("filekey.csv")
#### this code was taken from step08 ###
inputsfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
inputsfilenamesplits <- unlist(strsplit(inputsfilename,split="/"))
inputspath <- paste(inputsfilenamesplits[1:(length(inputsfilenamesplits)-1)],collapse = "/")
inputspattern <- inputsfilenamesplits[length(inputsfilenamesplits)]

inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[
   length(list.files(path = inputspath, pattern = inputspattern, full.names = T))])


modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

model <- readRDS(list.files(path = modelpath, pattern = "model", full.names = T)[
   length(list.files(path = modelpath, pattern = "model", full.names = T))])


library(stm)
install.packages('stm')

coef(model)
formula(model)


summary(model)
test = estimateEffect(metadata = model$settings$covariates$X,
                      stmobj = model,
                      formula = c(7,24) ~ Republican_Vote_Share_scaled * maxdryspell_scaled)

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
