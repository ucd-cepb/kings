
##### NOTE THAT THIS CODE BUILDS FROM WAY BACK TO START
##### TO SKIP SOME STEPS, SET "SKIP_AHEAD" = TRUE
SKIP_AHEAD<-TRUE
packs <- c('stm','tm','SnowballC','tidytext','data.table','tidyverse','sf','pbapply','geometry','Rtsne','rsvd','stringi','stringr','scico')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

source('functions/lex_clean.R')
source('functions/create_lang_meta.R')
source('functions/create_spat_meta.R')
source('functions/generate_place_names.R')

if(!SKIP_AHEAD){
gsp_text_with_lang <- create_lang_meta()

#retrieves the latest save of gsp_text_with_lang
#generated in create_lang_meta, which allows create_lang_meta() to be skipped

gsp_text_with_lang <- readRDS(
   list.files(path = "data_output",pattern = "lang", full.names = T)[length(
      list.files(path = "data_output", pattern = "lang", full.names = T))])


type = "area"
#or type = "pop"
gsp_svi_adjusted <- create_svi_meta(type)

#retrieves lates save of gsp_svi_adjusted of the given type, which allows
#create_svi_meta to be skipped
gsp_svi_adjusted <- readRDS(
   list.files(path = "data_output",pattern = type,full.names = T)[length(
      list.files(path = "data_output",pattern = type,full.names = T))])


#rows = num docs; cols = metadata types

#gsp_meta <- data.table(matrix(ncol = 4, nrow = 0))
#colnames(gsp_meta) <- c("GSA","community_attributes","ag_importance","soc_vuln")

gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_svi_adjusted, by = c("gsp_id"="gsp_num_id"))
#filtering NA admin = proxy for GSPs whose texts have not been processed
gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))

saveRDS(gsp_text_with_meta, file = paste0("data_output/","gsp_docs_w_meta_",format(Sys.time(), "%Y%m%d-%H:%M")))
}

#retrieves the latest save of gsp_text_with_meta
gsp_text_with_meta <- readRDS(
   list.files(path = "data_output",pattern = "meta", full.names = T)[length(
      list.files(path = "data_output", pattern = "meta", full.names = T))])
gsp_out <- lex_clean(gsp_text_with_meta)

#test svi for both
#test k = 5, 10, 20, 40, 80
#cut bad characters
#drop anything that doesn't have a letter
#regex or name density check against USGS placenames database
#or regex for terrible matches and outputs a stopword matrix
#figure out how often topic 9 words show up on the document term matrix (grep) row names for where equals > 0
#prevalence = how often topic is discussed. category vars included here. svi included.
#content = word frequency within topic. allows one categorical var, advised to be small


gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 K = 50, prevalence =~ admin + 
                    basin_plan +
                    sust_criteria +
                    monitoring_networks + 
                    projects_mgmt_actions + 
                    as.factor(gsp_id) +
                    SVI_na_adj+
                    as.factor(approval)+
                    as.factor(priority)+
                    ag_gw_asfractof_tot_gw,
                 max.em.its = 120,
                 data = gsp_out$meta, init.type = "Spectral")  

#dummy for how many gsas are involved: multiple or one

saveRDS(gsp_model, file = paste0("data_output/","model_",format(Sys.time(), "%Y%m%d-%H:%M")))

gsp_model_saved <- readRDS(list.files(path = "data_output", pattern = "model", full.names = T)[length(
   list.files(path = "data_output", pattern = "model", full.names = T))])


#inspect words associated with topics using labelTopics
labelTopics(gsp_model_saved, c(1:50))

#example:
#how to let searchK figure out how many topics to generate
storage <- searchK(gsp_out$documents, gsp_out$vocab, K = c(7, 10),
                   + prevalence =~ rating + s(day), data = meta)

#example:
#   see page 9 of stm documentation
#max iterations = max.em.its
#highly recommend init.type = "Spectral" because it eliminates initiation-
#based sensitivity


#topics are evaluated on two components:
#semantic coherence (frequency of co-occurrence of common words in a toipc)
#exclusivity of words to topics

#uses selectModels object
#example:
#how to plot quality of models
plotModels(poliblogSelect, pch = c(1, 2, 3, 4),
           + legend.position = "bottomright")
#can also use topicQuality

#choose your favorite model
selectedmodel <- poliblogSelect$runout[[3]]


#sageLabels can be used when the model has a content covariate
#both print highest probability words and FREX words associated with each topic
#FREX is weighted by frequency and exclusivity
#lift() weights words higher if they have lower frequency in other topics
#score() is similar, but log based -- see lda package

#findThoughts example
#thoughts3 <- findThoughts(poliblogPrevFit, texts = shortdoc, n = 2,
#+ topics = 6)$docs[[1]]
#par example
#par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))


#look at the relationship between metadata and topics
#estimateEffect
#call summary on estimateEffect object to print summary

#example:
#out$meta$rating <- as.factor(out$meta$rating)
#prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
                          #+ meta = out$meta, uncertainty = "Global")
#summary(prep, topics = 1)


#plot(...,type = "labels"),

#plot(...,type = "perspectives"), 

#plot(poliblogPrevFit, type = "summary", xlime = c(0,0.3))

#cloud function plots a word cloud

#plotQuote is a graphical wrapper so you can present documents as examples
#example
#plotQuote(thoughts3, width = 30, main = "Topic 6")


#calculate topic correlations: topicCorr



#example of loading workspace with model loaded to reduce compile time
load(results.rda)