library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)

source('functions/build_corpus.R')
source('functions/create_lang_meta.R')
source('functions/create_svi_meta.R')

gsp_text_with_lang <- create_lang_meta()

#retrieves the latest save of gsp_text_with_lang
#generated in create_lang_meta, which allows create_lang_meta() to be skipped

gsp_text_with_lang <- readRDS(
   list.files(path = "data_output",pattern = "lang", full.names = T)[length(
      list.files(path = "data_output", pattern = "lang", full.names = T))])


type = "area"
#or type = "pop"
gsp_svi_adjusted <- create_svi_meta(type)


#rows = num docs; cols = metadata types

#gsp_meta <- data.table(matrix(ncol = 4, nrow = 0))
#colnames(gsp_meta) <- c("GSA","community_attributes","ag_importance","soc_vuln")

gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_svi_adjusted, by = c("gsp_id"="gsp_num_id"))

saveRDS(gsp_text_with_meta, file = paste0("data_output/","gsp_docs_w_meta_",format(Sys.time(), "%Y%m%d-%H:%M")))

#retrieves the latest save of gsp_text_with_meta
gsp_text_with_meta <- readRDS(
   list.files(path = "data_output", pattern = "docs", full.names = T)[length(
      list.files(path = "data_output", pattern = "docs", full.names = T))])

gsp_corpus <- build_corpus(gsp_text_with_meta)
gsp_corpus <- readRDS(list.files(path = "data_temp", pattern = "corpus", full.names = T)[length(
   list.files(path = "data_temp", pattern = "corpus", full.names = T))])

#drops short words
#Makes a document-term matrix
gsp_dtm <- tm::DocumentTermMatrix(gsp_corpus, control=list(wordLengths=c(3,Inf), 
                                             tolower = FALSE))

#remove documents from metadata to match dtm
metadata <- NLP::meta(gsp_corpus)[unique(gsp_dtm$i), , drop = FALSE]
#120688 elements

ntokens <- sum(gsp_dtm$v)
V <- ncol(gsp_dtm)

#join metadata with dtm in tidyverse
dtm_tidy <- tidy(gsp_dtm) %>% 
   mutate("document" = as.integer(document)) %>% 
              inner_join(metadata, by = c("document" = "i"))
#8372004 observations in dtm_tidy

#use tidyverse to filter out terms found in < 3 gsps
dtm_tidy_med <- dtm_tidy %>% group_by(term) %>% filter(length(unique(gsp_id))>2) %>% ungroup()
#8150314 observations in dtm_tidy_med

#filter out terms found in at least 30 percent of pages
tidy_docs <- length(unique(dtm_tidy$document))
dtm_tidy_small <- dtm_tidy_med %>% group_by(term) 
dtm_tidy_small <- dtm_tidy_small %>% 
   filter( (n() / tidy_docs) < 0.3)
#7662540 observations in dtm_tidy_small
dtm_tidy_small <- dtm_tidy_small %>% ungroup()
gsp_dtm_small <- cast_dtm(dtm_tidy_small,document = document, term = term, value = count)
meta_small <- unique(dtm_tidy_small[,c(1,4:9)])

saveRDS(gsp_dtm_small, file = paste0("data_temp/","gsp_dtm_",format(Sys.time(), "%Y%m%d-%H:%M")))
gsp_dtm_small <- readRDS(list.files(path = "data_temp", pattern = "dtm", full.names = T)[length(
   list.files(path = "data_temp", pattern = "dtm", full.names = T))])

print(sprintf("Removed %i of %i terms (%i of %i tokens) for appearing in < 3 gsps or > 0.3 of pages", 
        V-ncol(gsp_dtm_small), V,
        ntokens-sum(gsp_dtm_small$v), ntokens
        ))
#removed 88580 of 121474 terms

#sometimes this hangs
gsp_out <- readCorpus(gsp_dtm_small, type = "slam") #using the read.slam() function in stm to convert

#This records documents dropped in cleaning process
#TODO check gsp_text_with_meta syntax
is_kept <- (1:length(gsp_text_with_meta[[1]][!is_comment&!is_reference]) %in% unique(gsp_dtm_small$i))
sum(is_kept)
#120264 kept pages

gsp_out <- list(documents=gsp_out$documents, vocab=as.character(gsp_out$vocab),
                meta=meta_small, docs.removed=which(!is_kept))

colnames(gsp_out$meta) <- colnames(meta_small)
saveRDS(gsp_out, file = paste0("data_temp/","gsp_slam_",format(Sys.time(), "%Y%m%d-%H:%M")))
gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
   list.files(path = "data_temp", pattern = "slam", full.names = T))])
#prevalence: how often topic occurs
#content: word frequency within topic
#test svi for both
#test k = 5, 10, 20, 40, 80
#cut bad characters
#drop anything that doesn't have a letter
#regex or name density check against USGS placenames database
#or regex for terrible matches and outputs a stopword matrix
#find groundwater glossary
#figure out how often topic 9 words show up on the document term matrix (grep) row names for where equals > 0
#simple model only includes categorical metadata
simple_gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 K = 20, prevalence =~ admin + basin + sust_criteria +
                    monitoring + projects_mgmt + as.factor(gsp_id) + SVI, max.em.its = 50,
                 data = gsp_out$meta[,2:6], init.type = "Spectral")  
#are we interested in num gsas or num organizations? (Linda knows about num orgs
#dummy for how many gsas are involved: multiple or one
#count of total orgs involved)
saveRDS(simple_gsp_model, file = paste0("data_output/","simple_model_",format(Sys.time(), "%Y%m%d-%H:%M")))

simple_gsp_model_saved <- readRDS(list.files(path = "data_output", pattern = "simple_model", full.names = T)[length(
   list.files(path = "data_output", pattern = "simple_model", full.names = T))])


#inspect words associated with topics using labelTopics
labelTopics(simple_gsp_model, c(1:20))

#TODO research prevalence and content
gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                  K = 20, prevalence =~ admin + basin + sust_criteria +
                    monitoring + projects_mgmt, max.em.its = 75,
                 data = gsp_out$meta[,2:6], init.type = "Spectral")  

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