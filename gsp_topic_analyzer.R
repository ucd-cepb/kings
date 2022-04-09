library(stm)
library(tm)
library(SnowballC)
library(tm)


testprocess <- textProcessor(documents = (c("hello fox world. The cat jumped over, the fox.",
             "green eggs and ham fox in box socks in sock","hi everyone","hello world bits")))



all_gsp_text <- NULL
all_text_subcat <- vector(mode = "list", length = 0)
all_text_cat <- vector(mode = "list", length = 0)
is_comment <- NULL
is_reference <- NULL

gsp_list <- list.files(path = "data_output", pattern = "_text", full.names = T)
for(k in 1:length(gsp_list)){
   
   gsp_k <- readRDS(gsp_list[k])
   key_k <- readRDS(paste0("data_output/gsp_num_id_",substr(gsp_list[k],24,27),"_categories"))
   #i = page number
   for (i in 1:length(gsp_k)){
      page_cat <- NULL
      page_subcat <- NULL
      #j = subcategory
      for(j in 1:5){
         if(i %in% key_k$page_vector[[j]]){
            page_cat <- append(page_cat, key_k$category[[j]])
         }
      }
      for (j in 6:21){
         if(i %in% key_k$page_vector[[j]]){
            page_subcat <- append(page_subcat, key_k$subcategory[[j]])
         }
      }
      
      if(i %in% key_k$page_vector[[23]]){
         is_reference <- append(is_reference, TRUE)
      }else{
         is_reference <- append(is_reference, FALSE)
      }
      
      if(i %in% key_k$page_vector[[46]]){
         is_comment <- append(is_comment, TRUE)
      }else{
         is_comment <- append(is_comment, FALSE)
      }
      
      all_text_cat <- append(all_text_cat, list(page_cat))
      all_text_subcat <- append(all_text_subcat, list(page_subcat))
   }
   all_gsp_text <- append(all_gsp_text, gsp_k)
}

#check percentage of pages tagged with more than one subcategory, as decimal
sum(lengths(all_text_subcat)>1)/length(all_gsp_text)
#check percentage of pages tagged with more than one category, as decimal
sum(lengths(all_text_cat)>1)/length(all_gsp_text)

#dummying out category
is_admin <- NULL
is_basin <- NULL
is_criteria <- NULL
is_monitoring <- NULL
is_projects <- NULL
for(i in 1:length(all_text_cat)){
   is_admin <- append(is_admin,
                      ifelse("Administrative Information" %in% all_text_cat[[i]],TRUE,FALSE))
   is_basin <- append(is_basin,
                       ifelse("Basin Setting" %in% all_text_cat[[i]],TRUE,FALSE))
   is_criteria <- append(is_criteria,
                      ifelse("Sustainable Management Criteria" %in% all_text_cat[[i]],TRUE,FALSE))
   is_monitoring <- append(is_monitoring,
                      ifelse("Monitoring Networks" %in% all_text_cat[[i]],TRUE,FALSE))
   is_projects <- append(is_projects,
                      ifelse("Projects and Management Actions" %in% all_text_cat[[i]],TRUE,FALSE))
}

#rows = num docs; cols = metadata types
#TODO add qualitative metadata
#gsp_meta <- data.table(matrix(ncol = 4, nrow = 0))
#colnames(gsp_meta) <- c("GSA","community_attributes","ag_importance","soc_vuln")
#bind to metadata table

#add cat metadata
gsp_text_with_meta <- data.table(text = all_gsp_text, admin = is_admin, basin = is_basin,
                                 sust_criteria = is_criteria, monitoring_networks = is_monitoring,
                                 projects_mgmt_actions = is_projects)

#use to filter out nulls in category
cat_selector <- !sapply(all_text_cat,is.null)
#use cat_selector to subset text and all metadata vectors

saveRDS(gsp_text_with_meta, file = paste0("data_output/","gsp_docs_w_meta_",format(Sys.time(), "%Y%m%d-%H:%M")))
saveRDS(all_text_subcat, file = paste0("data_temp/","gsp_docs_subcat_",format(Sys.time(), "%Y%m%d-%H:%M")))
saveRDS(all_text_cat, file = paste0("data_temp/","gsp_docs_cat_",format(Sys.time(), "%Y%m%d-%H:%M")))
saveRDS(cat_selector, file = paste0("data_temp/","gsp_docs_cat_notnull_",format(Sys.time(), "%Y%m%d-%H:%M")))
saveRDS(is_comment, file = paste0("data_temp/","gsp_docs_comment_",format(Sys.time(), "%Y%m%d-%H:%M")))
saveRDS(is_reference, file = paste0("data_temp/","gsp_docs_reference_",format(Sys.time(), "%Y%m%d-%H:%M")))

#retrieves the latest save of gsp_text_with_meta
gsp_text_with_meta <- readRDS(
   list.files(path = "data_output", pattern = "docs", full.names = T)[length(
      list.files(path = "data_output", pattern = "docs", full.names = T))])

#retrieves the latest save of is_comment and is_reference
is_comment <- readRDS(
   list.files(path = "data_temp", pattern = "comment", full.names = T)[length(
      list.files(path = "data_temp", pattern = "comment", full.names = T))])
is_reference <- readRDS(
   list.files(path = "data_temp", pattern = "reference", full.names = T)[length(
      list.files(path = "data_temp", pattern = "reference", full.names = T))])


#corpus pulls documents from column 1 of gsp_text_with_meta
#removes comments and references
#metadata is all other columns
#metadata: num rows = num documents. num columns = num metadata type
gsp_corpus <- VCorpus(VectorSource(gsp_text_with_meta[!is_comment&!is_reference,1]))
meta(gsp_corpus, tag = "admin", type = "indexed") <- gsp_text_with_meta[!is_comment&!is_reference,2]
meta(gsp_corpus, tag = "basin", type = "indexed") <- gsp_text_with_meta[!is_comment&!is_reference,3]
meta(gsp_corpus, tag = "sust_criteria", type = "indexed") <- gsp_text_with_meta[!is_comment&!is_reference,4]
meta(gsp_corpus, tag = "monitoring", type = "indexed") <- gsp_text_with_meta[!is_comment&!is_reference,5]
meta(gsp_corpus, tag = "projects_mgmt", type = "indexed") <- gsp_text_with_meta[!is_comment&!is_reference,6]
#too slow
gsp_processed <- textProcessor(gsp_text_with_meta[!is_comment&!is_reference,1],
                               metadata = gsp_text_with_meta[!is_comment&!is_reference,!1],verbose = T)
saveRDS(gsp_processed, file = paste0("data_temp/","gsp_processed_",format(Sys.time(), "%Y%m%d-%H:%M")))
gsp_processed$documents
gsp_processed$vocab
gsp_processed$meta
#alternative options: dfm from quanteda
#tm --> readCorpus
#corpus
#txtorg

#TODO add gsp_id metadata 
#TODO set minimum number of gsp_ids words need to appear in
#TODO optional: set max number of gsp_ids words appear in

#TODO stem, drop punctuation, remove stop words

#TODO replace with tm functions
#instead of lower.thres look how many gsps each word appears in and cut off 
gsp_out <- prepDocuments(gsp_processed$documents, gsp_processed$vocab,
                         gsp_processed$meta, lower.thresh = 20)
#test out different lower thresholds for removing uncommon words
plotRemoved(gsp_processed$documents, lower.thresh = seq(1, 200, by = 100))
#prepDocuments cleans, converts to proper format, removes uncommon words

#example:
data <- read.csv("poliblogs2008.csv")
processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab,
                        + processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#example:
#how to let searchK figure out how many topics to generate
storage <- searchK(gsp_out$documents, gsp_out$vocab, K = c(7, 10),
                   + prevalence =~ rating + s(day), data = meta)

gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 + K = 20, prevalence =~ rating + s(day), max.em.its = 75,
                 + data = gsp_out$meta, init.type = "Spectral")  


#example:
#   see page 9 of stm documentation
#max iterations = max.em.its
#highly recommend init.type = "Spectral" because it eliminates initiation-
#based sensitivity


#topics are evaluated on two components:
#semantic coherence (frequency of co-occurrence of common words in a toipc)
#exclusivity of words to topics

#example:
#how to plot quality of models
plotModels(poliblogSelect, pch = c(1, 2, 3, 4),
           + legend.position = "bottomright")
#can also use topicQuality

#choose your favorite model
selectedmodel <- poliblogSelect$runout[[3]]

#how to investigate model results. options:
#inspect words associated with topics using labelTopics
#example: labelTopics(poliblogPrevFit, c(6, 13, 18))
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