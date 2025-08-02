#Warning -- this script was quickly written and 
#has some hard coding in it! If you are editing it be careful
#or wait for Elise to come back to the office. 

#this script runs through a medium article on groundwater innovation 
#to determine topics, then it feeds key words from those topics into a
#seeded LDA to get "innovation" topics

#this is compared with a regular management LDA and the similar words are
#removed from the seed
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
library(stm)
library(Rtsne)
library(rsvd)
library(geometry)
data1 <- read.table("Case Studies in Geospatial Applications Bhunia et al Ch 19.txt",sep = "\t", quote = "")
data2 <- read.table("Medium_Innovation_Article.txt",sep = "\t",quote = "")
data3 <- read.table("Groundwater Management 2015.txt",sep = "\t", quote = "")
data4 <- read.table("Groundwater resources sustainability 2021.txt",sep = "\t", quote = "")

data12 <- rbind(data1, data2)

text1 <- stm::textProcessor(data1$V1)
textout <- stm::prepDocuments(text1$documents, text1$vocab, text1$meta)
docs <- textout$documents
vocab <- textout$vocab
meta <- textout$meta

myk1 <- searchK(docs, vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))
plot(myk$results$K, myk$results$heldout)
model1 <- stm(documents = docs, vocab = vocab,
    K = 10, init.type = "Spectral")
mylabels1 <- labelTopics(model1, n = 20)
mylabels1$lift

text12 <- stm::textProcessor(data12$V1)
textout <- stm::prepDocuments(text12$documents, text12$vocab, text12$meta)
docs <- textout$documents
vocab <- textout$vocab
meta <- textout$meta

myk12 <- searchK(docs, vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40))
plot(myk12$results$K, myk12$results$residual)
model12 <- stm(documents = docs, vocab = vocab,
              K = 10, init.type = "Spectral")
mylabels12 <- labelTopics(model12, n = 50)
mylabels12$lift


text2 <- stm::textProcessor(data2$V1)
textout <- stm::prepDocuments(text2$documents, text2$vocab, text2$meta)
docs <- textout$documents
vocab <- textout$vocab
meta <- textout$meta

myk <- searchK(docs, vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))
plot(myk$results$K, myk$results$heldout)
model2 <- stm(documents = docs, vocab = vocab,
              K = 15, init.type = "Spectral")
mylabels2 <- labelTopics(model2, n = 20)
mylabels2$lift


#normal groundwater management chapter in a book
text3 <- stm::textProcessor(data3$V1)
textout <- stm::prepDocuments(text3$documents, text3$vocab, text3$meta)
docs <- textout$documents
vocab <- textout$vocab
meta <- textout$meta

myk3 <- searchK(docs, vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))
plot(myk3$results$K, myk3$results$heldout)
model3 <- stm(documents = docs, vocab = vocab,
              K = 20, init.type = "Spectral")
mylabels3 <- labelTopics(model3, n = 100)
mylabels3$lift


#normal groundwater sustainability chapter in a book
text4 <- stm::textProcessor(data4$V1)
textout <- stm::prepDocuments(text4$documents, text4$vocab, text4$meta)
docs <- textout$documents
vocab <- textout$vocab
meta <- textout$meta

myk4 <- searchK(docs, vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40))
plot(myk4$results$K, myk4$results$residual)
model4 <- stm(documents = docs, vocab = vocab,
              K = 20, init.type = "Spectral")
mylabels4 <- labelTopics(model4, n = 100)
mylabels4$lift

#innovative
any(stringr::str_detect(mylabels1$lift, "innov"))
any(stringr::str_detect(mylabels2$lift, "innov"))
#non-innovative
any(stringr::str_detect(mylabels3$lift, "innov"))
any(stringr::str_detect(mylabels4$lift, "innov"))


#what innovative words are not lifted in the general groundwater management chapters?
#innov1 <- mylabels1$lift[!mylabels1$lift%in%c(mylabels3$lift, mylabels4$lift)]

#what innovative words are not lifted in the general groundwater management chapter?
#innov2 <- mylabels2$lift[!mylabels2$lift%in%c(mylabels3$lift, mylabels4$lift)]

#what innovative words are not lifted in the general groundwater management chapter?
#those become the seeds for the topic model
labelsdf <- as.data.frame(t(mylabels12$lift))
mytopics <- vector(mode = "list")
for(x in 1:ncol(labelsdf)){
   print(paste0("topic ", x, "is length ", length(labelsdf[,x][!labelsdf[,x] %in% c(mylabels3$lift, mylabels4$lift)])))
   print(labelsdf[,x][!labelsdf[,x] %in% c(mylabels3$lift, mylabels4$lift)])
   if(length(labelsdf[,x][!labelsdf[,x] %in% c(mylabels3$lift, mylabels4$lift)])>=20){
      mytopics <- append(mytopics, list(labelsdf[,x][!labelsdf[,x] %in% c(mylabels3$lift, mylabels4$lift)]))
   }
}
#mytopics saves the topics where a substantial proportion (20/50) is unique to the innovation
#articles and not the groundwater management chapters

mytopics

#read in qdf from GSPs
qdfm <- readRDS("data_temp/gsp_tok_20230902-11:48")
dict <- quanteda::dictionary(list(
   innov_top1 = mytopics[[1]],
   innov_top2 = mytopics[[2]],
   innov_top3 = mytopics[[3]],
   innov_top4 = mytopics[[4]],
   innov_top5 = mytopics[[5]]
))


install.packages("seededlda")
library(seededlda)
set.seed(3000)
seedmodel <- textmodel_seededlda(qdfm, dict,
                    valuetype = "fixed",
                    verbose = T)

saveRDS(seedmodel, "data/Innovation_Paper/")
