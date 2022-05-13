library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)

all_gsp_text <- NULL
all_text_subcat <- vector(mode = "list", length = 0)
all_text_cat <- vector(mode = "list", length = 0)
is_comment <- NULL
is_reference <- NULL
gsp_id <- NULL

gsp_list <- list.files(path = "data_output", pattern = "_text", full.names = T)
for(k in 1:length(gsp_list)){
   
   gsp_k <- readRDS(gsp_list[k])
   key_k <- readRDS(paste0("data_output/gsp_num_id_",substr(gsp_list[k],24,27),"_categories"))
   gsp_id <- append(gsp_id, rep.int(c(substr(gsp_list[k],24,27)),times = length(gsp_k)))
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
#TODO add qualitative metadata:
#  attributes of people who produced document, like GSA
#     findable on portal -> plan page -> "list of GSA(S) that collectively prepared the gsp"
#     or gsa_gsp_basin_coord

#  TODO attributes of community the document is for
#  TODO importance of agriculture in each GSA region
#  census tract

#  TODO social vulnerability index 2018
albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

#     https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
#downloaded by hand and placed in data_spatial_raw
censusplot <- st_read("data_spatial_raw/SVI_shapefiles_CA_censustracts/SVI2018_CALIFORNIA_tract.shp")
censusplot <- st_transform(censusplot,albersNA)
#st_crs pulls up projection
#alt: st_crs(gsp_submitted)
censusplot <- st_make_valid(censusplot)
#uses NAD83

#GSP Posted and GSP Submitted layers downloadable at
#gsp_map_url = "https://sgma.water.ca.gov/webgis/?jsonfile=https%3a%2f%2fsgma.water.ca.gov%2fportal%2fresources%2fjs%2fmapconfigs%2fGspSubmittalsConfig.js&_dc=0.23941161367525954"
#downloaded by hand and placed in data_spatial_raw
#GSP_Submitted should be a superset of GSP_Posted, so GSP_Submitted is used here

#Basin info downloadable by selecting
#Reference Layers > Groundwater Management > "Bulletin 118 Groundwater Basins - 2018" at
#  https://sgma.water.ca.gov/webgis/?appid=SGMADataViewer#boundaries

fname = unzip("data_spatial_raw/GSP_submitted.zip", list=TRUE)
unzip("data_spatial_raw/GSP_submitted.zip", files=fname$Name, exdir="data_spatial_raw/GSP_submitted", overwrite=TRUE)
fpath = file.path("data_spatial_raw/GSP_submitted", grep('shp$',fname$Name,value=T))
gsp_shapes <- st_read(fpath)
gsp_shapes <- st_transform(gsp_shapes,albersNA)
gsp_shapes <- st_make_valid(gsp_shapes)

#census_gspshape_overs = st_intersects(censusplot, gsp_shapes)
gspshape_census_overs = st_intersects(gsp_shapes, censusplot)
gspshape_census_props = pblapply(seq_along(gspshape_census_overs),function(i){
   #proportion of gsp in each census tract = area of census_gsp intersection / gsp area
   area_overlap = st_area(st_intersection(gsp_shapes[i,],censusplot[gspshape_census_overs[[i]],]))
   prop_gsp_in_tract = area_overlap/st_area(gsp_shapes[i,])
   prop_tract_in_gsp = area_overlap/st_area(censusplot[gspshape_census_overs[[i]],])
   data.table(census_tract_id = censusplot$FIPS[gspshape_census_overs[[i]]],
              population = censusplot$E_TOTPOP[gspshape_census_overs[[i]]],
              SVI_percentile = ifelse(censusplot$RPL_THEMES[gspshape_census_overs[[i]]]>=0,
                                      censusplot$RPL_THEMES[gspshape_census_overs[[i]]],NA),
              gsp_ids = gsp_shapes$GSP.ID[i],
              Prop_GSP_in_tract = as.numeric(prop_gsp_in_tract),
              Prop_tract_in_GSP = as.numeric(prop_tract_in_gsp)
              #return entries where over half the tract is in the GSP
              #as well as entries where less than half the tract is in the GSP
              #but over half a percent of the GSP is made up of that tract
              )[Prop_tract_in_GSP >= 0.5 | (Prop_tract_in_GSP < 0.5 & Prop_GSP_in_tract >= 0.005)]
}, cl = 4)
#each datatable is a different gsp

#within each datatable
#column of census tract ids
#column of SVIs
#percent that is each census tract

gspshape_census_dt = rbindlist(gspshape_census_props)
fwrite(gspshape_census_dt,file = 'data_temp/gsp_census_overlap.csv')
gspshape_census_dt <- as_tibble(fread(file = 'data_temp/gsp_census_overlap.csv'))

#gives somewhat lower SVIs (thicker left tail)
gsp_svi_adj_area <- summarize(group_by(gspshape_census_dt, gsp_ids),SVI_percentile, Prop_GSP_in_tract) %>% 
   #inflates SVI to account for small dropped census tracts by dividing by sum of proportion overlaps
   mutate(svi_inflated = SVI_percentile / sum(Prop_GSP_in_tract)) %>% 
   mutate(prop_na = sum(ifelse(is.na(SVI_percentile),Prop_GSP_in_tract,0))) %>% 
   #weighted sum of SVI portions by census tract 
   #determines what portion of each GSP has an NA value for SVI
   #adjusts SVI of GSP to account for NAs
   mutate(SVI_na_adj = sum((Prop_GSP_in_tract / (1-prop_na)) * svi_inflated, na.rm = T)) %>%
   ungroup() %>% 
   select(c("gsp_ids", "SVI_na_adj")) %>% 
   unique()

#gives somewhat higher SVIs (thinner left tail)
gsp_svi_adj_pop <- summarize(group_by(gspshape_census_dt, gsp_ids),SVI_percentile, Prop_GSP_in_tract, Prop_tract_in_GSP, population) %>% 
   #deflates pop to account for what percent of the tract is in the GSP
   mutate(pop_adj = population * Prop_tract_in_GSP) %>% 
   #calculates percent of GSP population that is in that tract
   mutate(pop_fraction = pop_adj / sum(pop_adj)) %>% 
   #tracts with pop of 0 have SVI of NA
   #weighted sum of SVI portions by population of census tracts
   mutate(SVI_na_adj = sum(pop_fraction * SVI_percentile, na.rm = T)) %>%
   ungroup() %>% 
   select(c("gsp_ids", "SVI_na_adj")) %>% 
   unique()

#id formatting
gsp_svi_adjusted <- gsp_svi_adjusted %>% 
   mutate(code = (as.character(gsp_ids)))%>% 
   mutate(num_zeros = 4 - str_length(code)) %>% 
   mutate(gsp_num_id = paste(ifelse(num_zeros > 0, "0", ""),ifelse(num_zeros > 1,"0",""),ifelse(num_zeros > 2, "0",""),code,sep = "")) %>% 
   select(!c(code,num_zeros,gsp_ids))


#gsp_meta <- data.table(matrix(ncol = 4, nrow = 0))
#colnames(gsp_meta) <- c("GSA","community_attributes","ag_importance","soc_vuln")
#


#remove non-visible characters
all_gsp_text <- stringr::str_replace_all(all_gsp_text,"[^[:graph:]]", " ")

#add cat metadata
gsp_text_with_meta <- data.table(text = all_gsp_text, admin = is_admin, basin = is_basin,
                                 sust_criteria = is_criteria, monitoring_networks = is_monitoring,
                                 projects_mgmt_actions = is_projects, gsp_id = gsp_id)
gsp_text_with_meta <- full_join(gsp_text_with_meta, gsp_svi_adjusted, by = c("gsp_id"="gsp_num_id"))

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

#builds corpus
#corpus pulls documents from column 1 of gsp_text_with_meta
#removes comments and references
#metadata is all other columns
#metadata: num rows = num documents. num columns = num metadata type
gsp_corpus <- VCorpus(VectorSource(gsp_text_with_meta[[1]][!is_comment&!is_reference]))
meta(gsp_corpus, tag = "admin", type = "indexed") <- gsp_text_with_meta[[2]][!is_comment&!is_reference]
meta(gsp_corpus, tag = "basin", type = "indexed") <- gsp_text_with_meta[[3]][!is_comment&!is_reference]
meta(gsp_corpus, tag = "sust_criteria", type = "indexed") <- gsp_text_with_meta[[4]][!is_comment&!is_reference]
meta(gsp_corpus, tag = "monitoring", type = "indexed") <- gsp_text_with_meta[[5]][!is_comment&!is_reference]
meta(gsp_corpus, tag = "projects_mgmt", type = "indexed") <- gsp_text_with_meta[[6]][!is_comment&!is_reference]
meta(gsp_corpus, tag = "gsp_id", type = "indexed") <- gsp_text_with_meta[[7]][!is_comment&!is_reference]
meta(gsp_corpus, tag = "i", type = "indexed") <- c(1:length(gsp_corpus))
#col names 
#NLP::meta(txt, colnames(metadata)[i]) <- metadata[,i]


#remove white spaces
gsp_corpus <- tm_map(gsp_corpus, stripWhitespace)

#convert to lower case
#if else needed because of API differences, adapted from textProcessor
if(utils::packageVersion("tm") >= "0.6") {
   gsp_corpus <- tm_map(gsp_corpus, content_transformer(tolower)) 
} else {
   gsp_corpus <- tm_map(gsp_corpus, tolower)
}

#remove punctuation
#ucp = T would remove larger set of punctuation
gsp_corpus <- tm_map(gsp_corpus, removePunctuation, preserve_intra_word_dashes = TRUE,ucp=F)

#TODO custom punctuation removal would go here based on this textProcessor template
#if(length(custompunctuation)==1 && 
#   substr(custompunctuation,0,1)=="[") {
#   #if there is only one entry and it starts with open bracket
#   #we are going to assume its a regular expression and let it
#   #through
#   punct_pattern <- custompunctuation
#} else {
#   punct_pattern <-sprintf("[%s]",paste0(custompunctuation,collapse=""))
#}
#gsp_corpus<- tm_map(gsp_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x)), 
#                    punct_pattern)

#Remove stopwords in English
#this takes a while
gsp_corpus <- tm_map(gsp_corpus, removeWords, stopwords("en")) 

#TODO custom stopwords would be removed here
#gsp_corpus <- tm_map(gsp_corpus, removeWords, customstopwords)

#remove numbers
gsp_corpus <- tm_map(gsp_corpus, removeNumbers)

#stem words
gsp_corpus <- tm_map(gsp_corpus, stemDocument, language="en")

saveRDS(gsp_corpus, file = paste0("data_temp/","gsp_corpus_",format(Sys.time(), "%Y%m%d-%H:%M")))
gsp_corpus <- readRDS(list.files(path = "data_temp", pattern = "corpus", full.names = T)[length(
   list.files(path = "data_temp", pattern = "corpus", full.names = T))])

#drops short words
#Makes a document-term matrix
gsp_dtm <- tm::DocumentTermMatrix(gsp_corpus, control=list(wordLengths=c(3,Inf), tolower = FALSE))

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
                    monitoring + projects_mgmt + gsp_id, max.em.its = 50,
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