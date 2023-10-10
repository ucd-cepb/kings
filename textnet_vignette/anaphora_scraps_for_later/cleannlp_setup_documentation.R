
library(reticulate)
library(cleanNLP)
cnlp_init_corenlp()

#cnlp_download_corenlp("en")
#default download directory: /Users/elisemiller/stanfordnlp_resources
#download location for en_ewt: /Users/elisemiller/stanfordnlp_resources/en_ewt_models.zip

cnlp_init_stringi()

cnlp_download_spacy("en")
#you can now load the package via spacy.load("en_core_web_sm")
cnlp_init_spacy()#can increase max_length from 1000000 to something larger if necessary



