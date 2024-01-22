
#conf_py <- py_discover_config(use_environment = "spacy_condaenv")

#ret_path <- "C:\\Users\\hgsha\\miniforge3\\envs\\spacy_condaenv\\python.exe"
#ret_path <- "C:\\Users\\hgsha\\miniforge3\\envs\\spacy_condaenv\\Lib\\R\\library\\spacyr\\python"
#install getting stuck at package metadata (repodata.json)
#Online research has a variety of ways to deal with it, so not sure which one
#"C:\Users\hgsha\miniforge3\envs\spacy_condaenv\Lib\R\library\spacyr\python"

#Datalab
#conf_py <- py_discover_config(use_environment = "spacy_condaenv")
#"C:/Users/hgsha/miniforge3/envs/spacy_condaenv/python.exe"
#py_config() --> conf_py <- py_discover_config(use_environment="spacy_condaenv")
#ret_path <- conf_py$python

library(dplyr)
library(devtools)
install_github('ucd-cepb/textnet')
library(textNet)
library(data.table)
library(spacyr)
library(reticulate)
ret_path <- "C:\\Users\\hgsha\\miniforge3\\envs\\spacy_condaenv\\python.exe"
filekey <- read.csv("filekey.csv")

#replace with your own python location

keep_hyph_together <- FALSE
generate_phrases <- T
parse_from_file <- F

ej_orgs <- read.csv("EJ_Paper/CBO_Dict_R.csv")#read in ej_org file
names(ej_orgs)[1] <- "Agency"
ej_orgs$agency_numwords <- stringr::str_count(ej_orgs$Agency, "\\s+")
ej_orgs$abbr_numwords <- stringr::str_count(ej_orgs$Abbr, "\\s+")
agencies <- arrange(ej_orgs, desc(agency_numwords))$Agency
abbrevs <- arrange(ej_orgs, desc(abbr_numwords))$Abbr
#note about phrases: concatenating phrases with an underscore makes it 
#extremely unlikely that Spacy will recognize it as an entity anymore,
#so that's why further down in the script we force it to recognize as entities
#the phrases we include in the concatenation list.
if(generate_phrases==T){
   phrases_to_concatenate <- c(
                               agencies[grepl("\\s",agencies)],#all agency names with spaces in them
                               abbrevs[grepl("\\s", abbrevs)])#all abbr names with spaces in them
}else{
   phrases_to_concatenate <- NA
}

pages <- readRDS(filekey[filekey$var_name=="cleaned_pdfs_for_ejpaper",]$filepath)[29] #list notation for 29th element in list
file_ids <- unlist(sapply(1:length(pages), function(q) rep(names(pages[q]),length(pages[[q]]))))
file_ids <- stringr::str_extract(file_ids,'[0-9]{1,}')
pages <- unlist(pages)

unique_files <- unique(file_ids)
parsed_filenames <- paste0(filekey[filekey$var_name=="Hannah_parsed_test_file_ej",]$filepath,unique_files)

nodeedge_filenames <- paste0(filekey[filekey$var_name=="nondisambiged_text_extracts_ejpaper",]$filepath,unique_files,".RDS")




#### overwrite existing results?
overwrite = T

all_parsed <- textNet::parse_text(ret_path, keep_hyph_together, phrases_to_concatenate, 
                                  concatenator="_",
                                  pages, file_ids, parsed_filenames, 
                                  overwrite)

#eng_words not found after running all_parsed
#make separate test folder in Box to save all of scripts, turn overwrite to True, create Google Doc with
#updated EJ stuff
#fixed

parsedtxt <- spacyr::spacy_parse(pages, 
                                 pos = T, tag = T, lemma = T, entity = T, dependency = T, 
                                 nounphrase = T)

#spacy_download_langmodel('en_core_web_sm')
###error: Can't find model 'en_core_web_sm' It doesn't seem to be a Python package or valid path
#to a data directory

#spacy_download_langmodel('en_core_web_sm')
#

ej_org_concat <- stringr::str_replace_all(ej_orgs$Agency,"\\s","_")
ej_org_abbr_concat <- stringr::str_replace_all(ej_orgs$Abbr,"\\s","_")
#we don't want to overwrite the entity type. For instance, the word "Groundwater" 
#may just be part of an organization called a "Groundwater Sustainability Agency"

all_parsed <- lapply(1:length(all_parsed), function (i){
   all_parsed[[i]][all_parsed[[i]]$token %in% ej_org_concat & all_parsed[[i]]$entity=="",]$entity <- "EJORG_B"
   return(all_parsed[[i]])})
all_parsed <- lapply(1:length(all_parsed), function (i){
   all_parsed[[i]][all_parsed[[i]]$token %in% ej_org_abbr_concat & all_parsed[[i]]$entity=="",]$entity <- "EJORG_B"
   return(all_parsed[[i]])})
#this makes sure that DACs and EJ ORGs are preserved in the network even if Spacy doesn't auto-detect them as entities
#note - the parsed filenames do not have the DAC/EJ custom entity recognition on them -- 
#only the all_parsed file in memory now does. If you reload the parsed filename files, you'll have to assign the DAC and EJORG entity attributes again

for(m in 1:length(all_parsed)){
   textNet::textnet_extract(all_parsed[[m]],concatenator="_",file = nodeedge_filenames[m],cl=4,
                            keep_entities = c('ORG','GPE','PERSON','EJORG'), 
                            return_to_memory=F, keep_incomplete_edges=T)
}

