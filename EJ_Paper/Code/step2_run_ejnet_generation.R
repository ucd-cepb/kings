install.packages("devtools")
library(reticulate)
install.packages("reticulate")
library(devtools)
install_github("cbail/textnets")
library(textNet)
library("spacyr")
library(textNet)
library(dplyr)
filekey <- read.csv("filekey.csv")
ret_path <- "/Users/hgsha/miniconda3/envs/spacy_condaenv/python"

#replace with your own python location

keep_hyph_together <- FALSE
generate_phrases <- T
parse_from_file <- F

dacs <- read.csv()#read in dac file
dacs$numwords <- stringr::str_count(dacs$name, "\\s+")#arrange by descending number of words
dacs <- arrange(dacs, desc(numwords))
ej_orgs <- read.csv("EJ_Paper/CBO_Dict_R.csv")#read in ej_org file
ej_orgs$agency_numwords <- stringr::str_count(ej_orgs$Agency, "\\s+")
ej_orgs$abbr_numwords <- stringr::str_count(ej_orgs$Abbr, "\\s+")
agencies <- arrange(ej_orgs, desc(agency_numwords))$Agency
abbrevs <- arrange(ej_orgs, desc(abbr_numwords))$Abbr
#note about phrases: concatenating phrases with an underscore makes it 
#extremely unlikely that Spacy will recognize it as an entity anymore,
#so that's why further down in the script we force it to recognize as entities
#the phrases we include in the concatenation list.
if(generate_phrases==T){
   phrases_to_concatenate <- c(dacs$name[grepl("\\s",dacs$name)],#all dac names with spaces in them
                               agencies[grepl("\\s",agencies)],#all agency names with spaces in them
                               abbrevs[grepl("\\s", abbrevs)])#all abbr names with spaces in them
}else{
   phrases_to_concatenate <- NA
}

pages <- readRDS(filekey[filekey$var_name=="cleaned_pdfs_for_ejpaper",]$filepath)
file_ids <- unlist(sapply(1:length(pages), function(q) rep(names(pages[q]),length(pages[[q]]))))
file_ids <- stringr::str_extract(file_ids,'[0-9]{1,}')
pages <- unlist(pages)

parsed_filenames <- paste0(filekey[filekey$var_name=="parsed_files_ejpaper",]$filepath,unique_files)
   
nodeedge_filenames <- paste0(filekey[filekey$var_name=="nondisambiged_extracts_ejpaper",]$filepath,unique_files,".RDS")




#### overwrite existing results?
overwrite = F

all_parsed <- textNet::parse_text(ret_path, keep_hyph_together, phrases_to_concatenate, 
                 concatenator="_",
                 pages, file_ids, parsed_filenames, 
                 parse_from_file,
                 overwrite)

dacs_concat <- stringr::str_replace_all(dacs$name,"\\s","_")
ej_org_concat <- stringr::str_replace_all(ej_org$Agency,"\\s","_")
ej_org_abbr_concat <- stringr::str_replace_all(ej_org$Abbr,"\\s","_")
#we don't want to overwrite the entity type. For instance, the word "Groundwater" 
#may just be part of an organization called a "Groundwater Sustainability Agency"
all_parsed <- lapply(1:length(all_parsed), function (i){
   #the _B extension tells textnet it's the start of an entity name
   all_parsed[[i]][all_parsed[[i]]$token %in% dacs_concat & all_parsed[[i]]$entity=="",]$entity <- "DAC_B"
   return(all_parsed[[i]])})
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
                   keep_entities = c('ORG','GPE','PERSON','LOC','DAC','EJORG'), 
                   return_to_memory=F, keep_incomplete_edges=T)
}

