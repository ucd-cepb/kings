library(textNet)
filekey <- read.csv("filekey.csv")

ret_path <- "/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python"
#replace with your own python location

keep_hyph_together <- FALSE
generate_phrases <- T
parse_from_file <- F

dacs <- read.csv()#read in dac file
ej_orgs <- read.csv()#read in ej_org file

source(filekey[filekey$var_name=="generate_proper_names_fn_ejpaper",]$filepath)

phrases_to_concatenate <- ifelse(generate_phrases==T, 
                                 unique(c(generate_proper_names(underscore=F,to_lower=F),
                                 dacs$name[grepl("\\s",dacs$name)],#all dac names with spaces in them
                                 ej_orgs$Agency[grepl("\\s",ej_orgs$Agency)],#all agency names with spaces in them
                                 ej_orgs$Abbr[grepl("\\s", ej_orgs$Abbr)])),#any abbrevs with spaces in them
                                 NA)

pages <- readRDS(filekey[filekey$var_name=="cleaned_pdfs_for_ejpaper",]$filepath)
file_ids <- unlist(sapply(1:length(pages), function(q) rep(names(pages[q]),length(pages[[q]]))))
file_ids <- str_extract(file_ids,'[0-9]{1,}')
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
   all_parsed[[i]][all_parsed[[i]]$token %in% dacs_concat & all_parsed[[i]]$entity=="",]$entity <- "DAC_B"
   return(all_parsed[[i]])})
all_parsed <- lapply(1:length(all_parsed), function (i){
   all_parsed[[i]][all_parsed[[i]]$token %in% ej_org_concat & all_parsed[[i]]$entity=="",]$entity <- "EJORG_B"
   return(all_parsed[[i]])})
all_parsed <- lapply(1:length(all_parsed), function (i){
   all_parsed[[i]][all_parsed[[i]]$token %in% ej_org_abbr_concat & all_parsed[[i]]$entity=="",]$entity <- "EJORG_B"
   return(all_parsed[[i]])})
#this makes sure that DACs and EJ ORGs are preserved in the network even if Spacy doesn't auto-detect them as entities
for(m in 1:length(all_parsed)){
   textNet::textnet_extract(all_parsed[[m]],concatenator="_",file = nodeedge_filenames[m],cl=4,
                   keep_entities = c('ORG','GPE','PERSON','LOC','DAC','EJORG'), 
                   return_to_memory=F, keep_incomplete_edges=T)
}

