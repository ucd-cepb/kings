library(data.table)

### set up a symbolic link
### on. mac this is > ln -s [Box Location] [Github Location]
### on windows, who knows
### can always hardwire if you have to
file_loc <- 'C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\'
extract_list = list.files(file_loc)

grab_orgs <- function(file,drop = 'PERSON'){
   # read in rds file
   temp = readRDS(file)
   # isolate nodelist
   nodes = temp$nodelist
   # filter out node types we don't want
   nodes = nodes[!nodes$entity_type%in% drop,]
   return(nodes)
}

empty_node_dt <- data.table()

### this is test reads in only first five!!!
### remove the index to read in all
for(file in extract_list[1:5]){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
   empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
}

#no tester
for(file in extract_list){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
   empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
}


empty_node_dt$org_type <- NA
empty_node_dt$CBO <- NA

#CBO Dictionary formatting
cbo_dictionary <- read.csv("EJ_Paper/CBODict_R_Update.csv")

#packages
library(dplyr)
library(devtools)
library(textNet)
library(spacyr)
library(reticulate)
ret_path <- "C:\\Users\\hgsha\\miniforge3\\envs\\spacy_condaenv\\python.exe"
filekey <- read.csv("filekey.csv")

#Same formatting as org list from cleaned extracts
#Pulled from Elise's code we were using when we were trying to run spacy
#Based on my understanding of the code, these are the lines that format it 
#like the names in the extracted networks
keep_hyph_together <- FALSE
generate_phrases <- T
parse_from_file <- F

names(cbo_dictionary)[1] <- "Agency"
cbo_dictionary$agency_numwords <- stringr::str_count(cbo_dictionary$Agency, "\\s+")
cbo_dictionary$abbr_numwords <- stringr::str_count(cbo_dictionary$Acronyms, "\\s+")
agencies <- arrange(cbo_dictionary, desc(agency_numwords))$Agency
abbrevs <- arrange(cbo_dictionary, desc(abbr_numwords))$Acronyms

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



### hard match against CBO dictionary
empty_node_dt$CBO[empty_node_dt$entity_name %in% cbo_dictionary$Agency] <- TRUE
empty_node_dt$org_type[empty_node_dt$entity_name %in% cbo_dictionary$Agency] <- 'CBO'

#### soft match against particular string type
empty_node_dt$org_type[grepl("resource_conservation_district",empty_node_dt$entity_name)] <- 'RCD'
empty_node_dt$org_type[grepl("groundwater_sustainability_agency",empty_node_dt$entity_name)] <- 'GSA'


### hard match against specific string type
empty_node_dt$org_type[empty_node_dt$entity_name == 'yolo_county_conservation_district'] <- 'RCD'
empty_node_dt$org_type[empty_node_dt$entity_name == 'groundwater_sustainability_plan'] <- 'Misc'
empty_node_dt$org_type[empty_node_dt$entity_name == 'sustainable_groundwater_management_act'] <- 'Misc'
empty_node_dt$org_type[empty_node_dt$entity_name == 'groundwater_sustainability_agency'] <- 'GSA'
empty_node_dt$org_type[empty_node_dt$entity_name == 'department_of_water_resources'] <- 'NL Gov'



head(nodes)

table(n$nodelist$entity_type)
head(n$nodelist)
