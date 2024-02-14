library(data.table)
library(stringr)

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

#all files
for(file in extract_list){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
   empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
}


empty_node_dt$org_type <- NA
empty_node_dt$CBO <- NA

#CBO Dictionary formatting
cbo_dictionary <- read.csv("EJ_Paper/CBO_Name_lc.csv")
sd_dictionary <- read.csv("EJ_Paper/Data/SD_Names.csv")
gov_dictionary <- read.csv("EJ_Paper/Data/govsci_tbl_noblank.csv")

#Formatting databases - str_replace_all(cbo_dictionary$Name, "//s", "_")



### hard match against dictionary
empty_node_dt$CBO[empty_node_dt$entity_name %in% cbo_dictionary$Name] <- TRUE
empty_node_dt$org_type[empty_node_dt$entity_name %in% cbo_dictionary$Name] <- 'CBO'
empty_node_dt$org_type[empty_node_dt$entity_name %in% sd_dictionary$Entity_Name] <- 'SD'
empty_node_dt$org_type[empty_node_dt$entity_name %in% gov_dictionary$Agency] <- 'NL_Gov'

#Remove local and blanks from NL Gov

#### soft match against particular string type - ERROR POPS UP HERE
empty_node_dt$org_type[grepl("resource_conservation_district",empty_node_dt$entity_name)] <- 'RCD'
empty_node_dt$org_type[grepl("groundwater_sustainability_agency",empty_node_dt$entity_name)] <- 'GSA'
empty_node_dt$org_type[grepl("groundwater_management_agency",empty_node_dt$entity_name)] <- 'GSA'


#empty_node_dt$org_type[grepl("conservation_district",empty_node_dt$entity_name)] <- 'RCD?'

#Need to go through identified CBO and alter coding
empty_node_dt$org_type[empty_node_dt$entity_name == 'nature_conservancy'] <- 'NGO'
empty_node_dt$CBO[empty_node_dt$entity_name == 'nature_conservancy'] <- FALSE


###how to convert dictionary in the same format

### hard match against specific string type
empty_node_dt$org_type[empty_node_dt$entity_name == 'yolo_county_conservation_district'] <- 'RCD'
empty_node_dt$org_type[empty_node_dt$entity_name == 'groundwater_sustainability_plan'] <- 'NR'
empty_node_dt$org_type[empty_node_dt$entity_name == 'sustainable_groundwater_management_act'] <- 'NR'
empty_node_dt$org_type[empty_node_dt$entity_name == 'groundwater_sustainability_agency'] <- 'GSA'
empty_node_dt$org_type[empty_node_dt$entity_name == 'department_of_water_resources'] <- 'NL Gov'
empty_node_dt$org_type[empty_node_dt$entity_name == 'groundwater_dependent_ecosystem'] <- 'NR'
empty_node_dt$org_type[empty_node_dt$entity_name == 'united_states_geological_survey'] <- 'NL Gov'
empty_node_dt$org_type[empty_node_dt$entity_name == 'board'] <- 'NR'


#View Data
head(nodes)
table(n$nodelist$entity_type)
head(n$nodelist)

#Export data:
write.csv(empty_node_dt, "EJ_Paper/Data/org_data_need_code.csv")
