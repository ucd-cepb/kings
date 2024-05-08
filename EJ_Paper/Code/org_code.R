library(data.table)
### set up a symbolic link
### on. mac this is > ln -s [Box Location] [Github Location]
### on windows, who knows
### can always hardwire if you have to
cbo_dictionary <- read.csv("EJ_Paper/CBODict_R_Update.csv")
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

nodes <- data.table()


### this is test reads in only first five!!!
### remove the index to read in all
for(file in extract_list[1:5]){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   nodes <- rbind(nodes,temp,fill = T,use.names = T)
   nodes = nodes[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
}

#no tester
for(file in extract_list){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   nodes <- rbind(nodes,temp,fill = T,use.names = T)
   nodes = nodes[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
}


nodes$org_type <- NA
nodes$CBO <- NA
### hard match against CBO dictionary
nodes$CBO[nodes$entity_name %in% cbo_dictionary$Name] <- TRUE
nodes$org_type[nodes$entity_name %in% cbo_dictionary$Name] <- 'CBO'

#### soft match against particular string type
nodes$org_type[grepl("resource_conservation_district",nodes$entity_name)] <- 'RCD'

### hard match against specific string type
nodes$org_type[nodes$entity_name == 'yolo_county_conservation_district'] <- 'RCD'

head(nodes)

table(n$nodelist$entity_type)
head(n$nodelist)
