library(data.table)
### set up a symbolic link
### on. mac this is > ln -s [Box Location] [Github Location]
### on windows, who knows
### can always hardwire if you have to
file_loc <- 'data/Text_Gov_Network_Methods_Paper/cleaned_extracts/'
extract_list = list.files(file_loc)

### convenience function for reading in and processing nodes
### extracts have ORG, PERSON, and GPE nodetypes
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


nodes$org_type <- NA
nodes$CBO <- NA
### hard match against CBO dictionary
nodes$CBO[nodes$entity_name %in% cbo_dictionary$cbo_name] <- TRUE
nodes$org_type[nodes$entity_name %in% cbo_dictionary$cbo_name] <- 'CBO'

#### soft match against particular string type
nodes$org_type[grepl('resource_conservation_district',nodes$entity_name)] <- 'RCD'

### hard match against specific string type
nodes$org_type[nodes$entity_name == 'yolo_county_conservation_district'] <- 'RCD

head(nodes)

table(n$nodelist$entity_type)
head(n$nodelist)
