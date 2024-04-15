library(data.table)
library(stringr)

### set up a symbolic link
### on. mac this is > ln -s [Box Location] [Github Location]
### on windows, who knows
### can always hardwire if you have to
file_loc <- 'C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\'

file_loc = 'data/EJ_Paper/cleaned_extracts_textgov_paper_version/'
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
# ### remove the index to read in all
# for(file in extract_list[1:5]){
#    print(file)
#    temp = grab_orgs(paste0(file_loc,file))
#    empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
#    empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
# }

#all files
for(file in extract_list){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
   empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),by=.(entity_name,entity_type)]
}


empty_node_dt$org_type <- NA
empty_node_dt$CBO <- NA

#Export data:
write.csv(empty_node_dt, "EJ_Paper/Data/unlabelled_org_list.csv")

