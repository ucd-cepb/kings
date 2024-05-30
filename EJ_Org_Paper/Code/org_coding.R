n = readRDS('data/Text_Gov_Network_Methods_Paper/cleaned_extracts/0013.RDS')
nodes = n$nodelist
nodes = nodes[nodes$entity_type!='PERSON',]

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
