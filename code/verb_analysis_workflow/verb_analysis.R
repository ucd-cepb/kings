library(data.table)
library(stringr)
library(dplyr)

file_list <- list.files(path = "cleaned_extracts", full.names = T)
gspids <- substr(file_list, 18,21)

verblist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      verblist[[m]] <- readRDS(paste0("cleaned_extracts/",gspids[m],".RDS"))$verblist
   }
}
vlist <- rbindlist(verblist)

vlist <- vlist[!duplicated(vlist$head_verb_lemma)]
vlist <- vlist[!str_detect(vlist$head_verb_lemma,"[0-9]")]

edgelist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      edgelist[[m]] <- readRDS(paste0("cleaned_extracts/",gspids[m],".RDS"))$edgelist
   }
}

nodelist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      nodelist[[m]] <- readRDS(paste0("cleaned_extracts/",gspids[m],".RDS"))$nodelist
   }
}

for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      edgelist[[m]] <- left_join(edgelist[[m]],nodelist[[m]][,c("entity_cat","entity_type")], by= c("source"="entity_cat"))
      colnames(edgelist[[m]])[which(colnames(edgelist[[m]])=="entity_type")] <- "source_entity_type"
      edgelist[[m]] <- left_join(edgelist[[m]],nodelist[[m]][,c("entity_cat","entity_type")], by= c("target"="entity_cat"))
      colnames(edgelist[[m]])[which(colnames(edgelist[[m]])=="entity_type")] <- "target_entity_type"
   }
}


elist <- rbindlist(edgelist)
evlist <- left_join(elist, vlist)

#dummy out verbnet types
all_type_ids <- unique(unlist(evlist$type_id))

type_dummies <- setDT(key=all_type_ids)
type_dummies[i,] <- lapply(evlist$type_id, function (i) t(sapply(all_type_ids, 
                                 function(j) j %in% i)))
type_dummies <- lapply(evlist$type_id, function (i) t(sapply(all_type_ids, 
       function(j) j %in% i)))
type_dms <- rbind(type_dummies)
type_dmms <- setDT(type_dms)

colnames(type_dms) <- paste0("type_",unique(unlist(evlist$head_lemma_type_id)))

#join on gsp_mini for approval, etc



#vars: source_entity_type, target_entity_type
#approval
#drinking water and ag variables



#vars: source_entity_type, target_entity_type
#approval
#drinking water and ag variables
gsp_meta <- readRDS("data_output/gsp_docs_w_meta")
gsp_mini <- unique(gsp_meta[,c(14,16,19:26,7)])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
#for meta
#network_properties <- as_tibble(evlist)
net_with_gsa_attr <- merge(gsp_mini, evlist)
   