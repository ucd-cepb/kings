library(data.table)
library(stringr)
library(dplyr)
#important! If you want to do any additional filtering of the dataset, please 
#DO NOT edit the cleaned_extracts file. 
#Instead, save the filtered dataset in a different folder
filekey <- read.csv("filekey.csv")

file_list <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,
                        full.names = T)
gspids <- unname(sapply(file_list, function(k) gsub("\\D", "", k)))

verblist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      verblist[[m]] <- readRDS(paste0(filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,gspids[m],".RDS"))$verblist
   }
}
vlist <- rbindlist(verblist)

vlist <- vlist[!duplicated(vlist$head_verb_lemma)]
vlist <- vlist[!str_detect(vlist$head_verb_lemma,"[0-9]")]

edgelist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      edgelist[[m]] <- readRDS(paste0(filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,gspids[m],".RDS"))$edgelist
   }
}

nodelist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      nodelist[[m]] <- readRDS(paste0(filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,gspids[m],".RDS"))$nodelist
   }
}

for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      edgelist[[m]] <- left_join(edgelist[[m]],nodelist[[m]][,c("entity_name","entity_type")], by= c("source"="entity_name"))
      colnames(edgelist[[m]])[which(colnames(edgelist[[m]])=="entity_type")] <- "source_entity_type"
      edgelist[[m]] <- left_join(edgelist[[m]],nodelist[[m]][,c("entity_name","entity_type")], by= c("target"="entity_name"))
      colnames(edgelist[[m]])[which(colnames(edgelist[[m]])=="entity_type")] <- "target_entity_type"
   }
}


elist <- rbindlist(edgelist)
evlist <- left_join(elist, vlist)

#dummy out verbnet types
all_type_ids <- as.character(sort(unique(as.numeric(unlist(evlist$type_id)))))
type_dummies <- setNames(data.table(matrix(nrow = nrow(evlist), ncol = length(all_type_ids))), paste0("type_",all_type_ids))

for(i in 1:nrow(evlist)){
   type_dummies[i,names(type_dummies):= lapply(all_type_ids, 
                              function(j) j %in% evlist$type_id[[i]])]
}

saveRDS(type_dummies, filekey[filekey$var_name=="verbtype_dummies_for_edgelist",]$filepath)
edgelist_with_vclass <- cbind(evlist, type_dummies)

edgelist_with_vclass$gsp_id <- unlist(lapply(edgelist_with_vclass$doc_sent_verb, 
                                      function(j) gsub(".*?([0-9]+).*", "\\1", j)))
saveRDS(edgelist_with_vclass, filekey[filekey$var_name=="edgelist_w_v_class",]$filepath)

#join on gsp_mini for approval, etc

#vars: source_entity_type, target_entity_type
#approval
#drinking water and ag variables
gsp_meta <- readRDS(filekey[filekey$var_name=="gsp_docs_lean",]$filepath)
gsp_mini <- unique(gsp_meta[,c("gsp_id","approval","mult_gsas",
                               "fract_of_area_in_habitat_log_scaled",
                               "urbangw_af_log_scaled",
                               "gwsum",
                               "percent_dac_by_pop_scaled",
                               "Republican_Vote_Share_scaled",
                               "Agr_Share_Of_GDP_scaled",
                               "Perc_Bach_Degree_Over25_scaled",
                               "local_govs_per_10k_people_log_scaled",
                               "maxdryspell_scaled")])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
#for meta
#network_properties <- as_tibble(evlist)
edgelist_w_meta <- merge(gsp_mini, edgelist_with_vclass)
saveRDS(edgelist_w_meta, filekey[filekey$var_name=="edgelist_w_meta",]$filepath)

