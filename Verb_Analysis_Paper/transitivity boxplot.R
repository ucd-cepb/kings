filekey <- read.csv("filekey.csv")

#load in network objects
edgesnodes <- list.files(path = filekey[filekey$var_name=="disambiged_unfiltered_extracts_superpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edgesnodes,'[0-9]{1,}')
gspids <- gspids[!gspids%in% c("0053", "0089")]

nets <- vector(mode = "list", length = 117)
for(i in 1:length(gspids)){
   nets[[i]] <- readRDS(paste0("data/Text_Gov_Network_Methods_Paper/full_directed_graphs/", "full_directed_graph_", gspids[i]))
}

netsold <- nets
nets <- netsold
infonets <- vector(mode = "list", length = 117)
moneynets <- vector(mode = "list", length = 117)

parsed <- vector(mode = "list", length = length(gspids))

for(i in 1:length(gspids)){
   parsed[[i]] <- readRDS(paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath,
                                 gspids[i]))
}

#add flow attributes
flow_attr <- vector(mode = "list", length = length(gspids))
library(tidyverse)
for(i in 1:length(parsed)){
   print(i)
   parsed[[i]]$doc_sent <- paste0(parsed[[i]]$doc_id, "_", parsed[[i]]$sentence_id)
   parsed[[i]]$nexttoken <- c(parsed[[i]]$token[2:length(parsed[[i]]$token)], NA)
   flow_attr[[i]] <- parsed[[i]] |> group_by(doc_sent) |> summarize(
      info_flow = any(tolower(token) %in% c("data",
                                            "database",
                                            "databases",
                                            "documentation",
                                            "information",
                                            "info",
                                            "results",
                                            "log",
                                            "model",
                                            "models",
                                            "report",
                                            "reports",
                                            "research",
                                            "sop",
                                            "sops",
                                            "study",
                                            "studies")),
      money_flow = any(tolower(token) %in% c("financial",
                                             "financially",
                                             "fund",
                                             "funds",
                                             "funding",
                                             "cost",
                                             "costs",
                                             "costly",
                                             "fee",
                                             "fees",
                                             "expensive",
                                             "expenditures",
                                             "$",
                                             "financing",
                                             "loan",
                                             "tax",
                                             # "assessment",   doesn't always mean financial assessment
                                             "revenue"
      )) |
         any(tolower(token) == "interest" & tolower(nexttoken) == "rate") |
         #any(!tolower(token) %in% c("water","groundwater") & tolower(nexttoken) %in% c("budget","budgets")) |
         any(!tolower(token) %in% c("groundwater", "aquifer") & tolower(nexttoken) == "reserves") |
         any(tolower(token) %in% c("grant", "grants") & !tolower(nexttoken) %in% c("access",
                                                                                   "authority",
                                                                                   "permission"))
   )
}

library(data.table)

all_flow_attrs <- rbindlist(flow_attr)
all_flow_attrs$money_flow <- ifelse(is.na(all_flow_attrs$money_flow),
                                    F, all_flow_attrs$money_flow)

#make subgraphs moneynets and infonets
library(igraph)
for(i in 1:length(nets)){
   print(i)
   docsents <- stringr::str_remove(igraph::edge_attr(nets[[i]], "doc_sent_verb"), "_\\d*$")
   flows_in_edge_order <- all_flow_attrs[match(docsents, all_flow_attrs$doc_sent),]
   nets[[i]] <- igraph::set.edge.attribute(nets[[i]], "info_flow", value = flows_in_edge_order$info_flow)
   infonets[[i]] <- igraph::delete_edges(nets[[i]], E(nets[[i]])[!edge_attr(nets[[i]], "info_flow")])
   nets[[i]] <- igraph::set.edge.attribute(nets[[i]], "money_flow", value = flows_in_edge_order$money_flow)
   moneynets[[i]] <- igraph::delete_edges(nets[[i]], E(nets[[i]])[!edge_attr(nets[[i]], "money_flow")])
   
}


for(i in 1:length(nets)){
   infonets[[i]] <- igraph::delete.vertices(infonets[[i]], igraph::V(infonets[[i]])[igraph::degree(infonets[[i]], igraph::V(infonets[[i]]), mode = "all",loops = F)==0])
   moneynets[[i]] <- igraph::delete.vertices(moneynets[[i]], igraph::V(moneynets[[i]])[igraph::degree(moneynets[[i]], igraph::V(moneynets[[i]]), mode = "all",loops = F)==0])
}

transinfo <- vector(mode = "numeric", length = 117)
transmoney <- vector(mode = "numeric", length = 117)

for(i in 1:length(nets)){
   transinfo[i] <- transitivity(infonets[[i]])
   transmoney[i] <- transitivity(moneynets[[i]])
}

info_subgraph <- transinfo
typeinfo <- rep("info", length(info_subgraph))
money_subgraph <- transmoney
typemoney <- rep("money", length(money_subgraph))

value <- append(info_subgraph, money_subgraph)
type <- append(typeinfo, typemoney)
infomoney <- data.frame(value, type)


library(ggplot2)

ggplot(infomoney) + geom_boxplot(aes(x = type, y = value)) + theme_bw()

install.packages("ergm")

install.packages("ergm.multi")

nets[[i]] <- igraph::set.vertex.attribute(nets[[i]], "betw_centr", value = betweenness(nets[[i]]))
#gwesp = triadic closure (bonding structure)
#money_flow = bonding behavior
#kstar = preferential attachment (bridging structure)
#betw_centr = betweenness centrality (bridging structure)
#info_flow = bridging behavior

saveRDS(nets, "networks_for_verb_paper.RDS")
saveRDS(all_flow_attrs, "all_flow_attrs_for_verb_paper.RDS")
saveRDS(flow_attr, "list_of_flow_attrs_for_verb_paper.RDS")
saveRDS(infomoney, "info_and_money_dataframe.RDS")
saveRDS(infonets, "info_subnetworks.RDS")
saveRDS(moneynets, "money_subnetworks.RDS")

#to undirected
undir_nets <- vector(mode = "list", length = 117)

for(i in 1:length(undir_nets)){
   undir_nets[[i]] <- as_undirected(nets[[i]], mode = "each")
}

for(i in 1:length(undir_nets)){
   undir_nets[[i]] <- set_edge_attr(undir_nets[[i]], 
      name = "is_info", 
      value = case_when(
         edge_attr(undir_nets[[i]], name = "info_flow") == F ~ 0,
         edge_attr(undir_nets[[i]], name = "info_flow") == T ~ 1)
         )
   undir_nets[[i]] <- set_edge_attr(undir_nets[[i]], 
       name = "is_money", 
       value = case_when(
          edge_attr(undir_nets[[i]], name = "money_flow") == F ~ 0,
          edge_attr(undir_nets[[i]], name = "money_flow") == T ~ 1)
   )
   undir_nets[[i]] <- set_edge_attr(undir_nets[[i]], 
       name = "neither", 
       value = case_when(
          edge_attr(undir_nets[[i]], name = "money_flow") == F &
             edge_attr(undir_nets[[i]], name = "info_flow") == F ~ 1,
          .default = 0)
   )
}

saveRDS(undir_nets, "undirected_nets_full.RDS")

unwanted_attrs <- c("head_verb_id",
                    "head_verb_name",
                    "parent_verb_id",
                    "helper_lemma",
                    "helper_token",
                    "info_flow",
                    "money_flow")
for(i in 1:length(undir_nets)){
   for(j in 1:length(unwanted_attrs)){
      undir_nets[[i]] <- remove.edge.attribute(undir_nets[[i]], unwanted_attrs[j])
   }
}

undir_network <- vector(mode = "list", length = 117)
for(i in 1:length(undir_nets)){
   undir_network[[i]] <- igraph::as_data_frame(undir_nets[[i]], what = "both")
}

for(i in 1:length(undir_network)){
   undir_network[[i]] <- network::network(x=undir_network[[i]]$edges, directed = F,
                           hyper = F, loops = T, multiple = T, 
                           bipartiate = F, vertices = undir_network[[i]]$vertices,
                           matrix.type = "edgelist")
}
   
   
layered_nets <- vector(mode = "list", length = 117)
for(i in 1:length(layered_nets)){
   layered_nets[[i]] <- Layer(undir_network[[i]], c(info="is_info", money="is_money", neither="neither"))
   
}

layered_nets

saveRDS(layered_nets, "data/Verb_Analysis_Paper/layered_nets.RDS")
