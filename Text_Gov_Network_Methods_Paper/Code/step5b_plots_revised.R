library(ggraph)
filekey <- read.csv("filekey.csv")
self_loops = F
govsci_tbl_clean <- readRDS(filekey[filekey$var_name=="govsci_tbl_clean",]$filepath)
vector_of_GSAs <- readRDS(filekey[filekey$var_name=="vector_of_GSAs",]$filepath)
vector_of_GSAs <- append(vector_of_GSAs, "Groundwater_Sustainability_Agency")
###Section 1 ####
#the remaining lines are systems, programs, models, databases, or concepts rather than agencies
nonagencies <- c("Irrigation_Management_Information_System",
  "Groundwater_Sustainability_Plan",
  "National_Cooperative_Soil_Survey_Geographic_Database",
  "Soil_Survey_Geographic_Database",
  "Integrated_Regional_Water_Management_Plan",
  "Integrated_Regional_Water_Management",
  "Precipitation_-_Elevation_Regressions_on_Independent_Slopes_Model",
  "Statewide_Groundwater_Elevation_Monitoring",
  "Groundwater_Dependent_Ecosystem",
  "Irrigated_Lands_Regulatory_Program", 
  "Environmental_Data_Exchange_Network",
  "Safe_Drinking_Water_Information_System",
  "Environmental_Quality_Act",
  "Comprehensive_Environmental_Response_Compensation_and_Liability_Act",
  "Public_Land_Survey_System",
  "National_Environmental_Policy_Act",
  "National_Pollutant_Discharge_Elimination_System",
  "National_Water_Information_System",
  "Environmental_Quality_Incentives_Program",
  "Groundwater_Ambient_Monitoring_and_Assessment",
  "Climate_Reference_Network",
  "Sustainable_Groundwater_Management_Act")



###Section 2 ####
#we don't need nicknames
govsci_tbl_clean <- unique(govsci_tbl_clean[,c("State","Agency")])
#only federal and state agencies
govsci_tbl_clean <- govsci_tbl_clean[State %in% c("federal","California"),]
vector_of_feds <- govsci_tbl_clean[State=="federal",]$Agency
vector_of_feds <- vector_of_feds[!vector_of_feds %in% nonagencies]
vector_of_stateagencies <- govsci_tbl_clean[State=="California",]$Agency
vector_of_stateagencies <- vector_of_stateagencies[!vector_of_stateagencies %in% nonagencies]
edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <- substr(edges_and_nodes, nchar(edges_and_nodes)-7,nchar(edges_and_nodes)-4)

#only plots orgs and people
#single gsa example = 16; 65 and 74 are multi-gsa
for(m in c(16, 65, 74)){
   full_graph <- readRDS(paste0(filekey[filekey$var_name=="full_directed_graphs_govnetpaper",]$filepath,gspids[m]))
   full_graph <- subgraph(full_graph, V(full_graph)[
      vertex_attr(full_graph,"entity_type") %in% c("ORG","PERSON")])
   
   #now remove loops so that isolates with a self-loop are not plotted
   full_graph_no_loops <- igraph::simplify(full_graph, remove.multiple = F, remove.loops = T)
   
   isolates = which(igraph::degree(full_graph_no_loops)==0)
   full_graph_noisolates = igraph::delete.vertices(full_graph_no_loops, isolates)
   
   igraph::E(full_graph_noisolates)$weight <- 1
   
   
   E(full_graph_noisolates)$head_verb_tense <- ifelse(E(full_graph_noisolates)$is_future==T, 
                                                      "Future",E(full_graph_noisolates)$head_verb_tense)
   E(full_graph_noisolates)$head_verb_tense <- case_when(
      E(full_graph_noisolates)$head_verb_tense == "Future" ~ "Future",
      E(full_graph_noisolates)$head_verb_tense == "VBD" | E(full_graph_noisolates)$head_verb_tense == "VBN" ~ "Past",
      E(full_graph_noisolates)$head_verb_tense == "VB" | 
         E(full_graph_noisolates)$head_verb_tense == "VBZ" | 
         E(full_graph_noisolates)$head_verb_tense == "VBP" | 
         E(full_graph_noisolates)$head_verb_tense == "VBG" ~ "Present",
      
   )
   
   edges <- as_data_frame(full_graph_noisolates)
   edges$head_verb_tense <- factor(edges$head_verb_tense, levels = c("Past","Present","Future"))
   
   edg <- edges |> group_by(to,from) |> mutate(most_common_tense = which.max(tabulate(head_verb_tense)))

   igraph::E(full_graph_noisolates)$most_common_tense <- edg$most_common_tense
   
   nodes <- as_data_frame(full_graph_noisolates, what = "vertices")
   nodes$scope <- case_when(nodes$name %in% tolower(vector_of_GSAs) ~ "GSA",
                            nodes$name %in% tolower(vector_of_feds) ~ "Federal",
                            nodes$name %in% tolower(vector_of_stateagencies) ~ "State",
                            T ~ "Other")
   nodes$scope <- factor(nodes$scope, levels = c("Federal","State","GSA","Other"))
   igraph::V(full_graph_noisolates)$scope <- nodes$scope
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "head_verb_id")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "head_verb_tense")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "head_verb_name")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "head_verb_lemma")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "parent_verb_id")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "neg")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "doc_sent_verb")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "doc_sent_parent")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "helper_lemma")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "helper_token")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "xcomp_verb")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "xcomp_helper_lemma")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "xcomp_helper_token")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "edgeiscomplete")
   full_graph_noisolates <- igraph::delete_edge_attr(full_graph_noisolates, "is_future")
   
   weighted_graph_noisolates <- igraph::simplify(full_graph_noisolates, 
                                 edge.attr.comb=list(weight="sum",
                                                     has_hedge="mean",
                                                     most_common_tense = "max"), remove.loops = !self_loops)
   
   E(weighted_graph_noisolates)$most_common_tense <- case_when(
      E(weighted_graph_noisolates)$most_common_tense  == 1 ~ "Past",
      E(weighted_graph_noisolates)$most_common_tense  == 2 ~ "Present",
      E(weighted_graph_noisolates)$most_common_tense  == 3 ~ "Future"
      
   )
   
   E(weighted_graph_noisolates)$most_common_tense <- factor(E(weighted_graph_noisolates)$most_common_tense,
                                                              levels = c("Past","Present","Future"))
   V(weighted_graph_noisolates)$degree <- igraph::degree(weighted_graph_noisolates, mode = "all")
   #order of these layers matters
   
   components <- igraph::components(weighted_graph_noisolates, mode="weak")
   giant_component_id <- which.max(components$csize)
   
   # ids
   vert_ids <- V(weighted_graph_noisolates)[components$membership == giant_component_id]
   
   # subgraph
   giant_component <- igraph::induced_subgraph(weighted_graph_noisolates, vert_ids)
   
   weighted_plot_noisolates <- ggraph(giant_component, layout = 'fr')+
      #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
      #Using Paul Tol color schemes
      scale_edge_color_manual(values = c("#88CCEE","#999933","#CC6677"))+
      geom_edge_fan(aes(color = most_common_tense),
                    alpha = 1, 
                    end_cap = circle(1,"mm"),
                    width = 0.4,
                    arrow = arrow(angle=15,length=unit(0.03,"inches"),ends = "last",type = "closed"))+
      #tol_high-contrast color scheme  "#DDAA33" is the GPE color
      scale_color_manual(values = c("#004488","#BB5566","#DDAA33","#DDDDDD"))+
      geom_node_point(aes(color = scope, size = degree),
                      alpha = 0.8)+
      #geom_node_text(aes(label = name), size=2, repel = F) +
      theme_void()
   
   
   ggsave(paste0("giant_component_plot_",gspids[m],".png"), plot = weighted_plot_noisolates, device = "png",
          path = filekey[filekey$var_name=="psj_govnetpaper_figures",]$filepath, width = 4020, height = 1890, dpi = 300,
          units = "px", bg = "white")
   
}

#only plots agencies "submit" and "supply"
for(m in c(41, 46)){
   fund_graph <- readRDS(paste0(filekey[filekey$var_name=="full_directed_graphs_govnetpaper",]$filepath,gspids[m]))
   fund_graph <- fund_graph - E(fund_graph)[!
      edge_attr(fund_graph,"head_verb_lemma") %in% c("supply","submit")]
   
   #now remove loops so that isolates with a self-loop are not plotted
   fund_graph <- igraph::simplify(fund_graph, remove.multiple = F, remove.loops = T)
   
   #plan and database nodes are not orgs so should be removed
   fund_graph <- fund_graph - V(fund_graph)[vertex_attr(fund_graph, "name") %in% 
                              c("tule_subbasin_water_management_database", 
                           "groundwater_sustainability_plan") ]
   
   edges <- as_data_frame(fund_graph, what = "edges")
   nodes <- as_data_frame(fund_graph, what = "vertices")
   
   nodes$scope <- case_when(nodes$name %in% tolower(vector_of_GSAs) ~ "GSA",
                            nodes$name %in% tolower(vector_of_feds) ~ "Federal",
                            nodes$name %in% tolower(vector_of_stateagencies) ~ "State",
                            T ~ "Other")
   
   nodes$scope <- factor(nodes$scope, levels = c("GSA","Federal","State","Other"))
   edges$verb <- igraph::E(fund_graph)$head_verb_lemma 
   
   nodes <- nodes[! nodes$name %in% c("department","subbasin",
                        "tule_subbasin_technical_advisory_committee_s"),]
                          
   edges$to <- case_when(edges$to == "department" ~ "department_of_water_resources",
                         edges$to == "subbasin" ~ "tule_subbasin",
                         edges$to == "tule_subbasin_technical_advisory_committee_s" ~ "tule_subbasin_technical_advisory_committee",
                         T ~ edges$to)
   
   edges$from <- case_when(edges$from == "department" ~ "department_of_water_resources",
                         edges$from == "subbasin" ~ "tule_subbasin",
                         edges$from == "tule_subbasin_technical_advisory_committee_s" ~ "tule_subbasin_technical_advisory_committee",
                         T ~ edges$from)
   
   fund_graph <- igraph::graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = T)
   
   isolates = which(igraph::degree(fund_graph)==0)
   fund_graph = igraph::delete.vertices(fund_graph, isolates)
   V(fund_graph)$scope <- factor(V(fund_graph)$scope, levels = c("GSA","Federal","State","Other"))
   
   components <- igraph::components(fund_graph, mode="weak")
   giant_component_id <- which.max(components$csize)
   
   # label ids
   vert_ids <- V(fund_graph)[components$membership == giant_component_id]
   
   # create subgraph
   giant_component <- igraph::induced_subgraph(fund_graph, vert_ids)
   
   
   #order of these layers matters
   fund_plot <- ggraph(giant_component, layout = 'fr')+
      #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
      scale_edge_color_manual(values = c("#88CCEE","#999933"))+
      geom_edge_fan(aes(color = verb),
                    end_cap = circle(1,"mm"),
                    width = 0.3,
                    arrow = arrow(angle=15,length=unit(0.08,"inches"),ends = "last",type = "closed"))+
      #tol_high-contrast color scheme  "#DDAA33" is the GPE color
      geom_node_point(aes(color = scope), size = 3,
                      alpha = 0.8)+
      scale_color_manual(values = c("#004488","#BB5566","#DDAA33"))+
      
      geom_node_text(aes(label = name), repel = TRUE)
      #geom_node_text(aes(label = labels),
      #               size = 3, repel = T, color = "black")+
      theme_void()
   ggsave(paste0("submit_supply_plot_",gspids[m],".png"), plot = fund_plot, device = "png",
          path = filekey[filekey$var_name=="psj_govnetpaper_figures",]$filepath, width = 2010, height = (1890/2), dpi = 300,
          units = "px", bg = "white")
}

#only plots agencies "grant" and "submit"
for(m in c(69)){
   fund_graph <- readRDS(paste0(filekey[filekey$var_name=="full_directed_graphs_govnetpaper",]$filepath,gspids[m]))
   
   fund_graph <- fund_graph - E(fund_graph)[!
                      edge_attr(fund_graph,"head_verb_lemma") %in% c("grant","submit")]
   
   #now remove loops so that isolates with a self-loop are not plotted
   fund_graph <- igraph::simplify(fund_graph, remove.multiple = F, remove.loops = T)
   
   igraph::E(fund_graph)$verb <- igraph::E(fund_graph)$head_verb_lemma 
   
   edges <- as_data_frame(fund_graph, what = "edges")
   nodes <- as_data_frame(fund_graph, what = "vertices")
   
   nodes$scope <- case_when(nodes$name %in% tolower(vector_of_GSAs) ~ "GSA",
                            nodes$name %in% tolower(vector_of_feds) | nodes$name == "united_states_bureau_of_reclamation" ~ "Federal",
                            nodes$name %in% tolower(vector_of_stateagencies) ~ "State",
                            T ~ "Other")
   
   nodes <- nodes[! nodes$name %in% c("department"),]
   
   edges$to <- case_when(edges$to == "department" ~ "department_of_water_resources",
                         T ~ edges$to)
   
   edges$from <- case_when(edges$from == "department" ~ "department_of_water_resources",
                           T ~ edges$from)
   
   fund_graph <- igraph::graph_from_data_frame(d = edges, 
                                               vertices = nodes, 
                                               directed = T)
   
   isolates = which(igraph::degree(fund_graph)==0)
   fund_graph = igraph::delete.vertices(fund_graph, isolates)
   V(fund_graph)$scope <- factor(V(fund_graph)$scope, levels = c("GSA","Federal","State","Other"))
   
   components <- igraph::components(fund_graph, mode="weak")
   giant_component_id <- which.max(components$csize)
   
   # ids
   vert_ids <- V(fund_graph)[components$membership == giant_component_id]
   
   # subgraph
   giant_component <- igraph::induced_subgraph(fund_graph, vert_ids)
   
   #order of these layers matters
   fund_plot <- ggraph(giant_component, layout = 'fr')+
      #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
      geom_edge_fan(aes(color = verb),
                    end_cap = circle(1,"mm"),
                    width = 0.3,
                    arrow = arrow(angle=15,length=unit(0.08,"inches"),ends = "last",type = "closed"))+
      #tol_high-contrast color scheme  "#DDAA33" is the GPE color
      scale_color_manual(values = c("#004488","#BB5566","#DDAA33","#222222"))+
      geom_node_point(aes(color = scope), size = 3,
                      alpha = 0.8)+
      geom_node_text(aes(label = name), repel = TRUE)+
   #geom_node_text(aes(label = labels),
   #               size = 3, repel = T, color = "black")+
   theme_void()
   
   
   ggsave(paste0("grant_submit_plot_",gspids[m],".png"), plot = fund_plot, device = "png",
          path = filekey[filekey$var_name=="psj_govnetpaper_figures",]$filepath, width = 2010, height = (1890/2), dpi = 300,
          units = "px", bg = "white")
   
}

