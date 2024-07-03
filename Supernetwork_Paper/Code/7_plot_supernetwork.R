library(igraph)
library(intergraph)
use_filtered_parsefiles <- T
if(use_filtered_parsefiles==T){
   supernetwork <- readRDS(filekey[filekey$var_name=="supernetwork_filtered_full_superpaper",]$filepath)
   
}else{
   supernetwork <- readRDS(filekey[filekey$var_name=="supernetwork_unfiltered_full_superpaper",]$filepath)
   
}

   full_graph <- subgraph(supernetwork, V(supernetwork)[
      vertex_attr(supernetwork,"entity_type") %in% c("ORG","PERSON")])
   
   #the act is not an org so should be removed
   full_graph <- full_graph - V(full_graph)[vertex_attr(full_graph, "name") %in% 
                                               c("sustainable_groundwater_management_act") ]
   
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
   
   edges <- igraph::as_data_frame(full_graph_noisolates)
   edges$head_verb_tense <- factor(edges$head_verb_tense, levels = c("Past","Present","Future"))
   
   edg <- edges |> group_by(to,from) |> mutate(most_common_tense = which.max(tabulate(head_verb_tense)))
   
   igraph::E(full_graph_noisolates)$most_common_tense <- edg$most_common_tense
   
   nodes <- igraph::as_data_frame(full_graph_noisolates, what = "vertices")
   
   govsci_tbl_clean <- readRDS(filekey[filekey$var_name=="govsci_tbl_clean",]$filepath)
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
   
   
   vector_of_GSAs <- readRDS(filekey[filekey$var_name=="vector_of_GSAs",]$filepath)
   vector_of_GSAs <- append(vector_of_GSAs, "Groundwater_Sustainability_Agency")
   vector_of_feds <- govsci_tbl_clean[State=="federal",]$Agency
   vector_of_feds <- vector_of_feds[!vector_of_feds %in% nonagencies]
   vector_of_stateagencies <- govsci_tbl_clean[State=="California",]$Agency
   vector_of_stateagencies <- vector_of_stateagencies[!vector_of_stateagencies %in% nonagencies]
   
   nodes$scope <- case_when(nodes$name %in% tolower(vector_of_GSAs) ~ "GSA",
                            nodes$name %in% tolower(vector_of_feds) | 
                               nodes$name == "united_states_bureau_of_reclamation" ~ "Federal" ,
                            nodes$name %in% tolower(vector_of_stateagencies) ~ "State",
                            T ~ "Other")
   nodes$scope <- factor(nodes$scope, levels = c("Federal","State","GSA","Other"))
   igraph::V(full_graph_noisolates)$scope <- nodes$scope
   
   full_graph_noisolates <- subgraph(full_graph_noisolates, V(full_graph_noisolates)[
      vertex_attr(full_graph_noisolates,"scope") %in% c("GSA","Federal","State")])
   self_loops = F
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
   
   V(weighted_graph_noisolates)$bigname <- ifelse(V(weighted_graph_noisolates)$degree > 5, V(weighted_graph_noisolates)$name, "")
   
   components <- igraph::components(weighted_graph_noisolates, mode="weak")
   giant_component_id <- which.max(components$csize)
   
   # ids
   vert_ids <- V(weighted_graph_noisolates)[components$membership == giant_component_id]
   
   # subgraph
   giant_component <- igraph::induced_subgraph(weighted_graph_noisolates, vert_ids)
   set.seed(233)
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
      geom_node_text(aes(label = bigname), size=2, repel = T, max.overlaps=30) +
      theme_void()+ theme(legend.position.inside = c(0.8,0.6))
   weighted_plot_noisolates
   
   if(use_filtered_parsefiles==T){
      ggsave(paste0("supernetwork_giantcomponent_filtered_plot.png"), plot = weighted_plot_noisolates, device = "png",
             path = filekey[filekey$var_name=="supernetwork_figures",]$filepath, width = 2200, height = 1890, dpi = 300,
             units = "px", bg = "white")
      
   }else{
      ggsave(paste0("supernetwork_giantcomponent_unfiltered_plot.png"), plot = weighted_plot_noisolates, device = "png",
             path = filekey[filekey$var_name=="supernetwork_figures",]$filepath, width = 2200, height = 1890, dpi = 300,
             units = "px", bg = "white")
      
   }
   
