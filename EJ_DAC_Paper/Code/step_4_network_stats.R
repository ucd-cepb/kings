library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/EJ_Paper/processed_networks")
extract_list <- list.files(network_fp)

gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))
gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

clean_gsa_names <- function(gsa_names) {
   # replace groundwater_sustainability_agency with gsa
   gsa_names_2 <- gsa_names
   gsa_names_2$GSA_Name <- str_replace(gsa_names$GSA_Name, 
                                       "groundwater_sustainability_agency", 
                                       "gsa")
   
   # add in gsa names with 'groundwater' in them already, removing 'groundwater_sustainability_agency'
   gsa_names_3 <- gsa_names %>% 
      mutate(GSA_Name = str_replace(gsa_names$GSA_Name, 
                                    "_groundwater_sustainability_agency", 
                                    "")) %>% 
      filter(grepl("groundwater", GSA_Name))
   
   gsa_names_4 <- data.frame(GSA_ID=c('147',
                                      '461',
                                      '457',
                                      '253',
                                      '456',
                                      '106',
                                      '415',
                                      '418',
                                      '384',
                                      '432',
                                      '163',
                                      '433',
                                      '434',
                                      '49',
                                      '24',
                                      '403', 
                                      '47',
                                      '422',
                                      '124',
                                      '420', 
                                      '419',
                                      '349'
                                      ),
                             GSA_Name = c('sacramento_central_groundwater_authority',
                                           'salinas_valley_basin_groundwater_sustainability_agency',
                                           'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_butte_valley',
                                           'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_scott_river',
                                           'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_shasta',
                                           'yuba_water_agency',
                                           'yuba_water_agency',
                                           'tehama_county_flood_control_and_water_conservation_district',
                                           'reclamation_district_no_501_groundwater_sustainability_agency_northern_delta_groundwater_sustainability_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'arroyo_santa_rosa_groundwater_sustainability_agency',
                                           'mga',
                                           'owens_valley_groundwater_authority',
                                           'madera_co_groundwater_sustainability_agency',
                                          'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_antelope',
                                          'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_bowman', 
                                          'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_los_molinos',
                                          'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_red_bluff',
                                          'yucaipa_groundwater_sustainability_agency'
                                          )
                             )
   
   gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4)
   return(gsa_names)
}

gsa_names <- clean_gsa_names(gsa_names)

net_stats <- function(network_graph, gsp_id) {
   
   # get gsa names from gsa_gsp and gsa_names
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids), 
                       gsa_names, 
                       by = "GSA_ID")$GSA_Name
   gsa_names2 <- c(gsa_names2, 'groundwater_sustainability_agency', 'gsa') #add in deafult
   gsa_ins <- c(which(V(network_graph)$name %in% gsa_names2))
   
   #distance to gsa(s)
   dists <- data.frame(matrix(ncol = length(gsa_ins), 
                              nrow = vcount(network_graph)))
   colnames(dists) <- paste0('X', gsa_ins)
   
   for (i in gsa_ins){
      dist <- distances(network_graph, 
                        to=i,
                        mode = 'in',
                        weights=NA)
      dist[is.infinite(dist)] <- NA # replace infinite distance with NA
      dist[is.nan(dist)] <- NA # replace NA with NA
      dists[[paste0('X', i)]] <- dist
   }
   
   colnames(dists) <- V(network_graph)$name[gsa_ins]
   leader_dist_min <- apply(dists, 1, min, na.rm = TRUE)
   
   #distance to gsa(s) WEIGHTED
   dists_w <- data.frame(matrix(ncol = length(gsa_ins), 
                              nrow = vcount(network_graph)))
   colnames(dists_w) <- paste0('X', gsa_ins)
   
   for (i in gsa_ins){
      dist_w <- distances(network_graph, 
                        to=i,
                        mode = 'in',
                        weights=E(network_graph)$weight)
      dist_w[is.infinite(dist_w)] <- NA # replace infinite distance with NA
      dist_w[is.nan(dist_w)] <- NA # replace NA with NA
      dists_w[[paste0('X', i)]] <- dist_w
   }
   
   colnames(dists_w) <- V(network_graph)$name[gsa_ins]
   leader_dist_min_w <- apply(dists_w, 1, min, na.rm = TRUE)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min',
                                    value = ifelse(is.infinite(leader_dist_min), 
                                                   NA, 
                                                   leader_dist_min))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min_w',
                                    value = ifelse(is.infinite(leader_dist_min_w), 
                                                   NA, 
                                                   leader_dist_min_w))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min_nona',
                                    value = ifelse(is.infinite(leader_dist_min), 
                                                   diameter(network_graph), 
                                                   leader_dist_min))
   
   leader_dist_min_w_nona <- ifelse(is.infinite(leader_dist_min_w), 
                                    diameter(network_graph), 
                                    leader_dist_min_w)
   
   leader_dist_min_w_nona_norm <- (leader_dist_min_w_nona - min(leader_dist_min_w_nona, na.rm = TRUE)) / (max(leader_dist_min_w_nona, na.rm = TRUE) - min(leader_dist_min_w_nona, na.rm = TRUE))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min_w_nona',
                                    value = leader_dist_min_w_nona)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min_w_nona_norm',
                                    value = leader_dist_min_w_nona_norm)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'GSA',
                                    value = ifelse(V(network_graph)$name %in% gsa_names2, 1, 0))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'is_place',
                                    value = ifelse(is.na(V(network_graph)$GEOID20), 0, 1))

   network_graph <- set_vertex_attr(network_graph,
                                    'in',
                                    value = node_indegree(network_graph, 
                                                          normalized=FALSE))
   
   in_w <- node_indegree(network_graph, 
                      normalized=FALSE,
                      alpha=1)
   
   in_w <- ifelse(is.nan(in_w), 0, in_w)
   
   in_w_norm <- (in_w - min(in_w, na.rm = TRUE)) / (max(in_w, na.rm = TRUE) - min(in_w, na.rm = TRUE))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'in_w',
                                    value = in_w)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'in_w_norm',
                                    value = in_w_norm)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'out',
                                    value = node_outdegree(network_graph,
                                                           normalized=FALSE))
   
   out_w <- node_outdegree(network_graph,
                      normalized=FALSE,
                      alpha=1)
   
   out_w <- ifelse(is.nan(out_w), 0, out_w)
   
   out_w_norm <- (out_w - min(out_w, na.rm = TRUE)) / (max(out_w, na.rm = TRUE) - min(out_w, na.rm = TRUE))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'out_w',
                                    value = out_w)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'out_w_norm',
                                    value = out_w_norm)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'deg',
                                    value = node_deg(network_graph, 
                                                     direction = 'all'))
   network_graph <- set_vertex_attr(network_graph,
                                    'eig',
                                    value = node_eigenvector(network_graph))
   
   network_graph <- set_vertex_attr(network_graph, 
                                    'pr',
                                    value = igraph::page_rank(network_graph,
                                                              weights = NA)$vector)  
   
   network_graph <- set_vertex_attr(network_graph,
                                    'pr_w',
                                    value = igraph::page_rank(network_graph,
                                                              weights = V(network_graph)$weight)$vector)
   
   
   
   return(network_graph)
}

# process stats for each network
for (g in seq_along(gsp_ids)) {
   gsp_id <- gsp_ids[g]
   
   graph_stats <- net_stats(network_graph = readRDS(paste0(network_fp, 
                                                           "/", 
                                                           extract_list[g])
                                                    ), 
                            gsp_id = gsp_ids[g]
                            )

   # Save graph
   saveRDS(object = graph_stats, 
           file = paste0(Sys.getenv("BOX_PATH"), 
                         "/EJ_Paper/cleaned_extracts_DACified/", 
                         extract_list[g]))
   
   print(paste0("Finished ", gsp_id))
   
}

# test 

neti <- 15
net <- net_stats(network_graph = readRDS(paste0(network_fp, "/",extract_list[neti])),
                   gsp_id = gsp_ids[neti])
df <- tibble(igraph::as_data_frame(net, what='vertices'))

gsast<- gsa_gsp %>% 
   filter(GSP_ID == gsp_ids[neti]) %>% 
   pull(GSA_IDs) %>% 
   str_split(",") %>% 
   unlist() %>% 
   as.integer() 

gsa_names %>%
   filter(GSA_ID %in% gsast) %>% 
   select(GSA_ID, GSA_Name)

df %>% 
   filter(str_detect(name, "groundwater_sustainability_agency")|
             str_detect(name, "gsa")) %>% 
   select(name, GSA, deg) %>% 
   print(n=Inf)

dfm1 <- df %>% 
   filter(str_detect(name, "groundwater_sustainability_agency")|
             str_detect(name, "gsa")) %>% 
   pull(name) %>% 
   str_replace_all("_", " ") %>%
   corpus() %>% 
   tokens() %>%
   tokens_compound(pattern = phrase(c('groundwater sustainability agency',
                                      'board of directors'))) %>% 
   dfm()

dfm2 <- gsa_names %>%
   filter(GSA_ID %in% gsast) %>% 
   pull(GSA_Name) %>% 
   str_replace_all("_", " ") %>%
   corpus() %>% 
   tokens() %>% 
   tokens_compound(pattern = phrase('groundwater sustainability agency')) %>% 
   dfm()

textstat_simil(dfm1, dfm2,
               method='dice',
               min_simil = 0.8)
gsa_names %>% filter(GSA_ID %in% gsast)