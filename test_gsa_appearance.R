library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")
extract_list <- list.files(network_fp)

# path to page-level data
pages_fp <- paste0(Sys.getenv("BOX_PATH"), 
                   "/Structural_Topic_Model_Paper/gsp_docs_lean")
page_features <- tibble(readRDS(pages_fp))

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

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

gsa_names_4 <- data.frame(GSA_ID=c('111',
                                   '29',
                                   '88',
                                   '89',
                                   '90',
                                   '52',
                                   '53',
                                   '94',
                                   '117',
                                   '156',
                                   '156'),
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
                                       'arroyo_santa_rosa_groundwater_sustainability_agency')
                          )
gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4)

# add node labels from Hannah doc

label_fp <- paste0(Sys.getenv("BOX_PATH"),
                   "/Multipurpose_Files/Dictionaries/googlesheets_dt_complete.csv")
label_dict <- read.csv(label_fp)

govsci_fp <- paste0(Sys.getenv("BOX_PATH"),
                    "/Multipurpose_Files/Dictionaries/govsci_tbl_noblank.csv")
govsci_dict <- read.csv(govsci_fp)

# process node/edgelist for use
net_process <- function(file, gsp_id){
   # grab nodelist
   nl <- tibble(readRDS(file)$nodelist)
   # tag places and DACs
   nl <- nl %>% left_join(label_dict, by=join_by(entity_name == entity_name))
   nl <- nl %>% left_join(govsci_dict, by=join_by(entity_name == Agency)) %>% 
      mutate(org_type = case_when(
         State == 'local' ~ "Loc_Gov",
         State == 'federal' ~ "NL_Gov",
         State == 'California' ~ "CA_Gov",
         TRUE ~ org_type
      )) %>% 
      select(-c(X, State, Abbr)) %>% 
      distinct(., entity_name, .keep_all = TRUE)
   
   # get gsa names from gsa_gsp and gsa_names
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids), 
                       gsa_names, 
                       by = "GSA_ID")$GSA_Name
   gsa_names2 <- c(gsa_names2, 'groundwater_sustainability_agency', 'gsa') #add in deafult
   
   gsa_df_sub <- data.frame(gsp_id=gsp_id, 
                            gsa_names=gsa_names2,
                            exp_gsas = length(gsa_ids)) %>% 
      mutate(in_net = ifelse(gsa_names %in% nl$entity_name, 1, 0))
   
   return(gsa_df_sub)
}

gsa_df <- data.frame(gsp_id = character(),
                     gsa_names = character(),
                     in_net = numeric(),
                     exp_gsas = numeric())

# apply functions to all networks
for (g in seq_along(gsp_ids)) {
   gsp_id <- paste0("gsp_",gsp_ids[g])
   
   print(paste0("Processing ", gsp_id))
   subdf <- net_process(file = paste0(network_fp,
                                      "/",
                                      extract_list[g]),
                        gsp_id = gsp_ids[g]
   )
   
   gsa_df <- dplyr::bind_rows(gsa_df, subdf)
   
}

gsa_df <- tibble(gsa_df)

df <- gsa_df %>% 
   group_by(gsp_id) %>%
   summarize(in_net = sum(in_net),
             exp_gsas = mean(exp_gsas)) %>% 
   mutate(diff = in_net - exp_gsas) %>% 
   filter(diff != 0)

print(df, n=100)


x <- '100'
print(gsa_df %>% filter(gsp_id == x), n=400)
file <- paste0(network_fp, "/", extract_list[which(gsp_ids == x)])
nl <- tibble(readRDS(file)$nodelist)
print(nl, n=50)
