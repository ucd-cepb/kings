library(readxl)
library(tidyverse)
library(writexl)
filekey <- read.csv("filekey.csv")

#board member data
boards <- read_excel(paste0(filekey[filekey$var_name=="bruno_folder",]$filepath,
                            "/GSA Boards.xlsx"))
gsp_meta <- readRDS(filekey[filekey$var_name == "gsp_planwise_metadata_rds",]$filepath)
boards$GSP_ID <- NA
boards$GSP_ID <- unlist(lapply(seq_along(boards$`Name of GSA`), function(i){
   mymatches <- suppressWarnings(
      which(str_detect(gsp_meta$name_gsas20230922, boards$`Name of GSA`[i])))
   print(paste0("board gsa name is ", boards$`Name of GSA`[i], "and relevant list of gsas is ", 
                gsp_meta$name_gsas20230922[mymatches], 
                "and relevant GSP name is ", gsp_meta$gsp_id[mymatches]))
   return(ifelse(length(mymatches)>0, gsp_meta$gsp_id[mymatches], NA))
}))

boards$`Name of GSA`[is.na(boards$GSP_ID)]


boards$Affil_Category <- case_when(
   str_detect(boards$Affiliation, 
              "Water and Sewer District|Municipal Utility District|Public Utiliti*es District|Public Utility District|Community Services* District|Drainage District|Stormwater District|Community Water System|Small Water System") ~ "ServicesDistrict",
   str_detect(boards$Affiliation, "Water Authority|Water Company|Water Service Company|Commercial Water Stakeholder|Canal Company") ~ "WaterCompany",
   str_detect(boards$Affiliation,
              "(Reclamation|Water Conservation|Groundwater Management|Water Conservation Improvement|Resource Conservation) District") ~ "ReclamationOrConservationDistrict",
   str_detect(boards$Affiliation,
              "City|County") ~ "LocalGov",
   str_detect(boards$Affiliation,
              "Tribal|Tribe|Wintun Nation") ~ "Tribal",
   str_detect(boards$Affiliation,
              "Agricultural|Water Agency|Water Management Agency|Water Storage District|Irrigation District|Water District|Water Management District") ~ "Agricultural",
   str_detect(boards$Affiliation,
              "Environmental Stakeholder|Land Conservation Stakeholder") ~ "Environmental",
   str_detect(boards$Affiliation,
              "Navy|Bureau of Land Management|Public Agency|University") ~ "PublicAgency",
   str_detect(boards$Affiliation,
              "Private Pumper|Domestic Well|Landowner") ~ "PrivatePumperLandowner",
   .default = "Other"
)
boards$Affiliation[boards$Affil_Category=="Other"]

#boards <- boards %>% select(GSP_ID, `Name of GSA`, `Single Agency GSA`, Affil_Category, Title, Name, Affiliation)
gsp_participants <- read_excel(paste0(filekey[filekey$var_name=="bruno_folder",]$filepath,
                                      "/GSP Participants.xlsx"))
gsp_participants$GSP <- gsp_participants$`Name of GSP`
gsp_participants$`Name of GSP` <- NULL
gsp_participants$Date <- NULL

policies <- read.csv(filekey[filekey$var_name=="bruno_data",]$filepath) 
policies$GSP_ID <- sprintf("%04d", policies$GSP_ID)
policies <- policies %>% select(GSP, GSP_ID)

gsp_participants$GSP <- case_when(
   #gsp_participants$GSP == "" ~ "Mound Basin Groundwater Sustainability Plan",
   #gsp_participants$GSP == "" ~ "Kern County Subbasin Olcese Groundwater Sustainability Plan",
   #gsp_participants$GSP == "" ~ "San Jacinto Groundwater Basin Groundwater Sustainability Plan",
   #gsp_participants$GSP == "" ~ "Santa Margarita Groundwater Agency Groundwater Sustainability Plan",
   gsp_participants$GSP == "East Bay Plan Subbasin Groundwater Sustainability Plan" ~ "East Bay Plain Subbasin Groundwater Sustainability Plan",
   gsp_participants$GSP == "Yolo Subbasin Groundwater Agency 2022 Groundwater Sustainability Plan" ~ "Yolo Subbasin Groundwater Sustainability Plan",
   gsp_participants$GSP == "Butte Subbasin Groundwater Sustainability Plan (GSP)" ~ "Butte Subbasin Groundwater Sustainability Plan",
   gsp_participants$GSP == "Northern American Subbasin Groundwater Sustainability Plan" ~ "North American Subbasin Groundwater Sustainability Plan",
   gsp_participants$GSP == "East Contra Costa Subbasin Groundwater Sustainability Plan" ~ "East Contra Costa Subbasin GSP",
   gsp_participants$GSP == "Groundwater Sustainability Plan Santa Rosa Plan Groundwater Subbasin" ~ "Groundwater Sustainability Plan Santa Rosa Plain Groundwater Subbasin",
   #gsp_participants$GSP == "" ~ "Bear Valley Basin Groundwater Sustainability Plan",
   #gsp_participants$GSP == "" ~ "Riverside-Arlington GSP",
   #gsp_participants$GSP == "" ~ "San Joaquin Pleasant Valley GSP",
   #gsp_participants$GSP == "" ~ "San Gabriel Valley GSP",
   #gsp_participants$GSP == "" ~ "South of Kern River GSP",
   #gsp_participants$GSP == "" ~ "Montecito Groundwater Basin Groundwater Sustainability Plan",
   #gsp_participants$GSP == "" ~ "Arroyo Santa Rosa Valley Groundwater Basin Groundwater Sustainability Plan",
   .default = gsp_participants$GSP
)

gsp_participants <- full_join(policies, gsp_participants)

boards_plus_participants <- full_join(gsp_participants, boards)



#gsp_participants$BrunoNameofGSA <- gsp_participants$`Name of GSA`
#gsp_participants$`Name of GSA` <- NULL

#ucd_version_gsp_participants <- data.frame(DavisNameofGSASept2023 = unlist(gsp_meta$name_gsas20230922),
#                                           GSP_ID = rep(gsp_meta$gsp_id, unlist(lapply(gsp_meta$name_gsas20230922,
#                                                                                    function(i) length(i)))))
#
#ucd_version_gsp_participants$DavisNameofGSASept2023 <- str_remove(
#   ucd_version_gsp_participants$DavisNameofGSASept2023, "\\s*\\(.*\\)$"
#)

#gsp_participants <- full_join(gsp_participants, ucd_version_gsp_participants)
ucd_version_gsp_participants <- data.frame(DavisNameofGSAJuly2023 = unlist(gsp_meta$name_gsas20230731),
                                           GSP_ID = rep(gsp_meta$gsp_id, unlist(lapply(gsp_meta$name_gsas20230731,
                                                                                       function(i) length(i)))),
                                           mult_gsas = rep(gsp_meta$mult_gsas, unlist(lapply(gsp_meta$name_gsas20230731,
                                                                                          function(i) length(i))))
                                           )             

ucd_version_gsp_participants$DavisNameofGSAJuly2023 <- str_remove(
   ucd_version_gsp_participants$DavisNameofGSAJuly2023, "\\s*\\(.*\\)$"
)

ucd_version_gsp_participants$`Name of GSA` <- ucd_version_gsp_participants$DavisNameofGSAJuly2023
ucd_version_gsp_participants$DavisNameofGSAJuly2023 <- NULL
ucd_version_gsp_participants$UCD_tracked <- 1
boards_plus_participants_plus_ucd <- full_join(boards_plus_participants, ucd_version_gsp_participants)
boards_plus_participants_plus_ucd$GSP <- sapply(1:nrow(boards_plus_participants_plus_ucd), 
        function(k) {
             ifelse(is.na(boards_plus_participants_plus_ucd$GSP[k]),
                   boards_plus_participants_plus_ucd$GSP[
                      boards_plus_participants_plus_ucd$GSP_ID == 
                         boards_plus_participants_plus_ucd$GSP_ID[k] &
                         !is.na(boards_plus_participants_plus_ucd$GSP)][1], 
                   boards_plus_participants_plus_ucd$GSP[k])}
)

boards_plus_participants_plus_ucd$GSP <- ifelse(boards_plus_participants_plus_ucd$GSP_ID != "0053",
                                                boards_plus_participants_plus_ucd$GSP,
                                                "Groundwater Sustainability Plan for South Yuba and North Yuba")
singlegsa <- boards_plus_participants_plus_ucd %>% filter(!is.na(UCD_tracked)) %>% 
   distinct(GSP_ID, `Name of GSA`, `Single Agency GSA`, .keep_all = T) %>% 
   select(GSP, GSP_ID, `Name of GSA`)

#write_xlsx(singlegsa, "Structural_Topic_Model_Paper/Single_or_Multiple_GSA.xlsx")

affils <- boards_plus_participants_plus_ucd %>% filter(
   is.na(Affiliation) & UCD_tracked == 1
)

#write_xlsx(affils, "Structural_Topic_Model_Paper/MissingAffils.xlsx")


