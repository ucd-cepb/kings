existingaffils <- boards_plus_participants_plus_ucd %>% filter(
   !is.na(Affiliation) & UCD_tracked == 1
)

existingaffils$`Date Collected` <- NULL
affils <- read_xlsx("Structural_Topic_Model_Paper/MissingAffils_completed.xlsx")

#we don't need date collected and the formatting is kind of messed up
affils$`Date Collected` <- NULL
boards_merge <- full_join(existingaffils, affils)

singlegsa <- read.csv("Structural_Topic_Model_Paper/Single_or_Multiple_GSA_edited.csv")
singlegsa <- singlegsa |> select(Name.of.GSA, Single.or.Multiple., URL.Extension)
singlegsa <- singlegsa |> rename(`Name of GSA` = Name.of.GSA)

singlegsa$`SingleGSA` <- case_when(
   singlegsa$`Single.or.Multiple.` == "Multiple" ~ "No",
   singlegsa$`Single.or.Multiple.` == "Single" ~ "Yes",
   .default = NA
)

singlegsa <- singlegsa |>  distinct(`Name of GSA`, .keep_all = T)

#comparison check
#comp <- left_join(singlegsa, boards_merge)
#comp <- comp |> distinct(`Name of GSA`, .keep_all = T) |> filter(`SingleGSA` != `Single Agency GSA`)

boards_merge <- full_join(
   singlegsa, boards_merge)

boards_merge$`Single Agency GSA` <- NULL

boards_merge$Affil_Category <- case_when(
   #get rid of this category if want to use finer grain water categories below
   str_detect(boards_merge$Affiliation, 
              "Water and Sewer District|Municipal Utility District|Public Utiliti*es District|Public Utility District|Community Services* District|Drainage District|Stormwater District|Community Water System|Small Water System|Water Authority|Water Authority|Water Company|Water Service Company|Commercial Water Stakeholder|Canal Company|(Reclamation|Water Conservation|Groundwater Management|Water Conservation Improvement|Resource Conservation) District|Water Agency|Water Management Agency|Water Storage District|Irrigation District|Water District|Water Management District") ~ "Water_or_Irrigation",
   
   str_detect(boards_merge$Affiliation, 
              "Water and Sewer District|Municipal Utility District|Public Utiliti*es District|Public Utility District|Community Services* District|Drainage District|Stormwater District|Community Water System|Small Water System") ~ "ServicesDistrict",
   str_detect(boards_merge$Affiliation, "Water Authority|Water Company|Water Service Company|Commercial Water Stakeholder|Canal Company") ~ "WaterCompany",
   str_detect(boards_merge$Affiliation,
              "(Reclamation|Water Conservation|Groundwater Management|Water Conservation Improvement|Resource Conservation) District") ~ "ReclamationOrConservationDistrict",
   str_detect(boards_merge$Affiliation,
              "City|County") ~ "LocalGov",
   str_detect(boards_merge$Affiliation,
              "Tribal|Tribe|Wintun Nation") ~ "Tribal",
   str_detect(boards_merge$Affiliation,
              "Agricultural|Water Agency|Water Management Agency|Water Storage District|Irrigation District|Water District|Water Management District") ~ "Agricultural",
   str_detect(boards_merge$Affiliation,
              "Environmental Stakeholder|Land Conservation Stakeholder") ~ "Environmental",
   str_detect(boards_merge$Affiliation,
              "Navy|Bureau of Land Management|Public Agency|University") ~ "PublicAgency",
   str_detect(boards_merge$Affiliation,
              "Private Pumper|Domestic Well|Landowner") ~ "PrivatePumperLandowner",
   .default = "Other"
)

boards_merge <- boards_merge %>% 
   group_by(GSP_ID) %>% mutate(board_prop_from_localgov = 
                                  sum(Affil_Category == "LocalGov"#,na.rm=T 
                                  ) / n())

boards_merge <- boards_merge %>% 
   group_by(GSP_ID) %>% mutate(board_prop_from_waterirrigation = 
                                  sum(Affil_Category == "Water_or_Irrigation"#,na.rm=T 
                                  ) / n())

cws <- read_excel("Structural_Topic_Model_Paper/Water Systems.xlsx")
cwsnames <- tolower(paste(cws$`Water System Name`, collapse = "|"))
cwsnames <- str_remove_all(cwsnames, "\\(.*\\)")
cwsnames <- str_replace_all(cwsnames, "\\.", "\\\\\\.")
cwsnames <- str_replace_all(cwsnames, "\\&", "\\\\\\&")
cwsnames <- str_replace_all(cwsnames, "\\:", "\\\\\\:")
cwsnames <- str_replace_all(cwsnames, "\\s\\s+", "\\s")

boards_merge$cws_match <- str_detect(tolower(
   boards_merge$Affiliation), cwsnames
)

#compare water system names to affiliation names
#for(i in 1:1497){
#   if(unique(sort(
#      boards_plus_participants_plus_ucd$Affiliation)) < sort(cws$`Water System Name`)){
#      boards_plus_participants_plus_ucd$Affiliation[
#         i:nrow(boards_plus_participants_plus_ucd)]
#   }
#}

#viewtogether <- cbind(append(unique(sort(
#   boards_plus_participants_plus_ucd$Affiliation)), rep(NA, 1497-472)),
#   sort(cws$`Water System Name`[str_detect(cws$`Water System Name`, "WATER")]))

#if any multi-gsas, count it as having exante_collab
boards_merge <- boards_merge %>% 
   group_by(GSP_ID) %>% mutate(exante_collab = 
                                  any(SingleGSA == "No", na.rm = T))

boardsummary <- boards_merge %>% 
   group_by(GSP_ID) %>% arrange(mult_gsas) %>% 
   distinct(GSP_ID, .keep_all=TRUE) %>% filter(!is.na(GSP_ID))

#-0.916 = 
#extremely high correlation. need to only pick one
cor.test(boardsummary$board_prop_from_localgov, 
         boardsummary$board_prop_from_waterirrigation)

table(boardsummary$mult_gsas, boardsummary$exante_collab)

agenciesneedingdata <- boards_merge %>% filter(!is.na(GSP_ID) & 
                     !is.na(`Name of GSA`) &
                      is.na(SingleGSA))

boardsummary$gsp_id <- boardsummary$GSP_ID
boardsummary$GSP_ID <- NULL
boardsummary <- boardsummary %>% select(gsp_id, exante_collab, board_prop_from_localgov)
