packs <- c('tidyverse','qdapDictionaries','stringi')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

generate_proper_names <- function(underscore = F){
   
   #water agency downloads page:
   #https://www.watereducation.org/water-related-organizationsagencies
   
   #gazetter downloads main page:
   #https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html
   
   #national counties gazetter file downloaded from
   #https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_counties_national.zip
   
   #national places gazetter file downloaded from
   #https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_place_national.zip
   
   #filter out state names from org list
   agency_names <- as_tibble(read_csv("data_raw/agency_list.csv")) %>% 
      filter(agencytypes != "Western States Water Agencies and Districts")
   
   agencies <- agency_names$agencies
   #removes u.s. from beginning of agency names
   agencies <- gsub("^[U|u]\\.*[S|s]\\.*\\s+","",agencies)
   #include alternate spelling of UC
   agencies <- append(agencies,
                      gsub("U.C.","UC",agencies[grep("U\\.C\\.",agencies)]))
   #include agriculture as full word
   agencies <- append(agencies,
                      gsub(" Ag "," Agriculture ",agencies[grep(" Ag ",agencies)]))
   #remove dashes with spaces
   agencies <- str_replace(agencies, " \\p{Pd} "," ")
   #remove commas. apostrophes stay, to match the corpus cleaning process. 
   agencies <- sub(",","",agencies)
   #remove ampersands, since they are removed by this point
   #in the cleaning process
   agencies <- str_squish(gsub("&","",agencies))
   #remove dashes between words
   agencies <- str_replace(agencies, "\\s+\\p{Pd}\\s+"," ")
      
   place_names <- as_tibble(read.table(
      "data_raw/2021_Gaz_place_national.txt",
      sep="\t", quote = "", header=TRUE)) %>% filter(USPS == "CA") %>% 
      mutate(NAME = tolower(NAME)) 
   
   #removing last word ("city" or "cdp")
   places <- gsub("\\s+\\w*$", "", x = place_names$NAME)
   
   common_names <- c("airport", "alpine", "bend", "cherokee", "commerce",
                     "cottonwood", "crest", "home garden",
                     "lakeside", "live oak", "nice", "oceanside",
                     "orange", "pike", "ponderosa", "riverbank", "strawberry",
                     "vineyard", "volcano", "walnut", "winters")
   patt <- paste0("^", common_names, "$", collapse="|")
   
   places <- places[!grepl(patt, places)]
   
   cnty_names <- as_tibble(read.table("data_raw/2021_Gaz_counties_national.txt",
      sep="\t", quote = "", header = TRUE)) %>% filter(USPS == "CA") %>% 
      mutate(NAME = tolower(NAME))
   
   #keeps two versions, one with the word County in the name and one without
   ctemp <- ifelse(cnty_names$NAME == "orange county" | 
                              cnty_names$NAME == "lake county",
                              cnty_names$NAME,
                              str_squish(str_remove(cnty_names$NAME, pattern = "county")))
   
   counties <- c(cnty_names$NAME, ctemp)
   names <- str_squish(tolower(c(counties, places, agencies)))
   
   #removing periods
   names <- gsub("\\.","",names)
   
   #split at / ( ) or ,
   names <- unlist(strsplit(names, "\\s*[,|/\\(\\)]\\s*"))
   
   #removing empty entries
   names <- unique(stri_remove_empty_na(names))
   
   if(underscore == T){
      names <- gsub("\\s+", "_", x = names)
   }
   
   return(names)
}