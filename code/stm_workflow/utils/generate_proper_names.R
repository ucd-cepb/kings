

generate_proper_names <- function(underscore = F, to_lower=T){
   packs <- c('tidyverse','qdapDictionaries','stringi')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   #water agency downloads page:
   #https://www.watereducation.org/water-related-organizationsagencies
   
   #gazetter downloads main page:
   #https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html
   
   #national counties gazetter file downloaded from
   #https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_counties_national.zip
   
   #national places gazetter file downloaded from
   #https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/2021_Gaz_place_national.zip
   
   #city population data downloaded from  
   #https://www2.census.gov/programs-surveys/popest/tables/2020-2022/cities/totals/SUB-IP-EST2022-POP-06.xlsx
   
   #filter out state names from org list
   agency_names <- as_tibble(read_csv("data/raw_large_files/agency_list.csv")) %>% 
      filter(agencytypes != "Western States Water Agencies and Districts")
   
   agencies_generic <- 
   case_when(
      agency_names$agencytypes == "Environmental Organizations" ~ "an_environmental_org",
      agency_names$agencytypes == "Water Associations and Groups" ~ "a_water_association",
      .default = NA_character_
   )
   agencies <- agency_names$agencies
   
   #adds an important agency not captured properly by website
   if(to_lower==T){
      agencies <- append(agencies,"state water resources control board")
   }else{
      agencies <- append(agencies,"State Water Resources Control Board")
   }
   agencies_generic <- append(agencies_generic, NA)
   
   #removes u.s. from beginning of agency names
   agencies <- gsub("^[U|u]\\.*[S|s]\\.*\\s+","",agencies)
   #include alternate spelling of UC
   ucs <- gsub("U.C.","UC",agencies[grep("U\\.C\\.",agencies)])
   agencies <- append(agencies,
                      ucs)
   agencies_generic <- append(agencies_generic, agencies_generic[grep("U\\.C\\.",agencies)])
   #include agriculture as full word
   ags <- gsub(" Ag "," Agriculture ",agencies[grep(" Ag ",agencies)])
   agencies <- append(agencies,
                      ags)
   agencies_generic <- append(agencies_generic,
                      gsub(" Ag "," Agriculture ",agencies_generic[grep(" Ag ",agencies)]))
   #remove dashes with spaces
   agencies <- str_replace(agencies, " \\p{Pd} "," ")
   #remove commas. apostrophes stay, to match the corpus cleaning process. 
   agencies <- sub(",","",agencies)
   #remove ampersands, since they are removed by this point
   #in the cleaning process
   agencies <- str_squish(gsub("&","",agencies))
   #remove dashes between words
   agencies <- str_replace(agencies, "\\s+\\p{Pd}\\s+"," ")
   
   basins <- read_excel("data_raw/final-515-table.xlsx")
   basins <- c(basins$`Basin Name`, basins$`Subbasin Name`)
   basins <- basins[!is.na(basins)]
   
   basins_generic <- rep("gw_basin",length(basins))
   
   gsas <- readRDS(list.files(path = "data/output_large_files", pattern = "web_repaired", full.names = T)[
      length(list.files(path = "data/output_large_files", pattern = "web_repaired", full.names = T))])
   gsas <- gsas$name_gsas
   #remove parentheses
   gsas <- str_squish(unlist(lapply(gsas, function(b) str_split(b,"\\(")[[1]][1])))
   gsas_generic <- rep("a_gsa",length(gsas))  
    
   calcities <- read_excel("data/raw_large_files/California_Cities_Population_Estimates.xlsx")
   colnames(calcities) <- c("name","apr2020","jul2020","jul2021","jul2022")
   calcities <- calcities[4:(nrow(calcities)-5),]
   calcities$name <- unlist(lapply(calcities$name, function(b) str_split(b, ",")[[1]][1]))
   calcities$type <- ifelse(grepl(" town", calcities$name),"town","city")
   #calcities$name <- str_remove(calcities$name,"\\stown|\\scity")
   calcities$apr2020 <- as.numeric(calcities$apr2020)
   calcities$size <- ifelse(calcities$apr2020<5000,"small_city",
                            ifelse(calcities$apr2020<150000,"midsize_city",
                             "large_city"))
   if(to_lower==T){
      calcities$name <- tolower(calcities$name)
      place_names <- as_tibble(read.table(
         "data/raw_large_files/2021_Gaz_place_national.txt",
         sep="\t", quote = "", header=TRUE)) %>% filter(USPS == "CA") %>% 
         mutate(NAME = tolower(NAME))
   } else{
      place_names <- as_tibble(read.table(
         "data/raw_large_files/2021_Gaz_place_national.txt",
         sep="\t", quote = "", header=TRUE)) %>% filter(USPS == "CA") 
   }
   places_generic <- ifelse(grepl(" cdp", tolower(place_names$NAME)),"a_cdp",
                           calcities$size[match(place_names$NAME,calcities$name)])
   
   #removing last word ("city" or "cdp")
   places <- gsub("\\s+\\w*$", "", x = place_names$NAME)
   
   if(to_lower==T){
      common_names <- c("airport", "alpine", "august","bend", "cherokee", "commerce",
                        "cottonwood", "crest", "home garden",
                        "lakeside", "live oak", "nice", "oceanside",
                        "orange", "pike", "ponderosa", "riverbank", "strawberry",
                        "vineyard", "volcano", "walnut", "winters")
   } else{
      common_names <- c("Airport", "Alpine", "August","Bend", "Cherokee", "Commerce",
                        "Cottonwood", "Crest", "Home Garden",
                        "Lakeside", "Live Oak", "Nice", "Oceanside",
                        "Orange", "Pike", "Ponderosa", "Riverbank", "Strawberry",
                        "Vineyard", "Volcano", "Walnut", "Winters")
   }
   
   patt <- paste0("^", common_names, "$", collapse="|")
   
   places_generic <- places_generic[!grepl(patt, places)]
   places <- places[!grepl(patt, places)]
   
   if(to_lower==T){
      cnty_names <- as_tibble(read.table("data/raw_large_files/2021_Gaz_counties_national.txt",
                                         sep="\t", quote = "", header = TRUE)) %>% filter(USPS == "CA") %>% 
         mutate(NAME = tolower(NAME))
   } else{
      cnty_names <- as_tibble(read.table("data/raw_large_files/2021_Gaz_counties_national.txt",
                                         sep="\t", quote = "", header = TRUE)) %>% filter(USPS == "CA")
   }
   
   
   #keeps two versions, one with the word County in the name and one without, 
   #except for counties that aren't recognizable unless the word "county" is included
   if(to_lower==F){
      ctemp <- ifelse(cnty_names$NAME == "Orange County" | 
                         cnty_names$NAME == "Lake County",
                      cnty_names$NAME,
                      str_squish(str_remove(cnty_names$NAME, pattern = "County")))
      
   } else{
      ctemp <- ifelse(cnty_names$NAME == "orange county" | 
                         cnty_names$NAME == "lake county",
                      cnty_names$NAME,
                      str_squish(str_remove(cnty_names$NAME, pattern = "county")))
   }
   
   
   counties <- c(cnty_names$NAME, ctemp)
   counties_generic <- rep("a_county",length(counties))
   
   #even if we remove places, we don't want to remove agencies
   if(to_lower==T){
      names <- str_squish(tolower(c(counties, places, agencies,basins,gsas, "united states", "california")))
      generic <- c(counties_generic, places_generic, agencies_generic, basins_generic, gsas_generic, "","")
      prnames <- data.frame(names, generic)
         
   } else{
      names <- str_squish(c(counties, places, agencies,basins,gsas, "United States", "California"))
      generic <- c(counties_generic, places_generic, agencies_generic, basins_generic, gsas_generic, "","")
      prnames <- data.frame(names, generic)
   }
   
   #removing periods
   prnames$names <- gsub("\\.","",prnames$names)
   
   #split at ( ) or ,
   splits <- strsplit(prnames$names, "\\s*[,|\\(\\)]\\s*")
   numreps <- sapply(splits, function(x) length(x))
   generic <- prnames$generic
   rm(prnames)
   prnames <- data.frame("names" = unlist(splits), "generic" = rep(generic, numreps))
   
   #split before " gsa"
   splits <- str_split(prnames$names, " gsa ")
   numreps <- sapply(splits, function(x) length(x))
   splits <- lapply(splits, function(x) x[1])
   splits <- sapply(1:length(splits), function(x) ifelse(numreps[x]>1,
                                                         paste0(splits[[x]]," gsa"), NA
   ))
   generic <- prnames$generic
   prnames <- data.frame("names" = c(prnames$names, unlist(splits)), 
                         "generic" = c(prnames$generic, prnames$generic))
   
   #removing remaining punctuation (slashes and non-intra-word hyphens) (don't want to do this before splitting at parentheses)
   prnames$names <- str_replace_all(prnames$names, "/"," ")
   prnames$names <- str_replace_all(prnames$names, " - "," ")
   
   #removing empty and duplicated entries
   prnames <- unique(prnames)
   prnames <- prnames[!is.na(prnames$names),]
   #consolidating ambiguous generic terms
   prnames <- prnames %>% group_by(names) %>% 
      summarize(combogeneric = paste(sort(unique(generic)),collapse="_or_"))
   
   if(underscore == T){
      prnames$names <- gsub("\\s+", "_", x = prnames$names)
   }
   
   return(prnames)
}