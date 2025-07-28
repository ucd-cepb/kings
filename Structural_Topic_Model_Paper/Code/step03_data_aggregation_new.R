dac_corr_check=F #Toggle this to run dac corr tests

packs <- c('stm','tm','SnowballC','tidytext','data.table',
           'tidyverse','sf','pbapply','geometry','Rtsne','rsvd',
           'stringi','stringr','scico','boxr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

source(filekey[filekey$var_name=="create_lang_meta_function",]$filepath)
source(filekey[filekey$var_name=="create_spat_meta_function",]$filepath)
source(filekey[filekey$var_name=="dac_svi_analysis_function",]$filepath)
source(filekey[filekey$var_name=="local_predictors_script",]$filepath)

if(dac_corr_check==T){
   #correlation check to determine which spatial metadata to use
   results_tract <- dac_svi_analysis("tract")
   results_place_pop <- dac_svi_analysis("place","pop")
   results_place_area <- dac_svi_analysis("place","area")
}

#Create Meta####
#gsp_text_with_lang <- create_lang_meta(run_repair = F)

#retrieves the latest save of gsp_text_with_lang
#generated in create_lang_meta, which allows create_lang_meta() to be skipped

gsp_text_with_lang <- readRDS(filekey[filekey$var_name=="gsp_docs_lang",]$filepath)
#as of May 29, 2024, the current version of gsp_text_with_lang used was constructed using 
#the webdata generated using the file Plan_Evolution_Papers/Code/sgma_plan_evo_webscraper.R
#it clarifies the "approval" column names using the date that data was webscraped.
#It also includes "version_approval" and "latest_approval" from a later webscraping date.
#For this project, since it uses the original documents, we care about "version_approval"
#of the first version of plans, rather than "latest_approval", which denotes the approval status
#of the most recently published plan version.

type = "pop"
#or type = "area"
scope = "blockgroup"
gsp_dac <- create_dac_meta(type, scope, overwrite = T)
#rows = num docs; cols = metadata types
gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))

#filtering NA admin = proxy for GSPs whose texts have not been processed
gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))

#gsp_text_with_meta_mini <- unique(gsp_text_with_meta[,c(7,14,16,19:26)])
gsp_text_with_meta$GSP.ID <- as.numeric(gsp_text_with_meta$gsp_id)
gsp_local$GSP.ID <- as.numeric(gsp_local$GSP.ID)
saveRDS(gsp_local, filekey[filekey$var_name=="gsplocal",]$filepath)
library(raster)
library(rgdal)
maxdryspell2040 <- raster::raster(filekey[filekey$var_name=="max_dry_spell_2040_data",]$filepath)
plot(maxdryspell2040)
gsp_local <- st_transform(gsp_local, raster::projection(maxdryspell2040))
gsp_local$max2040dryspell <- as.numeric(raster::extract(maxdryspell2040$cddm_year_ens32avg_rcp45_2040, 
                                         gsp_local, fun=mean, na.rm=T, sp=F))

max2012dryspell <- raster::raster(filekey[filekey$var_name=="max_dry_spell_2012_data",]$filepath)
plot(max2012dryspell)
gsp_local <- st_transform(gsp_local, raster::projection(max2012dryspell))
gsp_local$max2012dryspell <- as.numeric(raster::extract(max2012dryspell$cddm_year_ens32avg_rcp45_2012, 
                                                    gsp_local, fun=mean, na.rm=T, sp=F))

#DSCI weekly, 
#TODO could use geometric mean
huc8s <- sf::read_sf(filekey[filekey$var_name=="huc8s",]$filepath)
huc8s <- st_transform(huc8s, raster::projection(max2012dryspell))

drought <- read_csv(filekey[filekey$var_name=="dsci_2012_through_2016",]$filepath)
drought <- drought |> group_by(HUCId) |> summarize(meanD0 = mean(D0), meanD1 = mean(D1),
                                                   meanD2 = mean(D2), meanD3 = mean(D3), meanD4 = mean(D4),
                                                   dsci = mean(D0)-mean(D1) + 2*mean(D1)-2*mean(D2) + 3*mean(D2)-3*mean(D3) + 
                                                      4*mean(D3)-4*mean(D4) + 5*mean(D4))#converts from cumulative to non-cumulative, then applies formula
drought$HUCId <- as.character((drought$HUCId))
huc8s <- left_join(huc8s, drought, by = c("HUC8" = "HUCId"))

library(fasterize)
huc8sraster <- fasterize(huc8s, max2012dryspell, field = "dsci", fun = "max")
plot(huc8sraster)

gsp_local$DSCI <- as.numeric(raster::extract(huc8sraster$layer, 
                                          gsp_local, fun=mean, na.rm=T, sp=F))
plot(st_geometry(gsp_local), col = gsp_local$DSCI)

drywells <- read_csv(filekey[filekey$var_name=="drywells",]$filepath)
drywells <- drywells[!is.na(drywells$LONGITUDE) & !is.na(drywells$LATITUDE),]
drywells <- st_as_sf(drywells, coords = c("LONGITUDE","LATITUDE"),crs="EPSG:4326")
drywells$year <- substr(drywells$`CREATE DATE`, nchar(drywells$`CREATE DATE`)-4+1, nchar(drywells$`CREATE DATE`))
drywells <- drywells[drywells$year%in%c("2014","2015","2016","2017","2018","2019","2020"),]
drywells <- st_transform(drywells, raster::projection(max2012dryspell))




sf_use_s2(F)
intersects <- st_intersects(gsp_local, drywells)
#Warning: although coordinates are
#longitude/latitude, st_intersects
#assumes that they are planar
gsp_local$drywellcount2014_2020 <- lengths(intersects)
wellplot <- gsp_local
wellplot$DSCI <- NULL
wellplot$GSP.ID <- NULL
indices <- table(wellplot$drywellcount2014_2020)
grays <- rev(gray(seq(0,1,0.036)))
colors <- grays[sapply(seq_along(wellplot$drywellcount2014_2020), function(i)
                     which(as.numeric(names(indices))==wellplot$drywellcount2014_2020[i]))]
plot(max2012dryspell)
plot(wellplot, col = colors, add = T)
plot(st_geometry(drywells), pch = 20, cex=0.2, col="purple", add=T)

#CRS check
#plot(max2012dryspell)
#plot(st_geometry(huc8s), col = huc8s$OBJECTID, add = T)
#projection(huc8s)
#projection(drywells)
#projection(gsp_local)
#projection(max2012dryspell)
#projection(maxdryspell2040)

gsp_local_nogeom <- st_drop_geometry(gsp_local)

gsp_text_with_meta <- left_join(gsp_text_with_meta, gsp_local_nogeom)

mini515 <- final_515_table[,c("basin_id","well_MCL_exceedance_count")]
gsp_text_with_meta <- left_join(gsp_text_with_meta, mini515)

gsp_text_with_meta$urbangw_af_log_scaled <- scale(log(gsp_text_with_meta$urbangw_af))
gsp_text_with_meta$percent_dac_by_pop_scaled <- scale(gsp_text_with_meta$percent_dac_by_pop)
gsp_text_with_meta$fract_of_area_in_habitat_log_scaled <- scale(log(gsp_text_with_meta$fract_of_area_in_habitat))
gsp_text_with_meta$maxdryspell_scaled <- scale(gsp_text_with_meta$max2040dryspell)
gsp_text_with_meta$Agr_Share_Of_GDP_scaled <- scale(gsp_text_with_meta$Agr_Share_Of_GDP)
gsp_text_with_meta$Republican_Vote_Share_scaled <- scale(gsp_text_with_meta$Republican_Vote_Share)
gsp_text_with_meta$Perc_Bach_Degree_Over25_scaled <- scale(gsp_text_with_meta$Perc_Bach_Degree_Over25)
gsp_text_with_meta$local_govs_per_10k_people_log_scaled <- scale(log(gsp_text_with_meta$local_govs_per_10k_people))
gsp_text_with_meta$log_well_MCL_exceedance_count_by_log_pop_scaled <- scale(log(gsp_text_with_meta$well_MCL_exceedance_count+0.1) / log(gsp_text_with_meta$basin_population))
gsp_text_with_meta$log_drywell_per_log_person_scaled <- scale(log(gsp_text_with_meta$drywellcount2014_2020+0.1) / log(gsp_text_with_meta$basin_population))
gsp_text_with_meta$basin_population_log_scaled <- scale(log(gsp_text_with_meta$basin_population))
gsp_text_with_meta$well_MCL_exceedance_count_by_log_pop_scaled <- scale(gsp_text_with_meta$well_MCL_exceedance_count/
                                                                           log(gsp_text_with_meta$basin_population))
gsp_text_with_meta$dsci_scaled <- scale(gsp_text_with_meta$DSCI)

#plan 0008 started off its first version with two collaborating GSAs. The website info scraped 
#was updated after the removal of Fresno County GSA and after the region's amended plan publication,
#but since our research uses the first version of the pdf, we are assuming mult_gsas = T
#at the time of the plan creation
gsp_text_with_meta$mult_gsas <- ifelse(gsp_text_with_meta$gsp_id == "0008", T, gsp_text_with_meta$mult_gsas)
#on may 28, 2024 we spliced two versions of this file based on different webscraping dates,
#naming two versions of the name_gsas columns with the date of webscraping
#we do not use this variable in analysis; this is simply for record-keeping
#these are as follows:
#gsp_text_with_meta$name_gsas20230922 
#gsp_text_with_meta$name_gsas20230731 

gsp_text_with_meta$priority_category <- ifelse(gsp_text_with_meta$priority %in% c("Very Low", "Low"), "low_or_verylow",
                                               ifelse(gsp_text_with_meta$priority == "Medium", "med", 
                                                      ifelse(gsp_text_with_meta$priority == "High", "high", NA)))


saveRDS(gsp_text_with_meta, file = filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)
gsp_text_with_meta <- readRDS(file = filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)

gsp_text_with_meta$version_approval20230922 <- gsp_text_with_meta$version_approval

gspmini <- gsp_text_with_meta[!duplicated(gsp_text_with_meta$GSP.ID),]
gspmini <- select(gspmini, -c("text","is_comment","is_reference",
                                "page_num","admin","basin_plan","sust_criteria",
                                "monitoring_networks","projects_mgmt_actions"))
write_csv(gspmini, filekey[filekey$var_name=="gsp_planwise_metadata_csv",]$filepath)
saveRDS(gspmini, filekey[filekey$var_name=="gsp_planwise_metadata_rds",]$filepath)

gsp_text_lean <- gsp_text_with_meta[,c("text","gsp_id","is_comment","is_reference","page_num",
                                                 "admin","basin_plan","sust_criteria",
                                                 "monitoring_networks",
                                                 "projects_mgmt_actions",
                                                 "log_well_MCL_exceedance_count_by_log_pop_scaled",
                                                "basin_population_log_scaled",
                                                 "percent_dac_by_pop_scaled",
                                                "well_MCL_exceedance_count_by_log_pop_scaled",
                                                 "fract_of_area_in_habitat_log_scaled",
                                                 "dsci_scaled",
                                                 "Agr_Share_Of_GDP_scaled",
                                                 "Republican_Vote_Share_scaled",
                                                 "mult_gsas",
                                                 "priority_category",
                                                 "version_approval20230922",
                                                 "basin_population_log_scaled")]
gspmini$priority <- factor(gspmini$priority, ordered = T, levels = c("Very Low", "Low", "Medium", "High"))
gspmini$prioritynum <- ifelse(gspmini$priority %in% c("Very Low", "Low"), 0,
                              ifelse(gspmini$priority == "Medium", 1, 
                                ifelse(gspmini$priority == "High", 2, NA)))
   gspmini$drywellperperson <- gspmini$drywellcount2014_2020 / gspmini$basin_population
library(ggcorrplot)
ggcorrplot(cor(gspmini[,c(
   "prioritynum","gwsum","DSCI","max2040dryspell",
   "max2012dryspell",
   "drywellcount2014_2020",
   "drywellperperson",
   "well_MCL_exceedance_count","urbangw_af")]
   ), lab = T)
saveRDS(gsp_text_lean, file = filekey[filekey$var_name=="gsp_docs_lean",]$filepath)
gsp_text_lean <- readRDS(file = filekey[filekey$var_name=="gsp_docs_lean",]$filepath)

table(gspmini$version_approval20230922)
