
if('r-spatial/s2' %in% installed.packages()[,'Package']){
   remotes::install_github("r-spatial/s2") # requires openssl@1.1, can be installed via brew 
}

library(s2)
albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

packs <- c('sf','tigris','tidyverse',
           'data.table')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

sf_use_s2(TRUE)

fl = 'data/spatial_raw_large_files/SVI_shapefiles_CA_censustracts/SVI2018_CALIFORNIA_tract.shp'
census = st_read(fl)
fl = 'data/spatial_raw_large_files/GSP_submitted/SubmittedGSP_Master.shp'
gsp = st_read(fl)


###### VOTE DATA -- 2016 HOUSE ELECTIONS BY PRECINCT --- #####
#### OVERLAID ON GSP BOUNDARIES ####
prec <- fread('data/raw_large_files/dataverse_files/2016-precinct-house.tab')
prec <- prec[state=='California',]
prec$cfips <- formatC(prec$county_fips,width = 5,flag = '0')
prec$prec_key <- paste0(prec$cfips,prec$precinct)
votes_by_precinct <- prec[,sum(votes),by=.(prec_key,year,stage,party)]
votes_by_precinct[,total_votes:=sum(V1),by=.(prec_key,year,stage)]
votes_by_precinct[,vote_share:=V1/total_votes]
rep_vote_share_2016 <- votes_by_precinct[party=='republican',]
prec2016 <- st_read('data/spatial_raw_large_files/srprec_state_g16_v01_shp/srprec_state_g16_v01.shp')

prec2016$total_votes <- votes_by_precinct$total_votes[match(prec2016$SRPREC_KEY,votes_by_precinct$prec_key)]
prec2016$rep_vote_share <- votes_by_precinct$vote_share[match(prec2016$SRPREC_KEY,votes_by_precinct$prec_key)]
prec2016$rep_votes <- votes_by_precinct$V1[match(prec2016$SRPREC_KEY,votes_by_precinct$prec_key)]


prec2016 <- st_transform(prec2016,albersNA)
gsp <- st_transform(gsp, albersNA)
library(lwgeom)
prec2016 <- st_make_valid(prec2016)
gsp <- st_make_valid(gsp)

prec2016$total_votes[is.na(prec2016$total_votes)]<-0
prec2016$rep_votes[is.na(prec2016$rep_votes)]<-0
inters = st_intersection(st_buffer(gsp,0),prec2016)
inters$precinct_total_area <- st_area(prec2016)[match(inters$SRPREC_KEY,prec2016$SRPREC_KEY)]
inters$intersect_area <- st_area(inters)
inters$prop = inters$intersect_area/inters$precinct_total_area 
rep_vote_share_by_gsp = inters %>% 
   mutate(rep_votes_weighted = rep_votes * prop,
          tot_votes_weighted = total_votes * prop) %>%
   group_by(GSP.ID) %>%
   summarise(rep_votes = sum(rep_votes_weighted),tot_votes = sum(tot_votes_weighted)) %>%
   mutate(rep_vote_share = rep_votes/tot_votes)

gsp$Republican_Vote_Share <- rep_vote_share_by_gsp$rep_vote_share[match(gsp$GSP.ID,rep_vote_share_by_gsp$GSP.ID)]


gdp <- fread('data/raw_large_files/bea_county_gdp.csv',skip = 3)
gdp <- gdp[-c(which(gdp$GeoFips==''):nrow(gdp)),]
gdp$cfips <- paste0('0',as.character(gdp$GeoFips))
gdmelt <- melt(gdp,id.vars = c('cfips','Description'),variable.name = 'year',value.name = 'gdp',measure.vars = patterns('[0-9]{4}'))
gdmelt$gdp <- as.numeric(gdmelt$gdp)
gdmelt <- dcast(gdmelt,cfips + year ~ Description,value.var='gdp')
setnames(gdmelt,colnames(gdmelt)[3:4],c('Agr_GDP','Total_GDP'))
#gdmelt$Prop_Agr_GDP <- gdmelt$Agr_GDP/gdmelt$Total_GDP
gdmelt <- gdmelt[order(cfips,year),Agr_GDP_FFill:=nafill(Agr_GDP,'locf'),by=.(cfips)]
gdmelt <- gdmelt[order(cfips,year),Total_GDP_FFill:=nafill(Total_GDP,'locf'),by=.(cfips)]
gdmelt <- gdmelt[year==2016,]
counties <- tigris::counties(state = 'CA',class = 'sf',cb = T)
counties <- st_transform(counties,albersNA)
g_c_inter <- st_intersection(gsp,counties)
g_c_inter$county_area <- st_area(counties)[match(g_c_inter$GEOID,counties$GEOID)]
g_c_inter$prop_of_county <- st_area(g_c_inter)/g_c_inter$county_area

setnames(gdmelt,'cfips','CFIPS')
setnames(g_c_inter,'GEOID','CFIPS')

g_c_inter <- left_join(g_c_inter,gdmelt)
g_c_inter$county_ag_share <- g_c_inter$Agr_GDP_FFill * g_c_inter$prop_of_county
g_c_inter$county_total_share <- g_c_inter$Total_GDP_FFill * g_c_inter$prop_of_county
gsp_ag_gdp <- g_c_inter %>% group_by(GSP.ID) %>% summarise(
   gsp.ag = sum(county_ag_share,na.rm = T),
   gsp.gdp = sum(county_total_share,na.rm = T),
   gsp.agr.gdp.share = gsp.ag/gsp.gdp)

gsp$Agr_Share_Of_GDP <- gsp_ag_gdp$gsp.agr.gdp.share[match(gsp$GSP.ID,gsp_ag_gdp$GSP.ID)]

ed <- fread('data/raw_large_files/ACSST5Y2016.S1501_2023-07-19T133305/ACSST5Y2016.S1501-Data.csv',skip = 1)
ed <- ed[,.(Geography,`Geographic Area Name`,`Percent!!Estimate!!Population 25 years and over!!Bachelor's degree`)]
msa <- tigris::core_based_statistical_areas(cb = T,class = 'sf',year = 2016)
msa <- st_transform(msa,albersNA)
msa_inter <- st_intersects(msa,gsp)
msa_ca <- msa[sapply(msa_inter,length)>0,]
msa_ca$Perc_Bach_Degree_Over25 <- ed$`Percent!!Estimate!!Population 25 years and over!!Bachelor's degree`[match(msa_ca$AFFGEOID,ed$Geography)]
gsp$Perc_Bach_Degree_Over25 <- msa_ca$Perc_Bach_Degree_Over25[st_nearest_feature(gsp,msa_ca)]

library(readxl)
sheet_file <- 'data/raw_large_files/Govt_Units_2017_Final.xlsx'
keep_sheets <- which(readxl::excel_sheets(sheet_file) %in% c('General Purpose','Special District'))

sheet_list <- lapply(keep_sheets,function(x) read_excel(sheet_file,sheet = x))
sheet_dt <- rbindlist(sheet_list,use.names=T,fill = T)
sheet_dt <- sheet_dt[sheet_dt$STATE_AB=='CA',]
sheet_dt$CFIPS <- paste0(sheet_dt$FIPS_STATE,sheet_dt$FIPS_COUNTY)
gov_count <- sheet_dt[,.N,by=.(CFIPS)]
g_c_inter$govs_in_county <- gov_count$N[match(g_c_inter$CFIPS,gov_count$CFIPS)]

res = fread('data/raw_large_files/PEPPOP2019.PEPANNRES-2023-07-19T210103.csv')
pops2017 = res[,.(`Geographic Area Name (Grouping)`,`7/1/2017 population estimate!!Population`)]
setnames(pops2017,names(pops2017),c('County','Pop_Estimate'))
pops2017$CFIPS <- paste0('06',counties$COUNTYFP[match(str_remove(str_remove(pops2017$County,'\\,.*'),' County'),counties$NAME)])
g_c_inter$county_population <- str_remove_all(pops2017$Pop_Estimate[match(g_c_inter$CFIPS,pops2017$CFIPS)],'\\,')
g_c_inter$weighted_county_pop <- as.numeric(g_c_inter$county_population) * g_c_inter$prop_of_county
g_c_inter$weighted_county_pop <- as.numeric(g_c_inter$county_population) * g_c_inter$prop_of_county

govs_per_10k_people <- g_c_inter %>% group_by(GSP.ID) %>% 
   summarize(pop = sum(as.numeric(county_population)),govs = sum(govs_in_county)) %>%
   mutate(govs_per_10k_people = govs/{pop/10e3})
gsp$local_govs_per_10k_people <- govs_per_10k_people$govs_per_10k_people[match(gsp$GSP.ID,govs_per_10k_people$GSP.ID)]

### object that now has four gsp-level context predictors
## Republican_Vote_Share, Agr_Share_Of_GDP, Perc_Bach_Degree_Over25, local_govs_per_10k_people
gsp
gsp_local <- gsp[,c("GSP.ID","Republican_Vote_Share","Agr_Share_Of_GDP","Perc_Bach_Degree_Over25","local_govs_per_10k_people", "geometry")]
