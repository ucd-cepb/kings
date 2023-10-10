summarize_pws_by_gsp <- function(){
   library(sf)
   library(sp)
   library(stringr)
   library(tidyverse)
   library(data.table)
   library(lubridate)
   
   pws <- st_read('data_temp/SABL_Public_220819')
   td <- tempdir()
   tf <- tempfile(tmpdir = td)
   fpath = file.path("data_spatial_raw/GSP_submitted/SubmittedGSP_Master.shp")
   gsp <- st_read(fpath)
   gsp <- st_transform(gsp,st_crs(pws))
   pws_in_gsp <- st_intersects(pws,gsp)
   pws <- pws[sapply(pws_in_gsp,length)>0,]
   pws <- st_union(pws,by_feature = T,is_coverage = T)
   pws <- pws[pws$FEDERAL_CL %in% c('COMMMUNITY','NON-TRANSIENT NON-COMMUNITY'),]
   
   tf2 <- tempfile(tmpdir = td)
   download.file('https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip',destfile = tf2)
   unzip(zipfile = tf2,exdir = td)
   
   syssum <- fread(grep('SDWA_PUB_WATER.*.csv',list.files(td,full.names = T),value = T))
   syssum <- syssum[PWSID %in% pws$SABL_PWSID,]
   pws$PRIMARY_SOURCE <- syssum$PRIMARY_SOURCE_CODE[match(pws$SABL_PWSID,syssum$PWSID)]
   
   viols <- fread(grep('SDWA_VIOLATIONS_ENFORCEMENT.csv',list.files(td,full.names = T),value = T))
   viols <- viols[PWSID %in% pws$SABL_PWSID,]
   viols <- viols[mdy(viols$COMPL_PER_BEGIN_DATE)>=mdy('01/01/2014')&mdy(viols$COMPL_PER_BEGIN_DATE)<mdy('01/01/2019'),]
   viol_dt <- viols[,.N,by=.(PWSID,IS_HEALTH_BASED_IND)] %>% dcast(.,PWSID~IS_HEALTH_BASED_IND,fill = 0)
   names(viol_dt) <- c("PWSID",'NONHEALTH_VIOL_COUNT','HEALTH_VIOL_COUNT')
   
   pws$HEALTH_VIOL_COUNT_2014_2018 <- viol_dt$HEALTH_VIOL_COUNT[match(pws$SABL_PWSID,viol_dt$PWSID)]
   pws$NONHEALTH_VIOL_COUNT_2014_2018 <- viol_dt$NONHEALTH_VIOL_COUNT[match(pws$SABL_PWSID,viol_dt$PWSID)]
   pws$HEALTH_VIOL_COUNT_2014_2018 <- 
      replace(pws$HEALTH_VIOL_COUNT_2014_2018,is.na(pws$HEALTH_VIOL_COUNT_2014_2018),0)
   pws$NONHEALTH_VIOL_COUNT_2014_2018 <- 
      replace(pws$NONHEALTH_VIOL_COUNT_2014_2018,is.na(pws$NONHEALTH_VIOL_COUNT_2014_2018),0)
   pws <- pws[order(pws$BOUNDARY_T,decreasing = T),]
   pws <- pws[!duplicated(pws$SABL_PWSID),]
   
   pws <- st_simplify(pws)
   gsp <- st_simplify(gsp)
   #intersection adopts same structure as gsp
   gsp_inters_pws <- st_intersection(gsp,pws)
   gsp_inters_pws$area <- st_area(gsp_inters_pws)
   pws$area <- st_area(pws)
   
   ### some places (e.g., livermore) have jurisidictional boundary and
   # service boundary as separate rows. this keeps service boundary if available
   
   gsp_inters_pws$prop_of_pws <- round(as.numeric(gsp_inters_pws$area/pws$area[match(gsp_inters_pws$SABL_PWSID,pws$SABL_PWSID)]),2)
   gsp_inters_pws$service_count_prop <- gsp_inters_pws$SERVICE_CO*gsp_inters_pws$prop_of_pws
   gsp_inters_pws <- filter(gsp_inters_pws,prop_of_pws>0.00)
   gsp_inters_pws$PRIMARY_SOURCE <- ifelse(grepl('^G',gsp_inters_pws$PRIMARY_SOURCE),'GW','SW')
   #### this creates a weighted count of violations and of people served by gsp
   #service_co = number of service connections
   #prop_of_pws = area-weighted proportion of pws in gsp area
   #service_count = area-weighted num of service connections in gsp area
   #prop_service_gw_source = area-weighted proportion of gw-sourced service 
   #connections in gsp area compared to all service connections
   #hviol_avg_res = number of health violations experienced, on average,
   #by water system-using residents of this GSP
   pws_meta <- gsp_inters_pws %>% group_by(GSP.ID) %>% 
      summarise(
         pws_count = n(),
         tot_pop = sum(POPULATION, na.rm = T),
         service_count = sum(service_count_prop,na.rm = t),
         service_count_gw_source = sum(service_count_prop*{PRIMARY_SOURCE=='GW'}),
         mean_HEALTH_VIOL_COUNT = mean(HEALTH_VIOL_COUNT_2014_2018,na.rm=T),
         hviol_avg_res = sum(POPULATION*prop_of_pws*HEALTH_VIOL_COUNT_2014_2018,na.rm=T)/
            sum(POPULATION*prop_of_pws,na.rm=T)) %>%
      mutate(prop_service_gw_source = ifelse(service_count==0,0,service_count_gw_source/service_count)) %>% 
      mutate(gsp_id = paste0(strrep("0",4-nchar(GSP.ID)),GSP.ID))
   
   
   #nan prop_service_gw_source defaults to 0
}

