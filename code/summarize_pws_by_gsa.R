library(sf)
library(sp)
library(stringr)
library(tidyverse)
library(data.table)
library(lubridate)

pws <- st_read('data_temp/SABL_Public_220819')
td <- tempdir()
tf <- tempfile(tmpdir = td)
download.file('https://sgma.water.ca.gov/portal/service/gsadocument/submittedgsa',destfile = tf)
unzip(zipfile = tf,exdir = td)
gsa <- st_read(grep('GSA.*shp$',list.files(td,full.names = T),value = T))
gsa <- st_transform(gsa,st_crs(pws))
pws_in_gsa <- st_intersects(pws,gsa)
pws <- pws[sapply(pws_in_gsa,length)>0,]
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
pws <- pws[order(pws$BOUNDARY_T,decreasing = T),]
pws <- pws[!duplicated(pws$SABL_PWSID),]

pws <- st_simplify(pws)
gsa <- st_simplify(gsa)
gsa_inters_pws <- st_intersection(gsa,pws)
gsa_inters_pws$area <- st_area(gsa_inters_pws)
pws$area <- st_area(pws)

### some places (e.g., livermore) have jurisidictional boundary and
# service boundary as separate rows. this keeps service boundary if available
gsa_inters_pws$prop_of_pws <- round(as.numeric(gsa_inters_pws$area/pws$area[match(gsa_inters_pws$SABL_PWSID,pws$SABL_PWSID)]),2)
gsa_inters_pws$service_count_prop <- gsa_inters_pws$SERVICE_CO*gsa_inters_pws$prop_of_pws
gsa_inters_pws$HVIOL_PROP <- gsa_inters_pws$HEALTH_VIOL_COUNT_2014_2018*gsa_inters_pws$prop_of_pws
gsa_inters_pws$NHVIOL_PROP <- gsa_inters_pws$NONHEALTH_VIOL_COUNT_2014_2018*gsa_inters_pws$prop_of_pws

gsa_inters_pws$PRIMARY_SOURCE <- ifelse(grepl('^G',gsa_inters_pws$PRIMARY_SOURCE),'GW','SW')
#### this creates a weighted count of violations and of people served by GSA

gsa_inters_pws %>% group_by(GSA_ID) %>% 
   summarise(
      pws_count = n(),
      service_count = sum(service_count_prop,na.rm = t),
      service_count_gw_source = sum(service_count_prop*{PRIMARY_SOURCE=='GW'}),
      health_viol_count_2014_2018 = sum(HVIOL_PROP,na.rm = T)) %>%
   mutate(prop_service_gw_source = service_count_gw_source/service_count)