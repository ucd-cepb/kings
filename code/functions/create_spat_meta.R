library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)
library(ggplot2)
library(scico)
library(rcompanion)

#type = "pop" or "area"
create_svi_meta <- function(type, box_sync = F){
   if(type != "area" & type != "pop"){
      stop("type must be \"area\" or \"pop\"")
   }
   #  social vulnerability index 2018
   albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
   
   if(box_sync == T){
      #set up Renviron with Box App permissions specific to your user
      box_auth()
      box_fetch(dir_id = 167050269694, local_dir = "./data_spatial_raw")
   }
   # https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
   #downloaded by hand and placed in data_spatial_raw
   censusplot <- st_read("data_spatial_raw/SVI_shapefiles_CA_censustracts/SVI2018_CALIFORNIA_tract.shp")
   censusplot <- st_transform(censusplot,albersNA)
   #st_crs pulls up projection
   #alt: st_crs(gsp_submitted)
   censusplot <- st_make_valid(censusplot)
   #uses NAD83
   
   #GSP Posted and GSP Submitted layers downloadable at
   #gsp_map_url = "https://sgma.water.ca.gov/webgis/?jsonfile=https%3a%2f%2fsgma.water.ca.gov%2fportal%2fresources%2fjs%2fmapconfigs%2fGspSubmittalsConfig.js&_dc=0.23941161367525954"
   #downloaded by hand and placed in data_spatial_raw
   #GSP_Submitted should be a superset of GSP_Posted, so GSP_Submitted is used here
   
   #Basin info downloadable by selecting
   #Reference Layers > Groundwater Management > "Bulletin 118 Groundwater Basins - 2018" at
   #  https://sgma.water.ca.gov/webgis/?appid=SGMADataViewer#boundaries
   
   fname = unzip("data_spatial_raw/GSP_submitted.zip", list=TRUE)
   unzip("data_spatial_raw/GSP_submitted.zip", files=fname$Name, exdir="data_spatial_raw/GSP_submitted", overwrite=TRUE)
   fpath = file.path("data_spatial_raw/GSP_submitted", grep('shp$',fname$Name,value=T))
   gsp_shapes <- st_read(fpath)
   gsp_shapes <- st_transform(gsp_shapes,albersNA)
   gsp_shapes <- st_make_valid(gsp_shapes)
   
   #census_gspshape_overs = st_intersects(censusplot, gsp_shapes)
   gspshape_census_overs = st_intersects(gsp_shapes, censusplot)
   gspshape_census_props = pblapply(seq_along(gspshape_census_overs),function(i){
      #proportion of gsp in each census tract = area of census_gsp intersection / gsp area
      area_overlap = st_area(st_intersection(gsp_shapes[i,],censusplot[gspshape_census_overs[[i]],]))
      prop_gsp_in_tract = area_overlap/st_area(gsp_shapes[i,])
      prop_tract_in_gsp = area_overlap/st_area(censusplot[gspshape_census_overs[[i]],])
      data.table(census_tract_id = censusplot$FIPS[gspshape_census_overs[[i]]],
                 population = censusplot$E_TOTPOP[gspshape_census_overs[[i]]],
                 SVI_percentile = ifelse(censusplot$RPL_THEMES[gspshape_census_overs[[i]]]>=0,
                                         censusplot$RPL_THEMES[gspshape_census_overs[[i]]],NA),
                 gsp_ids = gsp_shapes$GSP.ID[i],
                 Prop_GSP_in_tract = as.numeric(prop_gsp_in_tract),
                 Prop_tract_in_GSP = as.numeric(prop_tract_in_gsp)
                 #return entries where over half a percent of the tract is in the GSP
                 #as well as entries where over half a percent of the GSP is made up of that tract
      )[Prop_tract_in_GSP >= 0.005 | Prop_GSP_in_tract >= 0.005]
   }, cl = 4)
   #each datatable is a different gsp
   
   #within each datatable
   #column of census tract ids
   #column of SVIs
   #percent that is each census tract
   
   gspshape_census_dt = rbindlist(gspshape_census_props)
   fwrite(gspshape_census_dt,file = 'data_temp/gsp_census_overlap.csv')
   gspshape_census_dt <- as_tibble(fread(file = 'data_temp/gsp_census_overlap.csv'))
   
   #gives somewhat lower SVIs (thicker left tail)
   gsp_svi_adj_area <- summarize(group_by(gspshape_census_dt, gsp_ids),SVI_percentile, Prop_GSP_in_tract) %>% 
      #inflates SVI to account for small dropped census tracts by dividing by sum of proportion overlaps
      mutate(svi_inflated = SVI_percentile / sum(Prop_GSP_in_tract)) %>% 
      mutate(prop_na = sum(ifelse(is.na(SVI_percentile),Prop_GSP_in_tract,0))) %>% 
      #weighted sum of SVI portions by census tract 
      #determines what portion of each GSP has an NA value for SVI
      #adjusts SVI of GSP to account for NAs
      mutate(SVI_na_adj = sum((Prop_GSP_in_tract / (1-prop_na)) * svi_inflated, na.rm = T)) %>%
      ungroup() %>% 
      select(c("gsp_ids", "SVI_na_adj")) %>% 
      unique()
   #weighted.mean(x=c(0.3,NA),w=c(0.4,0.6),na.rm=T)
   #gives somewhat higher SVIs (thinner left tail)
   gsp_svi_adj_pop <- summarize(group_by(gspshape_census_dt, gsp_ids),SVI_percentile, Prop_GSP_in_tract, Prop_tract_in_GSP, population) %>% 
      #deflates pop to account for what percent of the tract is in the GSP
      mutate(pop_adj = population * Prop_tract_in_GSP) %>% 
      #calculates percent of GSP population that is in that tract
      mutate(pop_fraction = pop_adj / sum(pop_adj)) %>% 
      #tracts with pop of 0 have SVI of NA
      #weighted sum of SVI portions by population of census tracts
      mutate(SVI_na_adj = sum(pop_fraction * SVI_percentile, na.rm = T)) %>%
      ungroup() %>% 
      select(c("gsp_ids", "SVI_na_adj")) %>% 
      unique()
   
   if(type == "pop"){
      gsp_svi_adjusted <- gsp_svi_adj_pop
   }else{
      gsp_svi_adjusted <- gsp_svi_adj_area
   }
   
   #id formatting
   gsp_svi_adjusted <- gsp_svi_adjusted %>% 
      mutate(code = (as.character(gsp_ids)))%>% 
      mutate(num_zeros = 4 - str_length(code)) %>% 
      mutate(gsp_num_id = paste(ifelse(num_zeros > 0, "0", ""),ifelse(num_zeros > 1,"0",""),ifelse(num_zeros > 2, "0",""),code,sep = "")) %>% 
      select(!c(code,num_zeros,gsp_ids))
   
   saveRDS(gsp_svi_adjusted, file = paste0("data_output/","gsp_svi_",type,"_",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   return(gsp_svi_adjusted)
   
}

create_dac_meta <- function(type, scope){
   if(type != "area" & type != "pop"){
      stop("type must be \"area\" or \"pop\"")
   }
   if(scope != "tract" & scope != "place" & scope != "blockgroup"){
      stop("scope must be \"tract,\" \"place,\" or \"blockgroup\"")
   }
   if(type == "area" & scope == "place"){
      stop("scope \"place\" may not be used with type \"area\"")
   }
   

   albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
   
   if(box_sync == T){
      #set up Renviron with Box App permissions specific to your user
      box_auth()
      box_fetch(dir_id = 167050269694, local_dir = "./data_spatial_raw")
   }
   
   #https://data.cnra.ca.gov/dataset/dacs-census/resource/e5712534-dc8a-4094-bb0d-91050a63d8f0
   #downloaded by hand and placed in data_spatial_raw; renamed dacs_2018 
   dacplot = switch(  
      scope,  
      "blockgroup"= st_read("data_spatial_raw/dacs_2018/DAC_BG18.shp"),  
      "tract"= st_read("data_spatial_raw/dacs_2018/DAC_CT18.shp"),  
      "place"= st_read("data_spatial_raw/dacs_2018/DAC_Pl18.shp")
   ) 
   dacplot <- st_transform(dacplot,albersNA)
   dacplot <- st_make_valid(dacplot)
   
   fname = unzip("data_spatial_raw/GSP_submitted.zip", list=TRUE)
   unzip("data_spatial_raw/GSP_submitted.zip", files=fname$Name, exdir="data_spatial_raw/GSP_submitted", overwrite=TRUE)
   fpath = file.path("data_spatial_raw/GSP_submitted", grep('shp$',fname$Name,value=T))
   gsp_shapes <- st_read(fpath)
   gsp_shapes <- st_transform(gsp_shapes,albersNA)
   gsp_shapes <- st_make_valid(gsp_shapes)
   
   #dac_gspshape_overs = st_intersects(dacplot, gsp_shapes)
   gspshape_dac_overs = st_intersects(gsp_shapes, dacplot)
   gspshape_dac_props = pblapply(seq_along(gspshape_dac_overs),function(i){
      #proportion of gsp in each dac region = area of gsp_dac intersection / gsp area
      area_overlap = st_area(st_intersection(gsp_shapes[i,],dacplot[gspshape_dac_overs[[i]],]))
      prop_gsp_in_region = area_overlap/st_area(gsp_shapes[i,])
      prop_region_in_gsp = area_overlap/st_area(dacplot[gspshape_dac_overs[[i]],])
      data.table(dac_plot_id = dacplot$GEOID[gspshape_dac_overs[[i]]],
                 population = dacplot$Pop18[gspshape_dac_overs[[i]]],
                 DAC = ifelse(dacplot$DAC18[gspshape_dac_overs[[i]]] == "Y", T, 
                              ifelse(dacplot$DAC18[gspshape_dac_overs[[i]]]=="N",F,NA)),
                 gsp_ids = gsp_shapes$GSP.ID[i],
                 Prop_GSP_in_region = as.numeric(prop_gsp_in_region),
                 Prop_region_in_GSP = as.numeric(prop_region_in_gsp)
                 #return entries where over half a percent of the tract is in the GSP
                 #as well as entries where over half a percent of the GSP is made up of that tract
      )[Prop_region_in_GSP >= 0.005 | Prop_GSP_in_region >= 0.005]
   }, cl = 4)
   #each datatable is a different gsp
   
   #within each datatable
   #column of dac region ids
   #percent that is each census tract
   
   gspshape_dac_dt = rbindlist(gspshape_dac_props)
   fwrite(gspshape_dac_dt,
          file = paste0('data_temp/gsp_dac_overlap_',type,'_',scope,'.csv'))
   gspshape_dac_dt <- as_tibble(
         fread(file = paste0('data_temp/gsp_dac_overlap_',type,'_',scope,'.csv')))
   
   gsp_id <- as.character(gsp_shapes$GSP.ID)
   
   
   if(type=="area"){
      
      gsp_dac_adj_area <- summarize(group_by(gspshape_dac_dt, gsp_ids),DAC, Prop_GSP_in_region) %>% 
         #finds percent of gsp place, tract, or blockgroup area that is designated a dac
         mutate(prop_gsp_in_any_region = sum(Prop_GSP_in_region)) %>% 
         mutate(prop_gsp_in_truedac = sum(ifelse(DAC == T, Prop_GSP_in_region,0),na.rm=T)) %>% 
         mutate(percent_dac_by_area = sum(ifelse(DAC == T,Prop_GSP_in_region,0),na.rm=T) / sum(Prop_GSP_in_region)) %>% 
         #if dac = na, the region is included in the total area but not the dac area
         ungroup() %>% 
         select(c("gsp_ids", "percent_dac_by_area")) %>% 
         unique()
      #if there are no places in the gsp, defaults to percent_dac_by_area = 0 
      gsps_wo_regions <- as.data.table(cbind(
         "gsp_ids" = gsp_id[!(gsp_id %in%gsp_dac_adj_area$gsp_ids)],
         "percent_dac_by_area" = rep(0,times = 
                                        length(gsp_id[!(gsp_id %in%gsp_dac_adj_area$gsp_ids)]))))
      gsp_dac_adj_area <- rbind(gsp_dac_adj_area, gsps_wo_regions)
      
      gsp_num_id = paste(ifelse((4-str_length(gsp_dac_adj_area$gsp_ids)) > 0, "0", ""),
                         ifelse((4-str_length(gsp_dac_adj_area$gsp_ids)) > 1,"0",""),
                         ifelse((4-str_length(gsp_dac_adj_area$gsp_ids)) > 2, "0",""),
                         (gsp_dac_adj_area$gsp_ids),sep = "")
      gsp_dac_adj_area <- cbind(gsp_dac_adj_area, gsp_num_id) %>% select(-gsp_ids)
      
      
      saveRDS(gsp_dac_adj_area, paste0('data_output/gsp_percent_dac_',type,'_',scope,'.csv'))
      return(gsp_dac_adj_area)
      
   }#end of type = area
   
   if(type=="pop"){
      gsp_dac_adj_pop <- summarize(group_by(gspshape_dac_dt, gsp_ids),DAC, Prop_region_in_GSP, population) %>% 
         #finds percent of gsp place, tract, or blockgroup area that is designated a dac
         mutate(percent_dac_by_pop = weighted.mean(x=(DAC%in%T),w=population*Prop_region_in_GSP,na.rm=T)) %>% 
         #if dac = na, the population is included in the total population but not the dac population
         #adjusts population to only include proportion of region in the GSP. Assumes equal density throughout region
         ungroup() %>% 
         select(c("gsp_ids", "percent_dac_by_pop")) %>% 
         unique()
      
      #if there are no places in the gsp, defaults to percent_dac_by_area = 0 
      gsps_wo_regions <- as.data.table(cbind(
         "gsp_ids" = gsp_id[!(gsp_id %in%gsp_dac_adj_pop$gsp_ids)],
         "percent_dac_by_pop" = rep(0,times = 
                                        length(gsp_id[!(gsp_id %in%gsp_dac_adj_pop$gsp_ids)]))))
      gsp_dac_adj_pop <- rbind(gsp_dac_adj_pop, gsps_wo_regions)
      
      gsp_num_id = paste(ifelse((4-str_length(gsp_dac_adj_pop$gsp_ids)) > 0, "0", ""),
                         ifelse((4-str_length(gsp_dac_adj_pop$gsp_ids)) > 1,"0",""),
                         ifelse((4-str_length(gsp_dac_adj_pop$gsp_ids)) > 2, "0",""),
                         (gsp_dac_adj_pop$gsp_ids),sep = "")
      gsp_dac_adj_pop <- cbind(gsp_dac_adj_pop, gsp_num_id) %>% select(-gsp_ids)
      
      saveRDS(gsp_dac_adj_pop, paste0('data_output/gsp_percent_dac_',type,'_',scope,'.csv'))
      return(gsp_dac_adj_pop)
   }#end of type=pop

   
   
    
 
   
   
    
}

