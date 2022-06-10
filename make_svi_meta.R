library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)

create_svi_meta <- function(type){
   #TODO add qualitative metadata:
   #  attributes of people who produced document, like GSA (number of actors and whether it was only one)
   #     findable on portal -> plan page -> "list of GSA(S) that collectively prepared the gsp"
   #     or gsa_gsp_basin_coord
   
   #  TODO attributes of community the document is for
   #  TODO importance of agriculture in each GSA region
   #  census tract
   
   #  TODO social vulnerability index 2018
   albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
   
   #     https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
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
   
   gsp_svi_adjusted <- ifelse(type == "pop", gsp_svi_adj_pop, 
                              ifelse(type == "area", gsp_svi_adj_area, NA))
   #id formatting
   gsp_svi_adjusted <- gsp_svi_adjusted %>% 
      mutate(code = (as.character(gsp_ids)))%>% 
      mutate(num_zeros = 4 - str_length(code)) %>% 
      mutate(gsp_num_id = paste(ifelse(num_zeros > 0, "0", ""),ifelse(num_zeros > 1,"0",""),ifelse(num_zeros > 2, "0",""),code,sep = "")) %>% 
      select(!c(code,num_zeros,gsp_ids))
   
   
}

