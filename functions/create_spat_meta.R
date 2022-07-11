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
create_svi_meta <- function(type){
   if(type != "area" & type != "pop"){
      stop("type must be \"area\" or \"pop\"")
   }
   #  social vulnerability index 2018
   albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
   
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
   if(scope != "tract" & type != "place"){
      stop("type must be \"tract\" or \"place\"")
   }

   albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
   
   #     https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
   #downloaded by hand and placed in data_spatial_raw
   censusplot <- st_read("data_spatial_raw/SVI_shapefiles_CA_censustracts/SVI2018_CALIFORNIA_tract.shp")
   censusplot <- st_transform(censusplot,albersNA)
   censusplot <- st_make_valid(censusplot)
   
   if(scope=="tract"){
      #https://data.cnra.ca.gov/dataset/dacs-census/resource/e5712534-dc8a-4094-bb0d-91050a63d8f0
      #downloaded by hand and placed in data_spatial_raw; renamed dacs_2018 
      #census tract 18
      dacplot <- st_read("data_spatial_raw/dacs_2018/DAC_CT18.shp")
      dacplot <- st_transform(dacplot,albersNA)
      dacplot <- st_make_valid(dacplot)
      
      dac_census_overs = st_intersects(dacplot, censusplot)
      dac_census_props = pblapply(seq_along(dac_census_overs),function(i){
         #proportion of dac tract in each census tract = area of census_dac intersection / dac area
         area_overlap = st_area(st_intersection(dacplot[i,],censusplot[dac_census_overs[[i]],]))
         prop_dac_in_tract = area_overlap/st_area(dacplot[i,])
         prop_tract_in_dac = area_overlap/st_area(censusplot[dac_census_overs[[i]],])
         data.table(census_tract_id = censusplot$FIPS[dac_census_overs[[i]]],
                    dac_tract_id = dacplot$GEOID10[i],
                    population = censusplot$E_TOTPOP[dac_census_overs[[i]]],
                    dacpop = dacplot$Pop18[i],
                    SVI_raw = ifelse(censusplot$SPL_THEMES[dac_census_overs[[i]]]>=0,
                                     censusplot$SPL_THEMES[dac_census_overs[[i]]],NA),
                    SVI_percentile = ifelse(censusplot$RPL_THEMES[dac_census_overs[[i]]]>=0,
                                            censusplot$RPL_THEMES[dac_census_overs[[i]]],NA),
                    DAC = ifelse(dacplot$DAC18[i] == "Y", T, 
                                 ifelse(dacplot$DAC18[i]=="N",F,NA)),
                    Prop_dac_in_tract = as.numeric(prop_dac_in_tract),
                    Prop_tract_in_dac = as.numeric(prop_tract_in_dac)
                    #return entries where over 90 percent of the census tract is in the dac tract
                    #as well as entries where over 90 percent of the dac tract is made up of the census tract
         )[Prop_tract_in_dac >= 0.9 | Prop_dac_in_tract >= 0.9]
      }, cl = 4)
      
      dac_svi_tbl <- list.rbind(dac_census_props)
      saveRDS(dac_svi_tbl, "data_output/dac_svi_tbl")
      
      dac_svi_tbl <- dac_svi_tbl[!is.na(dac_svi_tbl$DAC) & 
                                    !is.na(dac_svi_tbl$SVI_percentile)]
      
      
      #TODO which tracts are DAC and SDAC?
      
      
      #normality test shows data is non-normal
      set.seed(0)
      #shapiro.test(dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == T])
      ks.test(dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == T], "pnorm")
      ks.test(dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == F], "pnorm")
      #not a norm distr
      ks.test(dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == F], 
              dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == T], alternative = "greater")
      
      wilcox.test(dac_svi_tbl$SVI_percentile, as.numeric(dac_svi_tbl$DAC))
      #null is rejected; SVI of DACs is higher
      
      median(dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == T])
      median(dac_svi_tbl$SVI_percentile[dac_svi_tbl$DAC == F])
      
      ggplot(dac_svi_tbl, aes(x=DAC, y=SVI_percentile, fill=DAC)) +
         geom_boxplot()+theme_minimal()+scale_fill_scico_d(palette = "nuuk")
      
      #raw
      ks.test(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == T], "pnorm")
      ks.test(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == F], "pnorm")
      #not a norm distr
      ks.test(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == F], 
              dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == T], alternative = "greater")
      
      wt <- wilcox.test(dac_svi_tbl$SVI_raw, as.numeric(dac_svi_tbl$DAC))
      wilcoxonRG(x = dac_svi_tbl$SVI_raw,
                 g = as.numeric(dac_svi_tbl$DAC) )
      #null is rejected; SVI of DACs is higher
      
      median(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == T])
      median(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == F])
      
      ggplot(dac_svi_tbl, aes(x=DAC, y=SVI_raw, fill=DAC)) +
         geom_boxplot()+theme_minimal()+scale_fill_scico_d(palette = "nuuk")
      
      #since nominal, should not use pearson
      #cor.test(as.numeric(dac_svi_tbl$DAC), dac_svi_tbl$SVI_raw, method = "pearson",
      #        alternative = "greater")
      
      dac_svi_model <- lm(dac_svi_tbl$SVI_percentile ~ dac_svi_tbl$DAC)
      summary(dac_svi_model)
      apsrtable::apsrtable(dac_svi_model)
      
      return(dac_svi_tbl)
   }#end of scope=tract
   
   if(scope=="place"){
      dacplace <- st_read("data_spatial_raw/dacs_2018/DAC_Pl18.shp")
      dacplace <- st_transform(dacplace,albersNA)
      dacplace <- st_make_valid(dacplace)
      
      #census_gspshape_overs = st_intersects(censusplot, gsp_shapes)
      gspshape_place_overs = st_intersects(gsp_shapes, dacplace)
      gspshape_place_props = pblapply(seq_along(gspshape_place_overs),function(i){
         #proportion of gsp in each census tract = area of census_gsp intersection / gsp area
         area_overlap = st_area(st_intersection(gsp_shapes[i,],dacplace[gspshape_place_overs[[i]],]))
         prop_gsp_in_place = area_overlap/st_area(gsp_shapes[i,])
         prop_place_in_gsp = area_overlap/st_area(dacplace[gspshape_place_overs[[i]],])
         data.table(place_id = dacplace$GEOID[gspshape_place_overs[[i]]],
                    population = dacplace$Pop18[gspshape_place_overs[[i]]],
                    DAC = ifelse(dacplace$DAC18[gspshape_place_overs[[i]]] == "Y", T, 
                                 ifelse(dacplace$DAC18[gspshape_place_overs[[i]]]=="N",F,NA)),
                    gsp_id = paste(ifelse((4-str_length(gsp_shapes$GSP.ID[i])) > 0, "0", ""),
                                   ifelse((4-str_length(gsp_shapes$GSP.ID[i])) > 1,"0",""),
                                   ifelse((4-str_length(gsp_shapes$GSP.ID[i])) > 2, "0",""),
                                   (gsp_shapes$GSP.ID[i]),sep = ""),
                    Prop_GSP_in_place = as.numeric(prop_gsp_in_place),
                    Prop_place_in_GSP = as.numeric(prop_place_in_gsp)
                    #return entries where over half a percent of the place is in the GSP
                    #as well as entries where over half a percent of the GSP is made up of that place
         )[Prop_place_in_GSP >= 0.005 | Prop_GSP_in_place >= 0.005]
      }, cl = 4)
      
      gsp_ids <- as.character(gsp_shapes$GSP.ID)
      
      gsp_num_id = paste(ifelse((4-str_length(gsp_ids)) > 0, "0", ""),
                         ifelse((4-str_length(gsp_ids)) > 1,"0",""),
                         ifelse((4-str_length(gsp_ids)) > 2, "0",""),
                         (gsp_ids),sep = "")
      names(gspshape_place_props) = gsp_num_id
      
      
      gspshape_place_props[gsp_num_id[1]]
      #each list item is a different gsp
      
      #prop_pop_in_dac is percent of pop in places (except for dac=na places)
      #that lives in a dac place
      gsp_place_tbbl <- as_tibble(rbindlist(gspshape_place_props))
      gsp_place_tbbl <- gsp_place_tbbl %>% filter(!is.na(DAC)) %>% 
         group_by(gsp_id) %>% 
         mutate(prop_pop_in_dac = sum(ifelse(DAC == T, population*Prop_place_in_GSP, 0)) / 
                   sum(population*Prop_place_in_GSP))
      
      gsp_svi_adjusted <- readRDS(
         list.files(path = "data_output",pattern = type,full.names = T)[length(
            list.files(path = "data_output",pattern = type,full.names = T))])
      
      spat_tbl <- full_join(gsp_place_tbbl, gsp_svi_adjusted, by = c("gsp_id" = "gsp_num_id"))
      # proportion of pop in places that live in a dac
      #pop of places where DAC == T / (pop of places where !is.na(DAC))
      
      corr <- cor(spat_tbl$prop_pop_in_dac, spat_tbl$SVI_na_adj, method = "pearson", use = "complete.obs")
      #join this with svi of basin, area style and pop style
      
      saveRDS(spat_tbl, file = paste0("data_output/","spat_tbl_",type,"_",format(Sys.time(), "%Y%m%d-%H:%M")))
      
      return(spat_tbl)
      
   }#end of scope=place

   
   
    
 
   
   
    
}

