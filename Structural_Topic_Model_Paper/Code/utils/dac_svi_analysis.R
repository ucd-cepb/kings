packs <- c('tidyverse','sf','pbapply','ggplot2',
           'stringr','data.table','boxr','rlist')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

source(filekey[filekey$var_name=="create_spat_meta_function",]$filepath)

dac_svi_analysis <- function(scope, type = NA){
   
   albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
   
   if(scope=="tract"){
      
      if(!is.na(type)){
         print("Type not used for scope = 'tract'")
      }
      #is a census tract that is classified as a dac 
      #more likely to have a high SVI?
      
      #https://data.cnra.ca.gov/dataset/dacs-census/resource/e5712534-dc8a-4094-bb0d-91050a63d8f0
      #downloaded by hand and placed in data_spatial_raw; renamed dacs_2018 
      #census tract 18
      dacplot <- st_read(paste0(filekey[filekey$var_name=="dacs_2018_files",]$filepath,"/DAC_CT18.shp"))
      dacplot <- st_transform(dacplot,albersNA)
      dacplot <- st_make_valid(dacplot)
      
      # https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
      #downloaded by hand and placed in data_spatial_raw
      censusplot <- st_read(filekey[filekey$var_name=="svi_census_tracts_2018",]$filepath)
      censusplot <- st_transform(censusplot,albersNA)
      #st_crs pulls up projection
      #alt: st_crs(gsp_submitted)
      censusplot <- st_make_valid(censusplot)
      
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
      saveRDS(dac_svi_tbl, filekey[filekey$var_name=="dac_svi_tbl",]$filepath)
      
      dac_svi_tbl <- dac_svi_tbl[!is.na(dac_svi_tbl$DAC) & 
                                    !is.na(dac_svi_tbl$SVI_percentile)]
      
      
      #normality test shows data is non-normal
      set.seed(12143)
      #shapiro test used for smaller n
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
      norm_dact <- ks.test(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == T], "pnorm")
      norm_dacf <- ks.test(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == F], "pnorm")
      data_is_norm <- !(norm_dact$p.value <0.05 | norm_dacf$p.value <0.05)
      #not a norm distr
      ks <- ks.test(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == F], 
              dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == T], alternative = "greater")
      
      wt <- wilcox.test(dac_svi_tbl$SVI_raw, as.numeric(dac_svi_tbl$DAC))
      glass_rank <- wilcoxonRG(x = dac_svi_tbl$SVI_raw,
                 g = as.numeric(dac_svi_tbl$DAC) )
      #null is rejected; SVI of DACs is higher
      
      med_dac <- median(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == T])
      med_notdac <- median(dac_svi_tbl$SVI_raw[dac_svi_tbl$DAC == F])
      
      dac_svi_boxplot <- ggplot(dac_svi_tbl, aes(x=DAC, y=SVI_raw, fill=DAC)) +
         geom_boxplot()+theme_minimal()+scale_fill_scico_d(palette = "nuuk")+
         labs(x = "DAC",
              y = "Raw Social Vulnerability Index",
              title = "Social Vulnerability Index \nof Census Tract by DAC Status")+
         theme_bw()+theme(plot.title = element_text(hjust = 0.5))
      dac_svi_boxplot
      ggsave(paste0("dac_and_svi_tract.png"),plot = dac_svi_boxplot, device = "png", path = filekey[filekey$var_name=="svi_figures",]$filepath,
             width = 1800, height = 1600, dpi = 300, units = "px", bg = "white")
      
      #since nominal, should not use pearson
      #cor.test(as.numeric(dac_svi_tbl$DAC), dac_svi_tbl$SVI_raw, method = "pearson",
      #        alternative = "greater")
      
      return(list("dac_svi_tbl" = dac_svi_tbl,
                  "median_svi_of_dacs" = med_dac,
                  "median_svi_of_non_dacs" = med_notdac,
                  "glass_rank" = glass_rank, "mann-whitney" = wt, 
                  "ks_test" = ks,
                  "data_normality"= data_is_norm,
                  "plot" = paste0(filekey[filekey$var_name=="svi_figures",]$filepath,"/dac_and_svi_tract.png")))
   }#end of scope=tract
   
   if(scope=="place"){
      #do basins with a high proportion of the "place" population in 
      #dac "places" have a higher SVI?
      
      dacplace <- st_read(paste0(filekey[filekey$var_name=="dacs_2018_files",]$filepath,"/DAC_Pl18.shp"))
      dacplace <- st_transform(dacplace,albersNA)
      dacplace <- st_make_valid(dacplace)
      
      gspzip <- filekey[filekey$var_name=="gsp_submitted_zip",]$filepath
      fname = unzip(gspzip, list=TRUE)
      unzip(gspzip, files=fname$Name, exdir=substr(gspzip,1,nchar(gspzip)-4), overwrite=TRUE)
      fpath = file.path(substr(gspzip,1,nchar(gspzip)-4), grep('shp$',fname$Name,value=T))
      gsp_shapes <- st_read(fpath)
      gsp_shapes <- st_transform(gsp_shapes,albersNA)
      gsp_shapes <- st_make_valid(gsp_shapes)
      
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
      
      #TODO match to create_spat_meta method
      #prop_pop_in_dac is percent of pop in places (except for dac=na places)
      #that lives in a dac place
      
      #counting dac=na as dac=f would falsely fill in the middle of the x-axis

      gsp_place_tbbl <- as_tibble(rbindlist(gspshape_place_props))
      
         
      gsp_place_tbbl <- gsp_place_tbbl %>% filter(!is.na(DAC)) %>% 
         group_by(gsp_id) %>% 
         mutate(prop_pop_in_dac = sum(ifelse(DAC == T, population*Prop_place_in_GSP, 0)) / 
                   sum(population*Prop_place_in_GSP)) %>% ungroup()
      
      gsp_svi_adjusted <- create_svi_meta(type, box_sync = T, overwrite=T)
      
      spat_tbl <- inner_join(gsp_place_tbbl, gsp_svi_adjusted, by = c("gsp_id" = "gsp_num_id"))
      # proportion of pop in places that live in a dac
      #pop of places where DAC == T / (pop of places where !is.na(DAC))
      
      saveRDS(spat_tbl, file = paste0(filekey[filekey$var_name=="percent_place_pop_in_dac",]$filepath,type))
      
      uniq_spat_tbl <- spat_tbl %>% select(c("gsp_id","prop_pop_in_dac","SVI_na_adj")) %>% unique()
      corr <- cor(uniq_spat_tbl$prop_pop_in_dac, uniq_spat_tbl$SVI_na_adj, method = "pearson", use = "complete.obs")
      #join this with svi of basin, area style and pop style
      
      dac_svi_plot<-ggplot(uniq_spat_tbl, aes(prop_pop_in_dac, 
                                                 SVI_na_adj))+
         geom_point(size = 2, alpha = 0.33, color = scico(1, palette = "roma"))+ 
         labs(x = "Proportion of Place Population in a DAC Place (0-1)",
              y = "Raw Social Vulnerability Index",
              title = "Percent DAC and Social Vulnerability Index by GSP")+
         theme_bw()+theme(plot.title = element_text(hjust = 0.5))+geom_smooth()
      dac_svi_plot
      ggsave(paste0("dac_and_svi_place_",type,".png"),plot = dac_svi_plot, device = "png", path = filekey[filekey$var_name=="svi_figures",]$filepath,
             width = 3000, height = 1800, dpi = 300, units = "px", bg = "white")
      
      return(list("correlation" = corr, "spatial_table" = spat_tbl, 
                  "plot" = paste0(filekey[filekey$var_name=="svi_figures",]$filepath,"/dac_and_svi_place_",type,".png")))
      
   }#end of scope=place
}


