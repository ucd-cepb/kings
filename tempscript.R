#starts with tyler's version of gsp_text_with_meta, loaded from Box "gsp_docs_w_meta" v.17,
#and elise's version, from Box "gsp_docs_w_meta" v.18
gsp_text_with_langnew <- gsp_text_with_meta
gsp_text_with_langnew <- gsp_text_with_langnew[,c("text","admin",
                                                  "basin_plan",
                                                  "sust_criteria",
                                                  "monitoring_networks",
                                                  "projects_mgmt_actions",
                                                  "gsp_id",
                                                  "is_comment",
                                                  "is_reference",
                                                  "page_num",
                                                  "basin_id",
                                                  "link",
                                                  "basin",
                                                  "approval20230731",
                                                  "version_approval20230922",
                                                  "latest_approval",
                                                  "gsp_local_id",
                                                  "determination_letters",
                                                  "mult_gsas",
                                                  "name_gsas20230922",
                                                  "name_gsas20230731",
                                                  "latest_version",
                                                  "basin_name",
                                                  "priority",
                                                  "fract_of_area_in_habitat",
                                                  "urbangw_af",
                                                  "gwsum",
                                                  "exceedance")]

saveRDS(gsp_text_with_langnew, filekey[filekey$var_name=="gsp_docs_lang",]$filepath)
gspoutmeta <- gsp_out$meta
#rounding errors mess this up, but the two sources agree
gspoutmeta[,c("percent_dac_by_pop_scaled","fract_of_area_in_habitat_log_scaled",
              "maxdryspell_scaled","Agr_Share_Of_GDP_scaled")] <- list(NULL)
gspoutmeta[,c("basin_population_log_scaled")] <- list(NULL)
gspoutmeta[,c("well_MCL_exceedance_count_by_log_pop_scaled")] <- list(NULL)

#tyler's version of repub vote share is more up to date
gsp_text_with_meta$Republican_Vote_Share <- NULL
gsp_text_with_meta$Republican_Vote_Share_scaled <- NULL
#elise's version of mult_gsas and num_gsas has newer info for plan 0008, which
#was not in place at the time of plan submission, so we are going with
#tyler's version of mult_gsas
gsp_text_with_meta$mult_gsas <- NULL
gsp_text_with_meta$name_gsas <- NULL

gsp_text_with_meta$dsci_scaled <- scale(gsp_text_with_meta$DSCI)
combinedmeta <- left_join(gspoutmeta, gsp_text_with_meta)

out8 <- gspoutmeta[gspoutmeta$gsp_id=="0008",]
newmeta8 <- gsp_text_with_meta[gsp_text_with_meta$gsp_id=="0008",]

sum(is.na(combinedmeta$basin_name))


gsp_out$meta <- combinedmeta

saveRDS(gsp_out, file = paste0(filekey[filekey$var_name=="gsp_out_files",]$filepath,format(Sys.time(), "%Y%m%d-%H:%M")))

tps <- readRDS(filekey[filekey$var_name=="topic_prevalence",]$filepath)
tps$DW <- tps$V10 + tps$V16 + tps$V25 + tps$V26 + tps$V30
tps$EJ <- tps$V2 + tps$V25
tps$CC <- tps$V7 + tps$V24
tps$GDE <- tps$V14 + tps$V29

gspmini <- gsp_out$meta[!duplicated(gsp_out$meta$gsp_id),]

gsp_text_with_meta <- readRDS("temp_meta_save_file_7")


#sessionInfo() output from May 28, 2024
if(F){
   R version 4.3.0 (2023-04-21)
   Platform: x86_64-apple-darwin20 (64-bit)
   Running under: macOS Ventura 13.5.2
   
   Matrix products: default
   BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
   LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
   
   locale:
      [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
   
   time zone: America/Los_Angeles
   tzcode source: internal
   
   attached base packages:
      [1] stats     graphics  grDevices utils     datasets  methods   base     
   
   other attached packages:
      [1] viridis_0.6.5      data.table_1.15.2  htmlTable_2.4.2    ggcorrplot_0.1.4.1
   [5] fields_15.2        viridisLite_0.4.2  spam_2.10-0        huge_1.3.5        
   [9] igraph_2.0.2       reshape2_1.4.4     lubridate_1.9.3    forcats_1.0.0     
   [13] stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.5       
   [17] tidyr_1.3.1        tibble_3.2.1       tidyverse_2.0.0    scico_1.5.0       
   [21] ggrepel_0.9.5      ggplot2_3.5.0      stm_1.3.7         
   
   loaded via a namespace (and not attached):
      [1] dotCall64_1.1-1    gtable_0.3.4       xfun_0.42          raster_3.6-26     
   [5] htmlwidgets_1.6.4  lattice_0.22-5     tzdb_0.4.0         vctrs_0.6.5       
   [9] tools_4.3.0        generics_0.1.3     proxy_0.4-27       fansi_1.0.6       
   [13] pkgconfig_2.0.3    Matrix_1.6-5       KernSmooth_2.23-22 checkmate_2.3.1   
   [17] lifecycle_1.0.4    compiler_4.3.0     munsell_0.5.0      terra_1.7-71      
   [21] codetools_0.2-19   htmltools_0.5.7    maps_3.4.2         class_7.3-22      
   [25] pillar_1.9.0       MASS_7.3-60.0.1    classInt_0.4-10    digest_0.6.34     
   [29] tidyselect_1.2.0   stringi_1.8.3      sf_1.0-15          fastmap_1.1.1     
   [33] grid_4.3.0         colorspace_2.1-0   cli_3.6.2          magrittr_2.0.3    
   [37] utf8_1.2.4         broom_1.0.5        e1071_1.7-14       withr_3.0.0       
   [41] scales_1.3.0       backports_1.4.1    textNet_0.1.0      sp_2.1-3          
   [45] timechange_0.3.0   matrixStats_1.2.0  nnet_7.3-19        gridExtra_2.3     
   [49] hms_1.1.3          knitr_1.45         rlang_1.1.3        Rcpp_1.0.12       
   [53] glue_1.7.0         DBI_1.2.2          rstudioapi_0.15.0  R6_2.5.1          
   [57] plyr_1.8.9         units_0.8-5       
}


#how to select most recent file:
#finfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
#version_select <- which(finfo$mtime==max(finfo$mtime))

#model <- readRDS(rownames(finfo)[version_select])