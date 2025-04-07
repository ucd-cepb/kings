inputsfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
inputsfilenamesplits <- unlist(strsplit(inputsfilename,split="/"))
inputspath <- paste(inputsfilenamesplits[1:(length(inputsfilenamesplits)-1)],collapse = "/")
inputspattern <- inputsfilenamesplits[length(inputsfilenamesplits)]

inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[
   length(list.files(path = inputspath, pattern = inputspattern, full.names = T))])

#No longer necessary with new gsp_text_with_meta file from May 2024
#newscrapefilename <- filekey[filekey$var_name=="gsp_web_vars_planevolutionpaper",]$filepath
#newscrapefilenamesplits <- unlist(strsplit(newscrapefilename,split="/"))
#newscrapepath <- paste(newscrapefilenamesplits[1:(length(newscrapefilenamesplits)-1)],collapse = "/")
#newscrapepattern <- newscrapefilenamesplits[length(newscrapefilenamesplits)]
#newscrape <- readRDS(list.files(newscrapepath,pattern=newscrapepattern,full.names=T)[length(
#   list.files(newscrapepath,pattern=newscrapepattern))])

#newscrape <- newscrape[newscrape$version==1,]

#newscrape <- as.data.frame(cbind(gsp_id = newscrape$gsp_num_id, repairedapproval = newscrape$version_approval))
#mymeta <- dplyr::left_join(inputs$meta, newscrape)
mymeta <- inputs$meta
mymeta$expost_collab <- mymeta$mult_gsas

mymeta$priority_categorylow_or_verylow <- mymeta$priority_category == "low_or_verylow"
mymeta$priority_categorymed <- mymeta$priority_category == "med"

mymeta <- mymeta[,c('gsp_id', 'priority_categorylow_or_verylow',
                             'priority_categorymed',
                             'expost_collab',
                             'exante_collab',
                             'basin_population_log_scaled',
                             'dsci_scaled',
                             'log_well_MCL_exceedance_count_by_log_pop_scaled',
                             'percent_dac_by_pop_scaled',
                             'fract_of_area_in_habitat_log_scaled',
                             'Agr_Share_Of_GDP_scaled',
                             'Republican_Vote_Share_scaled')]


minimeta <- unique(mymeta[,c('gsp_id', 'priority_categorylow_or_verylow',
                             'priority_categorymed',
                             'expost_collab',
                             'exante_collab',
                             'basin_population_log_scaled',
                             'dsci_scaled',
                             'log_well_MCL_exceedance_count_by_log_pop_scaled',
                             'percent_dac_by_pop_scaled',
                             'fract_of_area_in_habitat_log_scaled',
                             'Agr_Share_Of_GDP_scaled',
                             'Republican_Vote_Share_scaled')])

#colnames(mymeta)[colnames(mymeta)=="repairedapproval"] <- "approval"
#mymeta$approval <- as.factor(mymeta$approval)
#colnames(minimeta)[colnames(minimeta)=="repairedapproval"] <- "approval"
#minimeta$approval <- as.factor(minimeta$approval)

library(ggcorrplot)
#pal <- c(scico(3, palette = "vik") )

#max_corr <- max(abs(cor_mat))
#max_corr <- ceiling(max_corr*1000)/1000+
#   ifelse(ceiling(max_corr*1000)%%5==0,0,(5-ceiling(max_corr*1000)%%5)/1000)
#min_corr <- max_corr*-1
View(mymeta)

corrplotbypage <- ggcorrplot(cor(mymeta[,c(
   "expost_collab",  
   "exante_collab",
   "priority_categorylow_or_verylow",                
   "priority_categorymed",
   "basin_population_log_scaled",                  
   "dsci_scaled",                                   
   "log_well_MCL_exceedance_count_by_log_pop_scaled",
   "percent_dac_by_pop_scaled",                      
   "fract_of_area_in_habitat_log_scaled",            
   "Agr_Share_Of_GDP_scaled",                        
   "Republican_Vote_Share_scaled"                   

)]), type = "upper", lab = T)#+
#   theme(axis.text.x = element_text(size = 20),  # Order: top, right, bottom, left
#         axis.text.y = element_text(size = 20))+
#   labs(title = "Correlation between Predictors",
#        x="Predictors", y = "Predictors")+
#   theme_classic()+theme(axis.text.x=element_text(size=20, angle = 80, vjust = 1, hjust = 1),
#                         axis.text.y=element_text(size=20),
#                         axis.title.x=element_text(size=24,margin=margin(t=5)),
#                         axis.title.y=element_text(size=24,hjust = 0.5),
#                         plot.title=element_text(size=36,hjust = 0.5))+
#   scale_fill_gradient2(low = pal[1], high = pal[3], 
#                        mid = pal[2], midpoint = 0, limit = c(min_corr, max_corr), 
#                        space = "Lab",
#                        name = "Corr", na.value="white")+
#   theme(legend.key=element_rect(colour="black",fill="white",
#                                 linetype="solid")) 

corrplotbygsp <- ggcorrplot(cor(minimeta[,c(
                     "expost_collab",
                     "exante_collab",
                     "priority_categorylow_or_verylow",                
                     "priority_categorymed",
                     "basin_population_log_scaled",                  
                     "dsci_scaled",                                   
                     "log_well_MCL_exceedance_count_by_log_pop_scaled",
                     "percent_dac_by_pop_scaled",                      
                     "fract_of_area_in_habitat_log_scaled",            
                     "Agr_Share_Of_GDP_scaled",                        
                     "Republican_Vote_Share_scaled"                   
                     
                  )]), type = "upper", lab = T)

saveRDS(corrplotbypage, paste0(filekey[filekey$var_name=="stmpaper_figures",]$filepath, "/corrplotbypage2"))

saveRDS(corrplotbygsp, paste0(filekey[filekey$var_name=="stmpaper_figures",]$filepath, "/corrplotbygsp2"))
        
corrplotbypage <- readRDS(paste0(filekey[filekey$var_name=="stmpaper_figures",]$filepath, "/corrplotbypage2"))

corrplotbypage <- corrplotbypage + scale_x_discrete(labels=c("Split Jurisdiction", 
                                                             "Has Multiagency GSA", 
                                                             "Basin Priority Low/Very Low",
                                                             "Basin Priority Medium",
                                                             "Basin Population",
                                                             "Hydroclimate Pressure",
                                                             "Drinking Water Contamination",
                                                             "% DAC Population",
                                                             "GDE Habitat",
                                                             "Agr. Share of Basin GDP")) +
   scale_y_discrete(labels = c("Has Multiagency GSA", 
                               "Basin Priority Low/Very Low",
                               "Basin Priority Medium",
                               "Basin Population",
                               "Hydroclimate Pressure",
                               "Drinking Water Contamination",
                               "% DAC Population",
                               "GDE Habitat",
                               "Agr. Share of Basin GDP",
                               "Republican Vote Share"))

ggsave("predictor_predictor_corrplot_by_page2.png",plot = corrplotbypage, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
       width = 6030, height = 3835, dpi = 400, units = "px", bg = "white")


corrplotbygsp <- readRDS(paste0(filekey[filekey$var_name=="stmpaper_figures",]$filepath, "/corrplotbygsp2"))

corrplotbygsp <- corrplotbygsp + scale_x_discrete(labels=c("Split Jurisdiction", 
                                                             "Has Multiagency GSA", 
                                                             "Basin Priority Low/Very Low",
                                                             "Basin Priority Medium",
                                                             "Basin Population",
                                                             "Hydroclimate Pressure",
                                                             "Drinking Water Contamination",
                                                             "% DAC Population",
                                                             "GDE Habitat",
                                                             "Agr. Share of Basin GDP")) +
   scale_y_discrete(labels = c("Has Multiagency GSA", 
                               "Basin Priority Low/Very Low",
                               "Basin Priority Medium",
                               "Basin Population",
                               "Hydroclimate Pressure",
                               "Drinking Water Contamination",
                               "% DAC Population",
                               "GDE Habitat",
                               "Agr. Share of Basin GDP",
                               "Republican Vote Share"))


ggsave("predictor_predictor_corrplot_by_gsp2.png",plot = corrplotbygsp, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
       width = 6030, height = 3835, dpi = 400, units = "px", bg = "white")
