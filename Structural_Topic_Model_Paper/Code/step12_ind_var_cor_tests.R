inputsfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
inputsfilenamesplits <- unlist(strsplit(inputsfilename,split="/"))
inputspath <- paste(inputsfilenamesplits[1:(length(inputsfilenamesplits)-1)],collapse = "/")
inputspattern <- inputsfilenamesplits[length(inputsfilenamesplits)]

inputs <- readRDS(list.files(path = inputspath, pattern = inputspattern, full.names = T)[
   length(list.files(path = inputspath, pattern = inputspattern, full.names = T))])

newscrapefilename <- filekey[filekey$var_name=="gsp_web_vars_planevolutionpaper",]$filepath
newscrapefilenamesplits <- unlist(strsplit(newscrapefilename,split="/"))
newscrapepath <- paste(newscrapefilenamesplits[1:(length(newscrapefilenamesplits)-1)],collapse = "/")
newscrapepattern <- newscrapefilenamesplits[length(newscrapefilenamesplits)]
newscrape <- readRDS(list.files(newscrapepath,pattern=newscrapepattern,full.names=T)[length(
   list.files(newscrapepath,pattern=newscrapepattern))])

newscrape <- newscrape[newscrape$version==1,]

newscrape <- as.data.frame(cbind(gsp_id = newscrape$gsp_num_id, repairedapproval = newscrape$version_approval))
mymeta <- dplyr::left_join(inputs$meta, newscrape)
minimeta <- unique(mymeta[,c('gsp_id', 'urbangw_af_log_scaled','percent_dac_by_pop_scaled',
                             'fract_of_area_in_habitat_log_scaled',
                             'maxdryspell_scaled',
                             'Agr_Share_Of_GDP_scaled',
                             'Republican_Vote_Share_scaled',
                             'Perc_Bach_Degree_Over25_scaled',
                             'local_govs_per_10k_people_log_scaled',
                             'mult_gsas',
                             'gwsum',
                             'repairedapproval')])
mymeta <- mymeta[,c('urbangw_af_log_scaled','percent_dac_by_pop_scaled',
                    'fract_of_area_in_habitat_log_scaled',
                    'maxdryspell_scaled',
                    'Agr_Share_Of_GDP_scaled',
                    'Republican_Vote_Share_scaled',
                    'Perc_Bach_Degree_Over25_scaled',
                    'local_govs_per_10k_people_log_scaled',
                    'mult_gsas',
                    'gwsum',
                    'repairedapproval')]
minimeta <- minimeta[,c('urbangw_af_log_scaled','percent_dac_by_pop_scaled',
                          'fract_of_area_in_habitat_log_scaled',
                          'maxdryspell_scaled',
                          'Agr_Share_Of_GDP_scaled',
                          'Republican_Vote_Share_scaled',
                          'Perc_Bach_Degree_Over25_scaled',
                          'local_govs_per_10k_people_log_scaled',
                          'mult_gsas',
                          'gwsum',
                          'repairedapproval')]
colnames(mymeta)[colnames(mymeta)=="repairedapproval"] <- "approval"
mymeta$approval <- as.factor(mymeta$approval)
colnames(minimeta)[colnames(minimeta)=="repairedapproval"] <- "approval"
minimeta$approval <- as.factor(minimeta$approval)

library(ggcorrplot)
#pal <- c(scico(3, palette = "vik") )

#max_corr <- max(abs(cor_mat))
#max_corr <- ceiling(max_corr*1000)/1000+
#   ifelse(ceiling(max_corr*1000)%%5==0,0,(5-ceiling(max_corr*1000)%%5)/1000)
#min_corr <- max_corr*-1


corrplotbypage <- ggcorrplot(cor(mymeta[,1:10]), type = "upper", lab = T)#+
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

corrplotbygsp <- ggcorrplot(cor(minimeta[,1:10]), type = "upper", lab = T)

ggsave("predictor_predictor_corrplot_by_page.png",plot = corrplotbypage, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
       width = 6030, height = 2835, dpi = 300, units = "px", bg = "white")


ggsave("predictor_predictor_corrplot_by_gsp.png",plot = corrplotbygsp, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
       width = 6030, height = 2835, dpi = 300, units = "px", bg = "white")
