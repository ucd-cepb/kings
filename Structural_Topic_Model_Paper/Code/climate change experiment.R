packs <- c('stm')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

gspoutfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
gspoutfilenamesplits <- unlist(strsplit(gspoutfilename,split="/"))
gspoutpath <- paste(gspoutfilenamesplits[1:(length(gspoutfilenamesplits)-1)],collapse = "/")
gspoutpattern <- gspoutfilenamesplits[length(gspoutfilenamesplits)]

slam_files <- list.files(path = gspoutpath, pattern = gspoutpattern, full.names = T)

newest_slam_file <- slam_files[which.max(file.info(slam_files)$mtime)]
gsp_out <- readRDS(newest_slam_file)

gsp_out$meta$priority_category <- as.factor(gsp_out$meta$priority_category)

form <- ~ admin + 
   basin_plan +
   sust_criteria +
   monitoring_networks + 
   projects_mgmt_actions + 
   mult_gsas +
   priority_category +
   basin_population_log_scaled +
   Agr_Share_Of_GDP_scaled *log_well_MCL_exceedance_count_by_log_pop_scaled +
   Agr_Share_Of_GDP_scaled *percent_dac_by_pop_scaled +
   Agr_Share_Of_GDP_scaled *fract_of_area_in_habitat_log_scaled+
   Agr_Share_Of_GDP_scaled *dsci_scaled+
   Republican_Vote_Share_scaled *log_well_MCL_exceedance_count_by_log_pop_scaled +
   Republican_Vote_Share_scaled *percent_dac_by_pop_scaled +
   Republican_Vote_Share_scaled *fract_of_area_in_habitat_log_scaled+
   Republican_Vote_Share_scaled *dsci_scaled
   
   
   
       


topic7<- estimateEffect(c(7) ~ admin + 
                           basin_plan +
                           sust_criteria +
                           monitoring_networks + 
                           projects_mgmt_actions + 
                           mult_gsas +
                           priority_category +
                           basin_population_log_scaled +
                           Agr_Share_Of_GDP_scaled + Republican_Vote_Share_scaled + 
                           log_well_MCL_exceedance_count_by_log_pop_scaled + 
                               percent_dac_by_pop_scaled + 
                               fract_of_area_in_habitat_log_scaled +
                               dsci_scaled,
                            model,
                            metadata = gsp_out$meta, uncertainty = "Global")
gsp_text_with_meta <- readRDS(file = filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)

gsp_text_with_meta$version_approval20230922 <- gsp_text_with_meta$version_approval

tps <- readRDS(filekey[filekey$var_name=="topic_prevalence",]$filepath)


modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

finfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
version_select <- which(finfo$mtime==max(finfo$mtime))

model <- readRDS(rownames(finfo)[version_select])


   
library(DescTools)
gsp_text_with_meta_full <- gsp_text_with_meta
sum(is.na(gsp_text_with_meta_full$text))
gsp_text_with_meta <- gsp_text_with_meta[!is_comment & !is_reference,]
climchange_count <- stringr::str_count(pattern = "climate change", string = gsp_text_with_meta$text)
budget_count <- stringr::str_count(pattern = "water budget", string = gsp_text_with_meta$text)

climchange_count <- ifelse(is.na(climchange_count), 0, climchange_count)
budget_count <- ifelse(is.na(budget_count),0, budget_count)
gsp_text_with_meta$climchange_count <- climchange_count
gsp_text_with_meta$budget_count <- budget_count

gspmini <- gsp_text_with_meta |> dplyr::group_by(gsp_id) |> dplyr::summarise(
   Republican_Vote_Share_scaled = mean(Republican_Vote_Share_scaled), 
   sumclimchange = sum(climchange_count, na.rm=T),
   sumbudget = sum(budget_count, na.rm=T),
   dsci_scaled = mean(dsci_scaled),
   Agr_Share_Of_GDP_scaled = mean(Agr_Share_Of_GDP_scaled),
   num_pages = max(page_num),
   version_approval20230922 = Mode(version_approval20230922),
   Perc_Bach_Degree_Over25_scaled = mean(Perc_Bach_Degree_Over25_scaled),
   priority_category = Mode(priority_category))

gspmini$meanclimchange <- gspmini$sumclimchange/gspmini$num_pages
gspmini$meanbudget <- gspmini$sumbudget/gspmini$num_pages
gspmini$version_approval20230922 <- factor(gspmini$version_approval20230922)

gspmini <- gspmini[!gspmini$gsp_id %in% c("0053","0089"),]
gspmini$tpsV7 <- tps$V7
gspmini$tpsV24 <- tps$V24

ggplot(gspmini, aes(x = tps$V7,
                    y = meanclimchange, color = Republican_Vote_Share_scaled)) + 
   geom_point() + geom_smooth(method=lm) +
   scale_color_viridis_c()

ggplot(allpages_meta, aes(x = prevV7,
                          y = climchange_count, color = Republican_Vote_Share_scaled)) + 
   geom_point() + geom_smooth(method=lm) +
   scale_color_viridis_c()

cor.test(allpages_meta$prevV7, allpages_meta$Republican_Vote_Share_scaled)

ggplot(gspmini, aes(x = version_approval20230922,
                    y = tpsV24, color = meanclimchange)) + 
   geom_boxplot() + geom_smooth(method=lm) +
   scale_color_viridis_c()

ggplot(allpages_meta, aes(x = prevV7,
                          y = Republican_Vote_Share_scaled, color = climchange_count)) + 
   geom_point() + geom_smooth(method=lm) +
   scale_color_viridis_c()

ggplot(gspmini, aes(x = version_approval20230922,
                          y = gspmini$tps$V7, color = climchange_count)) + 
   geom_boxplot() + geom_smooth(method=lm) +
   scale_color_viridis_c()

ggplot(gspmini, aes(x = dsci_scaled,
                    y = Republican_Vote_Share_scaled, color = tps$V7)) + 
   geom_point() + geom_smooth(method=lm) +
   scale_color_viridis_c()

cor.test(allpages_meta$climchange_count, allpages_meta$Republican_Vote_Share_scaled)




cor.test(gspmini$meanclimchange, gspmini$Republican_Vote_Share_scaled)
prev <- as.data.frame(model$theta)

gsp_out$meta$prevV7 <- prev$V7
gsp_out$meta$prevV24 <- prev$V24

allpages_meta <- dplyr::left_join(gsp_out$meta, gsp_text_with_meta)

cor.test(allpages_meta$prevV7, allpages_meta$dsci_scaled)
cor.test(allpages_meta$prevV7, allpages_meta$drywellcount2014_2020)
cor.test(allpages_meta$prevV7, allpages_meta$max2040dryspell)
cor.test(allpages_meta$prevV24, allpages_meta$dsci_scaled)
cor.test(allpages_meta$prevV24, allpages_meta$drywellcount2014_2020)
cor.test(allpages_meta$prevV24, allpages_meta$max2040dryspell)

cor.test(allpages_meta$Republican_Vote_Share_scaled, allpages_meta$drywellcount2014_2020)
cor.test(allpages_meta$Republican_Vote_Share_scaled, allpages_meta$dsci_scaled)
cor.test(allpages_meta$Republican_Vote_Share_scaled, allpages_meta$max2040dryspell)

cor.test(allpages_meta$prevV7, allpages_meta$drywellcount2014_2020 * allpages_meta$Republican_Vote_Share_scaled)
cor.test(allpages_meta$prevV7, allpages_meta$dsci_scaled * allpages_meta$Republican_Vote_Share_scaled)
cor.test(allpages_meta$prevV7, allpages_meta$max2040dryspell * allpages_meta$Republican_Vote_Share_scaled)
cor.test(allpages_meta$prevV24, allpages_meta$drywellcount2014_2020 * allpages_meta$Republican_Vote_Share_scaled)
cor.test(allpages_meta$prevV24, allpages_meta$dsci_scaled * allpages_meta$Republican_Vote_Share_scaled)
cor.test(allpages_meta$prevV24, scale(allpages_meta$max2040dryspell) * allpages_meta$Republican_Vote_Share_scaled)


cor.test(allpages_meta$prevV7, allpages_meta$Republican_Vote_Share_scaled)
cor.test(gspmini$tps$V7, gspmini$Republican_Vote_Share_scaled)


cor.test(gspmini$tps$V24, gspmini$Republican_Vote_Share_scaled)
cor.test(allpages_meta$prevV24, allpages_meta$Republican_Vote_Share_scaled)

#chosen for high % Republican and high number of CC mentions
cctexts <- allpages_meta[c(72936,72985,35628,35633, 96744, 78434, 46514, 108028, 108034, 116395, 24077, 
                                118745, 111685, 117437, 74042, 45123, 116371, 116379, 116387,
                                29839, 74042, 70247),]$text

allpages_meta[c(72936,72985,35628,35633, 96744, 78434, 46514, 108028, 108034, 116395, 24077, 
                118745, 111685, 117437, 74042, 45123, 116371, 116379, 116387,
                29839, 74042, 70247),]$Republican_Vote_Share_scaled

allpages_meta[c(72936,72985,35628,35633, 96744, 78434, 46514, 108028, 108034, 116395, 24077, 
                118745, 111685, 117437, 74042, 45123, 116371, 116379, 116387,
                29839, 74042, 70247),]$dsci_scaled
#last row chosen for high %topic 7

ggplot(gspmini, aes(x = tps$V7,
                    y = Republican_Vote_Share_scaled, color = meanclimchange)) + 
   geom_point() + geom_smooth(method=lm) +
   scale_color_viridis_c()

ggplot(gspmini, aes(x = meanclimchange,
                    y = Republican_Vote_Share_scaled, color = tps$V7)) + 
   geom_point() + geom_smooth(method=lm) +
   scale_color_viridis_c()

whichpageshaveCCtopics <- allpages_meta$prevV24 > 0.15 | allpages_meta$prevV7 > 0.15

gsp_out$documents
gsp_out$documents <- gsp_out$documents[whichpageshaveCCtopics]
gsp_out$meta <- gsp_out$meta[whichpageshaveCCtopics,]

words <- unique(unlist(lapply(gsp_out$documents, function (i) i[1,])))
mydf <- data.frame("originalnumber" = words, "newnumber" = words)
mydf <- mydf[order(mydf$originalnumber),]
mydf$newnumber <- 1:nrow(mydf)

library(plyr)

for(k in 1:length(gsp_out$documents)){
   gsp_out$documents[[k]][1,] <- mapvalues(gsp_out$documents[[k]][1,], 
                                           from = mydf$originalnumber,
                                           to = mydf$newnumber)
}


gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 K = 0,
                 content = Republican_Vote_Share_scaled,
                 init.type = "Spectral",
                 data = gsp_out$meta)

