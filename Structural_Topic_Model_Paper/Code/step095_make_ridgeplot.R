

library(stm)
library(data.table)
library(stringr)
filekey <- read.csv("filekey.csv")

tps <- readRDS(filekey[filekey$var_name=="topic_prevalence",]$filepath)

tps$CC <- rowSums(tps[,topic_ids[["cc"]]])
tps$DW <- rowSums(tps[,topic_ids[["dw"]]])
tps$EJ <- rowSums(tps[,topic_ids[["ej"]]])
tps$GDE <- rowSums(tps[,topic_ids[["gde"]]])
tps$gsp_id <- rownames(tps)
tps_unitopics <- tps[,c("CC","DW","EJ","GDE","gsp_id")]
tps_unitopics <- pivot_longer(tps_unitopics,
                              c("CC","DW","EJ","GDE"),
                              names_to = "category", values_to = "proportion")
tps_unitopics$category <- factor(tps_unitopics$category)
library(ggridges)
library(ggplot2)
library(viridis)
ridgeplot <- ggplot(tps_unitopics, aes(x = proportion, y = category, fill = category)) +
   geom_density_ridges() +
   scale_fill_manual(values = viridis(4)) +
   theme_ridges() +
   labs(title = 'Proportion of GSP Devoted to Each Topic Category')  +
   theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
   )
ggsave(plot = ridgeplot, path = filekey[filekey$var_name=="stmpaper_figures",]$filepath, filename = "ridgeplot.png",dpi = 450,width = 7,height = 4,units = 'in')

