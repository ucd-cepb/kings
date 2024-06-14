plan_top_prev_plots <- function(model, inputs, topics_of_interest, categ){
   
   filekey <- read.csv("filekey.csv")
   
   
   
   library(scico)
   #creates grid plot of topic percent by gsp
   theta <- as_tibble(model$theta)
   dig_max <- max(nchar(colnames(theta)))
   colnames(theta) <- paste0("Topic_",
                             sapply(colnames(theta),function(x)strrep("0",dig_max-nchar(x))),
                             str_remove(colnames(theta),"V"))
   theta <- add_column(theta,"gsp_id" = inputs$meta$gsp_id)
   theta_ag <- aggregate(. ~gsp_id, mean, data = theta)
   theta_long <- melt(theta_ag) 
   
   #TODO logarithmic scale and set 2% = white
   #sort by least to most common topic
   #sort GSP_ID by center of gravity
   #sort y axis and then cg
   #colnames -- Topic_XX melts to variable
   topic_theta_plot <- ggplot(theta_long, aes(x = gsp_id, y = variable))+
      geom_raster(aes(fill=value))+
      scale_fill_scico(palette = "tokyo",direction = -1,name = "Proportion\nof GSP\nallocated\nto this\ntopic\n(0-1)")+
      labs(x="GSP ID", y = "Topic", title = "Estimated Proportion of Topics in Each GSP")+
      theme_classic()+theme(axis.text.x=element_text(size=9, angle = 80, vjust = 0.4),
                            axis.text.y=element_text(size=10),
                            plot.title=element_text(size=13,hjust = 0.5))
   
   topic_theta_plot
   ggsave("theta_by_gsp.png",plot = topic_theta_plot, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
          width = 4422, height = 2079, dpi = 600, units = "px", bg = "white")
   
   #TODO update this by adding approval
   theta <- as_tibble(model$theta)
   dig_max <- max(nchar(colnames(theta)))
   colnames(theta) <- paste0("Topic_",
                             sapply(colnames(theta),function(x)strrep("0",dig_max-nchar(x))),
                             str_remove(colnames(theta),"V"))
   
   #repair approval status from meta with new approval scraping
   #no longer necessary with new gsp_text_with_meta file from May 2024
   #newscrapefilename <- filekey[filekey$var_name=="gsp_web_vars_planevolutionpaper",]$filepath
   #newscrapefilenamesplits <- unlist(strsplit(newscrapefilename,split="/"))
   #newscrapepath <- paste(newscrapefilenamesplits[1:(length(newscrapefilenamesplits)-1)],collapse = "/")
   #newscrapepattern <- newscrapefilenamesplits[length(newscrapefilenamesplits)]
   
   
   
   #newscrape <- readRDS(list.files(newscrapepath,pattern=newscrapepattern,full.names=T)[length(
   #   list.files(newscrapepath,pattern=newscrapepattern))])
   #newscrape <- newscrape[newscrape$version==1,]
   #newscrape <- as.data.frame(cbind(gsp_id = newscrape$gsp_num_id, repairedapproval = newscrape$version_approval))
   
   #mymeta <- dplyr::left_join(inputs$meta, newscrape)
   
   theta <- add_column(theta,"approval" = inputs$meta$version_approval)
   theta_ag <- aggregate(. ~approval, mean, data = theta)
   theta_long <- melt(theta_ag) %>% filter(variable %in% c("Topic_02","Topic_07","Topic_10","Topic_14","Topic_16","Topic_24","Topic_25","Topic_26","Topic_29","Topic_30"))
   
   approval_theta_plot <- ggplot(theta_long, aes(x = approval, y = variable))+
      geom_raster(aes(fill=value))+
      scale_fill_scico(palette = "tokyo",direction = -1,name = "Mean Proportion\nof Plan\nallocated\nto this\ntopic\n(0-1)\nby Approval Status")+
      labs(x="GSP ID", y = "Topic", title = "Estimated Proportion of Topics")+
      theme_classic()+theme(axis.text.x=element_text(size=9, angle = 80, vjust = 0.4),
                            axis.text.y=element_text(size=10),
                            plot.title=element_text(size=13,hjust = 0.5))
   
   saveRDS(approval_theta_plot, paste0(filekey[filekey$var_name=="stmpaper_figures",]$filepath, "/approval_theta_plot"))
   approval_theta_plot
   ggsave("approval_by_gsp.png",plot = approval_theta_plot, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
          width = 4422, height = 2079, dpi = 600, units = "px", bg = "white")
}
   

