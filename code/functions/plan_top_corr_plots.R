plan_top_corr_plots <- function(model, inputs, topics_of_interest, categ){
   
   #grid of topic percent by gsp
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
   ggsave("theta_by_gsp.png",plot = topic_theta_plot, device = "png", path = "figures",
          width = 4422, height = 2079, dpi = 300, units = "px", bg = "white")
   
   theta <- as_tibble(model$theta)
   dig_max <- max(nchar(colnames(theta)))
   colnames(theta) <- paste0("Topic_",
                             sapply(colnames(theta),function(x)strrep("0",dig_max-nchar(x))),
                             str_remove(colnames(theta),"V"))
   theta <- add_column(theta,"approval" = inputs$meta$approval)
   theta_ag <- aggregate(. ~approval, mean, data = theta)
   theta_long <- melt(theta_ag) %>% filter(variable %in% c("Topic_03","Topic_35","Topic_14","Topic_26","Topic_21","Topic_24","Topic_49","Topic_45","Topic_37"))
   
   approval_theta_plot <- ggplot(theta_long, aes(x = approval, y = variable))+
      geom_raster(aes(fill=value))+
      scale_fill_scico(palette = "tokyo",direction = -1,name = "Proportion\nof GSP\nallocated\nto this\ntopic\n(0-1)")+
      labs(x="GSP ID", y = "Topic", title = "Estimated Proportion of Topics in Each GSP")+
      theme_classic()+theme(axis.text.x=element_text(size=9, angle = 80, vjust = 0.4),
                            axis.text.y=element_text(size=10),
                            plot.title=element_text(size=13,hjust = 0.5))
   
   approval_theta_plot
   ggsave("approval_by_gsp.png",plot = approval_theta_plot, device = "png", path = "figures",
          width = 4422, height = 2079, dpi = 300, units = "px", bg = "white")
}
   

