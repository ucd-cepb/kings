#TODO finish filekey on this script
   library(scico)
   library(ggcorrplot)
   library(GGally)
  
      #NIFA paper 1 topic labels 
      topic_labels <- c("EJ - Topic 2 Stakeholder Engagement",
                        "CC - Topic 7 Climate Scenarios",
                        "DW - Topic 10 Water Toxicity",
                        "GDE - Topic 14 Habitats",
                        "DW - Topic 16 Water Servicing",
                        "CC - Topic 24 Water Hydrology and Change",
                        "DW_EJ - Topic 25 Beneficial Users",
                        "DW - Topic 26 Contamination Protocol",
                        "GDE - Topic 29 Species",
                        "DW - Topic 30 Monitoring"
                        )

      topic_labels <- c("DW - Topic 10+16+25+26+30",
                        "GDE - Topic 14 Habitats",
                        "EJ - Topic 2 Stakeholder Engagement",
                        "CC - Topic 24 Water Hydrology and Change",
                        "DW_EJ - Topic 25 Beneficial Users",
                        "GDE - Topic 29 Species",
                        "CC - Topic 7 Climate Scenarios"
      )

   tblfilename <- filekey[filekey$var_name=="effect_table_condensed_csvs_stmpaper",]$filepath
   tblfilenamesplits <- unlist(strsplit(tblfilename,split="/"))
   tblpath <- paste(tblfilenamesplits[1:(length(tblfilenamesplits)-1)],collapse = "/")
   tblpattern <- tblfilenamesplits[length(tblfilenamesplits)]
   
   tbl_list <- list.files(path = tblpath,
                       pattern = tblpattern, full.names = T)
   cor_tbls <- vector("list",length(tbl_list))
   for(i in 1:length(tbl_list)){
      cor_tbls[[i]] <- read.csv(tbl_list[i])
   }
   names(cor_tbls) <- topic_labels
   cor_tbls <- cor_tbls[order(names(cor_tbls))]
   pal <- c(scico(3, palette = "vik") )
   
   cor_mat <- matrix(unlist(sapply(cor_tbls, `[`, , 2)),ncol=length(cor_tbls),byrow = F)
   rownames(cor_mat) <- t(cor_tbls[[1]][1])
   colnames(cor_mat) <- names(cor_tbls)
   max_corr <- max(abs(cor_mat))
   max_corr <- ceiling(max_corr*1000)/1000+
      ifelse(ceiling(max_corr*1000)%%5==0,0,(5-ceiling(max_corr*1000)%%5)/1000)
   min_corr <- max_corr*-1
   corrplot <- ggcorrplot(cor_mat,
                          colors = pal)+
      theme(axis.text.x = element_text(size = 20),  # Order: top, right, bottom, left
            axis.text.y = element_text(size = 20))+
      labs(title = "Correlation between Topic Prevalence and Predictors",
           x="Predictors", y = "Topics")+
      theme_classic()+theme(axis.text.x=element_text(size=20, angle = 80, vjust = 1, hjust = 1),
                            axis.text.y=element_text(size=20),
                            axis.title.x=element_text(size=24,margin=margin(t=5)),
                            axis.title.y=element_text(size=24,hjust = 0.5),
                            plot.title=element_text(size=36,hjust = 0.5))+
      scale_fill_gradient2(low = pal[1], high = pal[3], 
                           mid = pal[2], midpoint = 0, limit = c(min_corr, max_corr), 
                           space = "Lab",
                           name = "Corr", na.value="white")+
      theme(legend.key=element_rect(colour="black",fill="white",
                                    linetype="solid")) 
   
   ggsave("predictor_top_corrplot.png",plot = corrplot, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
          width = 6030, height = 2835, dpi = 300, units = "px", bg = "white")
