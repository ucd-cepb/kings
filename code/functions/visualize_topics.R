visualize_topics <- function(model, inputs, text_col, topic_indicators){

   packs <- c('ggplot2','ggrepel','scico','stm','tidyverse','reshape2',
              'igraph','huge','fields','ggcorrplot')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   
   numTopics = model$settings$dim$K
   
   label_lg <- labelTopics(model, topics = c(1:numTopics), n = 50)
   label_sm <- labelTopics(model, topics = c(1:numTopics), n = 10)
   #sageLabels can be used when the model has a content covariate
   #both print highest dprobability words and FREX words associated with each topic
   #FREX is weighted by frequency and exclusivity
   #lift() weights words higher if they have lower frequency in other topics
   #score() is similar, but log based -- see lda package
   
   #inspect words associated with topics using labelTopics
   for(i in 1:numTopics){
      print(paste0("Top 10 FREX Words in Topic ", i, ":", collapse = ""))
      print(label_sm$frex[i,])
   }
   
   topics_of_interest<- NULL
   for(i in 1:numTopics){
      
      if(sum(grepl(pattern = paste(gsub("\\s+","_", x=unlist(topic_indicators,use.names=F)),collapse="|"), x=label_lg$frex[i,]))>0){
         print(paste0("Top 50 FREX Words in Topic ", i, ":", collapse = ""))
         print(label_lg$frex[i,])
         topics_of_interest<-append(topics_of_interest,paste0("Topic_",strrep("0",2-nchar(toString(i))),i))
         
      }
   }
   
   
   
   #TODO gratitude to francescoaberlin.blog for this tutorial
   #topics are evaluated on two components:
   #semantic coherence (frequency of co-occurrence of common words in a toipc)
   #exclusivity of words to topic
   m6_ex_sem<-as.data.frame(cbind(c(1:numTopics),
                                 exclusivity(model), 
                                 semanticCoherence(model=model, 
                                                   documents = inputs$documents), "model 6"))
   
   #can compare multiple models by adding to rbind
   models_ex_sem<-rbind(m6_ex_sem)
   
   colnames(models_ex_sem)<-c("K","exclusivity", "semantic_coherence", "model")
   models_ex_sem$exclusivity<-as.numeric(as.character(models_ex_sem$exclusivity))
   models_ex_sem$semantic_coherence<-as.numeric(as.character(models_ex_sem$semantic_coherence))
   
   options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
   
   topic_qual_plot<-ggplot(models_ex_sem, aes(semantic_coherence, 
                                              exclusivity, color = model))+
      geom_point(size = 2, alpha = 1) + 
      geom_text_repel(aes(label=K), nudge_x=.005, nudge_y=.005, 
                      size = 2.5, alpha = 0.6, force = 0.8, force_pull = 1.5,
                      max.overlaps = Inf)+
      labs(x = "Semantic coherence",
           y = "Exclusivity",
           title = "Comparing exclusivity and semantic coherence among model topics")+
      scale_color_scico_d(palette = "nuuk")+
      theme_minimal()+theme(plot.title = element_text(hjust = 0.5))
   topic_qual_plot
   ggsave("topic_quality_model_6.png",plot = topic_qual_plot, device = "png", path = "figures",
         width = 4020, height = 1890, dpi = 300, units = "px", bg = "white")
   
   
   #dim(readPNG("figures/topic_quality"))
   
   text_vect <- unlist(text_col)
   #testing if text_col includes docs.removed and removes them if applicable
   if (length(text_vect) != length(inputs$documents)){
      text_vect <- text_vect[-inputs$docs.removed]
   }
   
   #findThoughts example
   thoughts <- findThoughts(model, texts = text_vect, topics = c(1:numTopics), n = 3)
   topics = c(1:numTopics)
   #plotQuote is a graphical wrapper to help present documents as examples
   #example:
   thought_5 <- findThoughts(model, texts = text_vect, topics = 5, n = 3)
   plotQuote(thought_5, width = 30, main = "Topic 5")
   
   #TODO clean this up
   plot(model, type = "summary", xlim = c(0,0.3))

   #tagging pages as a topic
   theta <- as_tibble(model$theta)
   tags <- rep(NA, nrow(theta))
   for(i in 1:numTopics){
      for(j in 1:nrow(theta)){
         if(theta[[i]][j]>=0.7){
            tags[j] <- paste0("topic",i,collapse = "")
         }
      }
   }
   
   #grid of topic percent by gsp
   theta <- as_tibble(model$theta)
   dig_max <- max(nchar(colnames(theta)))
   colnames(theta) <- paste0("Topic_",
      sapply(colnames(theta),function(x)strrep("0",dig_max-nchar(x))),
      str_remove(colnames(theta),"V"))
   theta <- add_column(theta,"gsp_id" = inputs$meta$gsp_id)
   theta_ag <- aggregate(. ~gsp_id, mean, data = theta)
   theta_long <- melt(theta_ag) 
   
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
   theta_long <- melt(theta_ag) %>% filter(variable %in% c("Topic_41","Topic_03","Topic_16"))
   
   approval_theta_plot <- ggplot(theta_long, aes(x = approval, y = variable))+
      geom_raster(aes(fill=value))+
      scale_fill_scico(palette = "tokyo",direction = -1,name = "Proportion\nof GSP\nallocated\nto this\ntopic\n(0-1)")+
      labs(x="GSP ID", y = "Topic", title = "Estimated Proportion of Topics in Each GSP")+
      theme_classic()+theme(axis.text.x=element_text(size=9, angle = 80, vjust = 0.4),
                            axis.text.y=element_text(size=10),
                            plot.title=element_text(size=13,hjust = 0.5))
   
   approval_theta_plot
   ggsave("theta_by_gsp.png",plot = topic_theta_plot, device = "png", path = "figures",
          width = 4422, height = 2079, dpi = 300, units = "px", bg = "white")
   
   #TODO see vignette for authors
   tcs <- topicCorr(model, method = "simple", cutoff = 0.01, verbose = TRUE)
   tch <- topicCorr(model, method = "huge", verbose = TRUE)
   nums_of_interest <- sapply(X = topics_of_interest, function(x)as.integer(substr(x,7,nchar(x))))
   tch_cor <- tch$cor[nums_of_interest,nums_of_interest] 
   tcs_cor <- tcs$cor[nums_of_interest,nums_of_interest] 
   colnames(tcs_cor) <- as.list(topics_of_interest)
   rownames(tcs_cor) <- as.list(topics_of_interest)
   #TODO create category tags and ggplot based on those tags
   
   topics <- 1:nrow(tch$posadj)
   tch_pos <- tch$posadj[topics, topics]
   tch_pos_subset <- tch$posadj[nums_of_interest,nums_of_interest]
   tcs_pos_subset <- tch$posadj[nums_of_interest,nums_of_interest]
   tch_cor_subset <- tch$cor[nums_of_interest,nums_of_interest]
   tcs_cor_subset <- tcs$cor[nums_of_interest,nums_of_interest]
   g <- igraph::graph.adjacency(tch_pos, mode="undirected", weighted=TRUE, diag=FALSE)
   gh_subset <- igraph::graph.adjacency(tch_pos_subset, mode="undirected", weighted=TRUE, diag=FALSE)
   gs_subset <- igraph::graph.adjacency(tcs_pos_subset, mode="undirected", weighted=TRUE, diag=FALSE)
   
   igraph::E(g)$size <- 1
   igraph::E(g)$lty <- 2
   igraph::E(g)$color <- "black"
   igraph::V(g)$label <- V(g)
   igraph::E(gh_subset)$size <- 1
   igraph::E(gh_subset)$lty <- 2
   igraph::E(gh_subset)$color <- "black"
   igraph::V(gh_subset)$label <- nums_of_interest
   igraph::E(gs_subset)$size <- 1
   igraph::E(gs_subset)$lty <- 2
   igraph::E(gs_subset)$color <- "black"
   igraph::V(gs_subset)$label <- nums_of_interest
   
   layout <- igraph::layout.fruchterman.reingold
   num_neighb <- sapply(1:length(topics),function(x)length(neighbors(g,x)))
   num_neighbh_subset <- sapply(1:length(nums_of_interest),function(x)length(neighbors(gh_subset,x)))
   num_neighbs_subset <- sapply(1:length(nums_of_interest),function(x)length(neighbors(gs_subset,x)))
   
   color_vect <- viridis(max(num_neighb)+1)
   txt_vect_sm <- c("#000000","#FFFFFF")
   txt_invert <- ifelse(num_neighb < max(num_neighb)/2,2,1)
   
   set.seed(3)
   igraph::plot.igraph(g, layout=layout, vertex.color=color_vect[num_neighb+1], vertex.label.cex=0.75, 
                       vertex.label.color=txt_vect_sm[txt_invert], vertex.size=8)
   title("Network of Positively Correlated Topics")
   #Here we create a sample function on the vertices of the graph
   
   image.plot(legend.only=T, zlim=range(0:max(num_neighb)), col=color_vect,
              legend.lab="Number of Neighbors")
   
   category_vect <- viridis(5)
   cat_txt_vect <- c("#000000","#000000","#FFFFFF","#FFFFFF","#FFFFFF")
   
   is_cc <- vector(length=0)
   is_ej <- vector(length=0)
   is_dw <- vector(length=0)
   is_gde <- vector(length=0)
   for(i in 1:numTopics){
      is_cc[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", x=topic_indicators[["cc"]]),
                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      is_ej[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", topic_indicators[["ej"]]),
                                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      is_dw[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", topic_indicators[["dw"]]),
                                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      is_gde[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", topic_indicators[["gde"]]),
                                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      
   }
   is_multi <- ifelse(is_cc+is_ej+is_dw+is_gde >1,T,F)
   
   categ <- case_when(is_multi ~ "Multi",
                      is_cc ~ "CC",
                      is_ej ~ "EJ",
                      is_dw ~ "DW",
                      is_gde ~ "GDE")

   category_vect <- viridis(5)
   cat_color_h <- category_vect[as.numeric(as.factor(V(gh_subset)$category))]
   cat_color_s <- category_vect[as.numeric(as.factor(V(gs_subset)$category))]
   
   # Make the plot
   txt_vect <- c("#FFFFFF","#FFFFFF","#000000","#000000","#000000")
   txt_color_h <- txt_vect[as.numeric(as.factor(V(gh_subset)$category))]
   txt_color_s <- txt_vect[as.numeric(as.factor(V(gs_subset)$category))]
   
   V(gh_subset)$category <- categ[!is.na(categ)]#contain key words?
   V(gs_subset)$category <- categ[!is.na(categ)]#contain key words?
   set.seed(3)
   igraph::plot.igraph(gh_subset, layout=layout, vertex.color=cat_color_h,
                       vertex.label.cex=0.75, 
                       vertex.label.color=txt_color_h, vertex.size=8)
   igraph::plot.igraph(gs_subset, layout=layout, vertex.color=cat_color_s,
                       vertex.label.cex=0.75, 
                       vertex.label.color=txt_color_s, vertex.size=8)
   
   legend('topleft',legend = levels(as.factor(V(gh_subset)$category)), 
          pt.cex = 2, pch = 21, cex = 1.2, pt.bg = category_vect,
          inset = c(0,0))
   legend('topleft',legend = levels(as.factor(V(gs_subset)$category)), 
          pt.cex = 2, pch = 21, cex = 1.2, pt.bg = category_vect,
          inset = c(0,0))
   title("Network of Positively Correlated Topics of Interest")
   
    #TODO tidystm
   
   #sets scale to nearest multiple of .05
   max_corr <- max(abs(grid_mini)[abs(grid_mini)!=1])
   max_corr <- ceiling(max_corr*100)/100+
      ifelse(ceiling(max_corr*100)%%5==0,0,(5-ceiling(max_corr*100)%%5)/100)
   min_corr <- max_corr*-1
   #grid of topic percent by gsp
   
   topic_cor_grid <- tcs$cor
   colnames(topic_cor_grid) <- paste0("Topic_",
                                      sapply(1:numTopics,function(x)strrep("0",2-nchar(toString(x)))),
                                      1:numTopics)
   rownames(topic_cor_grid) <- paste0("Topic_",
                                      sapply(1:numTopics,function(x)strrep("0",2-nchar(toString(x)))),
                                      1:numTopics)
   grid_mini <- topic_cor_grid[,topics_of_interest]
   colnames(grid_mini) <- paste0(categ[!is.na(categ)],colnames(grid_mini))
   grid_mini <- grid_mini[,sort(colnames(grid_mini))]
   ggcorrplot(grid_mini,
              colors = c(scico(3, palette = "vik") ))+
      theme(axis.text.x = element_text(size = 8),  # Order: top, right, bottom, left
            axis.text.y = element_text(size = 11))+
      labs(title = "Topic Correlation among Topics of Interest")+
      theme_classic()+theme(axis.text.x=element_text(size=9, angle = 80, vjust = 0.4),
                            axis.text.y=element_text(size=10),
                            plot.title=element_text(size=13,hjust = 0.5))+
      scale_fill_gradient2(low = colors[1], high = colors[3], 
                           mid = colors[2], midpoint = 0, limit = c(min_corr, max_corr), 
                           space = "Lab",
                           name = "Corr", na.value="white")+
      geom_vline(aes(xintercept=0, color="Same Topic"))+
      guides(color=guide_legend(title=NULL, colour = "black",
                                override.aes=list(color="#00000000")))+
      theme(legend.key=element_rect(colour="black",fill="white",
                                    linetype="solid"))     
   #TODO stminsights
   #TODO stmBrowser
   #TODO stmCorrViz, including function toLDAvis, which enables export to the LDAvis
   #TODO stmprinter
   
   #look at the relationship between metadata and topics
   effect <- estimateEffect(1:50 ~ admin + 
                             basin_plan +
                             sust_criteria +
                             monitoring_networks + 
                             projects_mgmt_actions +
                             percent_dac_by_pop+
                             as.factor(approval) +
                             as.factor(priority)+ 
                             mult_gsas+
                             ag_gw_asfractof_tot_gw, 
                          model,
                          meta = inputs$meta, uncertainty = "None")
   summary(effect, topics = c(1:50))
   plot.estimateEffect(effect,covariate = "ag_gw_asfractof_tot_gw")
   #calculate topic correlations: topicCorr
}
