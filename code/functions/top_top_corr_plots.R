top_top_corr_plots <- function(model){
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
   
   
   #grid of topic percent by gsp
   
   topic_cor_grid <- tch$cor
   colnames(topic_cor_grid) <- paste0("Topic_",
                                      sapply(1:numTopics,function(x)strrep("0",2-nchar(toString(x)))),
                                      1:numTopics)
   rownames(topic_cor_grid) <- paste0("Topic_",
                                      sapply(1:numTopics,function(x)strrep("0",2-nchar(toString(x)))),
                                      1:numTopics)
   grid_mini <- topic_cor_grid[,topics_of_interest]
   #sets scale to nearest multiple of .05
   max_corr <- max(abs(grid_mini)[abs(grid_mini)!=1])
   max_corr <- ceiling(max_corr*100)/100+
      ifelse(ceiling(max_corr*100)%%5==0,0,(5-ceiling(max_corr*100)%%5)/100)
   min_corr <- max_corr*-1
   colnames(grid_mini) <- paste0(categ[!is.na(categ)],colnames(grid_mini))
   grid_mini <- grid_mini[,sort(colnames(grid_mini))]
   grid_mini <- as.matrix(grid_mini)
   pal <- c(scico(3, palette = "vik") )
   ggcorrplot(grid_mini,
              colors = pal)+
      theme(axis.text.x = element_text(size = 8),  # Order: top, right, bottom, left
            axis.text.y = element_text(size = 11))+
      labs(title = "Topic Correlation among Topics of Interest",
           x="All Topics", y = "Topics of Interest")+
      theme_classic()+theme(axis.text.x=element_text(size=9, angle = 80, vjust = 0.4),
                            axis.text.y=element_text(size=10),
                            plot.title=element_text(size=13,hjust = 0.5),
                            axis.title.x=element_text(margin = margin(t=5)))+
      scale_fill_gradient2(low = pal[1], high = pal[3], 
                           mid = pal[2], midpoint = 0, limit = c(min_corr, max_corr), 
                           space = "Lab",
                           name = "Corr", na.value="white")+
      geom_vline(aes(xintercept=0, color="Same Topic"))+
      guides(color=guide_legend(title=NULL, colour = "black",
                                override.aes=list(color="#00000000")))+
      theme(legend.key=element_rect(colour="black",fill="white",
                                    linetype="solid"))  
}