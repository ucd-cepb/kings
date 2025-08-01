filekey <- read.csv("filekey.csv")
#relies on a mod of stm::topicCorr that only uses a single plan as the data
#see Structural_Topic_Model_Paper/Code/step13_generate_gspwise_topiccorrnet.R

topiccorrs <- list.files(path = "data/Structural_Topic_Model_Paper/topic_networks",
                         pattern = "topic_network", full.names = T)


gspids <- stringr::str_remove(topiccorrs, "data/Structural_Topic_Model_Paper/topic_networks/topic_network_")

library(igraph)
library(ggplot2)
library(viridis)
library(ggraph)
library(stringr)

for(m in 1:length(topiccorrs)){
   topiccorr <- readRDS(topiccorrs[m])
   #adapted from kings/Structural_Topiic_Model_Paper/Code/utils/top_top_corr_plots.R
   #which is not used in the final paper
   
   for(i in 1:nrow(topiccorr$cor)){
      topiccorr$cor[i,i] = 1
   }
   saveRDS(topiccorr, paste0("data/Innovation_Paper/topic_correlation_networks/net_",gspids[m]))
   topics <- 1:nrow(topiccorr$posadj)
   tch_cor <- topiccorr$cor[topics,topics]
   tch_pos <- topiccorr$posadj[topics, topics]
   rownames(tch_pos) <- topics
   colnames(tch_pos) <- topics
   
   g <- igraph::graph_from_adjacency_matrix(tch_pos, mode = "undirected", weighted = T, diag = F,
                                            add.colnames = "topic_col", 
                                            add.rownames = "topic_row")
   g <- set_vertex_attr(g, name = "degr", value = degree(g))
   #num_neighb <- sapply(1:length(topics), function(x) length(neighbors(g, x)))
   #color_vect <- viridis(max(num_neighb)+1)
   #txt_vect_sm <- c("#000000","#FFFFFF")
   #txt_invert <- ifelse(num_neighb < max(num_neighb)/2,2,1)
   set.seed(3)
   
   myplot <- ggraph(g, layout = 'fr')+
      #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
      #Using Paul Tol color schemes
      #scale_edge_color_manual(values = c("#88CCEE","#999933","#CC6677"))+
      geom_edge_link(
                    alpha = 1, 
                    end_cap = circle(1,"mm"),
                    width = 0.4)+
      #tol_high-contrast color scheme  "#DDAA33" is the GPE color
      scale_color_viridis_b(nice.breaks = T)+
      geom_node_point(aes(color = degr), alpha = 1, size = 3)+
      geom_node_text(aes(label = topic_col), size=2, repel = T, max.overlaps=30) +
      theme_void()+ theme(legend.position.inside = c(0.8,0.6)) +
      ggtitle(paste0("Network of Positively Correlated Topics in GSP ", gspids[m]))
   ggsave(paste0("topic_correlation_gsp",gspids[m],".png"), plot = myplot, device = "png",
          path = "data/Innovation_Paper/topic_correlation_plots", width = 2200, height = 1890, dpi = 300,
          units = "px", bg = "white")
   
   
   
}





####


