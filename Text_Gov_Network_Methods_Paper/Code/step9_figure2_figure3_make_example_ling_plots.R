library(statnet)
library(intergraph)
library(latentnet)
library(udpipe)
filekey <- read.csv("filekey.csv")
s <- 'The SVBGSA and ASGSA established a Coordination Committee.'
s2 <- 'During development of the 2022 GSPs SVBGSA assessed how URCs and DACs may be engaged with the GSA.'
s <- 'The BVBGSA applied for and received a grant from CDWR to fund the preparation of the GSP'
x <- udpipe_download_model(language = "english")
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file)
x <- udpipe_annotate(ud_model, x = s)
x <- as.data.frame(x)
library(tidyverse)
library(ggthemes)
library(ggnetwork)
sp <- textplot::textplot_dependencyparser(x,title = 'Pre-processing example:',subtitle = 'Tokenization, PoS tagging, and dependency parsing') 
ggsave(sp,file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,'/sample_dep_parse.png'),dpi = 450,units = 'in',heigh = 4,width = 10)                                    

elist <- data.frame(from = c('BVBGSA','CDWR'),to = c('CDWR','BVBGSA'),
                    event = c('support request','funding'))
dep_net <- network(elist,directed = T)

gg_event_example <- ggplot(ggnetwork(dep_net),aes(x = x,y = y,xend = xend,yend = yend)) + 
   theme_void() + 
   geom_edges(curvature = 0.2,arrow = arrow(length = unit(0.1, "inches"))) + 
   geom_nodes() + 
   geom_nodelabel(nudge_y = c(-0.1,0.1),nudge_x = c(0.1,-0.1),aes(label = vertex.names),) + 
   geom_edgetext(aes(label = event),nudge_y = c(-0.2,0.2))
ggsave(gg_event_example,file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,'/event_example.png'),units = 'in',height = 2,width = 3,dpi = 300)

"The SVBGSA is a Joint Powers Authority (JPA), and its membership includes the County of Monterey, Monterey County Water Resources Agency (MCWRA), City of Salinas, City of Soledad, City of Gonzales, City of King (King City), the Castroville Community Services District (CCSD), and Monterey One Water."
elist <- data.frame(
   from = c('Monterey County','MCWRA','Soledad City','Gonzales City','King City','CCSD','Monterey One Water','SVBGSA'),
   to = c('SVBGSA'),
   event = c('membership'))

dep_net <- network(elist,loops = T)
occ_net <- network(tcrossprod(as.sociomatrix(dep_net)))


panelB <- ggplot(ggnetwork(dep_net)) + 
   geom_edges(aes(x = xend,y = yend,xend = x,yend = y),arrow = arrow(length = unit(0.1, "inches"))) + 
   geom_nodes(aes(x = x,y = y)) + 
   ggtitle('B. Syntax-based coding')+
   annotate("text", x = 0.4, y = -0.2, 
            label = "*directed edge")+
   geom_nodelabel_repel(aes(x = x,y = y,label = vertex.names)) + 
   theme_void()

occ_geom <- ggnetwork(occ_net)
occ_geom$event <- 'partnership'
#occ_geom <- occ_geom[occ_geom$vertex.names!='SVBGSA',]
panelA <- ggplot(occ_geom,aes(x = x,y = y,xend = xend,yend = yend)) + 
   geom_edges() + 
   geom_nodes() +
   geom_nodelabel_repel(aes(label = vertex.names)) + 
   theme_void() + 
   ggtitle('A. Co-occurrence-based coding')+
   annotate("text", x = 0.4, y = -0.2, 
            label = "*undirected edge")
library(gridExtra)

ggsave(grid.arrange(panelA,panelB,ncol = 2),file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,'/compare_coding.png'),
       height = 5,width= 8,dpi = 450,units = 'in')


