library(igraph)
library(pbapply)
library(data.table)
library(stringr)

network_stats <- readRDS("data/output_large_files/gov_dir_weight_no_gpe_network_properties")

gsp_meta <- readRDS("data/output_large_files/gsp_docs_w_meta")
gsp_meta <- gsp_meta[!is_comment & !is_reference,]
gsp_meta <- gsp_meta[,c(.SD,.N),by=gsp_id]
gsp_mini <- unique(gsp_meta[,c(1,14,16,19:27)])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
network_stats <- network_stats[!network_stats$gsp_id %in% c("0089","0053"),]
#for meta
network_stats <- as_tibble(network_stats)
comp <- merge(gsp_mini, network_stats)
comp$num_nodes <- as.numeric(comp$num_nodes)
comp$num_edges <- as.numeric(comp$num_edges)
comp$reciprocity <- as.numeric(comp$reciprocity)
comp$centralization <- as.numeric(comp$centralization)
comp$transitivity <- as.numeric(comp$transitivity)
comp$connectedness <- as.numeric(comp$connectedness)
comp$modularity <- as.numeric(comp$modularity)
comp$mult_gsas <- as.numeric(comp$mult_gsas)

cor.test(comp$N, comp$transitivity)
cor.test(comp$N, comp$centralization)
cor.test(comp$N, comp$modularity)
cor.test(comp$N, comp$connectedness)
cor.test(comp$N, comp$reciprocity)

library(tidyverse)
base <- ggplot(data = comp,aes(x = N)) + 
   scale_x_continuous(name = '# pages') + 
   theme_bw() 
g1 <- base + 
   geom_point(aes(y = num_nodes),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# nodes') + 
   ggtitle('# nodes')
g2 <- base + 
   geom_point(aes(y = num_nodes/N),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# nodes/page') + 
   ggtitle('# nodes per page')
g3 <- base + 
   geom_point(aes(y = num_edges),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# edges') + 
   ggtitle('# edges')
g4 <- base + 
   geom_point(aes(y = num_edges/N),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# edges/page') + 
   ggtitle('# edges per page')

library(gridExtra)
grom <- grid.arrange(g1,g2,g3,g4,ncol = 2)
ggsave(grom,file = 'figures/size_vs_pages.png',dpi = 450,units = 'in',height = 7,width = 7)

base2 <- ggplot(data = comp,aes(x = N)) + 
   scale_x_continuous(name = '# pages') + 
   theme_bw() 
g5 <- base2 + 
   geom_point(aes(y = centralization),pch = 21,alpha = 0.5) + 
   scale_y_continuous('centralization score') + 
   ggtitle('centralization')
g6 <- base2 + 
   geom_point(aes(y = transitivity),pch = 21,alpha = 0.5) + 
   scale_y_continuous('transitivity score') + 
   ggtitle('transitivity')
g7 <- base2 + 
   geom_point(aes(y = modularity),pch = 21,alpha = 0.5) + 
   scale_y_continuous('modularity score') + 
   ggtitle('modularity')
g8 <- base2 + 
   geom_point(aes(y = reciprocity),pch = 21,alpha = 0.5) + 
   scale_y_continuous('reciprocity score') + 
   ggtitle('reciprocity')
g9 <- base2 + 
   geom_point(aes(y = connectedness),pch = 21,alpha = 0.5) + 
   scale_y_continuous('connectedness score') + 
   ggtitle('connectedness')

grom2 <- (grid.arrange(g7, g9,ncol = 2))
ggsave(grom2,file = 'figures/net_stats_vs_pages.png',dpi = 450,units = 'in',height = 3.5,width = 7)

comp$mult_gsas <- as.factor(comp$mult_gsas)
res_aov <- aov(num_edges ~ mult_gsas,
               data = comp
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov$residuals) #fails residual normality test 

test <- wilcox.test(comp$num_edges ~ comp$mult_gsas) #not sig at 0.05
test

#however, because sample of each group >= 30, it's ok to use anova or t-test

#time to test homogeneity
boxplot(num_edges ~ mult_gsas,
        data = comp
)

leveneTest(num_edges ~ mult_gsas,
           data = comp
)#not homogeneous
plot(res_aov, which = 3)#red line not horizontal. homogeneity not met.

#this means we should run a welch anova
oneway.test(num_edges ~ mult_gsas, data = comp, 
            var.equal = FALSE)

#now for centralization:

res_aov <- aov(centralization ~ mult_gsas,
               data = comp
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov$residuals) #fails residual normality test 

#time to test homogeneity
boxplot(centralization ~ mult_gsas,
        data = comp
)

leveneTest(centralization ~ mult_gsas,
           data = comp
)#not homogeneous
plot(res_aov, which = 3)#red line not horizontal. homogeneity not met.

#this means we should run a welch anova
oneway.test(centralization ~ mult_gsas, data = comp, 
            var.equal = FALSE)

leveneTest(num_nodes ~ mult_gsas,
           data = comp
)
#since there are only two groups, better to run Welch student's t:
leveneTest(num_edges ~ mult_gsas,
           data = comp
)
t.test(num_edges ~ as.factor(mult_gsas),
       data = comp,
       var.equal = FALSE)
leveneTest(num_nodes ~ mult_gsas,
           data = comp
)
t.test(num_nodes ~ as.factor(mult_gsas),
       data = comp,
       var.equal = T)
leveneTest(centralization ~ mult_gsas,
           data = comp
)
t.test(centralization ~ as.factor(mult_gsas),
       data = comp,
       var.equal = FALSE)
leveneTest(modularity ~ mult_gsas,
           data = comp
)
t.test(modularity ~ as.factor(mult_gsas),
       data = comp,
       var.equal = T)
leveneTest(connectedness ~ mult_gsas,
           data = comp
)
t.test(connectedness ~ as.factor(mult_gsas),
       data = comp,
       var.equal = T)
leveneTest(transitivity ~ mult_gsas,
           data = comp
)
t.test(transitivity ~ as.factor(mult_gsas),
       data = comp,
       var.equal = T)
leveneTest(reciprocity ~ mult_gsas,
           data = comp
)
t.test(reciprocity ~ as.factor(mult_gsas),
       data = comp,
       var.equal = T)
cor.test(comp$mult_gsas, comp$num_edges)
cor.test(comp$mult_gsas, comp$num_nodes)
cor.test(comp$mult_gsas, comp$centralization)

comp$multiple_gsas <- as.logical(comp$mult_gsas)
comp$multiple_gsas <- as.factor(comp$multiple_gsas)

g10 <- ggplot(data = comp,aes(x = multiple_gsas, y=centralization)) + 
   geom_boxplot()+
   theme_bw() 

g11 <- ggplot(data = comp,aes(x = multiple_gsas, y=num_nodes)) + 
   geom_boxplot()+
   theme_bw() 


g12 <- ggplot(data = comp,aes(x = multiple_gsas, y=num_edges)) + 
   geom_boxplot()+
   theme_bw() 

grom3 <- (grid.arrange(g10,g11,g12, ncol=3))
ggsave(grom3,file = 'figures/mult_gsas.png',dpi = 450,units = 'in',height = 3.5,width = 8)




