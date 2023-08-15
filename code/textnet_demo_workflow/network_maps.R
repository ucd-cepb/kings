library(terra)
library(tigris)

network_properties <- readRDS("data_output/gov_dir_weight_no_gpe_network_properties")

gsp_shapes <- vect("data_spatial_raw/GSP_submitted/SubmittedGSP_Master.shp")
gsp_shapes <- terra::project(gsp_shapes, "+proj=robin +datum=WGS84")

city_boundaries <- vect("data_spatial_raw/City_Boundaries/City_Boundaries.shp")
city_boundaries <- terra::project(city_boundaries, "+proj=robin +datum=WGS84")

us <- tigris::states(cb=T, class = 'sf')
us <- vect(us)
us <- terra::project(us, "+proj=robin +datum=WGS84")
cal <- us[us$NAME=="California"]

gsp_shapes$num_nodes <- as.numeric(network_properties[match(as.numeric(gsp_shapes$`GSP ID`), 
        as.numeric(network_properties$gsp_id)),]$num_nodes)
gsp_shapes$num_edges <- as.numeric(network_properties[match(as.numeric(gsp_shapes$`GSP ID`), 
        as.numeric(network_properties$gsp_id)),]$num_edges)

gsp_shapes <- gsp_shapes[!gsp_shapes$`GSP ID` %in% c("89", "53"),]

colorpalette <- viridis(100)
library(RColorBrewer)
grps <- 10
brks <- quantile(gsp_shapes$num_nodes, 0:(grps-1)/(grps-1), na.rm=TRUE)
window(gsp_shapes) <- ext(cal)
plot(cal)
plot(gsp_shapes, "num_nodes", breaks = brks, col=viridis(grps), 
     add = T, legend=T)
title("Number of Nodes")
#plot(city_boundaries, col="#888888",border = NA, alpha = 0.5, add = T)

brks <- quantile(gsp_shapes$num_edges, 0:(grps-1)/(grps-1), na.rm=TRUE)
plot(gsp_shapes, "num_edges", breaks = brks, col=viridis(grps),
     add = T, legend=T)
title("Number of Edges")
#plot(city_boundaries, col="#888888",border = NA, alpha = 0.5, add = T)


gsp_shapes$area <- expanse(gsp_shapes)

plot(gsp_shapes$num_nodes, gsp_shapes$area)
plot(gsp_shapes$num_edges, gsp_shapes$area)


cor.test(gsp_shapes$num_nodes, gsp_shapes$area)
cor.test(gsp_shapes$num_edges, gsp_shapes$area)
