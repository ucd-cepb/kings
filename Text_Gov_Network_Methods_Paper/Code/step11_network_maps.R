library(terra)
library(tigris)
library(viridis)
filekey <- read.csv("filekey.csv")
network_properties <- readRDS(paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/gov_dir_weight_no_gpe_network_properties"))

gspzip <- filekey[filekey$var_name=="gsp_submitted_zip",]$filepath
gspzip <- substr(gspzip, 1, nchar(gspzip)-4)
gsp_shapes <- vect(paste0(gspzip,"/SubmittedGSP_Master.shp"))
gsp_shapes <- terra::project(gsp_shapes, "+proj=robin +datum=WGS84")

city_boundaries <- vect(filekey[filekey$var_name=="city_boundaries_govnetpaper",]$filepath)
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
# 1. Open png file
png(paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/nodes_map.png"), width = 900, height = "975")
plot(cal,pax=list(# params for axes
   cex.axis = 2
))
plot(gsp_shapes, "num_nodes", breaks = brks, col=viridis(grps), 
     add = T, legend=T, main = "Number of Nodes", cex.main=2.5, 
     plg=list(cex=2)#params for legend
     )
#title("Number of Nodes", cex=10)
#plot(city_boundaries, col="#888888",border = NA, alpha = 0.5, add = T)
dev.off()

brks <- quantile(gsp_shapes$num_edges, 0:(grps-1)/(grps-1), na.rm=TRUE)
png(paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/edges_map.png"), width = 900, height = "975")
plot(cal,pax=list(# params for axes
   cex.axis = 2
))
plot(gsp_shapes, "num_edges", breaks = brks, col=viridis(grps),
     add = T, legend=T, main = "Number of Edges", cex.main=2.5,
     plg=list(cex=2))
#title("Number of Edges")
#plot(city_boundaries, col="#888888",border = NA, alpha = 0.5, add = T)
dev.off()

gsp_shapes$area <- expanse(gsp_shapes)

plot(gsp_shapes$num_nodes, gsp_shapes$area)
plot(gsp_shapes$num_edges, gsp_shapes$area)


cor.test(gsp_shapes$num_nodes, gsp_shapes$area)
cor.test(gsp_shapes$num_edges, gsp_shapes$area)
