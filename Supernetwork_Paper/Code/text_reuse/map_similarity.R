library(data.table)
library(sf)
library(ggplot2)
library(stringr)
library(ggnetwork)
library(statnet)

library(ggthemes)  # Added for theme_map()

# Read the scores data
score_dt <- readRDS('Supernetwork_Paper/data_products/score_results/portal_page_scores_20250512.rds')
score_dt <- score_dt[str_remove(a,'\\.txt.*') != str_remove(b,'\\.txt.*'),]
score_dt$a_file <- str_remove(score_dt$a,'\\.txt.*')
score_dt$b_file <- str_remove(score_dt$b,'\\.txt.*')
score_dt$a_version <- str_extract(score_dt$a_file,'^v[1-9]')
score_dt$b_version <- str_extract(score_dt$b_file,'^v[1-9]')
# Extract page numbers from the numeric suffix of 'a' and 'b' items
score_dt$a_page_num <- as.numeric(str_extract(score_dt$a, "\\d+$"))
score_dt$b_page_num <- as.numeric(str_extract(score_dt$b, "\\d+$"))

# Extract gsp_id from the last 4 digits of a_file and b_file
score_dt$a_gsp_id <- str_extract(score_dt$a_file, "[0-9]{4}$")
score_dt$b_gsp_id <- str_extract(score_dt$b_file, "[0-9]{4}$")


documents <- readRDS('Supernetwork_Paper/data_products/page_metadata.RDS')

# Define the columns to be vectorized
columns_to_merge <- c("basin_plan", "sust_criteria", "monitoring_networks", "projects_mgmt_actions", "admin")

score_dt <- merge(score_dt,documents[,c('gsp_id','page_num',columns_to_merge),with = F],by.x = c('a_gsp_id','a_page_num'),by.y = c('gsp_id','page_num'))
setnames(score_dt,columns_to_merge,paste0('a_',columns_to_merge))
score_dt <- merge(score_dt,documents[,c('gsp_id','page_num',columns_to_merge),with = F],by.x = c('b_gsp_id','b_page_num'),by.y = c('gsp_id','page_num'))
setnames(score_dt,columns_to_merge,paste0('b_',columns_to_merge))

# Read the CSV file containing basin ids
basin_ids <- fread('EJ_DAC_Paper/Data/gsp_basin_ids.csv')
# Convert a_file and b_file to character vectors to ensure compatibility with data.table join
score_dt$a_file <- as.character(score_dt$a_file)
score_dt$b_file <- as.character(score_dt$b_file)



# Disable S2 geometry
sf::sf_use_s2(FALSE)
# Read the GSP shapefile
gsp_bounds <- st_read("Multipurpose_Files/GSP_Submitted")
gsp_bounds <- sf::st_make_valid(gsp_bounds)
# Make sure GSP.ID is formatted correctly with 4 digits
gsp_bounds$gsp_id <- formatC(as.numeric(gsp_bounds$GSP.ID), width = 4, flag = '0')

# Calculate points inside polygons
points_inside <- st_point_on_surface(gsp_bounds)

# Add these points to the original data
gsp_bounds$point_geometry <- st_geometry(points_inside)

# Create a result with point geometry
result_points <- gsp_bounds
st_geometry(result_points) <- st_geometry(points_inside)

# Calculate network from scores
shared_300_total <- score_dt[score > 300, .N, by = .(a_file, b_file, a_version, b_version,a_gsp_id,b_gsp_id)]

page_count <- documents[,.N,by=.(gsp_id)]
shared_300_total$a_page_total <- page_count$N[match(shared_300_total$a_gsp_id,page_count$gsp_id)]
shared_300_total$b_page_total <- page_count$N[match(shared_300_total$b_gsp_id,page_count$gsp_id)]
shared_300_total[,min_page_total:=min(a_page_total,b_page_total)]

shared_300_total[, min_page := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = c("a_page_total",'b_page_total')]
shared_300_total$plus300_ratio <- shared_300_total$N/shared_300_total$min_page


shared_v1 <- shared_300_total[a_version == 'v1' & b_version == 'v1']
# Get list of files for network vertices
flist <- list.files('Multipurpose_Files/portal_files/', pattern = 'txt')
flist <- str_remove(flist, '\\.txt')
flist_v1 <- flist[grepl('^v1', flist)]  
# Initialize the network
netV1 <- network.initialize(n = length(flist_v1),directed = FALSE)
netV1 %v% 'vertex.names' <- flist_v1


# Add edges to the network
add.edges(netV1, 
          names.eval = rep(list(list('plus300_pages', 'plus300_ratio')), nrow(shared_v1)),
          vals.eval = lapply(1:nrow(shared_v1), function(r) { as.list(shared_v1[r, c('N', 'plus300_ratio')]) }),
          head = match(shared_v1$a_file, netV1 %v% 'vertex.names'),
          tail = match(shared_v1$b_file, netV1 %v% 'vertex.names'))

# Create spatial attributes for vertices
# First, extract GSP IDs from file names
vertex_gsp_ids <- str_extract(netV1 %v% 'vertex.names', '[0-9]{4}$')

# Match these with the formatted GSP.ID in the spatial data
point_indices <- match(vertex_gsp_ids, result_points$gsp_id)

# Add this check after matching
if(any(is.na(point_indices))) {
   cat("Warning:", sum(is.na(point_indices)), "vertices have missing coordinates\n")
   # You might want to handle these somehow
}

# Add coordinates to the network vertices
netV1 %v% 'lon' <- st_coordinates(result_points$point_geometry)[point_indices, 1]
netV1 %v% 'lat' <- st_coordinates(result_points$point_geometry)[point_indices, 2]


# Create layout matrix from coordinates
geo_layout = st_coordinates(result_points$point_geometry)[point_indices,]

# Ensure the network object is properly fortified with consistent data
netV1_gg <- ggnetwork::fortify(netV1, layout = geo_layout,scale = F)


ggplot() + 
geom_sf(data = gsp_bounds) + 
geom_nodes(data = netV1_gg,aes(x = x,y = y)) + 
geom_edges(data = netV1_gg[netV1_gg$plus300_ratio>0.10,],
aes(x = x,y = y,xend = xend,yend = yend,size = round(100*plus300_ratio,0),alpha = round(100*plus300_ratio,0) ) ) + 
theme_map() + guides(alpha = 'none') + 
scale_size_continuous(range = c(0.1,5),name = '% highly similar pages') +
ggtitle('High % of page simliarity between GSPs')









# Fortify with layout
netV1_gg <- ggnetwork::fortify(netV1, layout = geo_layout)

# Create the plot
ggplot(netV1_gg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(size = plus300_pages), alpha = 0.5) +
  geom_nodes(size = 3) +
  theme_void()



fortify.network()
?ggnetwork
netV1_gg <- ggnetwork(netV1)
netV1_gg

,layout = cbind(netV1 %v% 'lon',netV1 %v% 'lat'))
netV1_gg



ggplot() + geom_nodes(data = netV1_gg,aes(x = lat,y = lon)) +
   geom_edges(data =data = netV1_gg,aes(x = lat,y = lon,xend = ) )
# Create a simple data frame with the coordinates
layout_df <- data.frame(
   x = netV1 %v% 'x',
   y = netV1 %v% 'y'
)

# Check for NA values
if(any(is.na(layout_df$x) | is.na(layout_df$y))) {
   cat("Warning: There are NA values in the coordinates\n")
   # You might want to handle these
}


# Create edge dataframe from the network object
edges <- as.data.frame(as.matrix.network.edgelist(netV1))
colnames(edges) <- c("from_idx", "to_idx")
edges$weight <- netV1 %e% "plus300_pages"

# Add coordinates for the edges
edges$x1 <- netV1 %v% "x"[edges$from_idx]
edges$y1 <- netV1 %v% "y"[edges$from_idx]
edges$x2 <- netV1 %v% "x"[edges$to_idx]
edges$y2 <- netV1 %v% "y"[edges$to_idx]

# Create node dataframe
nodes <- data.frame(
   id = 1:network.size(netV1),
   name = netV1 %v% "vertex.names",
   x = netV1 %v% "x",
   y = netV1 %v% "y"
)

# Filter out any edges with NA coordinates
if(any(is.na(edges$x1) | is.na(edges$y1) | is.na(edges$x2) | is.na(edges$y2))) {
   cat("Warning: Some edges have missing coordinates. Removing these edges.\n")
   edges <- edges[!is.na(edges$x1) & !is.na(edges$y1) & 
                     !is.na(edges$x2) & !is.na(edges$y2), ]
}




# Create a layout matrix using the coordinates
layout_matrix <- cbind(netV1 %v% "x", netV1 %v% "y")
# Use this matrix directly
netV1_gg <- ggnetwork(netV1, layout = layout_matrix)
# Get a layout from network.layout
layout_matrix <- network.layout.fruchtermanreingold(netV1)

# Use that layout with ggnetwork
netV1_gg <- ggnetwork(netV1, layout = layout_matrix)
layout_matrix

netV1_gg <- ggnetwork(netV1, layout = NULL, cell.jitter = 0, arrow.gap = 0)


ggnetwork(netV1)



net <- GGally::ggnet2(netV1)
net


# Convert network to ggnetwork format with node coordinates from spatial data
netV1_gg <- ggnetwork(netV1, layout = cbind(netV1 %v% 'x',  netV1 %v% 'y'))

# Replace the x and y coordinates directly
netV1_gg$x <- layout_df$x[netV1_gg$vertex.names]
netV1_gg$y <- layout_df$y[netV1_gg$vertex.names]
netV1_gg$xend <- layout_df$x[netV1_gg$vertex.namesend]
netV1_gg$yend <- layout_df$y[netV1_gg$vertex.namesend]



netV1


duplicated(netV1 %v% 'y')


# Plot using ggplot2
ggplot() +
   geom_sf(data = gsp_bounds, color = "black", fill = "lightblue", alpha = 0.5) +
   geom_edges(data = netV1_gg, 
              aes(x = x, y = y, xend = xend, yend = yend, size = plus300_pages), 
              color = "blue", alpha = 0.7) +
   geom_nodes(data = netV1_gg, 
              aes(x = x, y = y), 
              color = "red", size = 3) +
   scale_size_continuous(name = "Pages with score > 300",
                         range = c(0.2, 2)) +
   theme_minimal() + 
   labs(title = "GSP Network Overlay", 
        subtitle = "Links represent text similarity between GSP documents")

# If you want to also plot just the basins with their names
ggplot() +
   geom_sf(data = gsp_bounds, aes(fill = Basin_Name)) +
   theme_minimal() +
   labs(title = "GSP Basins", fill = "Basin Name")