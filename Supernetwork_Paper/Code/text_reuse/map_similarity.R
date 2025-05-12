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

# Read the CSV file containing basin ids
basin_ids <- fread('EJ_DAC_Paper/Data/gsp_basin_ids.csv')

# Convert a_file and b_file to character vectors to ensure compatibility with data.table join
score_dt$a_file <- as.character(score_dt$a_file)
score_dt$b_file <- as.character(score_dt$b_file)

# Use match() to create a_basin_id and b_basin_id columns directly
score_dt$a_basin_id <- basin_ids$basin_id[match(
   str_extract(score_dt$a_file,'[0-9]{4}$'),
   formatC(basin_ids$gsp_id, width = 4, flag = '0')
)]
score_dt$b_basin_id <- basin_ids$basin_id[match(
   str_extract(score_dt$b_file,'[0-9]{4}$'),
   formatC(basin_ids$gsp_id, width = 4, flag = '0')
)]

# Disable S2 geometry
sf::sf_use_s2(FALSE)

# Read the GSP shapefile
gsp_bounds <- st_read("Multipurpose_Files/GSP_Submitted")
gsp_bounds <- sf::st_make_valid(gsp_bounds)

# Make sure GSP.ID is formatted correctly with 4 digits
gsp_bounds$GSP.ID_fmt <- formatC(as.numeric(gsp_bounds$GSP.ID), width = 4, flag = '0')

# Calculate points inside polygons
points_inside <- st_point_on_surface(gsp_bounds)

# Add these points to the original data
gsp_bounds$point_geometry <- st_geometry(points_inside)

# Create a result with point geometry
result_points <- gsp_bounds
st_geometry(result_points) <- st_geometry(points_inside)

# Calculate network from scores
shared_300 <- score_dt[score > 300, .N, by = .(a_file, b_file, a_version, b_version)]
shared_v1 <- shared_300[a_version == 'v1' & b_version == 'v1']

# Get list of files for network vertices
flist <- list.files('Multipurpose_Files/portal_files/', pattern = 'txt')
flist <- str_remove(flist, '\\.txt')

# Initialize the network
netV1 <- network.initialize(n = sum(grepl('^v1',flist)), directed = FALSE, multiple = FALSE, loops = TRUE)
netV1 %v% 'vertex.names' <- flist

# Add edges to the network
add.edges(netV1, 
          names.eval = 'plus300_pages',
          vals.eval = shared_v1$N,
          head = match(shared_v1$a_file, netV1 %v% 'vertex.names'),
          tail = match(shared_v1$b_file, netV1 %v% 'vertex.names'))

# Create spatial attributes for vertices
# First, extract GSP IDs from file names
vertex_gsp_ids <- str_extract(netV1 %v% 'vertex.names', '[0-9]{4}$')

# Match these with the formatted GSP.ID in the spatial data
point_indices <- match(vertex_gsp_ids, result_points$GSP.ID_fmt)

# Add coordinates to the network vertices
netV1 %v% 'x' <- st_coordinates(result_points$point_geometry)[point_indices, 1]
netV1 %v% 'y' <- st_coordinates(result_points$point_geometry)[point_indices, 2]

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

list.vertex.attributes(netV1)

netV1
# Convert to ggnetwork format using a different method
netV1_gg <- ggnetwork(x = netV1)
class(netV1)
any(duplicated(network.vertex.names(netV1)))

# Replace the x and y coordinates directly
netV1_gg$x <- layout_df$x[netV1_gg$vertex.names]
netV1_gg$y <- layout_df$y[netV1_gg$vertex.names]
netV1_gg$xend <- layout_df$x[netV1_gg$vertex.namesend]
netV1_gg$yend <- layout_df$y[netV1_gg$vertex.namesend]



# Convert network to ggnetwork format with node coordinates from spatial data
netV1_gg <- ggnetwork(netV1, layout = cbind(netV1 %v% 'x',  netV1 %v% 'y'))

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