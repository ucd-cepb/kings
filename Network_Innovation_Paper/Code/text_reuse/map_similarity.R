library(data.table)
library(sf)
library(ggplot2)
library(stringr)
library(ggnetwork)
library(statnet)

library(ggthemes)  # Added for theme_map()

# Core corpus + id-crosswalk helpers. Page-score entity keys are now
# '<gspDocId>_<page_num>' (see hash_and_compare_pages.R); the legacy
# v*_gsp_num_id_*.txt naming is gone. Recover gsp_id/version via the crosswalk.
source("Network_Innovation_Paper/Code/_corpus.R")
cw <- load_id_crosswalk()

# Read the scores data (newest regenerated file)
score_dt <- readRDS(latest_page_scores())
score_dt <- score_dt[str_remove(a,'_[0-9]+$') != str_remove(b,'_[0-9]+$'),]
score_dt$a_file <- str_remove(score_dt$a,'_[0-9]+$')   # gspDocId stem
score_dt$b_file <- str_remove(score_dt$b,'_[0-9]+$')
# Extract page numbers from the numeric suffix of 'a' and 'b' items
score_dt$a_page_num <- as.numeric(str_extract(score_dt$a, "[0-9]+$"))
score_dt$b_page_num <- as.numeric(str_extract(score_dt$b, "[0-9]+$"))

# Recover legacy 4-digit gsp_id + version from the crosswalk (keyed on gspDocId)
score_dt$a_gsp_id  <- cw$gsp_id[match(score_dt$a_file, cw$gsp_doc_id)]
score_dt$b_gsp_id  <- cw$gsp_id[match(score_dt$b_file, cw$gsp_doc_id)]
score_dt$a_version <- cw$version[match(score_dt$a_file, cw$gsp_doc_id)]
score_dt$b_version <- cw$version[match(score_dt$b_file, cw$gsp_doc_id)]


documents <- readRDS('Network_Innovation_Paper/data_products/page_metadata.RDS')

# Define the columns to be vectorized
columns_to_merge <- c("basin_plan", "sust_criteria", "monitoring_networks", "projects_mgmt_actions", "admin")

page_sums <- documents[, setNames(lapply(.SD, sum, na.rm = TRUE), paste0('p_', columns_to_merge)), .SDcols = columns_to_merge,by=.(gsp_id)]


score_dt <- merge(score_dt,documents[,c('gsp_id','page_num',columns_to_merge),with = F],by.x = c('a_gsp_id','a_page_num'),by.y = c('gsp_id','page_num'))
setnames(score_dt,columns_to_merge,paste0('a_',columns_to_merge))
score_dt <- merge(score_dt,documents[,c('gsp_id','page_num',columns_to_merge),with = F],by.x = c('b_gsp_id','b_page_num'),by.y = c('gsp_id','page_num'))
setnames(score_dt,columns_to_merge,paste0('b_',columns_to_merge))

# Read the CSV file containing basin ids
source("Network_Innovation_Paper/Code/_paths.R")
basin_ids <- fread(nip_input('gsp_basin_ids.csv'))
# Convert a_file and b_file to character vectors to ensure compatibility with data.table join
score_dt$a_file <- as.character(score_dt$a_file)
score_dt$b_file <- as.character(score_dt$b_file)

# Disable S2 geometry
sf::sf_use_s2(FALSE)
# Read the GSP shapefile
gsp_bounds <- st_read(nip_input("GSP_Submitted"))
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

#### need to replicate this in the loop below
shared_300_total[, min_page := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = c("a_page_total",'b_page_total')]
shared_300_total$plus300_ratio <- shared_300_total$N/shared_300_total$min_page


# One document per plan. Default = the original/earliest submission (reproduces
# the legacy '^v1' selection); set NIP_DOC_SELECT=latest to use the resubmitted
# doc where one exists. Filter on the gspDocId stem, not the manifest `version`
# field (which is an unrelated adoption-cycle axis — see select_plan_docs()).
sel_docs <- select_plan_docs(cw)$gsp_doc_id
shared_v1 <- shared_300_total[a_file %in% sel_docs & b_file %in% sel_docs]
# Network vertices = the selected-doc gspDocId stems of the core parquet corpus.
flist <- str_remove(list.files(core_txt_clean(), pattern = '\\.parquet$'), '\\.parquet$')
flist_v1 <- flist[flist %in% sel_docs]
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
# Recover the 4-digit gsp_id for each gspDocId vertex via the crosswalk.
vertex_gsp_ids <- cw$gsp_id[match(netV1 %v% 'vertex.names', cw$gsp_doc_id)]

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

# replicate this plot but for ONLY pages that are base_plan == TRUE#

# Initialize lists to store network and ggplot objects for each plan section type
network_list <- list()
ggplot_list <- list()

# Iterate over each plan section type
for (section in columns_to_merge) {
    section = 'admin'
    # Filter the documents data for pages where the section is TRUE
    documents_section <- documents[get(section) == TRUE,.N,by=.(gsp_id)]
    setnames(documents_section,'N','section_page_count')
    
    shared_dt <- score_dt[score > 300 & a_file %in% sel_docs & b_file %in% sel_docs & get(paste0('a_',section))==T & get(paste0('b_',section)) , .N, by = .(a_file, b_file, a_version, b_version,a_gsp_id,b_gsp_id)]
    setnames(shared_dt,'N','plus300_pages')
    shared_dt$section <- section
    
    # Merge shared_dt with documents_section to create a_N and b_N columns
    shared_dt <- merge(shared_dt, documents_section, by.x = 'a_gsp_id', by.y = 'gsp_id', all.x = TRUE)
    setnames(shared_dt, 'section_page_count', 'a_section_count')
    

    shared_dt <- merge(shared_dt, documents_section, by.x = 'b_gsp_id', by.y = 'gsp_id', all.x = TRUE)
    setnames(shared_dt, 'section_page_count', 'b_N')
    
    
    
   
    # Initialize the network for the current section
    netV1_section <- network.initialize(n = length(flist_v1), directed = FALSE)
    netV1_section %v% 'vertex.names' <- flist_v1
    
    # Add edges to the network for the current section
    add.edges(netV1_section, 
              names.eval = rep(list(list('plus300_pages', 'plus300_ratio')), nrow(shared_dt)),
              vals.eval = lapply(1:nrow(shared_v1_section), function(r) { as.list(shared_dt[r, c('N', 'plus300_ratio')]) }),
              head = match(shared_v1_section$a_file, netV1_section %v% 'vertex.names'),
              tail = match(shared_v1_section$b_file, netV1_section %v% 'vertex.names'))
    
    # Create spatial attributes for vertices in the current section network
    vertex_gsp_ids_section <- str_extract(netV1_section %v% 'vertex.names', '[0-9]{4}$')
    point_indices_section <- match(vertex_gsp_ids_section, result_points$gsp_id)
    
    # Add coordinates to the network vertices for the current section
    netV1_section %v% 'lon' <- st_coordinates(result_points$point_geometry)[point_indices_section, 1]
    netV1_section %v% 'lat' <- st_coordinates(result_points$point_geometry)[point_indices_section, 2]
    
    # Create layout matrix from coordinates for the current section
    geo_layout_section = st_coordinates(result_points$point_geometry)[point_indices_section,]
    
    # Ensure the network object is properly fortified with consistent data for the current section
    netV1_section_gg <- ggnetwork::fortify(netV1_section, layout = geo_layout_section, scale = F)
    
    # Store the network object in the list
    network_list[[section]] <- netV1_section
    
    # Create and store the ggplot object for the current section
    ggplot_list[[section]] <- ggplot() + 
        geom_sf(data = gsp_bounds) + 
        geom_nodes(data = netV1_section_gg, aes(x = x, y = y)) + 
        geom_edges(data = netV1_section_gg[netV1_section_gg$plus300_ratio > 0.10,],
                   aes(x = x, y = y, xend = xend, yend = yend, size = round(100 * plus300_ratio, 0), alpha = round(100 * plus300_ratio, 0))) + 
        theme_map() + guides(alpha = 'none') + 
        scale_size_continuous(range = c(0.1, 5), name = '% highly similar pages') +
        ggtitle(paste('High % of page similarity between GSPs (', section, ' Only)', sep = ''))
}






