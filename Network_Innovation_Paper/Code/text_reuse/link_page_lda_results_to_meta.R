library(data.table)
library(sf)
library(ggplot2)
library(stringr)
library(ggnetwork)
library(statnet)

library(ggthemes)  # Added for theme_map()

# Read the scores data
score_dt <- readRDS('Network_Innovation_Paper/data_products/score_results/portal_page_scores_20250512.rds')
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


documents <- readRDS('Network_Innovation_Paper/data_products/page_metadata.RDS')

# Define the columns to be vectorized
columns_to_merge <- c("basin_plan", "sust_criteria", "monitoring_networks", "projects_mgmt_actions", "admin")

page_sums <- documents[, setNames(lapply(.SD, sum, na.rm = TRUE), paste0('p_', columns_to_merge)), .SDcols = columns_to_merge,by=.(gsp_id)]


score_dt <- merge(score_dt,documents[,c('gsp_id','page_num',columns_to_merge),with = F],by.x = c('a_gsp_id','a_page_num'),by.y = c('gsp_id','page_num'))
setnames(score_dt,columns_to_merge,paste0('a_',columns_to_merge))
score_dt <- merge(score_dt,documents[,c('gsp_id','page_num',columns_to_merge),with = F],by.x = c('b_gsp_id','b_page_num'),by.y = c('gsp_id','page_num'))
setnames(score_dt,columns_to_merge,paste0('b_',columns_to_merge))

# Read the CSV file containing basin ids
basin_ids <- fread('EJ_DAC_Paper/Data/gsp_basin_ids.csv')
# Convert a_file and b_file to character vectors to ensure compatibility with data.table join
score_dt$a_file <- as.character(score_dt$a_file)
score_dt$b_file <- as.character(score_dt$b_file)

score_dt$a_version <- str_extract(score_dt$a_file,'^v[1-9]')
score_dt$b_version <- str_extract(score_dt$b_file,'^v[1-9]')


