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


documents <- readRDS('Network_Innovation_Paper/data_products/02_text_preprocessing/page_metadata.RDS')

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


