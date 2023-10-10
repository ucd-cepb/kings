#takes an xls file and identifies the pdf pages associated with each
#required element of a management plan, returns this info as a data.table
packs <- c('tidyverse','readxl','data.table')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

source(filekey[filekey$var_name=="read_plan_element_function",]$filepath)
source(filekey[filekey$var_name=="consolidate_pgs_function",]$filepath)

create_page_key <- function(file){
   plan <- read_plan_element(file)
   clean_plan <- consolidate_pgs(plan)
   return(clean_plan)
}









