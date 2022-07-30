#takes an xls file and identifies the pdf pages associated with each
#required element of a management plan, returns this info as a data.table
library(tidyverse)
library(readxl)
library(data.table)

source('code/functions/read_plan_element.R')
source('code/functions/consolidate_pgs.R')

create_page_key <- function(file){
   plan <- read_plan_element(file)
   clean_plan <- consolidate_pgs(plan)
   return(clean_plan)
}









