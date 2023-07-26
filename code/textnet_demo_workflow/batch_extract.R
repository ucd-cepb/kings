
library(data.table)
library(textNet)
library(stringr)
#### overwrite existing results?
overwrite = F
dir.create('data/network_extracts')
library(spacyr)
parsed_files <- list.files('data/spacyd_gsps/',full.names = T)

for(fl in parsed_files){
   id <- str_extract(fl,'[0-9]{1,}')
   save_file <- paste0('data/network_extracts/',id,'.RDS')
   if(!file.exists(save_file)|overwrite == T){
      print(paste0('analyzing document ',fl))
      temp <- readRDS(fl)
      custom_entity_extract(x = temp,cl = 8,return_to_memory = F,file = save_file)
   }
}

     