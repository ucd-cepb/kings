
#retrieves the latest save of gsp_text_with_meta
water_dictionary <- read_csv(list.files(path = "data_raw", pattern = "Dictionary", full.names = T)[1])
water_dictionary <- gsub("\\s*\\([^\\)]+\\)","",as.character(water_dictionary[[3]]))
water_dictionary <- grep(" ", water_dictionary, value = T)
#split at / and cut single words
#feed in compound words, program joins with underscores
saveRDS(is_reference, file = paste0("data_temp/","gsp_docs_reference_",format(Sys.time(), "%Y%m%d-%H:%M")))

str_remove(water_dictionary, "")


