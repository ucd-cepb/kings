#used in lex_clean
#add_terms should be a vector
custom_dictionary <- function(add_terms = NULL){
   #retrieves the latest save of dictionary
   
   water_dictionary <- data.table(matrix(nrow = 0, ncol = 3))
   for(i in 1:length(list.files(path = "data/raw_large_files", pattern = "Dictionary"))){
      print(i)
      if(i == 1){
         water_dictionary <- as.data.table(read_csv(list.files(
            path = "data/raw_large_files", pattern = "Dictionary", full.names = T)[i]))
      }else{
         water_dictionary <- rbind(water_dictionary, 
                                   as.data.table(read_csv(list.files(
                                      path = "data/raw_large_files", 
                                      pattern = "Dictionary", full.names = T)[i])))
      }
   }#end of for
   
   water_dictionary <- as.character(water_dictionary[[3]])
   
   #add custom terms
   dictionary <- unique(append(tolower(water_dictionary), add_terms))
   
   #removing parenthetical parts
   dictionary <- gsub("\\s*\\([^\\)]+\\)","",dictionary)
   
   #removing periods
   dictionary <- gsub("\\.","",dictionary)
   
   #split at / or ,
   dictionary <- unlist(strsplit(dictionary, "\\s*(,|/)\\s*"))
   
   #keep multi-word terms
   dictionary <- grep(" ", dictionary, value = T)
   
   #remove any empty strings 
   dictionary <- stri_remove_empty_na(dictionary)
   
   
}
