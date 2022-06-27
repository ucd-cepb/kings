#add_terms should be a vector
custom_dictionary <- function(add_terms){
   #retrieves the latest save of dictionary
   water_dictionary <- data.table(matrix(nrow = 0, ncol = 3))
   for(i in 1:length(list.files(path = "data_raw", pattern = "Dictionary"))){
      print(i)
      if(i == 1){
         water_dictionary <- as.data.table(read_csv(list.files(
            path = "data_raw", pattern = "Dictionary", full.names = T)[i]))
      }else{
         water_dictionary <- rbind(water_dictionary, 
                                   as.data.table(read_csv(list.files(
                                      path = "data_raw", 
                                      pattern = "Dictionary", full.names = T)[i])))
      }
   }#end of for
   
   #removing parenthetical parts and isolating term column
   water_dictionary <- gsub("\\s*\\([^\\)]+\\)","",as.character(water_dictionary[[3]]))
   
   #split at / or ,
   water_dictionary <- unlist(strsplit(water_dictionary, "\\s*(,|/)\\s*"))
   
   #keep multi-word terms
   water_dictionary <- grep(" ", water_dictionary, value = T)
   
   water_dictionary <- unique(append(tolower(water_dictionary), add_terms))
}
