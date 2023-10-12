web_data_repair <- function(new_tbl, old_tbl){
   old_tbl <- old_tbl %>% mutate (gsp_id = gsp_num_id) %>% select(!c(name_gsas,gsp_num_id)) 
   
   old_tbl <- cbind(old_tbl, "basin_id" = sub(" .*", "", old_tbl$basin))
   
   #join by vars visible on home portal page
   repair <- full_join(new_tbl, old_tbl, by = c("basin", "basin_id",
                                                "gsp_local_id","approval"))
   
   repaired <- as.data.table(cbind("basin" = repair$basin,
                     "basin_id" = repair$basin_id, 
                     "gsp_local_id" = repair$gsp_local_id, 
                     "approval" = repair$approval,
                     "link" = ifelse(
                        !is.na(repair$link.x),repair$link.x,repair$link.y),
                     "gsp_id" = ifelse(
                        !is.na(repair$gsp_id.x),repair$gsp_id.x,repair$gsp_id.y),
                     "mult_gsas" = ifelse(
                        !is.na(repair$mult_gsas.x),repair$mult_gsas.x,repair$mult_gsas.y)))
   repaired <- cbind(repaired, "name_gsas" = repair$name_gsas)
   
   for(i in 1:length(repaired[[1]])){
      if(is.null(repaired$name_gsas[[i]]) & !is.na(repaired$gsp_id[i])){
         #information collected from Consultation Initiation Letters downloaded from
         #https://sgma.water.ca.gov/portal/gsp/assessments/x
         #where x is the gsp_id without leading zeroes
         if(repaired$gsp_id[i]=="0008"){
            repaired$name_gsas[[i]] = c(
               "Westlands Water District GSA",
               "County of Fresno GSA")
            repaired$mult_gsas[[i]] <- T
         }
         if(repaired$gsp_id[i]=="0032"){
            repaired$name_gsas[[i]] = c(
            "Cuyama Basin GSA")
            repaired$mult_gsas[[i]] <- F
         }
         if(repaired$gsp_id[i]=="0035"){
            repaired$name_gsas[[i]] = c(
               "City of Paso Robles GSA",
               "Paso Basin - County of San Luis Obispo GSA",
               "San Miguel Community Services District GSA",
               "Shandon - San Juan GSA")
            repaired$mult_gsas[[i]] <- T
         }
      }
   }
   return(repaired)
}
