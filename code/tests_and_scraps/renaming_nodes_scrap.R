supernodesdt <- supernodesdt[!duplicated(supernodesdt, by=c("name","entity_type")),]
supernodesdt <- supernodesdt[,index := 1:.N,by="name"]
supernodesdt <- supernodesdt[,`:=` (dups, ifelse(any(index>1),T,F)), by = "name"]
supernodesdt <- supernodesdt[,`:=` (name, ifelse(dups==T,paste0(name,"#",index),name))]
s