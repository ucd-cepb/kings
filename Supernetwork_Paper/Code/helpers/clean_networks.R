#any filtering of nodes and edges to remove
#low-quality stuff goes here
#I recommend just using this basic script, which
#filters out any edges that are missing a source or target node
#and filters out any edges where the verb is not an English word


clean <- function(myextracts){
   if(class(myextracts)!="list"){
      stop("This cleaning function is designed to be used on a list, where each element represents the textnet_extract of a different GSP number.")
   }
   
   for(k in seq_along(myextracts)){
      #only keep edges that have both a source and a target node.
      #But this doesn't remove any nodes from the nodelist
      #So it is still possible to identify isolates in the network
      #and the number of times they show up in the original text (num_appearances)
      myextracts[[k]]$edgelist <- myextracts[[k]]$edgelist[myextracts[[k]]$edgelist$edgeiscomplete==T,]
      
      #only keep edges where the verb representing the edge is an English word
      myextracts[[k]]$edgelist <- myextracts[[k]]$edgelist[myextracts[[k]]$edgelist$head_verb_lemma %in% eng_words,]
   }
   
   return(myextracts)
   
}