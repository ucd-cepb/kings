method = "huge"
verbose = T
model <- readRDS(list.files(path = "data_output/mdl", pattern = "model", full.names = T)[length(
   list.files(path = "data_output/mdl", pattern = "model", full.names = T))])
gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
   list.files(path = "data_temp", pattern = "slam", full.names = T))])



if(method=="huge"){
   for(m in unique(gsp_out$meta$gsp_id)){
      out <- list()
      pages <- gsp_out$meta$gsp_id == m
      #adapted from stm package
      set.seed(20000)
      if(!requireNamespace("huge", quietly=TRUE)) stop("Install the huge package to use this function")
      X.npn <- huge::huge.npn(model$theta[pages,],verbose=verbose) # Nonparanormal
      out.npn <- huge::huge(X.npn,nlambda=30, verbose=verbose)
      ric.npn <- huge::huge.select(out.npn, verbose=verbose)
      MLE <- cor(model$theta[pages,])
      out$posadj <- ric.npn$refit*(MLE>0)
      out$poscor <- ric.npn$refit*(MLE>0)*MLE
      out$cor <- ric.npn$refit*MLE
      class(out) <- "topicCorr"
      saveRDS(out,paste0("topic_network_",m))

   }
   

}

#if(method=="simple"){
#   set.seed(20000)
#   tcs <- topicCorr(model, method = "simple", verbose = TRUE)
#TODO insert simple method procedures here
#}