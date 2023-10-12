filekey <- read.csv("filekey.csv")
#stm::topicCorr mod for only a single plan
method = "huge"
verbose = T

if(method=="huge"){
   for(m in unique(inputs$meta$gsp_id)){
      out <- list()
      pages <- inputs$meta$gsp_id == m
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
      saveRDS(out,paste0(filekey[filekey$var_name=="topic_corr_files",]$filepath,m))
      
   }
   
   
}

#if(method=="simple"){
#   set.seed(20000)
#   tcs <- topicCorr(model, method = "simple", verbose = TRUE)
#TODO insert simple method procedures here
#}