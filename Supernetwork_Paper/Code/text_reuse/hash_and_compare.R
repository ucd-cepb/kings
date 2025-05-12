

##### A FEW STEPS HERE, PARTICULARLY THE CORPUS GENERATION, ARE EXTREMELY COMPUTATIONALLY INTENSIVE
##### THIS WAS RUN ON A GOOGLE VIRTUAL MACHINE WITH 16 CORES AND 360 GB OF RAM (MEMORY IS THE LIMITING FACTOR, ADDING MORE CORES DOES NOT HELP)
##### IF MEMORY GETS TOO TIGHT, THE CORPUS GENERATION WILL APPEAR TO WORK, BUT A MESSAGE WILL OCCUR THAT SAYS A BUNCH (E.G. 1M OR 100K) OF THE TEXTS
# WERE TOO SHORT AND THUS DROPPED. IF THIS OCCURS, IT MEANS THE CODE DID NOT WORK RIGHT AND SHOULD BE RUN AGAIN.
where = 'remote'# or "remote"
rerun_existing = F

pack = c('data.table','quanteda','tm','textclean','stringr','pbapply',
         'parallel','doParallel','benchmarkme','tidyverse','textreuse','dplyr')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

# Read PDF texts from the portal_files directory
pdf_directory <- 'kings/Multipurpose_Files/portal_files'

text_files <- list.files('Multipurpose_Files/portal_files/',pattern= 'txt$',full.names = T)
finfo <- file.info(text_files)
#finfo <- finfo[finfo$ctime > lubridate::ymd_hms('2025-05-12 11:00:00 PDT'),]
#text_files <- text_files[text_files %in% rownames(finfo)]

page_delimiter <- "<<PAGE_BREAK>>"

text_list <- lapply(text_files,function(x) {
   print(x)
   read_back <- readLines(x, warn = FALSE)
   read_back <- paste(read_back, collapse = "\n")
   pages <- unlist(strsplit(read_back, page_delimiter, fixed = TRUE))
})


cleanText <- function(text, cut_prop = 0.1,space_prop_multiplier = 5) {
   text = gsub('\"\"', '', text, fixed = TRUE)
   chars = nchar(text)
   periods = stringr::str_count(text, "\\.")
   numbers = stringr::str_count(text, "[0-9]")
   caps = stringr::str_count(text, '[A-Z]')
   tildes = stringr::str_count(text, '~')
   quotes = stringr::str_count(text, '\\"')
   spaces = stringr::str_count(text, '\\s')
   valid_indices = chars > 400 & chars <= 1e10 & (periods/chars) < cut_prop & (quotes/chars) < cut_prop & 
      (tildes/chars) < cut_prop & (numbers/chars) < cut_prop & 
      (caps/chars) < cut_prop & (spaces/chars) < (cut_prop * space_prop_multiplier)
   text[!valid_indices] <- NA
   return(text)
}

library(pbapply)
text_list2 <- pblapply(text_list,cleanText,cl = 8)

file_rep_list <- mapply(function(x,y) rep(x,y),x = basename(files),y = sapply(text_list2,length))
file_vec <- unlist(file_rep_list)
page_list <- lapply(text_list2,seq_along)
page_vec <- unlist(page_list)

text_vec <- unlist(text_list2)
names(text_vec) <- paste0(file_vec,'_',page_vec)


gc()
minhash <- minhash_generator(n = 240, seed = 40)
progress_bars = T
mcores = 8
options("mc.cores" = mcores)
portal_corpus = TextReuseCorpus(text = text_vec,
                                tokenizer = tokenize_ngrams, n = 10,
                                minhash_func = minhash, keep_tokens = TRUE,
                                progress = progress_bars, skip_short = T)
gc()

# Adjust the number of cores to match the available resources on your computer.
# For example, if you have 8 cores available, you might set mcores to 4 to leave
# some resources for other processes.
mcores = 4
options("mc.cores" = mcores)
split_corpus_ntiles = dplyr::ntile(x = seq(portal_corpus), n = mcores*10)
split_corpus = split(portal_corpus, split_corpus_ntiles)
rm(portal_corpus)
gc()

#cluster = makeCluster(mcores)
#registerDoParallel(cl = cluster)
#parallel::clusterEvalQ(cluster, 'require(data.table)')
#parallel::clusterEvalQ(cluster, 'require(textreuse)')

##### this is currently just running sequentially ###
split_buckets = foreach(x = split_corpus) %do% {textreuse::lsh(x, bands = 60)}

while(any(sapply(split_buckets, is.null))){
  null_fails = which(sapply(split_buckets, is.null))
  split_buckets[null_fails] <- pblapply(null_fails, function(x) lsh(split_corpus[[x]], bands = 60, progress = F), cl = 5)
}

portal_buckets = do.call(rbind, split_buckets)
portal_candidates <- lsh_candidates(buckets = portal_buckets)
require(dplyr)
candidate_splits = split(portal_candidates, ntile(1:nrow(portal_candidates), n = nrow(portal_candidates) %/% 10000))
gc()

parallel::clusterEvalQ(cluster, 'require(data.table)')
parallel::clusterEvalQ(cluster, 'require(textreuse)')
score_list = foreach(i = candidate_splits) %dopar% {
  send_names = unique(c(i$a, i$b))
  send_text = flist[send_names]
  score_list = mapply(function(aa, bb) textreuse::align_local(a = aa, b = bb)$score, aa = send_text[i$a], bb = send_text[i$b])
  data.table::data.table(a = i$a, b = i$b, score = score_list)
}
stopCluster(cluster)
gc()

score_dt = rbindlist(score_list)
score_dt <- score_dt[score >= 300,]

dir.create('Supernetwork_Paper/data_products/score_results/', showWarnings = FALSE)
saveRDS(score_dt, paste0("Supernetwork_Paper/data_products/score_results/portal_page_scores_scratch_file.rds"), compress = TRUE)
