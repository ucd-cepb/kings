

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

text_list <- lapply(text_files[1:10],readLines)

cleanText <- function(text, cut_prop = 0.1) {
   cut = cut_prop
   text = gsub('\"\"', '', text, fixed = TRUE)
   chars = nchar(text)
   periods = stringr::str_count(text, "\\.")
   numbers = stringr::str_count(text, "[0-9]")
   caps = stringr::str_count(text, '[A-Z]')
   tildes = stringr::str_count(text, '~')
   quotes = stringr::str_count(text, '\\"')
   spaces = stringr::str_count(text, '\\s')
   valid_indices = chars > 400 & chars <= 1e5 & (periods/chars) < cut & (quotes/chars) < cut & 
      (tildes/chars) < cut & (numbers/chars) < cut & 
      (caps/chars) < cut & (spaces/chars) < (cut * 2)
   text[valid_indices] <- ifelse(chars[valid_indices] > 1e5, NA, text[valid_indices])
   text[valid_indices]
}

length(text_list)
text_list[[10]]


text_list <- lapply(text_list,cleanText)



text_files
sapply(text_list,length)


1e5 == 100000


# Prepare the text list for processing
flist = unlist(pdf_texts)
names(flist) <- names(pdf_texts)
gc()

minhash <- minhash_generator(n = 240, seed = 40)
progress_bars = T
mcores = 4
options("mc.cores" = mcores)

portal_corpus = TextReuseCorpus(text = flist,
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

cluster = makeCluster(mcores)
registerDoParallel(cl = cluster)
parallel::clusterEvalQ(cluster, 'require(data.table)')
parallel::clusterEvalQ(cluster, 'require(textreuse)')
split_buckets = foreach(x = split_corpus) %dopar% {textreuse::lsh(x, bands = 60)}

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
