library(tidyverse)

mynets <- lapply(layered_nets, function (i) as.data.frame(i))
mynets <- data.table::rbindlist(mynets)

   #current_net <- layered_nets[[i]]
   c_df <- as.data.frame(mynets)
   c_df_neither <- c_df %>% filter(!is_info & !is_money)
   #testing that there is such thing as a multiple edge on the same layer between two nodes, which there is
   test_summary <- c_df_neither %>% group_by(.head, .tail) %>% summarize(count_ties = length(unique(doc_sent_verb))) %>% filter(count_ties > 1)
   testthat::expect_gt(nrow(test_summary), 0)
   
   #top 20 "neither" verbs
   head(sort(table(c_df_neither$head_verb_lemma), decreasing = T), n = 20)
   
   c_df_neither_be <- c_df_neither %>% filter(head_verb_lemma=="be")
   #top 20 xcomps
   head(sort(table(unlist(c_df_neither_be$xcomp_verb)), decreasing = T), n = 20)
