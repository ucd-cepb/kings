library(statnet)
library(tidyverse)
jsim <- statnetjsim <- readRDS('Network_Innovation_Paper/data_products/03B_text_reuse/score_results/section_jaccard_dissimilarity_20250709.rds')




table(section_to_section$jaccard_dissimilarity<0.6)

section_to_section <- jsim |> 
   filter(gsp_id_1 != gsp_id_2) |> 
   filter(section_1==section_2) |> 
   arrange(gsp_id_1,gsp_id_2) |> 
   filter(jaccard_dissimilarity)

library(statnet)
v_ids <- sort(union(section_to_section$gsp_id_1,section_to_section$gsp_id_2))
net <- network.initialize(n = length(v_ids),directed = F,hyper = F,loops = F,multiple = T)
net %v% 'vertex.names' <- v_ids

add.edges(net,
          tail = match(section_to_section$gsp_id_1,network.vertex.names(net)),
          head = match(section_to_section$gsp_id_2,network.vertex.names(net)),
          names.eval=rep(list(list("section","jaccard_dis")),nrow(section_to_section)), 
          vals.eval=lapply(1:nrow(section_to_section),function(r){as.list(section_to_section[r,4:5])}))



library(MASS)

library(data.table)
section_to_section2 <- jsim |> filter(gsp_id_1 != gsp_id_2) |> filter(section_1==section_2) |> arrange(gsp_id_1,gsp_id_2) |> 
   filter(section_1 == 'admin')
dmat <- pivot_wider(section_to_section2,id_cols = gsp_id_1,names_from = gsp_id_2,values_from = jaccard_dissimilarity)
rownames(dmat) <- dmat$gsp_id_1
dmat <- dmat |> dplyr::select(-gsp_id_1) |> as.matrix()
dconfig <- as.dist(t(dmat))
fit <- isoMDS(dconfig, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = 
        rownames(fit$points), cex=.7)


row.names(dconfig)
colnames(config)

list.ergm.proposals()
help("ergm-proposals")
ergm(net ~ sum(pow = 1),reference = ~Unif(0,1),control = control.ergm(MCMC.prop = ~randomtoggle))

?ergmTerms()
ergmReference?Unif
search.ergmTerms(search = 'Sum')

add.edges(net,tail = section_to_section$gsp_id_1,head = section_to_section$gsp_id_2,names.eval = list(rep('section')))

?add.edges




# Save section metadata
section_info_file <- paste0(CONFIG$output_dir, "section_metadata_", 
                            format(Sys.time(), "%Y%m%d"), ".rds")
saveRDS(section_info, section_info_file, compress = TRUE)


# Save summary
summary_file <- paste0(CONFIG$output_dir, "section_jaccardy_dissimilarity_", 
                       format(Sys.time(), "%Y%m%d"), ".csv")
write.csv(summary_stats, summary_file, row.names = FALSE)


cat(paste("Section metadata saved to", section_info_file, "\n"))
cat(paste("Summary statistics saved to", summary_file, "\n"))
cat("Process completed successfully\n")




# Create summary statistics
summary_stats <- all_dissimilarities %>%
   group_by(same_gsp, same_section_type) %>%
   summarise(
      mean_dissimilarity = mean(jaccard_dissimilarity, na.rm = TRUE),
      median_dissimilarity = median(jaccard_dissimilarity, na.rm = TRUE),
      sd_dissimilarity = sd(jaccard_dissimilarity, na.rm = TRUE),
      n_pairs = n(),
      .groups = "drop"
   )
