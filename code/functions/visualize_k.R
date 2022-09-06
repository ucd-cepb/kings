packs <- c('ggplot2','cowplot','furrr','future','stm','tidyverse','stringr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

k_saves <- list.files(path = "data_temp", pattern = "k_diag", full.names = T)
# prepare regular expression
dig_regex <- "[[:digit:]]+"
#checks for previously saved models
saved_nums <- str_extract_all(paste0(k_saves, collapse = "_"), dig_regex)[[1]]

k_choice <- case_when(!"5" %in% saved_nums ~ 5,
                      !"10" %in% saved_nums ~ 10,
                      !"20" %in% saved_nums ~ 20,
                      !"40" %in% saved_nums ~ 40,
                      !"80" %in% saved_nums ~ 80,
                      !"140" %in% saved_nums ~ 140,
                      !"200" %in% saved_nums ~ 200,
                      T ~ NA_real_)

if(is.na(k_choice)){
   
   k_all <- readRDS(k_saves[1])
   for (i in 2:length(k_saves)){
      k_all <- rbind(k_all, readRDS(k_saves[i]))
   }
   saveRDS(k_all, "data_temp/k_all")
   k_pl <- k_all %>%
      transmute(K,
                #`Lower bound` = lbound,
                Exclusivity = exclusivity,
                Residuals = residual,
                `Semantic coherence` = semantic_coherence,
                `Held-out likelihood` = eval_heldout) %>%
      gather(key = Metric, value = Value, -K) %>%
      ggplot(aes(K, Value, color = Metric)) +
      geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
      facet_wrap(~Metric, scales = "free_y") +
      theme_bw()+
      scale_color_scico_d(palette = "roma")+
      theme(plot.margin = unit(c(25,25,25,25), "pt"))+
      labs(x = "K (number of topics)",
           y = NULL,
           title = "Model performance by number of topics"#,
           #subtitle = "subtitle"
      )
   
   ggsave(paste0("k_plot_custom_k_all.png"),plot = k_pl, device = "png", path = "figures",
          width = 4020, height = 1890, dpi = 300, units = "px", bg = "white")
   
   #checkResiduals(k_options$runout[[1]], obj$documents, tol = 0.01)
   #can skip topic stability check as well as multi-modality check if model uses Spectral
   
   
}else{
   print("Not all models generated yet.")
}

