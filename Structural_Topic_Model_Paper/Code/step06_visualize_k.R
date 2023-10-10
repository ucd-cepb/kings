packs <- c('ggplot2','cowplot','furrr','future','stm','tidyverse','stringr','scico')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

kdiagsfilename <- filekey[filekey$var_name=="k_diag_files_stmpaper",]$filepath
kdiagsfilenamesplits <- unlist(strsplit(kdiagsfilename,split="/"))
kdiagspath <- paste(kdiagsfilenamesplits[1:(length(kdiagsfilenamesplits)-1)],collapse = "/")
kdiagspattern <- kdiagsfilenamesplits[length(kdiagsfilenamesplits)]


k_saves <- list.files(path = kdiagspath, pattern = kdiagspattern, full.names = T)
# prepare regular expression
dig_regex <- "[[:digit:]]+"
#checks for previously saved models
saved_nums <- str_extract_all(paste0(k_saves, collapse = "_"), dig_regex)[[1]]

k_choice <- case_when(!"5" %in% saved_nums ~ 5,
                      !"10" %in% saved_nums ~ 10,
                      !"20" %in% saved_nums ~ 20,
                      !"30" %in% saved_nums ~ 30,
                      !"40" %in% saved_nums ~ 40,
                      !"50" %in% saved_nums ~ 50,
                      !"60" %in% saved_nums ~ 60,
                      !"70" %in% saved_nums ~ 70,
                      !"80" %in% saved_nums ~ 80,
                      !"100" %in% saved_nums ~ 100,
                      !"120" %in% saved_nums ~ 120,
                      !"160" %in% saved_nums ~ 160,
                      !"200" %in% saved_nums ~ 200,
                      T ~ NA_real_)

if(is.na(k_choice)){
   
   k_all <- readRDS(k_saves[1])
   for (i in 2:length(k_saves)){
      k_all <- rbind(k_all, readRDS(k_saves[i]))
   }
   saveRDS(k_all, filekey[filekey$var_name=="k_diag_allmodels_stmpaper",]$filepath)
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
      geom_point(show.legend=FALSE)+
      facet_wrap(~Metric, scales = "free_y") +
      theme_bw()+
      scale_color_scico_d(palette = "roma")+
      theme(plot.margin = unit(c(25,25,25,25), "pt"))+
      labs(x = "K (number of topics)",
           y = NULL,
           title = "Model performance by number of topics"#,
           #subtitle = "subtitle"
      )
   
   ggsave(paste0("k_plot_custom_k_all.png"),plot = k_pl, device = "png", path = filekey[filekey$var_name=="k_diag_figures",]$filepath,
          width = 4020, height = 1890, dpi = 300, units = "px", bg = "white")
   
   #checkResiduals(k_options$runout[[1]], obj$documents, tol = 0.01)
   #can skip topic stability check as well as multi-modality check if model uses Spectral
   
   
}else{
   print("Not all models generated yet.")
}

