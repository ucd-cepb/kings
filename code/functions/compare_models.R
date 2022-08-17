compare_models <- function(optimize_K=F, k_set, topic_stability = F, obj, numTopics = 0, enterChoice = F){
   packs <- c('ggplot2','cowplot','furrr','future','stm','tidyverse')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   check <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(check>0)){print(check)}else{print("got 'em all")}
   lapply(packs, require, character.only = TRUE)
   
   if(optimize_K==T){
      
      k_nums <- paste(k_set, sep = "", collapse = "_")
      
      plan(multisession)
      
      heldout <- make.heldout(obj$documents,
                              obj$vocab, 
                              N=floor(0.05 * length(obj$documents)), 
                              proportion=0.5, 
                              seed=12)
      
      heldout
      #not sure if this overwrites heldout seed
      model_collection <- tibble(K = k_set) %>%
         mutate(k_model = future_map(K, ~stm(heldout$documents, heldout$vocab, 
                                                 K = .,
                                                 prevalence =~ admin + 
                                                    basin_plan +
                                                    sust_criteria +
                                                    monitoring_networks + 
                                                    projects_mgmt_actions + 
                                                    percent_dac_by_pop+
                                                    as.factor(approval)+
                                                    as.factor(priority)+
                                                    mult_gsas+
                                                    ag_gw_asfractof_tot_gw,
                                                 init.type = "Spectral",
                                                 max.em.its = 200,
                                                 data = obj$meta), .options = furrr_options(seed = T)))
   
      saveRDS(model_collection, file = paste("data_temp/k_model_collection_",k_nums))
      
      k_opt <- model_collection %>%
         mutate(exclusivity = map(k_model, exclusivity),
                semantic_coherence = map(k_model, semanticCoherence, heldout$documents),
                eval_heldout = map(k_model, eval.heldout, heldout$missing),
                residual = map(k_model, checkResiduals, heldout$documents),
                bound =  map_dbl(k_model, function(x) max(x$convergence$bound)),
                lfact = map_dbl(k_model, function(x) lfactorial(x$settings$dim$K)),
                lbound = bound + lfact,
                iterations = map_dbl(k_model, function(x) length(x$convergence$bound)))
      #TODO thank Julia Silge
      k_opt
      
      saveRDS(k_opt, file = paste0("data_temp/k_opt_",k_nums))
      
      k_pl <- k_opt %>%
         transmute(K,
                   `Lower bound` = lbound,
                   Residuals = map_dbl(residual, "dispersion"),
                   `Semantic coherence` = map_dbl(semantic_coherence, mean),
                   `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
         gather(Metric, Value, -K) %>%
         ggplot(aes(K, Value, color = Metric)) +
         geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
         facet_wrap(~Metric, scales = "free_y") +
         theme_bw()+
         theme(plot.margin = unit(c(25,25,25,25), "pt"))+
         labs(x = "K (number of topics)",
              y = NULL,
              title = "Model performance by number of topics"#,
              #subtitle = "subtitle"
              )
      
      ggsave(paste0("k_plot_custom_",k_nums,".png"),plot = k_pl, device = "png", path = "figures",
             width = 4020, height = 1890, dpi = 300, units = "px", bg = "white")
      
      #checkResiduals(k_options$runout[[1]], obj$documents, tol = 0.01)
      if(enterChoice == T){
         #functionality to choose your favorite model
         sel_model_num <- as.integer(
            readline(prompt = "Enter the number of your preferred model: "))
         selected_model <- k_options$runout[[sel_model_num]]
      }
      
   }
   #can skip this as well as multi-modality check if model uses Spectral
   if(topic_stability==T){
      stab_compare <- selectModel(out$documents, out$vocab, 
                                   K = numTopics,
                                   prevalence =~ admin + 
                                      basin_plan +
                                      sust_criteria +
                                      monitoring_networks + 
                                      projects_mgmt_actions + 
                                      SVI_na_adj+
                                      as.factor(approval)+
                                      as.factor(priority)+
                                      ag_gw_asfractof_tot_gw, 
                                   max.em.its = 75,
                                   data = out$meta, 
                                   runs = 20, 
                                   seed = 12143278)
      plotModels(stab_compare, legend.position = "bottomright")
      #can also add residuals checks and held-out likelihood estimation
      
   }
   
   model_comp <- list("k_options" = ifelse(exists("k_options"),k_options,NA), 
                      "k_plot" = ifelse(exists("k_plot.png"),k_plot,NA),
                      "my_k_model" = ifelse(exists("selected_model"), selected_model,NA),
                      "stab_compare" = ifelse(exists("stab_compare"),stab_compare,NA),
                      "stability_plot" = ifelse(exists("stability_plot"),stability_plot,NA))
   saveRDS(model_comp, "data_temp/model_comparison")
   saveRDS(k_options, "data_temp/k_options")
   return(model_comp)
   
   #old version
   #let searchK figure out how many topics to generate
   #using 1 core so progress will be shown
   k_options <- searchK(obj$documents, obj$vocab, 
                        K = c(5, 10, 20, 40, 80),
                        prevalence =~ admin + 
                           basin_plan +
                           sust_criteria +
                           monitoring_networks + 
                           projects_mgmt_actions + 
                           percent_dac_by_pop+
                           as.factor(approval)+
                           as.factor(priority)+
                           mult_gsas+
                           ag_gw_asfractof_tot_gw,
                        N = floor(0.05 * length(obj$documents)),
                        cores = 1,
                        init.type = "Spectral",
                        data = obj$meta)
   compar_plot <- plot(k_options)
   compar_plot
   p1 <- ggplot(k_options$results, aes(x=K, y=exclus)) 
   p2 <- ggplot(mtcars, aes(qsec, mpg)) +
      geom_point()
   
   plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
   ggsave("k_plot.png",plot = compar_plot, device = "png", path = "figures",
          width = 4020, height = 1890, dpi = 300, units = "px", bg = "white")
   
}