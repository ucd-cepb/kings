#TODO finish filekey on this file
visualize_topics_grouped <- function(model, inputs, text_col, topic_indicators,scatter=F,effects=F){

   packs <- c('ggplot2','ggrepel','scico','stm','tidyverse','reshape2',
              'igraph','huge','fields','ggcorrplot','htmlTable','data.table','viridis','igraph')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   
   filekey <- read.csv("filekey.csv")
   source(filekey[filekey$var_name=="topic_topic_corr_plot_function",]$filepath)
   
   numTopics = model$settings$dim$K
   
   set.seed(80000)
   label_lg <- labelTopics(model, topics = c(1:numTopics), n = 50)
   label_sm <- labelTopics(model, topics = c(1:numTopics), n = 10)
   #sageLabels can be used when the model has a content covariate
   #both print highest dprobability words and FREX words associated with each topic
   #FREX is weighted by frequency and exclusivity
   #lift() weights words higher if they have lower frequency in other topics
   #score() is similar, but log based -- see lda package
   
   #inspect 50 ten frex words associated with each topics using labelTopics
   for(i in 1:numTopics){
      cat(paste0(paste0("Topic ", i, ":", collapse = ""),'\n')) 
      paste0(paste(label_lg$frex[i,],collapse = ", "),'\n') %>% cat()
   }
   
   for(i in 1:numTopics){
      cat(paste0(paste0("Topic ", i, ":", collapse = ""),'\n')) 
      paste0(paste(label_sm$frex[i,],collapse = ", "),'\n') %>% cat()
   }
   
   topics_of_interest <- NULL
   nums_of_interest <- NULL
   for(i in 1:numTopics){
      
      if(sum(grepl(pattern = paste(gsub("\\s+","_", x=unlist(topic_indicators,use.names=F)),collapse="|"), 
                   x=label_lg$frex[i,]))>0){
         print(paste0("Top 50 FREX Words in Topic ", i, ":", collapse = ""))
         print(label_lg$frex[i,])
         nums_of_interest <- append(nums_of_interest,i)
         topics_of_interest<-append(topics_of_interest,paste0("Topic_",strrep("0",2-nchar(toString(i))),i))
         
      }
   }
   is_cc <- vector(length=0)
   is_ej <- vector(length=0)
   is_dw <- vector(length=0)
   is_gde <- vector(length=0)
   for(i in 1:numTopics){
      is_cc[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", x=topic_indicators[["cc"]]),
                                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      is_ej[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", topic_indicators[["ej"]]),
                                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      is_dw[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", topic_indicators[["dw"]]),
                                                   collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      is_gde[i] <- ifelse(sum(grepl(pattern = paste(gsub("\\s+","_", topic_indicators[["gde"]]),
                                                    collapse="|"), x=label_lg$frex[i,]))>0,T,F)
      
   }
   is_multi <- ifelse(is_cc+is_ej+is_dw+is_gde >1,T,F)
   
   categ <- case_when(is_multi ~ "Multi",
                      is_cc ~ "CC",
                      is_ej ~ "EJ",
                      is_dw ~ "DW",
                      is_gde ~ "GDE")
   categ_no_na <- categ[!is.na(categ)]
   
   categ_no_multi <- ifelse(is.na(categ),NA,paste0(
      ifelse(is_cc==T,"CC",""),ifelse(is_dw==T,"DW",""),
      ifelse(is_ej==T,"EJ",""),ifelse(is_gde==T,"GDE","")
   ))
   
   categ_no_m_na <- categ_no_multi[!is.na(categ_no_multi)]
   
   c_temp <- lapply(c(1:numTopics), function(x) c(ifelse(is_cc[x]==T,"CC",""),
                   ifelse(is_dw[x]==T,"DW",""),
                   ifelse(is_ej[x]==T,"EJ",""),
                   ifelse(is_gde[x]==T,"GDE","")))
   
   categ_print <- lapply(c_temp, function(x) paste0(x[nzchar(x)],collapse = " and "))
   
   #print top 50 FREX words in topics of interest
   for(i in 1:length(topics_of_interest)){
      cat(paste0(paste0("Topic ", nums_of_interest[i], 
                        ifelse(nzchar(categ_print[[nums_of_interest[i]]])," (",""),categ_print[[nums_of_interest[i]]],
                        ifelse(nzchar(categ_print[[nums_of_interest[i]]]),"):",""), collapse = ""),'\n')) 
      paste0(paste(label_lg$frex[nums_of_interest[i],],collapse = ", "),'\n') %>% cat()
   }
   
   #TODO gratitude to francescoaberlin.blog for this tutorial
   #topics are evaluated on two components:
   #semantic coherence (frequency of co-occurrence of common words in a toipc)
   #exclusivity of words to topic

   if(scatter==T){
      set.seed(80000)
      m20240528_ex_sem<-as.data.frame(cbind(c(1:numTopics),
                                     exclusivity(model), 
                                     semanticCoherence(model=model, 
                                                       documents = inputs$documents), "model 20240528"))
      
      #can compare multiple models by adding to rbind
      models_ex_sem<-rbind(m20240528_ex_sem)
      
      colnames(models_ex_sem)<-c("K","exclusivity", "semantic_coherence", "model")
      models_ex_sem$exclusivity<-as.numeric(as.character(models_ex_sem$exclusivity))
      models_ex_sem$semantic_coherence<-as.numeric(as.character(models_ex_sem$semantic_coherence))
      
      options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
      
      topic_qual_plot<-ggplot(models_ex_sem, aes(semantic_coherence, 
                                                 exclusivity, #color = model
                                                 ))+
         geom_point(size = 4, alpha = 1) + 
         geom_text_repel(aes(label=K), nudge_x=.005, nudge_y=.005, 
                         size = 4, alpha = 0.6, force = 0.8, force_pull = 1.5,
                         max.overlaps = Inf)+
         labs(x = "Semantic coherence",
              y = "Exclusivity",
              title = "Comparing exclusivity and semantic coherence among model topics")+
         scale_color_scico_d(palette = "nuuk")+
         theme_minimal()+theme(plot.title = element_text(hjust = 0.5))
      topic_qual_plot
      saveRDS(topic_qual_plot,paste0(filekey[filekey$var_name=="stmpaper_figures",]$filepath,"/topic_qual_plot"))
      ggsave("topic_quality_model.png",plot = topic_qual_plot, device = "png", path = filekey[filekey$var_name=="stmpaper_figures",]$filepath,
             width = 4020, height = 1890, dpi = 600, units = "px", bg = "white")
      
      
   }
   
   #dim(readPNG("figures/topic_quality"))
   
   metadata <- inputs$meta
   meta_with_text <- left_join(metadata, gsp_text_with_meta)
   text_vect <- meta_with_text$text
   
   #findThoughts example
   thought18 <- findThoughts(model, texts = text_vect, topics = c(18), n=10)
   topics = c(1:numTopics)
   #plotQuote is a graphical wrapper to help present documents as examples
   #example:
   #thought_5 <- findThoughts(model, texts = text_vect, topics = 5, n = 3)
   #
   
   #TODO clean this up
   #plot(model, type = "summary", xlim = c(0,0.3))

   #tagging pages as a topic
   theta <- as_tibble(model$theta)
   tags <- rep(NA, nrow(theta))
   for(i in 1:numTopics){
      for(j in 1:nrow(theta)){
         if(theta[[i]][j]>=0.7){
            tags[j] <- paste0("topic",i,collapse = "")
         }
      }
   }
   
   top_top_corr_plots(model, method = "huge", topics_of_interest, categ)
   
   saveRDS(topics_of_interest, filekey[filekey$var_name=="topics_of_interest_stmpaper",]$filepath)   
   
   #TODO pick up work here
   if(effects ==T){
      #look at the relationship between metadata and topics
      set.seed(432)
      
      #interactions
      set.seed(432)
      interactioneffects <- estimateEffectDEV(c(7,24) ~
                                        maxdryspell_scaled * Republican_Vote_Share_scaled ,
                                     stmobj = model, group = T,
                                     meta = inputs$meta, uncertainty = "Global")
      plot(interactioneffects, covariate = "Republican_Vote_Share_scaled",
           topics = c(7, 24), model = model, method = "difference",
           cov.value1 = -2, cov.value2 = 2,
           xlab = "More Conservative ... More Liberal",
           main = "Effect of Liberal vs. Conservative",
           xlim = c(-0.1, 0.1), labeltype = "custom",
           custom.labels = c("Climate Scenarios","Water Hydrology and Change"))
      
      plot(interactioneffects,"Republican_Vote_Share_scaled", method = "continuous", 
             topics = 7, 
           model= model,printlegend = FALSE,xlab= "Republican Vote Share (Scaled)") 
      
      
      
      source(filekey[filekey$var_name=="estimate_effect_dev_function",]$filepath)
      set.seed(432)
      esteffect <- vector(mode = "list", length = 4)
      greplist <- list(grep("DW",categ_no_multi), grep("EJ",categ_no_multi), 
                       grep("CC",categ_no_multi), grep("GDE",categ_no_multi))
      unitopics <- c("DW","EJ","CC","GDE")
      for(i in 1:4){
         esteffect[[i]] <- estimateEffectDEV(greplist[[i]] ~ admin + 
                                   basin_plan +
                                   sust_criteria +
                                   monitoring_networks + 
                                   projects_mgmt_actions + 
                                   mult_gsas +
                                   priority_category +
                                   basin_population_log_scaled +
                                   (Agr_Share_Of_GDP_scaled +
                                       Republican_Vote_Share_scaled) *
                                   (log_well_MCL_exceedance_count_by_log_pop_scaled +
                                       percent_dac_by_pop_scaled +
                                       fract_of_area_in_habitat_log_scaled +
                                       dsci_scaled),
                                model,
                                meta = inputs$meta, uncertainty = "Global", group = T)
         set.seed(43)
         sumef <- summary(esteffect[[i]])
         efbytopic <- as.data.table(cbind("Factors" = rownames(sumef$tables[[1]]),
                                          sumef$tables[[1]]))
         write_csv(efbytopic, file = paste0(filekey[filekey$var_name=="effect_table_csvs_stmpaper",]$filepath,
                                            sumef$topics,"_",unitopics[i],".csv"))
         efbytopicskinny <- as.data.table(cbind("Factors" = rownames(sumef$tables[[1]]),
                                                sumef$tables[[1]]))[,c("Factors","Estimate","Pr(>|t|)")]
         write_csv(efbytopicskinny, file = paste0(filekey[filekey$var_name=="effect_table_condensed_csvs_stmpaper",]$filepath,
                                                  sumef$topics,"_",unitopics[i],".csv"))
         
         
      }
     
      saveRDS(esteffect,filekey[filekey$var_name=="estimate_unitopic_effects",]$filepath)
      
      
      
   }
   
   
   
   
}
