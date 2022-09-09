visualize_topics <- function(model, inputs, text_col, topic_indicators){

   packs <- c('ggplot2','ggrepel','scico','stm','tidyverse','reshape2',
              'igraph','huge','fields','ggcorrplot','htmlTable','data.table')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   
   numTopics = model$settings$dim$K
   
   label_lg <- labelTopics(model, topics = c(1:numTopics), n = 50)
   label_sm <- labelTopics(model, topics = c(1:numTopics), n = 10)
   #sageLabels can be used when the model has a content covariate
   #both print highest dprobability words and FREX words associated with each topic
   #FREX is weighted by frequency and exclusivity
   #lift() weights words higher if they have lower frequency in other topics
   #score() is similar, but log based -- see lda package
   
   #inspect words associated with topics using labelTopics
   for(i in 1:numTopics){
      print(paste0("Top 10 FREX Words in Topic ", i, ":", collapse = ""))
      print(label_sm$frex[i,])
   }
   
   topics_of_interest <- NULL
   nums_of_interest <- NULL
   for(i in 1:numTopics){
      
      if(sum(grepl(pattern = paste(gsub("\\s+","_", x=unlist(topic_indicators,use.names=F)),collapse="|"), x=label_lg$frex[i,]))>0){
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
   
   
   #TODO gratitude to francescoaberlin.blog for this tutorial
   #topics are evaluated on two components:
   #semantic coherence (frequency of co-occurrence of common words in a toipc)
   #exclusivity of words to topic
   m7_ex_sem<-as.data.frame(cbind(c(1:numTopics),
                                 exclusivity(model), 
                                 semanticCoherence(model=model, 
                                                   documents = inputs$documents), "model 7"))
   
   #can compare multiple models by adding to rbind
   models_ex_sem<-rbind(m7_ex_sem)
   
   colnames(models_ex_sem)<-c("K","exclusivity", "semantic_coherence", "model")
   models_ex_sem$exclusivity<-as.numeric(as.character(models_ex_sem$exclusivity))
   models_ex_sem$semantic_coherence<-as.numeric(as.character(models_ex_sem$semantic_coherence))
   
   options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
   
   topic_qual_plot<-ggplot(models_ex_sem, aes(semantic_coherence, 
                                              exclusivity, color = model))+
      geom_point(size = 2, alpha = 1) + 
      geom_text_repel(aes(label=K), nudge_x=.005, nudge_y=.005, 
                      size = 2.5, alpha = 0.6, force = 0.8, force_pull = 1.5,
                      max.overlaps = Inf)+
      labs(x = "Semantic coherence",
           y = "Exclusivity",
           title = "Comparing exclusivity and semantic coherence among model topics")+
      scale_color_scico_d(palette = "nuuk")+
      theme_minimal()+theme(plot.title = element_text(hjust = 0.5))
   topic_qual_plot
   ggsave("topic_quality_model_7.png",plot = topic_qual_plot, device = "png", path = "figures",
         width = 4020, height = 1890, dpi = 300, units = "px", bg = "white")
   
   
   #dim(readPNG("figures/topic_quality"))
   
   text_vect <- unlist(text_col)
   #testing if text_col includes docs.removed and removes them if applicable
   if (length(text_vect) != length(inputs$documents)){
      text_vect <- text_vect[-inputs$docs.removed]
   }
   
   #findThoughts example
   thoughts <- findThoughts(model, texts = text_vect, topics = c(1:numTopics), n = 3)
   topics = c(1:numTopics)
   #plotQuote is a graphical wrapper to help present documents as examples
   #example:
   thought_5 <- findThoughts(model, texts = text_vect, topics = 5, n = 3)
   plotQuote(thought_5, width = 30, main = "Topic 5")
   
   #TODO clean this up
   plot(model, type = "summary", xlim = c(0,0.3))

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
   
      
   #TODO stminsights
   #TODO stmBrowser
   #TODO stmCorrViz, including function toLDAvis, which enables export to the LDAvis
   #TODO stmprinter
   
   
   #look at the relationship between metadata and topics
   effect <- estimateEffect(nums_of_interest ~ admin + 
                               basin_plan +
                               sust_criteria +
                               monitoring_networks + 
                               projects_mgmt_actions + 
                               percent_dac_by_pop+
                               factor(approval)+
                               factor(priority, 
                                    levels = c("High","Medium","Low","Very Low"), ordered = FALSE)+
                               mult_gsas+
                               ag_gw_asfractof_tot_gw+
                               hviol_avg_res+
                               prop_service_gw_source+
                               service_count,
                          model,
                          meta = inputs$meta, uncertainty = "Global")
   sumef <- summary(effect, topics = c(nums_of_interest))
   saveRDS(effect,"data_temp/estimateEffects")
   
   for(i in 1:length(sumef$tables)){
      efbytopic <- as.data.table(cbind("Factors" = rownames(sumef$tables[[i]]),
                        sumef$tables[[i]]))
      efbytopic$Factors <- gsub(", levels = c\\(\"High\", \"Medium\", \"Low\", \"Very Low\"\\), ordered = FALSE","",efbytopic$Factors)
      write_csv(efbytopic, file = paste0("data_temp/eftbl_topic_",sumef$topics[i],"_",categ_no_m_na[i],".csv"))
   }
   
   #TODO clean this up
   plot.estimateEffect(effect,covariate = "ag_gw_asfractof_tot_gw")
}
