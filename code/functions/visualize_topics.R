visualize_topics <- function(model, inputs, text_col){

   packs <- c('ggplot2','ggrepel','scico')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   
   numTopics = model$settings$dim$K
   labels <- labelTopics(model, topics = c(1:numTopics), n = 10)
   #sageLabels can be used when the model has a content covariate
   #both print highest probability words and FREX words associated with each topic
   #FREX is weighted by frequency and exclusivity
   #lift() weights words higher if they have lower frequency in other topics
   #score() is similar, but log based -- see lda package
   
   #inspect words associated with topics using labelTopics
   for(i in 1:numTopics){
      print(paste0("Top 10 FREX Words in Topic ", i, ":", collapse = ""))
      print(labels$frex[i,])
   }
   
   #TODO gratitude to francescoaberlin.blog for this tutorial
   #topics are evaluated on two components:
   #semantic coherence (frequency of co-occurrence of common words in a toipc)
   #exclusivity of words to topic
   m5_ex_sem<-as.data.frame(cbind(c(1:numTopics),
                                 exclusivity(model), 
                                 semanticCoherence(model=model, 
                                                   documents = inputs$documents), "model 5"))
   
   #can compare multiple models by adding to rbind
   models_ex_sem<-rbind(m5_ex_sem)
   
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
  ggsave("topic_quality.png",plot = topic_qual_plot, device = "png", path = "figures",
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
   plot.STM(gsp_model_saved, type = "labels", topics = c(4,6))
   plot.STM(gsp_model_saved, type = "perspective", topics = c(48, 41))
   plot(gsp_model_saved,type = "labels", topics = c(1:4))
   plot(gsp_model_saved,type = "perspectives", topics = c(1,40))
   plot(gsp_model_saved, type = "summary", xlim = c(0,0.3))

   #tagging pages as a topic
   theta <- as_tibble(gsp_model_saved$theta)
   tags <- rep(NA, nrow(theta))
   for(i in 1:numTopics){
      for(j in 1:nrow(theta)){
         if(theta[[i]][j]>=0.7){
            tags[j] <- paste0("topic",i,collapse = "")
         }
      }
   }
   
   #grid of topic percent by gsp
   theta <- as_tibble(cbind(gsp_model_saved$theta,model$meta$gsp_id) %>%
                         group_by(gsp_id) %>% summarize(perc_topic = sum(theta)/n())
   
   
   #look at the relationship between metadata and topics
   effect <- estimateEffect(1:50 ~ admin + 
                             basin_plan +
                             sust_criteria +
                             monitoring_networks + 
                             projects_mgmt_actions +
                             percent_dac_by_pop+
                             as.factor(approval) +
                             as.factor(priority)+ 
                             mult_gsas+
                             ag_gw_asfractof_tot_gw, 
                          model,
                          meta = inputs$meta, uncertainty = "None")
   summary(effect, topics = c(1:50))
   
   #calculate topic correlations: topicCorr
}
