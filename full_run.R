source('code/sgma_web_scraper.R')
source('code/gsp_pdf_reader.R')
source('code/gsp_topic_analyzer.R')

#scrape data
sgma_web_scraper(box_sync = F, use_repaired = T)
#clean web data
gsp_pdf_reader(box_sync = F)
#format metadata, clean corpus, run model, graph results
gsp_topic_analyzer(dac_corr_check = F, build_meta = T, clean_lex = T, 
                   model_compare = F, run_model = T, viz_results = T, 
                   ntopics = 50)

#clean lex and test how many topics to use
gsp_topic_analyzer(dac_corr_check = F, build_meta = F, clean_lex = T, 
                   model_compare = T, run_model = T, viz_results = T, 
                   ntopics = 50)