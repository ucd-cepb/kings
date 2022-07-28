source('code/sgma_web_scraper.R')

#scrape data
sgma_web_scraper(box_sync = F, use_repaired = T)
#clean web data
gsp_pdf_reader(box_sync = F)
#format metadata, clean corpus, run model, graph results
gsp_topic_analyzer(build_meta = T, clean_lex = T, 
                   model_compare = F, run_model = T, viz_results = T, 
                   ntopics = 50)
