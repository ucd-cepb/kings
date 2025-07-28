library(textNet)
library(findpython)

appos <- list("We are. The US Armed Forces Chief, Miranda Landsby, is the best of all worlds in the US Army. This finding of Mary is of utmost importance to Sally. I, the President of the Presidio Club, am a person who is friends with Jonas. I am hungry. The agency is afloat. We are truly doomed.")
appos <- list("I want to be friends with him. He is friends with me. He is a person who was. I am a person who is a friend. I am friends with him. We are friends. I am tired. He is tired of me. We are tired. They are tired of us.")
ret_path <- find_python_cmd(required_modules = c('spacy', 'en_core_web_lg'))
names(appos) <- "doc1"
apdf1 <- textNet::parse_text(ret_path = ret_path, keep_hyph_together = F,
                            phrases_to_concatenate = NA,
                            concatenator = "_", 
                            parsed_filenames = "appos_files",
                            text_list = appos
                              )

View(apdf1[[1]])
apnet <- textNet::textnet_extract(apdf1[[1]], keep_incomplete_edges = T)
View(apnet$edgelist)
