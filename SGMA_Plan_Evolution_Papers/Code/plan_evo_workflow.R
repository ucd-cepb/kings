#note -- the assessment links in the gsp_web_vars file are not always proper links -- a check must be done to determine whether the link is broken or not

#after scraping/collecting pdfs, 
#(example shown in SGMA_Plan_Evolution_Papers > Code > sgma_plan_evo_webscraper), 
#documents need to be processed

#an example of this is shown in Text_Gov_Network_Methods_Paper > Code > Step1_pdf_reader_cleaner.R
#It uses the function textNet::pdf_clean
#Things to consider here: 
# 1) are there any pages, eg public comments and references, that you want to exclude?
#See "keep_pages" variable as an example
#2) Do you want to use the built-in header/footer removal or create a custom one for your purposes?

#next run the textNet function parse_text() 
#You can use the textNet vignette for help on syntax and process with this.
# Take note of any warning messages about the proportion of English words, which could signal pdf formatting issues

#Next run the textNet function textnet_extract()
#You can use the textNet vignette for help on syntax and process with this.

#Next decide if there are any custom entities and abbreviations you want to recognize
#The govscienceuseR has a list of state agencies for every state; Tyler or Elise can help you find it
#Highly recommend going through that list and adding their abbreviations by hand, since they are not comprehensive 
#See Text_Gov_Network_Methods_Paper/Code/utils/govscicleaning.R for an example

#Next run find_acronyms() and disambiguate() to clean up the nodes.
#See Text_Gov_Network_Methods_Paper/Code/step4_process_edgelists_nodelists.R for an example
#of how state/federal government agencies, local agencies, and the results of find_acronyms()
#can be fed into the disambiguate() function to clean up nodes.
#The textNet vignette can also provides an example of this

#To analyze how the networks change over time, you can use the 
#export_to_network() function from textNet to convert the data files into network objects, 
#which would enable them to be analyzed with a tergm to analyze the changes over time

