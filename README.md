# kings
analyzing CA SGMA GSPs to understand groundwater management networks

This repository contains several analysis projects, each organized as folders under the "code" folder.

The code that accompanies the large demonstration case study of the textnet package is found under "textnet_demo_workflow". The files in this folder generate the network data used in the case study, as well as analysis outputs and accompanying figures. We ran our code in order, from step 1 to step 11, to generate our analysis. These scripts no longer need to be run, as the outputs are already generated. Some additional metadata files are necessary for this analysis, which were generated through the "stm_workflow". 

The code that accompanies the structural topic model is found under "stm_workflow". The files in this folder generate the cleaned lexicon, topic model, analysis outputs, and accompanying figures. We ran our code in order, from step 1 to step 8, to generate our analysis.

The code in the "verb_analysis_workflow" begins an exploratory analysis of the verbs found in the edges of the textnet_demo network. This is not complete.

The code in the "approval_analysis_workflow" begins an exploratory analysis of the change in plans over time and their relationship to approval status. This is not complete.

Quick Start:

The "gsp_text_with_meta" file is the main file that is used in all subsequent analysis. This is the result of steps 1-3 of "stm_workflow". (Step 1, which generates the plan metadata and downloads a specific set of GSP pdfs, is not fully reproducible, as it was customized to the website specifications and content at particular dates, which have since shifted. However, by starting with the "gsp_text_with_meta" file and the GSP pdfs that are included on the accompanying Box drive, the various analyses are reproducible.


Note on Data Files:

Large lex files are saved locally immediately after generation with a tag for the date and time created. The latest version is automatically retrieved so that analysis can be completed in stages, and to enable RStudio restarts in case of memory scarcity without loss of progress. These files are too large to include in Github.

Outline of scripts and functions:

(1) sgma_web_scraper (collects raw data)
uses rselenium to crawl main GSP portal and generate a data table including:

Numeric id for each GSP
Basin name
DWR approval status
GSP Local ID if applicable
Link to the webpage for that GSP if applicable.

Crawls each GSP webpage to:
Save each plan summary spreadsheet in data_raw: gsp_num_id_xxxx.xlsx
Saves each plan pdf in data_raw: gsp_num_id_xxxx.pdf
List names & number of GSAs involved (1 or multiple) and add this to the data table

Writes the data table vars to data_output/gsp_ids.csv
   
(2) gsp_pdf_reader (generates readable versions of data)
uses pdftools::pdf_text to read each gsp_num_id_xxxx.pdf into a character vector 
   with an element for each page
Saves each as an RDS file: gsp_num_id_xxxx_text
Uses helper function: 
   (2A) functions/create_page_key
   Takes a gsp_num_id_xxxx.xlsx file and returns formatted tibble 
      called gsp_num_id_xxxx_categories that tags sections and subsections 
      using helper function:
      (2Ai) functions/read_plan_element
      and uses helper function:
      (2Ab) functions/consolidate_pgs
      to create a vector of page numbers associated with each
      category, subcategory, and plan element,
      which can be used to subset gsp_num_id_xxxx_text to retrieve 
      the pages associated with a plan category or element. 

(3) gsp_topic_analyzer:
uses helper function:
   (3A) functions/create_lang_meta, which creates & saves a data table 
   gsp_text_with_lang including all gsps, where each row is a page. 
      Columns include matching metadata about that page.
      Includes data from CNRA's final_515_table, downloaded to 
         kings/data_raw 
Uses helper function:
   (3B) functions/create_dac_meta, which generates spatial metadata
   regarding social vulnerability index and disadvantaged community status 
   from several shapefiles that are too large to include in Github. These files
   may be obtained through syncing with Box. 
      (3Bi) The dac_svi_analysis function evaluates the relationship between
         svi and dac status. 
appends spatial data to gsp_text_with_lang to create gsp_text_with_meta
Uses helper function:
   (3C) functions/lex_clean, which cleans the text data in 
      gsp_text_with_meta and prepares it for use in the stm:
      Formats mathematical script as normal font.
      Removes parenthetical parts at the ends of words, e.g. "word(s)"
      Tokenizes using quanteda, removing punctuation, urls, extra spaces,
      and numbers.
      Removes any case-sensitive custom stopwords.
      Uses helper function:
      (3Ci) generate_place_names, which formats a collection of all
         place names and county names in California, from Gazetteer
         files.
      (3Cii) custom_dictionary, which formats the EPA climate change
         dictionary and a USGS water dictionary and adds results of
         generate_place_names and any other custom words to prepare
         n-gram tokens.
      Creates n-gram tokens using quanteda, based on custom_dictionary.
      Generates a dfm.
      Removes stopwords, including basic English stopwords, place names, 
      symbols that indicate poor text conversion, months, 
      and words that have no letters (eg negative numbers or number ranges)
      Removes words with under a certain number of characters
      Removes terms found in <3 GSPs or >30% of pages
   Uses cleaned data from lex_clean in an STM model
