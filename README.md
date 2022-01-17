# kings
analyzing CA SGMA GSPs to understand groundwater management networks

tentative outline of scripts and functions:

sgma_web_scraper:
1) takes the site id for each GSP and adds it to GSA_GSP_Basin_Coord.csv
2) saves each plan summary in data_raw (necessary?)
3) saves each pdf in data_raw (necessary?)

gsp_xls_scraper:
1) takes an xls file and identifies the pdf sections associated with each
required element of a management plan, saves this info as a data frame

gsp_pdf_scraper:
1) turns each pdf into a .txt file with each page as one line
2) identifies the location of each section header
3) creates a folder of .txt files, where each file contains the text under each 
section

gsp_element_compiler:
1) associates each required element of the gsp with its corresponding section
.txt files, as a long dataframe

gsp_topic_analyzer:
1) connect gsp_element_compiler results with the stm package for analysis

