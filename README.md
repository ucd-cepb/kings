# kings
analyzing CA SGMA GSPs to understand groundwater management networks

outline of scripts and functions:

sgma_web_scraper:
uses rselenium and rvest:
1) takes the numeric id for each GSP, basin name, GSP local id if applicable,
and the link to the webpage for that GSP if applicable, and writes them to gsp_ids.csv
2) saves each plan summary spreadsheet in data_raw
3) saves each plan pdf in data_raw

TODO
gsp_xls_scraper:
1) takes an xls file and identifies the pdf sections associated with each
required element of a management plan, saves this info as a data frame

TODO
gsp_pdf_scraper:
uses tokenizer
may use tabulizer
1) turns each pdf into a .txt file with each page as one line
2) identifies the location of each section header

gsp_element_compiler:
1) associates each required element of the gsp with its corresponding section .txt files, as a long dataframe

gsp_topic_analyzer:
1) connect gsp_element_compiler results with the stm package for analysis

