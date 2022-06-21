# kings
analyzing CA SGMA GSPs to understand groundwater management networks

outline of scripts and functions:

sgma_web_scraper:
uses rselenium and rvest:
1) takes the numeric id for each GSP, basin name, GSP local id if applicable,
and the link to the webpage for that GSP if applicable, and writes them to gsp_ids.csv
2) saves each plan summary spreadsheet in data_raw
3) saves each plan pdf in data_raw
   
gsp_pdf_reader:
uses pdftools
may use tokenizer
may use tabulizer
1) turns each pdf into a .txt file with each page as one line
2) identifies the location of each section header

uses create_page_key, which takes an xlsx file and returns a clean data table with page numbers in the page_vector column that can be used to subset pdf_text to retrieve the pages associated with a plan element. 
Calls two smaller functions: 
   read_plan_element reads & cleans Elements of the Plan xlsx, tagging sections and subsections.
   consolidate_pgs consolidates the page numbers associated with each objective

gsp_topic_analyzer:
1) calls create_lang_meta, which creates a data table matching page text
with metadata about that page.
2) processes the text into a corpus using quanteda
3) cleans the corpus using quanteda and tm packages and prepares it for analysis
4) generates model with stm package

