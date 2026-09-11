#devtools::install_github("govscienceuseR/referenceExtract")
library(referenceExtract)
library(data.table)
library(stringr)
library(pbapply)
#note: to make this work, you probably want to install chruby on your computer, and then use chruby to run anystyle in new ruby that isn't what the system uses
#https://mac.install.guide/ruby/12.html
#then you can install https://github.com/inukshuk/anystyle

# CLOBBER=TRUE re-parses EVERY PDF from scratch (runs for a very long time);
# the default (FALSE) only parses PDFs not already cached under extracted_references/.
# Honors the global CLOBBER threaded by run_all.R; still defaults to incremental.
CLOBBER <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

source("Network_Innovation_Paper/Code/_paths.R")
# Source PDFs. Core names them gsp_doc_id_<stem>.pdf (not v*_gsp_num_id_*.pdf);
# join back to the legacy gsp_id via data_products/00_ingest/id_crosswalk.csv if needed.
doc_loc <- core_pdfs()
js_loc <- nip_product('03A_reference_extraction', 'extracted_references/')
if(!dir.exists(js_loc)){dir.create(js_loc, recursive = TRUE)}
fls <- list.files(doc_loc,full.names=T,pattern = 'pdf$|PDF$')

ref_dir<-js_loc
js_files <- list.files(js_loc)

if(!CLOBBER){
  need <- !basename(str_replace(fls,'pdf$|PDF$','json')) %in% basename(js_files)
  fls <- fls[need]
}
library(pdftools)

### might need to install anystyle
# gem install anystyle 
referenceExtract::reference_extract(files = fls,ref_dir = js_loc,cores = 4)