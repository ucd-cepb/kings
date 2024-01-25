library(textNet)
library(dotenv)
library(spacyr)

load_dot_env()
ret_path = Sys.getenv('RETICULATE_PYTHON')

# part 1
pdfs <- c(list.files(path = "C:/Users/aaron/Downloads", pattern = "djls", full.names = T))
ocr <- F
maxchar <- 10000
old_new_text <- textNet::pdf_clean(pdfs, keep_pages=T, ocr=F, maxchar=10000,
                                   export_paths=NULL, return_to_memory=T, suppressWarn = F)
names(old_new_text) <- c("old","new")

# part 2
pages <- unlist(old_new_text)
file_ids <- unlist(sapply(1:length(old_new_text), function(q) rep(names(old_new_text[q]),length(old_new_text[[q]]))))

water <- c("surface water", "Surface water", "groundwater", "Groundwater", "San Joaquin River", "Cottonwood Creek", "Chowchilla Canal Bypass", "Friant Dam", "Sack Dam", "Friant Canal", "Chowchilla Bypass", "Fresno River", "Sacramento River", "Merced River","Chowchilla River", "Bass Lake", "Crane Valley Dam", "Willow Creek", "Millerton Lake", "Mammoth Pool", "Dam 6 Lake", "Delta","Tulare Lake", "Madera-Chowchilla canal", "lower aquifer", "upper aquifer", "upper and lower aquifers", "lower and upper aquifers", "Lower aquifer", "Upper aquifer", "Upper and lower aquifers", "Lower and upper aquifers")
water_phrases <- water[stringr::str_detect(water,"\\s")]

old_new_parsed <- textNet::parse_text(ret_path,
                                      keep_hyph_together = F,
                                      phrases_to_concatenate = water_phrases,
                                      concatenator = "_",
                                      pages,
                                      file_ids,
                                      parsed_filenames=c("old_parsed","new_parsed"),
                                      overwrite = T)