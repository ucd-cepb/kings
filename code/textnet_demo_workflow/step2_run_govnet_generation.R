library(textNet)


ret_path <- "/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python"
generate_phrases <- FALSE
keep_hyph_together <- FALSE
test_data <- F
mini_data <- F
parse_from_file <- F
#mini_data uses only a sample of pages to make testing and editing go faster
source("code/textnet_demo_workflow/utils/generate_proper_names.R")
if(test_data==T){
   pages <- (c("Invalid_sentence. DWR and DOT collaborate with the GSA to implement CDFA regulations.", 
             "Incorrect sentence. Suzy plays darts with John and runs. We want them to respond. Matt swims and plays tennis with Chris. Matt laughs while playing tennis with Chris. Ali walks to the store and buys a watermelon. GSAs form a committee to decide things. Sam wants to go to Germany with Fran. George wants Fran to go to Cuba with him. GSAs made breakfast to stave off hunger. For Fran to go to Cuba would be a blast. Jessie will walk with Pedro. Leaders form a committee, which makes decisions for LA. GSAs form a committee for deciding things. GSAs made a decision to organize files. He is supposed to have been considering it all along. He has supposedly been considering it all along. He is supposed to have been studying this whole time. Percy Willis advises Frankie. Governor Smith advises and councils Benny Jordan. Philip Johnson and Sarah McMurray meet and discuss with their collaborators at USDA. National_Conservation_Service and US_Dept_of_Ag are to work together on providing the GSA the appropriate regulation language. Yolo County is to send the GSA all correspondence related to the basin setting. They shall report to the Board.", 
             "The CDC will collaborate with NASA and will coordinate with SGMA on the project. The FBI and CIA agree to meet with several other agencies. The GSA is to submit their plan to the consultant. When the NSA meets with organizations such as the SWRCB, it is to take care to copy them on all correspondence.", 
             "If they are to partner together, the GDE plan is to be documented. The GSAs may not outsource their work to any other organizations. Sacramento Water Board may work with other partners and submit documentation as deemed necessary. Davis City Council may decide to incorporate the recommendations of BCDC."))
   file_ids <- c("A","B","B","B")
   unique_files <- unique(file_ids)
   parsed_filenames <- paste0("data_temp/test_prs",unique_files)
   
}else{
   pages <- readRDS("data_output/cleaned_pdfs")
   file_ids <- unlist(sapply(1:length(pages), function(q) rep(names(pages[q]),length(pages[[q]]))))
   file_ids <- str_extract(file_ids,'[0-9]{1,}')
   pages <- unlist(pages)
   
   if(mini_data==T){
      pages <- sample(pages, 20, replace = F)
   }
   
   unique_files <- unique(file_ids)
   
   if(mini_data==T){
      parsed_filenames <- paste0("data_output/prs_mini_",unique_files)
   }else if(test_data==T){
      parsed_filenames <- paste0("data_output/prs_test_",unique_files)
   }
   else{
      parsed_filenames <- paste0("data_output/parsed_",unique_files)
   }
}

if(mini_data==T | test_data==T){
   nodeedge_filenames <- paste0("network_extr_test/",unique_files,".RDS")
   
}else{
   nodeedge_filenames <- paste0("network_extracts/",unique_files,".RDS")
   
}

phrases_to_concatenate <- ifelse(generate_phrases==T, generate_proper_names(underscore=F,to_lower=F),NA)


#### overwrite existing results?
overwrite = F

all_parsed <- textNet::parse_text(ret_path, keep_hyph_together, phrases_to_concatenate, 
                  concatenator="_",
                  pages, file_ids, parsed_filenames, 
                 parse_from_file,
                 overwrite)
for(m in 1:length(all_parsed)){
   textNet::textnet_extract(all_parsed[[m]],concatenator="_",file = nodeedge_filenames[m],cl=4,
                   keep_entities = c('ORG','GPE','PERSON'), 
                   return_to_memory=F, keep_incomplete_edges=T)
}

gspids <- stringr::str_extract(nodeedge_filenames,'[0-9]{1,}')
pctalphaineng <- vector(mode="numeric",length=length(gspids))
pctlettersineng <- vector(mode="numeric",length=length(gspids))
for(m in 1:length(gspids)){
   parsed <- readRDS(paste0("data_output/parsed_",gspids[m]))
   alphatokens <- parsed$token[str_detect(parsed$token, "[[:alpha:]]")]
   lettertokens <- parsed$token[str_detect(parsed$token, "[a-zA-Z]")]
   pctalphaineng[m] <- sum(alphatokens %in% eng_words)/length(alphatokens) 
   pctlettersineng[m] <- sum(lettertokens %in% eng_words)/length(lettertokens) 
   print(paste0("GSPID ",gspids[m]," has ",pctlettersineng[m] ," % of its letter-containing tokens in the English dictionary."))
   print(paste0("GSPID ",gspids[m]," has ",pctalphaineng[m] ," % of its alpha-containing tokens in the English dictionary."))
   
}

plot(gspids,pctalphaineng,type="l",col="red")
lines(gspids,pctlettersineng,col="blue")
#gspids5 ("0012"), 14 ("0021"), 34 ("0048"), 68 ("0089"), 103 ("0129"), and 108 ("0134")
#identified as having a low percentage of letter-containing words in the 
#English language.
#Of these, only 68 ("0089") was below 50%.
dif <- pctalphaineng - pctlettersineng
plot(gspids,dif)
#gspid29 ("0042") and gspid34 ("0048") are outliers,
#but the diference is no more than 7%, so this was
#considered acceptable tolerance.
