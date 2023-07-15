library(pdftools)
library(tesseract)
library(stringr)

pdfs <- list.files(path = "data_raw/portal", pattern = "pdf", full.names = T)

k <- 2
pagenum <- 34
text <- suppressMessages(pdf_text(pdfs[[k]]))
#text 2 has 4-6 lines of header

   #test took 20 minutes to parse 24 pages at 600 dpi. There are 95 total in the 688 page "0007" GSP
   #if not enough chars, it might be a photocopy that needs to be scanned
   for(i in 1:length(text)){
      if (nchar(text[i])< 20){
         text[i] <- pdf_ocr_text(pdfs[[k]], pages=i,language = "eng")
      }
   }
   
   #if too many chars, it probably is not an actual page of text but rather a map or figure
   for(i in 1:length(text)){
      if (nchar(text[i])> 10000){
         text[i] <- NA
      }
   }

   #10 spaces is normal for a line of regular text
   #10 words is normal for a line of regular text
   #18 spaces is normal for a line of small text 
   #20 words is normal for a line of small text 
   
for(pagenum in 1:length(text)){
   linebreaks <- str_split(text[pagenum],"\\n")[[1]]
   
   if(length(linebreaks)>6){
      #search for last set of at least two \\n in a row 
      #which corresponds to a row of linebreaks that has only \\s* in it
      #before the sixth \\n 
      #which is row six in linebreaks
      #and delete it and all the rows above it
      linebreakhead <- linebreaks[1:6]
      emptylines <- which(str_detect(linebreakhead,"^\\s*$"))
      headercut <- emptylines[length(emptylines)]
      linebreaks <- linebreaks[(headercut+1):length(linebreaks)]
   }else{
      emptylines <- which(str_detect(linebreak,"^\\s*$"))
      if(length(emptylines)>=1){
         #just remove everything before the first set of two \\n,
         headercut <- emptylines[1]
         linebreaks <- linebreaks[(headercut+1):length(linebreaks)]
      }else{
         # or if one doesn't exist, 
         #remove everything since it's probably just a figure caption 
         #or header on a map page
         linebreaks <- NA
      }
      
   }
   
   
   #and search for first set of at least two \\n in a row 
   #on or after the 3rd-from-bottom group of \\n  
   #which is where the indicator of "emptylinegroups" starting from the bottom is < 3
   #and delete everything below it
   #as long as there are four or fewer lines of text after that cut line
   counter = 0
   emptylines <- which(str_detect(linebreaks,"^\\s*$"))
   
   if(length(emptylines)>=1){
      emptylinegroups <- sapply(c(1:length(emptylines)),
                                function(i) {ifelse(i!=1 && (emptylines[i]==emptylines[i-1]+1),NA,emptylines[i])
                                })
      emptylinegroups <- emptylinegroups[!is.na(emptylinegroups)]
      footercut <- ifelse(length(emptylinegroups)<3,emptylinegroups[1],
                          emptylinegroups[length(emptylinegroups)-2])
      lineswithtext <- which(str_detect(linebreaks,"^\\s*$",negate = T))
      
      while(sum(lineswithtext > footercut)>4 & counter <=2){
         counter = counter + 1
         #the footer is too big! only allow max four footer lines
         footercut <- ifelse(length(emptylinegroups)<(3-counter),emptylinegroups[1],
                             emptylinegroups[length(emptylinegroups)-(2-counter)])
         
      }
      if(sum(lineswithtext > footercut)>4){
         #there are too many lines of text after the last empty row. keep all text.
      }else{
         #found the footer! cut everything after footercut
         if(footercut==1){
            #that means there's nothing useful on the page -- it's all footer
            linebreaks <- NA
         }
         linebreaks <- linebreaks[1:(footercut-1)]
      }
   }else{
      #don't remove anything, since
      #there are no empty lines.
      #TODO what to do here?

   }
   text <- paste(linebreaks, collapse = " ")
}      #backup strategy (not implemented): search for > 25 \\s | < 8 \\w before an \\n (before the sixth \\n) and delete the line
#which is a bunch of spaces, or not enough words in the line



 

   #remove page numbers: 5+ spaces followed by (a combo of 1-6
   #roman and arabic numerals) or (a letter, hyphen, and
   #set of numbers such as c-28) at the end of a page
   text <- str_remove(text,"\\s{3,}([0-9|x|v|i]{1,6}|([a-z]+\\p{Pd}[0-9]+))\\s*$")
   
   
   saveRDS(text, paste0("data_cleaned/",substr(pdfs[k],17,31),"_txtnet_format"))