if(!require(xml2)) install.packages("xml2")
if(!require(XML)) install.packages("XML")
if(!require(httr)) install.packages("httr")

#connect to server
connNLP <- function(host="localhost", port="30200",
                    tokenize.whitespace="true", annotators="",
                    outputFormat="xml") {
   conn <- paste("http://",host,":",port,"/?",sep="")
   annotators <- gsub(",","%2C",annotators)
   properties <- paste("properties=%7B",
                       "%22tokenize.whitespace%22%3A",
                       "%22",tokenize.whitespace,"%22%2C",
                       "%22annotators%22%3A",
                       "%22",annotators,"%22",
                       "%2C%22outputFormat%22%3A",
                       "%22",outputFormat,"%22%7D",
                       sep="")
   conn <- paste(conn,properties,sep="")
   request <- list("conn"=conn,"out"=outputFormat)
   return(request)
}

#analyze text document
getNLP <- function(text,conn){
   text <- gsub("'","",text)
   if(tolower(conn['out'])=="xml"){
      require("XML")
      require("httr")
      result <- POST(conn['conn'][[1]],body = text,encode = "multipart")
      doc <- xmlParse(content(result,"parsed","application/xml",encoding = "UTF-8"))
   }
   if(tolower(conn['out'])=="json"){
      result <- POST(conn['conn'][[1]],body = text,encode = "json")
      doc <- content(result, "parsed","application/json",encoding = "UTF-8")
   }
   if(tolower(conn['out'])=="text"){
      result <- POST(conn['conn'][[1]],body = text,encode = "multipart")
      doc <- content(result, "parsed","application/text",encoding = "UTF-8")
   }
   return(doc)
}

#get corenlp sentiment annotator
getSent <- function(docNLP){
   scr <- as.integer(xpathSApply(docNLP, "//sentences/sentence/@sentimentValue"))
   sent <- toString(xpathSApply(docNLP, "//sentences/sentence/@sentiment"))
   result <- list(sentiment = c(sent),score=c(scr))
   return(result)
}

#sentiment annotator for each word
getWords <- function(docNLP){
   word <- xpathSApply(docNLP, "//token/word",xmlValue)
   word_sent <- xpathSApply(docNLP, "//token/sentiment",xmlValue)
   df <- data.frame(word,word_sent)
   return(df)
}

#function to pull info from RSS
getRSS <- function(url=""){
   doc <- xmlParse(url)
   title <- xpathSApply(doc, "//item/title", xmlValue)
   desc <- xpathSApply(doc, "//item/description", xmlValue)
   date <- xpathSApply(doc, "//item/pubDate", xmlValue)
   date <- strptime(date,format ="%a, %d %b %Y %H:%M:%S",tz="GMT")
   rss <- list(title=title,description = desc,date = date)
   return(rss)
}

conn <- connNLP(port = "9000", tokenize.whitespace = "true",annotators="sentiment",outputFormat="xml")
