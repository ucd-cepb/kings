#always load the filekey into the script like this
filekey <- read.csv("filekey.csv")

#how to get a filepath for a var
filekey[filekey$var_name=="myvar",]$filepath

#separating file name pattern from filepath, to search for
#multiple files matching a pattern
xfilename <- filekey[filekey$var_name=="myvar",]$filepath
xfilenamesplits <- unlist(strsplit(xfilename,split="/"))
xpath <- paste(xfilenamesplits[1:(length(xfilenamesplits)-1)],collapse = "/")
xpattern <- xfilenamesplits[length(xfilenamesplits)]

#get the most recent file in the xpath that matches xpattern
recentxfile <- readRDS(list.files(path = xpath, pattern = xpattern, full.names = T)[
   length(list.files(path = xpath, pattern = xpattern, full.names = T))])

