packs <- c('stm')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

gspoutfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
gspoutfilenamesplits <- unlist(strsplit(gspoutfilename,split="/"))
gspoutpath <- paste(gspoutfilenamesplits[1:(length(gspoutfilenamesplits)-1)],collapse = "/")
gspoutpattern <- gspoutfilenamesplits[length(gspoutfilenamesplits)]

slam_files <- list.files(path = gspoutpath, pattern = gspoutpattern, full.names = T)

newest_slam_file <- slam_files[which.max(file.info(slam_files)$mtime)]
gsp_out <- readRDS(newest_slam_file)

gsp_out$meta$priority_category <- as.factor(gsp_out$meta$priority_category)

gsp_text_with_meta <- readRDS(file = filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)

gsp_text_with_meta$version_approval20230922 <- gsp_text_with_meta$version_approval

tps <- readRDS(filekey[filekey$var_name=="topic_prevalence",]$filepath)


modelfilename <- filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath
modelfilenamesplits <- unlist(strsplit(modelfilename,split="/"))
modelpath <- paste(modelfilenamesplits[1:(length(modelfilenamesplits)-1)],collapse = "/")
modelpattern <- modelfilenamesplits[length(modelfilenamesplits)]

finfo <- file.info(list.files(path = modelpath, pattern = "model", full.names = T))
version_select <- which(finfo$mtime==max(finfo$mtime))

model <- readRDS(rownames(finfo)[version_select])
prev <- as.data.frame(model$theta)

gsp_out$meta$prevV7 <- prev$V7
gsp_out$meta$prevV24 <- prev$V24
gsp_out$meta$CC_topics <- prev$V7 + prev$V24

allpages_meta <- dplyr::left_join(gsp_out$meta, gsp_text_with_meta)

library(DescTools)
gspmini <- gsp_out$meta |> dplyr::group_by(gsp_id) |> dplyr::summarise(
   Republican_Vote_Share_scaled = mean(Republican_Vote_Share_scaled), 
   dsci_scaled = mean(dsci_scaled),
   Agr_Share_Of_GDP_scaled = mean(Agr_Share_Of_GDP_scaled),
   num_pages = max(page_num),
   version_approval = Mode(version_approval),
   CC_topics = mean(CC_topics),
   prevV7 = mean(prevV7),
   prevV24 = mean(prevV24),
   projects_mgmt_actions = mean(projects_mgmt_actions),
   sust_criteria = mean(sust_criteria),
   monitoring_networks = mean(monitoring_networks),
   basin_plan = mean(basin_plan),
   admin = mean(admin))

gspmini$version_approval <- factor(gspmini$version_approval)

gspmini <- gspmini[!gspmini$gsp_id %in% c("0053","0089"),]

cor.test(gspmini$CC_topics, gspmini$Republican_Vote_Share_scaled)
cor.test(gspmini$CC_topics, gspmini$dsci_scaled)
cor.test(gspmini$sust_criteria, gspmini$dsci_scaled)

gspmini_sorted <- gspmini[order(gspmini$dsci_scaled),]

lowdrought <- median(gspmini_sorted$CC_topics[1:10])
median(gspmini_sorted$projects_mgmt_actions[1:10])
median(gspmini_sorted$basin_plan[1:10])
median(gspmini_sorted$monitoring_networks[1:10])
median(gspmini_sorted$admin[1:10])
median(gspmini_sorted$sust_criteria[1:10])

gspmini_sorted <- gspmini[order(gspmini$dsci_scaled, 
                                decreasing = F),]
sum(gspmini_sorted$sust_criteria[1:10], 
           gspmini_sorted$admin[1:10],
           gspmini_sorted$monitoring_networks[1:10],
           gspmini_sorted$basin_plan[1:10],
           gspmini_sorted$projects_mgmt_actions[1:10])/10
sum(gspmini_sorted$sust_criteria, 
    gspmini_sorted$admin,
    gspmini_sorted$monitoring_networks,
    gspmini_sorted$basin_plan,
    gspmini_sorted$projects_mgmt_actions)/nrow(gspmini_sorted)

median(gspmini_sorted$m[1:10])
lowdrought_gsps <- gspmini_sorted$gsp_id[1:10]
highdrought <- median(gspmini_sorted$CC_topics[(nrow(gspmini_sorted)-9):nrow(gspmini_sorted)])
highdrought_gsps <- gspmini_sorted$gsp_id[nrow(gspmini_sorted):(nrow(gspmini_sorted)-9)]
median(gspmini_sorted$projects_mgmt_actions[nrow(gspmini_sorted):(nrow(gspmini_sorted)-9)])
median(gspmini_sorted$basin_plan[nrow(gspmini_sorted):(nrow(gspmini_sorted)-9)])
median(gspmini_sorted$monitoring_networks[nrow(gspmini_sorted):(nrow(gspmini_sorted)-9)])
median(gspmini_sorted$admin[nrow(gspmini_sorted):(nrow(gspmini_sorted)-9)])
median(gspmini_sorted$sust_criteria[nrow(gspmini_sorted):(nrow(gspmini_sorted)-9)])


lowdrought_examples <- gsp_out$meta[gsp_out$meta$gsp_id %in% lowdrought_gsps,]
lowdrought_examples <- lowdrought_examples[order(lowdrought_examples$CC_topics,decreasing = T),]
#LOWDROUGHT
#high Repub
lowdrought_examples$text[lowdrought_examples$gsp_id=="0088"][1:5]
lowdrought_examples$text[lowdrought_examples$gsp_id=="0083"][1:5]
lowdrought_examples$text[lowdrought_examples$gsp_id=="0082"][1:5]

#med Repub
lowdrought_examples$text[lowdrought_examples$gsp_id=="0090"][1:5]
lowdrought_examples$text[lowdrought_examples$gsp_id=="0070"][1:20]
#low Repub
lowdrought_examples$text[lowdrought_examples$gsp_id=="0136"][1:5]
lowdrought_examples$text[lowdrought_examples$gsp_id=="0101"][1:5]
lowdrought_examples$text[lowdrought_examples$gsp_id=="0132"][1:5]
lowdrought_examples$text[lowdrought_examples$gsp_id=="0124"][1:5]

#HIGHDROUGHT
highdrought_examples <- gsp_out$meta[gsp_out$meta$gsp_id %in% highdrought_gsps,]
highdrought_examples <- highdrought_examples[order(highdrought_examples$CC_topics,decreasing = T),]
unique(highdrought_examples$gsp_id)

#high Repub
highdrought_examples$text[highdrought_examples$gsp_id=="0057"][1:20]
highdrought_examples$text[highdrought_examples$gsp_id=="0065"][1:10]
highdrought_examples$text[highdrought_examples$gsp_id=="0056"][1:10]
highdrought_examples$text[highdrought_examples$gsp_id=="0030"][1:10]
highdrought_examples$text[highdrought_examples$gsp_id=="0050"][1:10]
highdrought_examples$text[highdrought_examples$gsp_id=="0036"][1:10]
highdrought_examples$text[highdrought_examples$gsp_id=="0058"][1:10]

#med Repub
highdrought_examples$text[highdrought_examples$gsp_id=="0023"][1:10]
highdrought_examples$page_num[highdrought_examples$gsp_id=="0043"][7]

#low Repub
highdrought_examples$text[highdrought_examples$gsp_id=="0063"][7]


