library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

places <- read.csv("EJ_DAC_Paper/Data/all_places.csv")
dacdataonly <- places[places$DAC==1,]
notdac <- places[places$DAC==0,]

edgesnodes <- list.files(path = filekey[filekey$var_name=="disambiged_unfiltered_extracts_superpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edgesnodes,'[0-9]{1,}')
gspids <- gspids[!gspids%in% c("0053", "0089")]
parsed <- vector(mode = "list", length = length(gspids))
for(i in 1:length(gspids)){
   parsed[[i]] <- readRDS(paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath,
                            gspids[i]))
}

#make flow attributes
flow_attr <- vector(mode = "list", length = length(gspids))

for(i in 1:length(parsed)){
   parsed[[i]]$doc_sent <- paste0(parsed[[i]]$doc_id, "_", parsed[[i]]$sentence_id)
   parsed[[i]]$nexttoken <- c(parsed[[i]]$token[2:length(parsed[[i]]$token)], NA)
   flow_attr[[i]] <- parsed[[i]] |> group_by(doc_sent) |> summarize(
      info_flow = any(tolower(token) %in% c("data",
                                "database",
                                "databases",
                                "documentation",
                                "information",
                                "info",
                                "results",
                                "log",
                                "model",
                                "models",
                                "report",
                                "reports",
                                "research",
                                "sop",
                                "sops",
                                "study",
                                "studies")),
      money_flow = any(tolower(token) %in% c("financial",
                                 "financially",
                                 "fund",
                                 "funds",
                                 "funding",
                                 "cost",
                                 "costs",
                                 "costly",
                                 "fee",
                                 "fees",
                                 "expensive",
                                 "expenditures",
                                 "$",
                                 "financing",
                                 "loan",
                                 "tax",
                                # "assessment",   doesn't always mean financial assessment
                                 "revenue"
                                 )) |
         any(tolower(token) == "interest" & tolower(nexttoken) == "rate") |
         #any(!tolower(token) %in% c("water","groundwater") & tolower(nexttoken) %in% c("budget","budgets")) |
         any(!tolower(token) %in% c("groundwater", "aquifer") & tolower(nexttoken) == "reserves") |
         any(tolower(token) %in% c("grant", "grants") & !tolower(nexttoken) %in% c("access",
                                                                    "authority",
                                                                    "permission"))
      )
}


edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)

edgelist_w_meta$doc_sent <- stringr::str_remove(edgelist_w_meta$doc_sent_verb, "_\\d*$")
#need both the nodes in each edge to be not GPE
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$source_entity_type!="GPE" & 
                                      edgelist_w_meta$target_entity_type!="GPE",]

edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$edgeiscomplete==T,]

edgelist_w_meta$is_collab <- edgelist_w_meta$head_verb_lemma %in% 
   c("collaborate",
     "communicate",
     "cooperate",
     "coordinate",
     "agree",
     "engage",
     "participate",
     "consult",
     "partner",
     "inform",
     "translate",
     "connect",
     "present",
     "exchange",
     "distribute",
     "meet",
     "join",
     "share",
     "disseminate",
     "provide",
     "transmit")

edgelist_w_meta$sourcegsa <- stringr::str_detect(edgelist_w_meta$source,
   "groundwater_sustainability_agency$|gsa$"
)

edgelist_w_meta$targetgsa <- stringr::str_detect(edgelist_w_meta$target,
                                                 "groundwater_sustainability_agency$|gsa$"
)

#how many edges with GSAs are collaborative verbs?
gsa_collab <- sum(edgelist_w_meta[edgelist_w_meta$targetgsa==T | 
                                     edgelist_w_meta$sourcegsa==T,]$is_collab)
length_gsa <- nrow(edgelist_w_meta[edgelist_w_meta$targetgsa==T | 
                                      edgelist_w_meta$sourcegsa==T,])
nongsa_collab <- sum(edgelist_w_meta[!(edgelist_w_meta$targetgsa==T | 
                                        edgelist_w_meta$sourcegsa==T),]$is_collab)
length_nongsa <- nrow(edgelist_w_meta[!(edgelist_w_meta$targetgsa==T | 
                                         edgelist_w_meta$sourcegsa==T),])

prop.test(c(gsa_collab, nongsa_collab), c(length_gsa, length_nongsa), p = NULL, alternative = "two.sided",
          correct = TRUE)



info_money <- sum(edgeframe[edgeframe$info_flow==T,]$money_flow)
length_info <- nrow(edgeframe[edgeframe$info_flow==T,])
noninfo_money <- sum(edgeframe[edgeframe$info_flow==F,]$money_flow)
length_noninfo <- nrow(edgeframe[edgeframe$info_flow==F,])

prop.test(c(info_money, noninfo_money), c(length_info, length_noninfo), p = NULL, alternative = "two.sided",
          correct = TRUE)


#how many edges in high-priority plans are collaborative verbs?
hipri_collab <- sum(edgelist_w_meta[edgelist_w_meta$priority_category=="high",]$is_collab)
length_hipri <- nrow(edgelist_w_meta[edgelist_w_meta$priority_category=="high",])
lowpri_collab <- sum(edgelist_w_meta[edgelist_w_meta$priority_category!="high",]$is_collab)
length_lowpri <- nrow(edgelist_w_meta[edgelist_w_meta$priority_category!="high",])

prop.test(c(hipri_collab, lowpri_collab), c(length_hipri, length_lowpri), p = NULL, alternative = "two.sided",
          correct = TRUE)





edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)
#need both the nodes in each edge to be not GPE
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$source_entity_type=="GPE" | 
                                      edgelist_w_meta$target_entity_type=="GPE",]

edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$edgeiscomplete==T,]

edgelist_w_meta$sourcedac <- edgelist_w_meta$source %in% dacdataonly$NAME20
edgelist_w_meta$targetdac <- edgelist_w_meta$target %in% dacdataonly$NAME20
edgelist_w_meta$sourcenotdac <- edgelist_w_meta$source %in% notdac$NAME20
edgelist_w_meta$targetnotdac <- edgelist_w_meta$target %in% notdac$NAME20


edgelist_w_meta$is_collab <- edgelist_w_meta$head_verb_lemma %in% 
   c("collaborate",
     "communicate",
     "cooperate",
     "coordinate",
     "agree",
     "engage",
     "participate",
     "consult",
     "partner",
     "inform",
     "translate",
     "connect",
     "present",
     "exchange",
     "distribute",
     #"meet",
     "join",
     "share",
     "disseminate",
     "provide",
     "transmit",
     
     #second collection "reporting"
     "discuss",
     "notify",
     "report",
     "prepare",
     "submit",
     "approve",
     "facilitate",
     "authorize",
     "recognize",
     "recommend",
     "give",
     "deliver",
     "appoint",
     "propose"
     
     )

edgelist_w_meta$is_manag <- edgelist_w_meta$head_verb_lemma %in% 
   c("develop",
     "identify",
     "establish",
     "conduct",
     "adopt",
     "monitor",
     "estimate",
     "implement",
     "incorporate",
     "review",
     "manage",
     "maintain",
     "support",#implies collab??
     "evaluate",
     "operate",
     "collect",
     "utilize",
     "plan",
     "initiate",
     "measure",
     "determine",
     "result",
     "project"
     
   )


#how many edges in high-priority plans are collaborative verbs?
dac_collab <- sum(edgelist_w_meta[edgelist_w_meta$sourcedac==T |
                                     edgelist_w_meta$targetdac==T,]$is_collab)
length_dac <- nrow(edgelist_w_meta[edgelist_w_meta$sourcedac==T |
                                      edgelist_w_meta$targetdac==T,])
nodac_collab <- sum(edgelist_w_meta[edgelist_w_meta$sourcenotdac==T |
                                       edgelist_w_meta$targetnotdac==T,]$is_collab)
length_nodac <- nrow(edgelist_w_meta[edgelist_w_meta$sourcenotdac==T |
                                        edgelist_w_meta$targetnotdac==T,])

prop.test(c(dac_collab, nodac_collab), c(length_dac, length_nodac), p = NULL, alternative = "two.sided",
          correct = TRUE)


#now let's do some structure analysis on the network object
library(igraph)
supernetwork <- readRDS(filekey[filekey$var_name=="supernetwork_unfiltered_full_superpaper",]$filepath)

verbtable <- as.data.frame(sort(table(edgeframe$head_verb_lemma), decreasing = T))
verbtable[verbtable$Var1 %in% 
             c("collaborate",
               "communicate",
               "cooperate",
               "coordinate",
               "agree",
               "engage",
               "participate",
               "consult",
               "partner",
               "inform",
               "translate",
               "connect",
               "present",
               "exchange",
               "distribute",
               "meet",
               "join",
               "share",
               "disseminate",
               "provide",
               "transmit"),]

all_flow_attrs <- rbindlist(flow_attr)
all_flow_attrs$money_flow <- ifelse(is.na(all_flow_attrs$money_flow),
                                    F, all_flow_attrs$money_flow)
docsents <- stringr::str_remove(igraph::get.edge.attribute(supernetwork, "doc_sent_verb"), "_\\d*$")

flows_in_edge_order <- all_flow_attrs[match(docsents, all_flow_attrs$doc_sent),]

supernetwork <- igraph::set.edge.attribute(supernetwork, "info_flow", value = flows_in_edge_order$info_flow)
supernetwork <- igraph::set.edge.attribute(supernetwork, "money_flow", value = flows_in_edge_order$money_flow)

igraph::get.edge.attribute(supernetwork, "head_verb_lemma")

supernetwork <- set.vertex.attribute(supernetwork, name = "is_place",
                    value = get.vertex.attribute(supernetwork, "name") %in% places$NAME20)
supernetwork <- set.vertex.attribute(supernetwork, name = "is_dac",
                                     value = get.vertex.attribute(supernetwork, "name") %in% dacdataonly$NAME20)
supernetwork <- set.vertex.attribute(supernetwork, name = "is_gsa",
                                     value = stringr::str_detect(
                                        get.vertex.attribute(supernetwork, "name"), "groundwater_sustainability_agency$|gsa$"))


col_rpt_mg <- igraph::delete_edges(supernetwork, 
                     igraph::E(supernetwork)[edge_attr(supernetwork, "is_collab") == F &
                                                edge_attr(supernetwork, "is_report") == F &
                                                edge_attr(supernetwork, "is_manag") == F])


myincidentsgsa <- E(col_rpt_mg)[.inc(V(col_rpt_mg)[vertex_attr(col_rpt_mg, "is_gsa") == T])]
myincidentsnongsa <- E(col_rpt_mg)[.inc(V(col_rpt_mg)[vertex_attr(col_rpt_mg, "is_gsa") == F &
                                                         vertex_attr(col_rpt_mg, "entity_type") == "ORG"])]

#edit this to look at priority rather than gsas
myincidentsgsa <- E(col_rpt_mg)[.inc(V(col_rpt_mg)[vertex_attr(col_rpt_mg, "is_gsa") == T])]
myincidentsnongsa <- E(col_rpt_mg)[.inc(V(col_rpt_mg)[vertex_attr(col_rpt_mg, "is_gsa") == F &
                                                         vertex_attr(col_rpt_mg, "entity_type") == "ORG"])]


myincidentsdac <- E(col_rpt_mg)[.inc(V(col_rpt_mg)[vertex_attr(col_rpt_mg, "is_dac") == T])]
myincidentsnondac <- E(col_rpt_mg)[.inc(V(col_rpt_mg)[vertex_attr(col_rpt_mg, "is_dac") == F &
                                                             vertex_attr(col_rpt_mg, "is_place") == T])]

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "is_collab"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "is_collab"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "is_collab"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "is_collab"))

#nonC_collab, group1, group2, group3, CapitalC, coordin, cooper

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "CapitalC"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "CapitalC"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "CapitalC"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "CapitalC"))

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "nonC_collab"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "nonC_collab"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "nonC_collab"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "nonC_collab"))

#"inform"
mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "group3"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "group3"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "group3"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "group3"))

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "consult"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "consult"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "consult"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "consult"))

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "collabonly"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "collabonly"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "collabonly"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "collabonly"))

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "is_report"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "is_report"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsgsa, name = "is_report"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnongsa, name = "is_report"))

mean(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "is_manag"))
mean(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "is_manag"))

View(sort(table(get.edge.attribute(col_rpt_mg, index = myincidentsdac, name = "head_verb_lemma")), decreasing = T))
View(sort(table(get.edge.attribute(col_rpt_mg, index = myincidentsnondac, name = "head_verb_lemma")), decreasing = T))

supernetwork <- set.edge.attribute(supernetwork, name = "is_collab",
                   value = get.edge.attribute(supernetwork, 
                                              "head_verb_lemma") %in% 
   c("collaborate",
     "communicate",
     "cooperate",
     "coordinate",
     "agree",
     "engage",
     "participate",
     "consult",
     "partner",
     "inform",
     "translate",
     "connect",
     "present",
     "exchange",
     "distribute",
     "meet",
     "join",
     "share",
     "disseminate",
     "provide",
     "transmit"))

supernetwork <- set.edge.attribute(supernetwork, name = "CapitalC",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("collaborate",
                                        "communicate",
                                        "cooperate",
                                        "coordinate",
                                        "consult"))

supernetwork <- set.edge.attribute(supernetwork, name = "coordin",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("coordinate"))

supernetwork <- set.edge.attribute(supernetwork, name = "cooper",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("cooperate"))


supernetwork <- set.edge.attribute(supernetwork, name = "collabonly",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("collaborate"))


supernetwork <- set.edge.attribute(supernetwork, name = "group1",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("collaborate",
                                        "inform",
                                        "consult"))

supernetwork <- set.edge.attribute(supernetwork, name = "group2",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("inform",
                                        "consult"))

supernetwork <- set.edge.attribute(supernetwork, name = "group3",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("inform"))

supernetwork <- set.edge.attribute(supernetwork, name = "consult",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("consult"))


supernetwork <- set.edge.attribute(supernetwork, name = "nonC_collab",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("agree",
                                        "engage",
                                        "participate",
                                        "partner",
                                        "inform",
                                        "translate",
                                        "connect",
                                        "present",
                                        "exchange",
                                        "distribute",
                                        "meet",
                                        "join",
                                        "share",
                                        "disseminate",
                                        "provide",
                                        "transmit"))

supernetwork <- set.edge.attribute(supernetwork, name = "is_report",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("discuss",
                                        "notify",
                                        "report",
                                        "prepare",
                                        "submit",
                                        "approve",
                                        "facilitate",
                                        "authorize",
                                        "recognize",
                                        "recommend",
                                        "give",
                                        "deliver",
                                        "appoint",
                                        "propose"))

supernetwork <- set.edge.attribute(supernetwork, name = "is_manag",
                                   value = get.edge.attribute(supernetwork, 
                                                              "head_verb_lemma") %in% 
                                      c("develop",
                                        "identify",
                                        "establish",
                                        "conduct",
                                        "adopt",
                                        "monitor",
                                        "estimate",
                                        "implement",
                                        "incorporate",
                                        "review",
                                        "manage",
                                        "maintain",
                                        "support",#implies collab??
                                        "evaluate",
                                        "operate",
                                        "collect",
                                        "utilize",
                                        "plan",
                                        "initiate",
                                        "measure",
                                        "determine",
                                        "result",
                                        "project"))
supernetwork <- igraph::set.edge.attribute(supernetwork, "between", value = igraph::edge_betweenness(supernetwork))
supernetwork <- igraph::set.vertex.attribute(supernetwork, "Vbetween", value = igraph::betweenness(supernetwork))

mean(edgeframe$between[edgeframe$info_flow==T])
mean(edgeframe$between[edgeframe$money_flow==T])

install.packages("ggpubr")
library("ggpubr")
#var is not diff, because p of this F-test >> 0.05
var.test(edgeframe$between[edgeframe$info_flow==T], 
         edgeframe$between[edgeframe$money_flow==T])

t.test(edgeframe$between[edgeframe$info_flow==T], 
       edgeframe$between[edgeframe$money_flow==T], 
       alternative = "two.sided", var.equal = T)

edgeframe <- igraph::as_data_frame(supernetwork, what = "edges")

boxplot(log(edgeframe$between) ~ edgeframe$money_flow + edgeframe$info_flow)

sup <- supernetwork
#sup <- set.vertex.attribute(sup, "name", 
 #                  value = stringr::str_remove_all(get.vertex.attribute(sup, "name"), "\\W" ))
#sup <- set.edge.attribute(sup, "head_verb_lemma", 
 #                  value = stringr::str_remove_all(get.edge.attribute(sup, "head_verb_lemma"), "\\W" ))

sup <- igraph::delete_vertices(sup, V(sup)[vertex_attr(sup, "entity_type") == "GPE"])
collb <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "is_collab")])
info <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "info_flow")])
money <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "money_flow")])


manag <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "is_manag")])
ecount(manag)
#
for(q in igraph::edge_attr_names(manag)){
   manag <- igraph::delete_edge_attr(manag, q)
}
ecount(manag)

igraph::E(manag)$weight <- 1
manag <- igraph::simplify(manag, edge.attr.comb=list(weight="sum"), remove.loops = T)
manag <- igraph::delete.vertices(manag, igraph::V(manag)[igraph::degree(manag, igraph::V(manag), mode = "all",loops = F)==0])

transitivity(manag, type = "global")
#transitivity(as_undirected(manag,mode = "collapse"), type = "weighted")
reciprocity(manag)
#set.seed(4567)
#randomedges <- sample(E(manag), size = ecount(manag) - ecount(collb), replace = F)
#manag <- igraph::delete.edges(manag, randomedges)
#transitivity(manag, type = "global")



for(q in igraph::edge_attr_names(collb)){
   collb <- igraph::delete_edge_attr(collb, q)
}
ecount(collb)

igraph::E(collb)$weight <- 1
collb <- igraph::simplify(collb, edge.attr.comb=list(weight="sum"), remove.loops = T)
collb <- igraph::delete.vertices(collb, igraph::V(collb)[igraph::degree(collb, igraph::V(collb), mode = "all",loops = F)==0])

transitivity(collb, type = "global")
reciprocity(collb)


#is_report
rept <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "is_report")])
ecount(rept)
#
for(q in igraph::edge_attr_names(rept)){
   rept <- igraph::delete_edge_attr(rept, q)
}
ecount(rept)

igraph::E(rept)$weight <- 1
rept <- igraph::simplify(rept, edge.attr.comb=list(weight="sum"), remove.loops = T)
rept <- igraph::delete.vertices(rept, igraph::V(rept)[igraph::degree(rept, igraph::V(rept), mode = "all",loops = F)==0])

transitivity(rept, type = "global")
#transitivity(as_undirected(rept,mode = "collapse"), type = "weighted")
reciprocity(rept)



#nonC_collab, group1, group2, group3, CapitalC, coordin, cooper
nonC <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "nonC_collab")])
ecount(nonC)
#
for(q in igraph::edge_attr_names(nonC)){
   nonC <- igraph::delete_edge_attr(nonC, q)
}
ecount(nonC)

igraph::E(nonC)$weight <- 1
nonC <- igraph::simplify(nonC, edge.attr.comb=list(weight="sum"), remove.loops = T)
nonC <- igraph::delete.vertices(nonC, igraph::V(nonC)[igraph::degree(nonC, igraph::V(nonC), mode = "all",loops = F)==0])

transitivity(nonC, type = "global")
#transitivity(as_undirected(nonC,mode = "collapse"), type = "weighted")
reciprocity(nonC)



#CapitalC, group1, group2, group3, coordin, cooper
CapitalC <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "CapitalC")])
ecount(CapitalC)
#
for(q in igraph::edge_attr_names(CapitalC)){
   CapitalC <- igraph::delete_edge_attr(CapitalC, q)
}
ecount(CapitalC)

igraph::E(CapitalC)$weight <- 1
CapitalC <- igraph::simplify(CapitalC, edge.attr.comb=list(weight="sum"), remove.loops = T)
CapitalC <- igraph::delete.vertices(CapitalC, igraph::V(CapitalC)[igraph::degree(CapitalC, igraph::V(CapitalC), mode = "all",loops = F)==0])

transitivity(CapitalC, type = "global")
#transitivity(as_undirected(CapitalC,mode = "collapse"), type = "weighted")
reciprocity(CapitalC)



#collabonly
#group1, group2, group3, coordin, cooper
collabonly <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "collabonly")])
ecount(collabonly)
#
for(q in igraph::edge_attr_names(collabonly)){
   collabonly <- igraph::delete_edge_attr(collabonly, q)
}
ecount(collabonly)

igraph::E(collabonly)$weight <- 1
collabonly <- igraph::simplify(collabonly, edge.attr.comb=list(weight="sum"), remove.loops = T)
collabonly <- igraph::delete.vertices(collabonly, igraph::V(collabonly)[igraph::degree(collabonly, igraph::V(collabonly), mode = "all",loops = F)==0])

transitivity(collabonly, type = "global")
#transitivity(as_undirected(collabonly,mode = "collapse"), type = "weighted")
reciprocity(collabonly)

library(ggraph)
collabonlyplot <- ggraph(collabonly, layout = 'fr')+
   geom_edge_fan(alpha = 1, 
                 end_cap = circle(1,"mm"),
                 width = 0.4,
                 arrow = arrow(angle=15,length=unit(0.03,"inches"),ends = "last",type = "closed"))+
   geom_node_point(
                   alpha = 0.8)+
   theme_void()+ theme(legend.position.inside = c(0.8,0.6))
collabonlyplot

group3plot <- ggraph(group3, layout = 'fr')+
   geom_edge_fan(alpha = 1, 
                 end_cap = circle(1,"mm"),
                 width = 0.4,
                 arrow = arrow(angle=15,length=unit(0.03,"inches"),ends = "last",type = "closed"))+
   geom_node_point(
      alpha = 0.8)+
   theme_void()+ theme(legend.position.inside = c(0.8,0.6))
group3plot

#group1, group2, group3, coordin, cooper
group1 <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "group1")])
ecount(group1)
#
for(q in igraph::edge_attr_names(group1)){
   group1 <- igraph::delete_edge_attr(group1, q)
}
ecount(group1)

igraph::E(group1)$weight <- 1
group1 <- igraph::simplify(group1, edge.attr.comb=list(weight="sum"), remove.loops = T)
group1 <- igraph::delete.vertices(group1, igraph::V(group1)[igraph::degree(group1, igraph::V(group1), mode = "all",loops = F)==0])

transitivity(group1, type = "global")
#transitivity(as_undirected(group1,mode = "collapse"), type = "weighted")
reciprocity(group1)


# group2, group3, coordin, cooper
group2 <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "group2")])
ecount(group2)
#
for(q in igraph::edge_attr_names(group2)){
   group2 <- igraph::delete_edge_attr(group2, q)
}
ecount(group2)

igraph::E(group2)$weight <- 1
group2 <- igraph::simplify(group2, edge.attr.comb=list(weight="sum"), remove.loops = T)
group2 <- igraph::delete.vertices(group2, igraph::V(group2)[igraph::degree(group2, igraph::V(group2), mode = "all",loops = F)==0])

transitivity(group2, type = "global")
#transitivity(as_undirected(group2,mode = "collapse"), type = "weighted")
reciprocity(group2)




# group3, coordin, cooper
group3 <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "group3")])
ecount(group3)
#
for(q in igraph::edge_attr_names(group3)){
   group3 <- igraph::delete_edge_attr(group3, q)
}
ecount(group3)

igraph::E(group3)$weight <- 1
group3 <- igraph::simplify(group3, edge.attr.comb=list(weight="sum"), remove.loops = T)
group3 <- igraph::delete.vertices(group3, igraph::V(group3)[igraph::degree(group3, igraph::V(group3), mode = "all",loops = F)==0])

transitivity(group3, type = "global")
#transitivity(as_undirected(group3,mode = "collapse"), type = "weighted")
reciprocity(group3)



# coordin, cooper
coordin <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "coordin")])
ecount(coordin)
#
for(q in igraph::edge_attr_names(coordin)){
   coordin <- igraph::delete_edge_attr(coordin, q)
}
ecount(coordin)

igraph::E(coordin)$weight <- 1
coordin <- igraph::simplify(coordin, edge.attr.comb=list(weight="sum"), remove.loops = T)
coordin <- igraph::delete.vertices(coordin, igraph::V(coordin)[igraph::degree(coordin, igraph::V(coordin), mode = "all",loops = F)==0])

transitivity(coordin, type = "global")
#transitivity(as_undirected(coordin,mode = "collapse"), type = "weighted")
reciprocity(coordin)



# cooper
cooper <- igraph::delete_edges(sup, E(sup)[!edge_attr(sup, "cooper")])
ecount(cooper)
#
for(q in igraph::edge_attr_names(cooper)){
   cooper <- igraph::delete_edge_attr(cooper, q)
}
ecount(cooper)

igraph::E(cooper)$weight <- 1
cooper <- igraph::simplify(cooper, edge.attr.comb=list(weight="sum"), remove.loops = T)
cooper <- igraph::delete.vertices(cooper, igraph::V(cooper)[igraph::degree(cooper, igraph::V(cooper), mode = "all",loops = F)==0])

transitivity(cooper, type = "global")
#transitivity(as_undirected(cooper,mode = "collapse"), type = "weighted")
reciprocity(cooper)


edgeframesup <- igraph::as_data_frame(sup, what = "edges")

mean(edgeframesup$between[edgeframesup$coordin==T])
mean(edgeframesup$between[edgeframesup$cooper==T])
mean(edgeframesup$between[edgeframesup$collabonly==T])
mean(edgeframesup$between[edgeframesup$group1==T])
mean(edgeframesup$between[edgeframesup$group2==T])
mean(edgeframesup$between[edgeframesup$group3==T])
mean(edgeframesup$between[edgeframesup$CapitalC==T])
mean(edgeframesup$between[edgeframesup$nonC_collab==T])
mean(edgeframesup$between[edgeframesup$is_collab==T])
mean(edgeframesup$between[edgeframesup$is_report==T])
mean(edgeframesup$between[edgeframesup$is_manag==T])


for(q in igraph::edge_attr_names(info)){
   info <- igraph::delete_edge_attr(info, q)
}
ecount(info)

igraph::E(info)$weight <- 1
info <- igraph::simplify(info, edge.attr.comb=list(weight="sum"), remove.loops = T)
info <- igraph::delete.vertices(info, igraph::V(info)[igraph::degree(info, igraph::V(info), mode = "all",loops = F)==0])

transitivity(info, type = "global")
transitivity(as_undirected(info,mode = "collapse"), type = "weighted")
reciprocity(info)


for(q in igraph::edge_attr_names(money)){
   money <- igraph::delete_edge_attr(money, q)
}
ecount(money)

igraph::E(money)$weight <- 1
money <- igraph::simplify(money, edge.attr.comb=list(weight="sum"), remove.loops = T)
money <- igraph::delete.vertices(money, igraph::V(money)[igraph::degree(money, igraph::V(money), mode = "all",loops = F)==0])

transitivity(money, type = "global")
transitivity(as_undirected(money,mode = "collapse"), type = "weighted")
reciprocity(money)






edgelist_w_meta$is_transf <- 
   edgelist_w_meta$type_26 == T |
   edgelist_w_meta$type_33 == T |
   edgelist_w_meta$type_34 == T |
   edgelist_w_meta$type_35 == T |
   edgelist_w_meta$type_45 == T |
   edgelist_w_meta$type_54 == T |
   edgelist_w_meta$type_87 == T 


#only keep the six main verb tenses
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c(
   "VB","VBD","VBG","VBN","VBP","VBZ"),]
#only keep verbs tagged in verbnet
edgelist_w_meta <- edgelist_w_meta[unlist(lapply(seq_along(edgelist_w_meta$type_id), function(k) !is.null(edgelist_w_meta$type_id[[k]]))),]

edgelist_w_meta$head_verb_tense <- as.factor(edgelist_w_meta$head_verb_tense)

edgelist_w_meta <- edgelist_w_meta %>% mutate(verbtype = 
                                                 ifelse(is_collab==T, ifelse(is_transf==F,"Collab","both"), 
                                                        ifelse(is_transf==T, "Outcome","neither")))
edgelist_w_meta$head_verb_tense <- as.character(edgelist_w_meta$head_verb_tense)
edgelist_w_meta <- edgelist_w_meta %>% mutate(verb_tense = ifelse(
   is_future==T,"Future",head_verb_tense))
edgelist_w_meta$verb_tense <- factor(edgelist_w_meta$verb_tense,
                                     levels = c("VBN","VBD","VBG","VBZ","VBP","VB","Future"))
edgelist_w_meta$verbtype <- factor(edgelist_w_meta$verbtype,
                                   levels = c("Outcome","both","neither","Collab"))

teamsandtasks <- edgelist_w_meta[!edgelist_w_meta$verbtype %in% c("both"),]

portiontransf <- sum(edgelist_w_meta$is_transf)/length(edgelist_w_meta$is_collab)
portionnotcollab <- sum(!edgelist_w_meta$is_collab)/length(edgelist_w_meta$is_collab)

portiontransf / portionnotcollab
byplan <- edgelist_w_meta %>% group_by(gsp_id) %>% summarize(pct_collab = mean(is_collab), pct_transf = mean(is_transf), 
                                                             num_hedge = sum(is_transf & has_hedge),
                                                             num_action_verbs = sum(is_transf))
byplan$pct_transf_hedge <- byplan$num_hedge / byplan$num_action_verbs
minimeta <- edgelist_w_meta[!duplicated(edgelist_w_meta$gsp_id),c("gsp_id", "mult_gsas", "approval",
                                                                  "Republican_Vote_Share_scaled",
                                                                  "Agr_Share_Of_GDP_scaled",
                                                                  "percent_dac_by_pop_scaled")]
byplan <- left_join(byplan, minimeta)
byplan$approval <- factor(byplan$approval, ordered = T, levels = c("Incomplete", 
                                                                   "Review In Progress", 
                                                                   "Approved"))
cor(byplan$pct_collab, byplan$pct_transf, method = "pearson")

####H1 confirmed?####
alpha <- cor(byplan$pct_collab, byplan$pct_transf, method = "pearson") #> -1*portiontransf / portionnotcollab

set.seed(431)
N <- 1000
bootstrap <- vector(mode = "list", length = N)
corrs <- vector(mode = "numeric", length = N)
for(i in 1:N){
   bootstrap[[i]] <- sample(byplan$pct_transf, length(byplan$pct_transf), replace = T)
   corrs[i] <- cor(byplan$pct_collab, bootstrap[[i]], method = "pearson")
}
boxplot(corrs)
stripchart(alpha,              # Data
           pch = 19,          # Pch symbols
           col = "blue",           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)

####H2A####
set.seed(936834)
summary(glm(is_collab ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#VERB CLASSES AND mult_gsas
#class 13 verbs_of_change_of_possession
summary(glm(type_13 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 14 learn_verbs
summary(glm(type_14 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#class 16 verbs_of_concealment
summary(glm(type_16 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed

#class 22 verbs_of_combining_and_attaching
summary(glm(type_22 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#class 23 verbs_of_separating_and_disassembling
summary(glm(type_23 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed, although most common word is "vary" and "divide"

#class 36 verbs_of_social_interaction, eg "communicate", "collaborate", "correspond", "cooperate"
summary(glm(type_36 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 37 verbs_of_communication
summary(glm(type_37 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 58 verbs_of_urging_and_begging
summary(glm(type_58 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed! Main verb is "require"


#class 70 rely_verbs
summary(glm(type_70 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 71 conspire_verbs (main verb is collaborate. also includes "partner" but collaborate is covered elsewhere)
summary(glm(type_71 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 72 help_verbs
summary(glm(type_72 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 73 cooperate_verbs
summary(glm(type_73 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 107 involve_verbs #main verb is include
summary(glm(type_107 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#aov test h2a

res_aov_collab <- aov(pct_collab ~ as.factor(mult_gsas),
                      data = byplan
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov_collab$residuals)

# QQ-plot
library(car)
qqPlot(res_aov_collab$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov_collab$residuals) 
#since p>0.05, we aren't certain that residuals are abnormally distributed
#we proceed with anova or t-test

#time to test homogeneity

colnames(byplan)[colnames(byplan)=="pct_collab"] <- "prop_collab"

boxplot(prop_collab ~ mult_gsas,
        data = byplan
)

leveneTest(prop_collab ~ mult_gsas,
           data = byplan
)#is homogeneous
plot(res_aov_collab, which = 3)#red line is close to horizontal. homogeneity met.

#this means we can run a regular anova, ie res_aov or equivalently:
oneway.test(prop_collab ~ mult_gsas, data = byplan, 
            var.equal = T)
#the two groups are different!
TukeyHSD(res_aov_collab)
#mult_gsas have 0.02 (20%) more collaborative verbs compared to single_gsas.


####H2B####
set.seed(2370923)
summary(glm(is_transf ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#VERB CLASSES AND mult_gsas
#class 26 creation and transformation
summary(glm(type_26 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 33 judgement_verbs
summary(glm(type_33 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#confirmed

#class 34 verbs_of_assessment
summary(glm(type_34 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 35 verbs_of_searching
summary(glm(type_35 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#confirmed

#class 45 verbs_of_change of state
summary(glm(type_45 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 54 measure verbs
summary(glm(type_54 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 87 verbs_of_focusing and comprehending
summary(glm(type_87 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed


#aov test h2b
res_aov_transf <- aov(pct_transf ~ as.factor(mult_gsas),
                      data = byplan
)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov_transf$residuals)

# QQ-plot
library(car)
qqPlot(res_aov_transf$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aov_transf$residuals) 
#since p>0.05, we aren't certain that residuals are abnormally distributed
#we proceed with anova or t-test

#time to test homogeneity

colnames(byplan)[colnames(byplan)=="pct_transf"] <- "prop_transf"

boxplot(prop_transf ~ mult_gsas,
        data = byplan
)

leveneTest(prop_transf ~ mult_gsas,
           data = byplan
)#is homogeneous
plot(res_aov_transf, which = 3)#red line is close to horizontal. homogeneity met.

#this means we can run a regular anova, ie res_aov or equivalently:
oneway.test(prop_transf ~ mult_gsas, data = byplan, 
            var.equal = T)
#the two groups are not different
TukeyHSD(res_aov_transf)
#mult_gsas do not have a sig difference in prop transf verbs 



####H3A, H3B, H4A, H4B ####


#TODO make anovas for H3-H4

model34 <- MASS::polr(approval ~ pct_transf_hedge + pct_transf + pct_collab + mult_gsas, data = byplan,
           Hess = T)
summary(model34)

model34b <- nnet::multinom(formula = factor(approval, ordered = F) ~ pct_transf_hedge + 
                              pct_transf + pct_collab + mult_gsas, data = byplan)
summary(model34b)

#### H5 ####
#is there a significant difference in percentage of verbs that are base form between
#collab and transformative?

basetest <- edgelist_w_meta[edgelist_w_meta$verbtype %in% c("Outcome", "Collab"),c("verbtype", "verb_tense")]
basetest$is_vb <- ifelse(basetest$verb_tense=="VB", T, F)
basetest$verb_tense <- NULL
prop.test(c(sum(basetest[basetest$verbtype=="Collab",]$is_vb),
            sum(basetest[basetest$verbtype=="Outcome",]$is_vb)), 
          c(nrow(basetest[basetest$verbtype=="Collab",]), 
            nrow(basetest[basetest$verbtype=="Outcome",])), p = NULL, alternative = "two.sided", correct = TRUE)

#there is no difference in the proportion of base form verbs between outcome and collab verbs, so we can
#interpret the bayesian plot as comparing the proportion of collab verbs vs outcome verbs for each verb tense

library(brms)
library(tidybayes)
library(tidyverse)

edgelist_w_meta_sample <- edgelist_w_meta %>%
   group_by(verbtype, gsp_id) %>%
   summarize(VB = sum(verb_tense == 'VB'),
             VBN = sum(verb_tense == 'VBN'),
             VBD = sum(verb_tense == 'VBD'),
             VBP = sum(verb_tense == 'VBP'),
             VBZ = sum(verb_tense == 'VBZ'),
             VBG = sum(verb_tense == 'VBG'),
             Future = sum(verb_tense == 'Future')) %>%
   mutate(cell_size = VB+VBN+VBD+VBP+VBZ+VBG+Future)



edgelist_w_meta_sample$cell_counts = with(edgelist_w_meta_sample, 
                                          cbind(VB,VBN,VBD,VBP,VBZ,VBG,Future))
colnames(edgelist_w_meta_sample$cell_counts) <- c(
   "VB","VBN","VBD","VBP","VBZ","VBG","Future")
edgelist_w_meta_sample <- edgelist_w_meta_sample %>% select(-VB,-VBN,-VBD,-VBP,-VBZ,-VBG,-Future)

dat_tibble <- tibble(
   # verb_tense = factor(edgelist_w_meta_sample$verb_tense,levels=c(
   #   "VBN","VBD","VB","VBP","VBZ","VBG","Future")
   #),
   cell_size = edgelist_w_meta_sample$cell_size,
   cell_counts = edgelist_w_meta_sample$cell_counts,
   verbtype = edgelist_w_meta_sample$verbtype,
   gsp_id = as.factor(edgelist_w_meta_sample$gsp_id)#,
   #has_hedge = as.numeric(edgelist_w_meta_sample$has_hedge)
)

set.seed(13)
options(mc.cores=parallel::detectCores())

priors <- c(
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBN'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar = 'muVBD'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBP'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBZ'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muVBG'),
   prior('student_t(5,0,1)',class = 'Intercept',dpar='muFuture'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBN'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBD'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBP'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBZ'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muVBG'),
   prior('student_t(5,0,1)',class = 'sd',dpar = 'muFuture')
)

#cell_counts is count of each verb tense
#trials(cell_size) needs to be the row
formula <- brmsformula(
   cell_counts | trials(cell_size) ~ verbtype + (1|gsp_id))#random intercept by plan id.
#instead of treating 117 different levels, we think of them as random draws from a single variance
model <- brm(formula, dat_tibble, multinomial(), priors, control = 
                list(adapt_delta = 0.95), chains = 4, iter = 5000, seed = 12, save_pars = save_pars(all = T))
#check output warning for convergence

saveRDS(model, "data/Verb_Analysis_Paper/brmsH5model.RDS")

#look at coefficient stability
myplot <- plot(model)
length(myplot)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part1.png", myplot[[1]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part2.png", myplot[[2]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part3.png", myplot[[3]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part4.png", myplot[[4]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part5.png", myplot[[5]], width=7, height=4, units = "in", dpi=700)
ggsave("data/Verb_Analysis_Paper/figures/H5model_part6.png", myplot[[6]], width=7, height=4, units = "in", dpi=700)

#look at posterior distribution of each coefficient
summ <- as.data.frame(posterior_summary(model))
splits <- str_split(rownames(summ), "_")
summ$metric <- sapply(splits, function(i) i[1])
summ$tense <- str_extract(sapply(splits, function(i) i[2]), "VB[A-Z]*|Future")
summ$type <- str_remove(sapply(splits, function(i) i[3]), "verbtype")
summ$tensetype <- paste0(summ$tense, summ$type)
summ$zeroes <- 0
summ <- summ[summ$metric == "b" & summ$type != "Intercept",]
myplot <- ggplot(summ, aes(x = Estimate, y = type, color = type)) +  geom_point()  +
   facet_wrap(vars(summ$tense), drop = T, nrow = 6, ncol = 1) + geom_vline(aes(xintercept = zeroes), lty = 2) +
   geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5)) 
ggsave("data/Verb_Analysis_Paper/figures/H5coeffs.png", myplot, width=7, height=4, units = "in", dpi=700)

summ <- as.data.frame(posterior_summary(model))
splits <- str_split(rownames(summ), "_")
summ$metric <- sapply(splits, function(i) i[1])
summ$tense <- str_extract(sapply(splits, function(i) i[2]), "VB[A-Z]*|Future")
summ$type <- str_remove(sapply(splits, function(i) i[3]), "verbtype")
summ$tensetype <- paste0(summ$tense, summ$type)
summ$zeroes <- 0
summ <- summ[summ$metric == "b" & summ$type == "Intercept",]
myplot <- ggplot(summ, aes(x = Estimate, y = type, color = type)) +  geom_point()  +
   facet_wrap(vars(summ$tense), drop = T, nrow = 6, ncol = 1) +
   geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5)) 
myplot
ggsave("data/Verb_Analysis_Paper/figures/H5prob.png", myplot, width=7, height=4, units = "in", dpi=700)



#### H6 ####

summary(glm(has_hedge ~ verbtype, data = edgelist_w_meta, family = "binomial"))

#### scraps ####




byplan <- byplan %>% filter(verbtype %in% c("Tasks","Teamwork"))
library(GGally)
byplanmini <- byplan %>% ungroup() 
byplanmini <- byplanmini[,c(#"approval",
   "Agr_Share_Of_GDP_scaled",
   "percent_dac_by_pop_scaled",
   "Republican_Vote_Share_scaled",
   #"Perc_Bach_Degree_Over25_scaled",
   "pct_transf_hedge","pct_collab","pct_transf")]
byplanmini$approval <- factor(byplanmini$approval, levels = c("Incomplete","Review In Progress", "Approved"))
colnames(byplanmini)[colnames(byplanmini)=="pct_tasks"] <- "prop_outcomes"
colnames(byplanmini)[colnames(byplanmini)=="pct_tasks_hedged"] <- "prop_outcomes_hedged"
colnames(byplanmini)[colnames(byplanmini)=="pct_collab"] <- "prop_collab"


library(GGally)
pairs <- byplanmini %>% ggpairs()
pairs
library(ggplot2)
fit_approval <- nnet::multinom(formula = as.factor(approval) ~ pct_tasks_hedged + 
                                  pct_collab + pct_tasks,
                               data = byplan, model = F)

fdf <- broom::tidy(fit_approval, exponentiate = FALSE, conf.int = TRUE)
View(fdf)

simple <- nnet::multinom(formula = as.factor(approval) ~ pct_tasks_hedged ,
                         data = byplan, model = F)
summary(simple)
colnames(byplan)[colnames(byplan)=="pct_tasks_hedged"] <- "prop_outcomes_hedged"
byplan$approval <- ifelse(byplan$approval=="Incomplete","Rejected",byplan$approval)
boxplot(prop_outcomes_hedged ~ approval,
        data = byplan
)


res_aov <- aov(pct_tasks_hedged ~ as.factor(approval),
               data = byplan
)
ggplot(aes(x=approval,y=pct_tasks_hedged), data = byplan)+
   geom_boxplot(fill="#666666") + theme_bw()
TukeyHSD(res_aov)
oneway.test(pct_multi ~ verbtype, data = byplan, 
            var.equal = T)
res_aov <- aov(pct_multi ~ as.factor(verbtype),
               data = byplan
)
boxplot(pct_multi ~ verbtype,
        data = byplan
)
TukeyHSD(res_aov)

edgelist_w_meta$doc_sent <- unlist(lapply(1:length(mysplits), function(i) paste(mysplits[[i]][1:length(mysplits[[i]])-1],collapse="_")))
bysentence <- edgelist_w_meta %>% group_by(doc_sent, gsp_id) %>% summarize(has_collab = max(is_collab), has_transf = max(is_transf))
res_aov <- aov(has_collab ~ as.factor(has_transf),
               data = bysentence
)
res_aov
boxplot(has_collab ~ has_transf,
        data = bysentence
)
TukeyHSD(res_aov)
mean(bysentence$has_transf)
mean(bysentence[bysentence$has_collab==1,bysentence$has_transf])

get_variables(model)
#this works!
m13.4hedge <- ulam(
   alist(
      has_hedge ~ dbinom( 1, p) ,
      logit(p) <- a2[verbtype] + g2[gsp_id],
      # adaptive priors
      a2[verbtype] ~ dnorm( a_bar , sigma_a ),
      g2[gsp_id] ~ dnorm( 0 , sigma_g ),
      # hyper-priors
      a_bar ~ dnorm( 0 , 1 ),
      sigma_a ~ dexp(0.75),
      sigma_g ~ dexp(0.75)
   ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )

#less efficient!
m13.4hedge_reparam <- ulam(
   alist(
      has_hedge ~ dbinom( 1, p) ,
      logit(p) <- a_bar + z[verbtype]*sigma_a + x[gsp_id]*sigma_g,
      # adaptive priors
      z[verbtype] ~ dnorm( 0 , 1 ),
      x[gsp_id] ~ dnorm( 0 , 1 ),
      # hyper-priors
      a_bar ~ dnorm( 0 , 1 ),
      sigma_a ~ dexp(1),
      sigma_g ~ dexp(1)
   ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )


