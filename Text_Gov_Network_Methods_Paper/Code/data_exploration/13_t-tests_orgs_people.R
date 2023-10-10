


orgs <- readRDS("data/output_large_files/gov_dir_weight_orgs_properties")

people <- readRDS("data/output_large_files/gov_dir_weight_people_properties")

#not including the yuba duplicate and the improper pdf formatting
orgs <- orgs[c(1:38,40:67,69:119),]
people <- people[c(1:38,40:67,69:119),]

t.test(as.numeric(orgs$reciprocity), as.numeric(people$reciprocity))
#orgs have more reciprocity (against hypothesis)
t.test(as.numeric(orgs$transitivity), as.numeric(people$transitivity))
#people have more transitivity (confirms hypothesis), probably because so many of them only have one edge
t.test(as.numeric(orgs$num_communities), as.numeric(people$num_communities))
#orgs have more communities 
t.test(as.numeric(orgs$modularity), as.numeric(people$modularity))
#orgs have more clustering (against hypothesis)

boxplot(as.numeric(orgs$reciprocity), as.numeric(people$reciprocity), names=c("orgs","people"))
title("Reciprocity")
boxplot(as.numeric(orgs$transitivity), as.numeric(people$transitivity), names=c("orgs","people"))
title("Transitivity")
boxplot(as.numeric(orgs$num_communities), as.numeric(people$num_communities), names=c("orgs","people"))
title("Number of Communities")
boxplot(as.numeric(orgs$modularity), as.numeric(people$modularity), names=c("orgs","people"))
title("Modularity")

