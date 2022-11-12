library(spacyr)
library(reticulate)
library(tidyverse)
library(tidytext)
library(quanteda)
#prerequisites: step 1, install python
#step 2, install miniconda from https://conda.io/miniconda.html
#step 3 (unsure if this was required) I installed virtualenv, numpy, conda, and spacy
Sys.setenv(RETICULATE_PYTHON="/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python")
py_config()
#spacy_install()
#spacy_download_langmodel(model = 'en_core_web_lg')
spacy_initialize(model = "en_core_web_lg")

gsp_text_with_meta <- readRDS("data_output/gsp_docs_w_meta")
gsp_planonly <- gsp_text_with_meta[is_comment==FALSE & is_reference==FALSE]
single_plan <- gsp_planonly[gsp_id=="0007"]
#part 1: POS tagging with spacy
parsedtxt <- spacy_parse(single_plan$text,
                         pos = T,
                         tag = T,
                         lemma = T,
                         entity = T,
                         dependency = T,
                         nounphrase = T)
#part 2: use NER to identify entities (Yadav and Bethard, 2019) with spacy and tidytext
entities <- entity_extract(parsedtxt,type="all")

subj-obj-verb between orgs or people
#step 1 tagged entities with nsubj and nobj 
#step 2 (opt) dependency parsing (disambiguating pronouns)
#step 3 types of verbs -- analyzing types, groupings
#step 4 = verbnet, nature of verbs, categorization. match with verb cats from verbnet
#step 5 (opt) lemmatization - how do we chop the verbs? do we use lemma or not
#step 6 (opt) hedging and polarity (should be able to rely on POS tagging)
spacy_data %>% group_by(sentence_id, doc_id) %>% summarize(tally(pos==nsubj>0),tally(pos==nobj>0))
#step 7 cross-reference database of agency names
#keep orgs and
#tag entities whose name matches external database
#take a couple steps to remove noise in orgs
#dig into the probability under each org (keep orgs with > 0.75 prob)
#could use filters to identify good sentences (subject and verb)
#goal of four-ish layers with different types of connections, then count within each category (could use verbnet here)
#don't worry about tense
#look at how the cues we know (like "might") is classified in spacyr parse
orgs <- unique(entities[entities$entity_type=="ORG",][,'entity'])

spacy_finalize()

#later add:     conduct hedging detection on verbs
#later add:     conduct polarity detection on verbs
#part 5: use event extraction (define event) based on 
#later add        non-hedging, positive verb relationships between entities (Bethard and Martin, 2006)
#verb net lexicon -- dictionary of verbs
#focus on one or two plans
#1 million char max
#could call pdftotext, then run cleaning, then collapse into giant string, then tokenizer
#deal with generic nouns when we scale up eg county that refer to different counties
#part 6: build network based on identified events
#part 7: Use SNA to identify:
#                 central actors
#                 peripheral actors
#                 network connectivity
#                 network centralization
#part 8: make plot of how these factors differ for GSPs
#part 9: make a regression about how each of those four 
#        factors is related to topic prevalence
#part 10: make a regression about how each of those four
#         factors is related to topic correlation network connectivity/centralization