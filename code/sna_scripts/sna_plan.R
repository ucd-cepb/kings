#part 1: POS tagging with spacy
#part 2: use NER to identify entities (Yadav and Bethard, 2019) with spacy and tidytext
#part 3: conduct hedging detection on verbs
#part 4: conduct polarity detection on verbs
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