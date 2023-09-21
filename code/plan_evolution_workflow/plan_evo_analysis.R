#after scraping, analyze:

#how do the networks change over time? This would involve running the textNet
#functions parse_text(), then textnet_extract(), then find_acronyms() and disambiguate().
#You can use the textNet vignette for help on syntax and process with this.
#Then use export_to_network() to convert them into network objects, 
#so you can use a tergm to analyze the changes over time

#how to predict network-level attribute (approval) based on network attribute differences?
#ask experts about this

#read in determination letters and compare their content (through STM or 
#"textnets" graph, which finds occurrence of terms in a document https://textnets.readthedocs.io/en/stable/tutorial.html
#not "textNet" graph, which finds occurrence of entities in a sentence) )
#to the basin-level attributes 

#compare determination letter content to network-level attributes of the corresponding GSP plan version entity network (made through textNet)

