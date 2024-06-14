import spacy
from spacy import displacy

nlp = spacy.load("en_core_web_lg")
doc = nlp("The BVBGSA applied for and received a grant from CDWR to fund the preparation of the GSP")

options = {"compact": True, 
           "color": "black", "distance": 85}

from pathlib import Path
mypath = "mydependency.svg"
mysvg = displacy.render(doc, style = "dep", options = options, jupyter=False)
output_path = Path(mypath)
output_path.open("w",encoding="utf-8").write(mysvg)
displacy.serve(doc, style = "dep",options = options, port = 8882, host = "127.0.0.1")


