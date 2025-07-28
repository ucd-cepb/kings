## Introduction

• Network extraction: textnet\_extract(), which generates a graph
database from parsed text based upon tags and dependency relations

• Disambiguation: tools for cleaning, recoding, and aggregating node and
edge attributes, such as the find\_acronyms() function, which can be
paired with the disambiguation() function to identify acronyms in the
text and replace them with the full entity name.

• Exploration: the export\_to\_network() function for exporting the
graph database to igraph and network objects, top\_features() for
viewing node and edge attributes, and combine\_networks() for
aggregating multiple document-based graphs based on common nodes.

## Example

document, along with verb attributes imported from *VerbNet*
(Kipper-Schuler 2006).

ackage (Pedersen and RStudio 2024)

h or network object from the textnet\_extract output is included in the
package as the function export\_to\_network(). It returns a list that
contains the igraph or network itself as the first element, and an
attribute table as the second element. Functions from the *sna* (Butts
2024), *igraph* (Csárdi et al. 2024), and *network* packages (Butts et
al. 2023) are invoked to create a network attribute table of common
network-level attributes; see package documen

-   doc\_id, a unique ID for each page
-   sentence\_id, a unique ID for each sentence
-   token\_id, a unique ID for each token
-   token, the token, generally a word, represented as a string
-   lemma, the canonical or dictionary form of the token
-   pos, a code referring to the token’s part of speech, defined
    according to [Universal
    Dependencies](http://universaldependencies.org/u/pos/) (Nivre 2017).
-   tag, a code referring to the token’s part of speech, according to
    [Penn Treebank](https://catalog.ldc.upenn.edu/docs/LDC99T42/)
    (Marcus et al. 1999).
-   head\_token\_id, a numeric ID referring to the token\_id of the head
    token of the current row’s token
-   dep\_rel, the dependency label according to [ClearNLP
    Dependency](https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md)
    labels (Choi 2024)
-   entity, the entity type category defined by [OntoNotes
    5.0](https://catalog.ldc.upenn.edu/docs/LDC2013T19/OntoNotes-Release-5.0.pdf)
    (Weischedel et al. 2012). This is represented as as string, ending
    in “\_B” if it is the first token in the entity or “\_I” otherwise).

as the output conforms to spaCy tagging standards: Universal
Dependencies tags for the “pos” part-of-speech column (Nivre 2017), and
Penn Treebank tags for the “tags” column (Marcus et al. 1999). The
textnet\_extract function expects the parsed table to follow specific
conventions. First, a row must be included for each token. The column
names expected by textnet\_extract are:

## References

Butts, Carter T. 2024. *Sna: Tools for Social Network Analysis* (version
2.8). <https://cran.r-project.org/web/packages/sna/index.html>.

Butts, Carter T., David Hunter, Mark Handcock, Skye Bender-deMoll,
Jeffrey Horner, Li Wang, Pavel N. Krivitsky, Brendan Knapp, Michał
Bojanowski, and Chad Klumb. 2023. *Network: Classes for Relational Data*
(version 1.18.2).
<https://cran.r-project.org/web/packages/network/index.html>.

Choi, Jinho. 2024. “ClearNLP Dependency Labels. ClearNLP Guidelines.”
August 7, 2024.
<https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md>.

Csárdi, Gábor, Tamás Nepusz, Vincent Traag, Szabolcs Horvát, Fabio
Zanini, Daniel Noom, Kirill Müller, Maëlle Salmon, Michael Antonov, and
Chan Zuckerberg Initiative igraph author details. 2024. *Igraph: Network
Analysis and Visualization* (version 2.1.1).
<https://cran.r-project.org/web/packages/igraph/index.html>.

Kipper-Schuler, K. 2006. *VerbNet* (version 3.3). University of Colorado
Boulder. <https://verbs.colorado.edu/verb-index/vn3.3/index.php>.

Marcus, Mitchell, Beatrice Santorini, Mary Ann Marcinkiewicz, and Ann
Taylor. 1999. “Treebank-3 Documentation.”
https://doi.org/<https://doi.org/10.35111/gq1x-j780>.

Nivre, Joakim. 2017. “Universal POS Tags.” Universal Dependencies.
<https://universaldependencies.org/u/pos/>.

Pedersen, Thomas Lin, and RStudio. 2024. *Ggraph: An Implementation of
Grammar of Graphics for Graphs and Networks* (version 2.2.1).
<https://cran.r-project.org/web/packages/ggraph/index.html>.

Weischedel, Ralph, Sameer Pradhan, Lance Ramshaw, Jeff Kaufman, Michelle
Franchini, Mohammed El-Bachouti, Nianwen Xue, et al. 2012. “OntoNotes
Release 5.0 with OntoNotes DB Tool V0.999 Beta.” BBN Technologies.
<https://catalog.ldc.upenn.edu/docs/LDC2013T19/OntoNotes-Release-5.0.pdf>.
