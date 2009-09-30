This extractor tries to squeeze a BGF from the hypertext documentation.
In order to do so, is applies a number of heuristics and fixes the most
cmmonly encountered problems in manually created language specifications.

It works equally well with Java Language Standard 2 and 3, but requires
an additional action for Java Language Standard 1. This action virtually
equals running some sort of //pre XPath query, but the HTML was too dirty
and non-well-formed to use the real xpath utility.
