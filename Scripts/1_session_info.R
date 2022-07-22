# script for getting session information

# parse XML
doc <- xmlParse("~/Desktop/RA/hansard-proj/hansard.xml")

# store session information in data frame
session_info <- xmlToDataFrame(node=getNodeSet(doc, "//session.header"))