# script for getting session information

library(XML)
library(here)

# parse XML
doc <- xmlParse(here("hansard-proj/XML_files", "2021_11_30.xml"))

# store session information in data frame
session_info <- xmlToDataFrame(node=getNodeSet(doc, "//session.header"))
