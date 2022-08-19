# Script for getting session information 
# Content includes: date, parliament number, session number, period number, page number, proof

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("XML_files", "2021_11_30.xml"))

# store session information in tibble, correct class of variables
session_info <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//session.header")) %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date),
         parliament.no = as.numeric(parliament.no),
         session.no = as.numeric(session.no),
         period.no = as.numeric(period.no),
         page.no = as.numeric(page.no),
         proof = as.numeric(proof))
