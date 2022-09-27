# script for getting session information 
# content includes: date, parliament number, session number, period number, page number, proof

# load required packages
library(XML)
library(here)
library(tidyverse)

# define function
parse_session_info <- function(filename){
  
  # parse XML file
  hansard_xml <- xmlParse(here("input", filename))
  
  # store session information in tibble, correct class of variables
  session_info <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//session.header")) %>% 
    as_tibble() %>% 
    mutate(date = as.Date(date),
           parliament.no = as.numeric(parliament.no),
           session.no = as.numeric(session.no),
           period.no = as.numeric(period.no),
           page.no = as.numeric(page.no),
           proof = as.numeric(proof))
  
  return(session_info)
}

# call function, pass file name
parse_session_info("2022_08_01.xml")

