# script for getting session information 
# content includes: date, parliament number, session number, period number, page number, proof

# load required packages
library(XML)
library(here)
library(tidyverse)

# define function
parse_session_info <- function(filename){
  
  # parse XML file
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))
  
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

# ex: call function, pass file name
# parse_session_info("2018-08-23.xml")

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/input/")

# define empty tibble
session_info_all <- tibble()

# loop through all files, parse session info, store in tibble
for(i in 1:length(files_all)){
  this_session_info <- parse_session_info(files_all[i])
  session_info_all <- rbind(session_info_all, this_session_info)
}

# export all session info
write.csv(session_info_all, paste0("/Volumes/Verbatim/output/old-data/session_info/session_info_all.csv"), row.names = FALSE)
