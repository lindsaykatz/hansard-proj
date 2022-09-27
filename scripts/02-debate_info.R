# script for getting debate information 
# content includes: business start, title, page number, type, body

# load required packages
library(XML)
library(here)
library(tidyverse)

# define function
parse_debate_info <- function(filename){
  
  # parse XML file
  hansard_xml <- xmlParse(here("input", filename))
  
  #################### CHAMBER ####################
  ######### BUSINESS START #########
  # store business start in tibble, add flag for federation chamber, extract date, body, and start time
  bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start")), 
                            fedchamb_flag = 0) %>% 
    mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
           date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
           body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"),
           start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"))
  
  ######### DEBATE INFORMATION #########
  # store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and federation chamber
  debate_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debateinfo")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = as.numeric(page.no),
           fedchamb_flag = 0,
           sub1_flag = 0,
           sub2_flag = 0)
  
  #################### FEDERATION CHAMBER ####################
  # use if-else statement to ensure code works for Hansard with and without federation chamber
  # check that there is a business start to know if federation chamber exists
  
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")))) > 0) {
    
    ######### BUSINESS START #########
    # store business start in tibble, add flag for federation chamber, extract date and start time
    bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")), 
                            fedchamb_flag = 1) %>% 
      mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
             date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
             body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"),
             start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"))
    
    # merge into single business start tibble
    bus_start <- rbind(bus_start_chamb, bus_start_fed)
    
    ######### DEBATE INFORMATION #########
    # store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and federation chamber
    debate_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debateinfo")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             fedchamb_flag = 1,
             sub1_flag = 0,
             sub2_flag = 0)
    
    # merge chamber & federation chamber tibbles into single debate information tibble
    debate_info <- rbind(debate_info_chamb, debate_info_fed)
    
  } else {
    
    # re-name chamber business start
    bus_start <- bus_start_chamb
    
    # re-name chamber debate info
    debate_info <- debate_info_chamb

  }
  
  # return a list of two tibbles
  return(list(bus_start, debate_info))
  
}

# ex: call function, get business start tibble
parse_debate_info("2021_11_25.xml")[[1]]

# ex: call function, get debate info tibble
parse_debate_info("2021_11_25.xml")[[2]]
