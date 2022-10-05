# script to get speech interjection info

# load required packages
library(XML)
library(here)
library(tidyverse)

parse_interject <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))
  
  #### CHAMBER ####
  # sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
  # use if-else statements in case there are no interjections
  sub1_interject_chamb <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
  # use if-else statements in case there are no interjections, or there is no sub-debate 2
  sub2_interject_chamb <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no ={if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  #### FEDERATION CHAMBER ####
  # sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
  # don't need if-else here b/c this only executes if interjections exist in sub-debate 1 (condition of outer if statement)
  sub1_interject_fed <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
  
  # sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
  # use if-else statements in case there are no interjections, or there is no sub-debate 2
  sub2_interject_fed <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
  
  # if else statement used in case of no interjections and/or sub-debate 2
  # merge chamber and federation chamber tibbles, add flags for question, answer, and each sub-debate, arrange by page number
  if (nrow(sub1_interject_chamb) > 0 | nrow(sub1_interject_fed) > 0) {
    interject_sub1 <- rbind(sub1_interject_chamb, sub1_interject_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(question = 0, answer = 0, sub1_flag = 1, sub2_flag = 0)
  } else {
    interject_sub1 <- rbind(sub1_interject_chamb, sub1_interject_fed)
  }
  
  # same thing for sub-debate 2 interjections
  if (nrow(sub2_interject_chamb) > 0 | nrow(sub2_interject_fed) > 0) {
    interject_sub2 <- rbind(sub2_interject_chamb, sub2_interject_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(question = 0, answer = 0, sub1_flag = 0, sub2_flag = 1)
  } else {
    interject_sub2 <- rbind(sub2_interject_chamb, sub2_interject_fed)
  }
  
  # return a list of two tibbles
  return(list(interject_sub1, interject_sub2))
  
}

# ex: call function, get sub-debate 1 interjections
parse_interject("2022_08_02.xml")[[1]]

# ex: call function, get sub-debate 2 interjections
parse_interject("2022_08_02.xml")[[2]]
