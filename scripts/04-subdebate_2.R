# script for getting sub-debate 2 info & text
# note: sub-debate 2 is nested within sub-debate 1

# load required packages
library(XML)
library(here)
library(tidyverse)

parse_sub2 <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("input", filename))
  
  # # define interjection words
  # interject_words <- c("Order!", "Order.", "interjecting", "Interjecting", "interjected", "Interjected", "interjections", "interjection", "Interjections", 
  #                      "Interjection", "interject", "Interject", "The time for the discussion has concluded", "I thank the honourable member for", 
  #                      "I thank the member for", "should withdraw that remark", "In accordance with standing order 193 the time for constituency statements has concluded", 
  #                      "There being no further grievances, the debate is adjourned","the time for members' statements has concluded", 
  #                      "The original question was that this bill be now read a second time", "Is the amendment seconded?",
  #                      "Do you want to seek the call again?", "The question is that the amendment be disagreed to", "The question now is that the bill be agreed to", 
  #                      "will resume his seat", "will resume her seat", "will resume their seat")
  
  #### CHAMBER ####
  # store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
  # include if-else statements in case sub-debate 2 does not exist
  sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # store sub-debate 2 talker info & speech in tibble, correct variable class and extract time, add flag for federation chamber
  sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  #### FEDERATION CHAMBER ####
  # use if-else statement in case Hansard doesn't have sub-debate 2 in federation chamber
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebateinfo"))) > 0) {
    
    # store sub-debate 2 information & text in tibble, correct variable class
    sub2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             fedchamb_flag = 1)
    
    # store sub-debate 2 talker info & speech in tibble, correct variable class and extract time, add flag for federation chamber
    sub2_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
             party = as.factor(party),
             fedchamb_flag = 1)
    
    # merge chamber and federation tibbles together, arrange by page number
    sub2_info <- rbind(sub2_info_chamb, sub2_info_fed) %>% 
      arrange(page.no)
    
    # merge chamber and federation tibbles together, flag for interjections, arrange by page number and time stamp
    sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_fed) %>% 
      arrange(page.no, time.stamp) 
      # mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0)) 
    
    # remove name, title, and time stamp from speech body
    sub2_speech$body <- str_remove(sub2_speech$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
    
    # case when there is no title, just name and time stamp to be removed from body
    sub2_speech$body <- str_remove(sub2_speech$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
    
  } else if (nrow(sub2_info_chamb) > 0) {
    # else if we have sub-debate 2 for chamber, do processing just with chamber tibbles
    
    # merge chamber and federation tibbles together, arrange by page number
    sub2_info <- sub2_info_chamb %>% arrange(page.no)
      
    # merge chamber and federation tibbles together, flag for interjections, arrange by page number and time stamp
    sub2_speech <- sub2_speech_chamb %>% 
      arrange(page.no, time.stamp)
      # mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0)) 
      
    # remove name, title, and time stamp from speech body
    sub2_speech$body <- str_remove(sub2_speech$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
      
    # case when there is no title, just name and time stamp to be removed from body
    sub2_speech$body <- str_remove(sub2_speech$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
    
    } else {
      
    # no sub-debate 2 for either chamber or federation chamber, return empty tibbles
    sub2_info <- tibble()
    sub2_speech <- tibble()
    
    }

  # return a list of two tibbles
  return(list(sub2_info, sub2_speech))
  
}

# ex: call function, get sub-debate 2 info tibble
parse_sub2("2021_11_30.xml")[[1]]

# ex: call function, get sub-debate 2 speech tibble
parse_sub2("2021_11_30.xml")[[2]]
