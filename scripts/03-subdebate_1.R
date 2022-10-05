# script for getting sub-debate 1 info & text

# load required packages
library(XML)
library(here)
library(tidyverse)

######## TUESDAY 6:00 NEED TO ADD IF-ELSE TO SUBDEBATE 1 CHAMBER.

# define function
parse_sub1 <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))

  # # define interjection words - commenting this out b/c found better way to do so after everything is split (see 99-everything script)
  # interject_words <- c("Order!", "Order.", "interjecting", "Interjecting", "interjected", "Interjected", "interjections", "interjection", "Interjections", 
  #                      "Interjection", "interject", "Interject", "The time for the discussion has concluded", "I thank the honourable member for", 
  #                      "I thank the member for", "should withdraw that remark", "In accordance with standing order 193 the time for constituency statements has concluded", 
  #                      "There being no further grievances, the debate is adjourned", "the time for members' statements has concluded", 
  #                      "The original question was that this bill be now read a second time", "Is the amendment seconded?",
  #                      "Do you want to seek the call again?", "The question is that the amendment be disagreed to", "The question now is that the bill be agreed to", 
  #                      "will resume his seat", "will resume her seat", "will resume their seat") 
  
  #### CHAMBER ####
  # store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
  sub1_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.text"))) %>%
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # store sub-debate 1 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
  sub1_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  #### FEDERATION CHAMBER ####
  # use if-else statement in case Hansard doesn't have federation chamber
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebateinfo"))) > 0) {
    
    # store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
    sub1_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.text"))) %>%
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             fedchamb_flag = 1)
    
    # store sub-debate 1 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
    sub1_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
             party = as.factor(party),
             fedchamb_flag = 1)
    
    # merge chamber and federation tibbles together, flag for which sub-debate, arrange by fedchamb flag, and page
    sub1_info <- rbind(sub1_info_chamb, sub1_info_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0)
    
    # merge chamber and federation tibbles together, flag for question and answer, and which sub-debate, arrange by fedchamb flag, page and time
    sub1_speech <- rbind(sub1_speech_chamb, sub1_speech_fed) %>% 
      arrange(fedchamb_flag, time.stamp, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0,
             question = 0,
             answer = 0)
     # mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))
  
  } else {
    
    # merge chamber and federation tibbles together, arrange by page number
    sub1_info <- sub1_info_chamb %>% 
      arrange(page.no)
    
    # merge chamber and federation tibbles together, flag for interjections, arrange by page number and time stamp
    sub1_speech <- sub1_speech_chamb %>% 
      arrange(page.no, time.stamp)
      
    # mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))
    
  }
  
  # remove name, title, and time stamp from speech body
  sub1_speech$body <- str_remove(sub1_speech$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when there is no title, just name and time stamp to be removed from body
  sub1_speech$body <- str_remove(sub1_speech$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # return a list of two tibbles
  return(list(sub1_info, sub1_speech))
  
}

# ex: call function, get sub-debate 1 info tibble
parse_sub1("2018-08-23.xml")[[1]]

# ex: call function, get sub-debate 1 speech tibble
parse_sub1("2018-08-23.xml")[[2]]

