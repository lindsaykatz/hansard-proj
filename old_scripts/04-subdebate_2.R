# script for getting sub-debate 2 info & text
# note: sub-debate 2 is nested within sub-debate 1

# load required packages
library(XML)
library(here)
library(tidyverse)

parse_sub2 <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))

  #### CHAMBER ####
  # store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
  # include if-else statements in case sub-debate 2 does not exist
  sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # sometimes sub-debate 2 isn't nested in sub-debate 1 (most recent date where this is the case is 2021-10-21)
  # if the number of rows of this is > 1, let's bind that onto what we already have to ensure we aren't missing anything
  if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebate.text"))))) > 0) {
    
    sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebateinfo")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
      rbind(., sub2_info_chamb)
    }
  
  # store sub-debate 2 talker info & speech in tibble, correct variable class and extract time, add flag for federation chamber
  sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # same idea as for info, nesting changes, want to account for this so we don't miss anything
  if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.start/talker")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.text"))))) > 0) {
    
    sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.start/talker")),
                               xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
             party = {if ("party" %in% names(.)) as.factor(party) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
      rbind(., sub2_speech_chamb)
    }
  
  #### FEDERATION CHAMBER ####
  # use if-else statement in case Hansard doesn't have sub-debate 2 in federation chamber
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebateinfo"))) > 0) {
    
    # store sub-debate 2 information & text in tibble, correct variable class
    sub2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             fedchamb_flag = 1)
    
    # same idea as for chamber, nesting changes, want to account for this so we don't miss anything
    if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/subdebateinfo")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/subdebate.text"))))) > 0) {
      
      sub2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/subdebateinfo")),
                               xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/subdebate.text"))) %>% 
        as_tibble() %>% 
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
               fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
        rbind(., sub2_info_fed)
      }
    
    # store sub-debate 2 talker info & speech in tibble, correct variable class and extract time, add flag for federation chamber
    sub2_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
             party = as.factor(party),
             fedchamb_flag = 1)
    
    # same idea as for chamber, nesting changes, want to account for this so we don't miss anything
    if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/speech/talk.text"))))) > 0) {
      
      sub2_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/speech/talk.start/talker")),
                                 xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.2/speech/talk.text"))) %>% 
        as_tibble() %>% 
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
               time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
               party = {if ("party" %in% names(.)) as.factor(party) else NULL},
               fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
        rbind(., sub2_speech_fed)
      }
    
    # merge chamber and federation tibbles together, arrange by page number
    sub2_info <- rbind(sub2_info_chamb, sub2_info_fed) %>% 
      arrange(page.no)
    
    # merge chamber and federation tibbles together, flag for interjections, arrange by page number and time stamp
    sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_fed) %>% 
      arrange(page.no, time.stamp) 
    
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
parse_sub2("2021-10-21.xml")[[1]]

# ex: call function, get sub-debate 2 speech tibble
parse_sub2("2021-10-21.xml")[[2]]
