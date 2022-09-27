# script to get questions and answers
# question time only takes place in sub-debate 1, in Chamber

# load required packages
library(XML)
library(here)
library(tidyverse)

parse_q_and_a <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("input", filename))
  
  # define interjection words
  # interject_words <- c("Order!", "Order.", "interjecting", "Interjecting", "interjected", "Interjected", "interjections", "interjection", "Interjections", 
  #                      "Interjection", "interject", "Interject", "The time for the discussion has concluded", "I thank the honourable member for", "I thank the member for", "should withdraw that remark", 
  #                      "In accordance with standing order 193 the time for constituency statements has concluded", "There being no further grievances, the debate is adjourned",
  #                      "the time for members' statements has concluded", "The original question was that this bill be now read a second time", "Is the amendment seconded?",
  #                      "Do you want to seek the call again?", "The question is that the amendment be disagreed to", "The question now is that the bill be agreed to", "will resume his seat", "will resume her seat",
  #                      "will resume their seat") 
  
  #### QUESTION AND ANSWER INFO AND TEXT ####
  # store questions in tibble, correct variable class, add flag for question/answer, extract time
  sub1_q <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker")),
              xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = as.numeric(page.no),
           party = as.factor(party),
           question = 1,
           answer = 0,
           time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"))
  
  # store answers in tibble, correct variable class, add flag for question/answer, extract time
  sub1_a <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.start/talker")),
              xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = as.numeric(page.no),
           party = as.factor(party),
           question = 0,
           answer = 1,
           time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"))
  
  # merge questions and answers, add flag for sub-debate 1 and 2, and federation chamber (always 0 b/c only have question time in chamber)
  # had to add order and fix things up b/c sometimes time stamp is the same and then things are ordered wrong
  # if a question and answer have the same time stamp, and the question is preceeded by another question, swap the question with the answer that has the same time stamp
   sub1_q_a <- rbind(sub1_q, sub1_a) %>% 
    arrange(time.stamp) %>% 
    rowid_to_column("order") %>% 
    mutate(order = ifelse(lead(time.stamp)==time.stamp & lag(question)==question & order!=1, order+1, order)) %>% 
    mutate(order = ifelse(lead(order)==order, order+1, order)) %>% arrange(order) %>% 
    select(-order) %>% 
    mutate(sub1_flag = 1, 
           sub2_flag = 0,
           fedchamb_flag = 0)
  # mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))
  
  # cleaning up name, title and time stamps from body
  sub1_q_a$body <- str_remove(sub1_q_a$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when there is no title, just name and time stamp in body
  sub1_q_a$body <- str_remove(sub1_q_a$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  ######### QUESTION AND ANSWER INTERJECTIONS #########
  # store question interjections in tibble, correct variable class, add flag for whether question/answer
  # use if-else statements in case there are no interjections
  sub1_q_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           question = {if("page.no" %in% names(.)) 1 else NULL},
           answer = {if("page.no" %in% names(.)) 0 else NULL})
  
  # store answer interjections in tibble, correct variable class, add flag for whether question/answer
  # use if-else statements in case there are no interjections
  sub1_a_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           question = {if("page.no" %in% names(.)) 0 else NULL},
           answer = {if("page.no" %in% names(.)) 1 else NULL})
  
  # merge question and answer interjections, arrange by page number
  # if there are question and/or answer interjections, merge, arrange and add flags
  if(nrow(sub1_q_interject) > 0 | nrow(sub1_a_interject) > 0) {
    sub1_q_a_interject <- rbind(sub1_q_interject, sub1_a_interject) %>% 
      arrange(page.no) %>% 
      mutate(sub1_flag = 1, 
             sub2_flag = 0,
             fedchamb_flag = 0)
  } else {
    # else just bind, resulting in empty tibble
    sub1_q_a_interject <- rbind(sub1_q_interject, sub1_a_interject)
  }
  
  return(list(sub1_q_a, sub1_q_a_interject))
  
}

# ex: call function, get sub-debate 1 interjections
parse_q_and_a("2022_08_01.xml")[[1]]

# ex: call function, get sub-debate 2 interjections
parse_q_and_a("2022_08_01.xml")[[2]]
