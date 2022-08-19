# Script to get questions and answers
# Only found in sub-debate 1, in Chamber

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("XML_files", "2021_11_30.xml"))

# define interjection words
interject_words <- c("Order!", "Order.", "interjecting", "Interjecting", "interjected", "Interjected", "interjections", "interjection", "Interjections", 
                     "Interjection", "interject", "Interject", "The time for the discussion has concluded", "I thank the honourable member for", "I thank the member for", "should withdraw that remark", 
                     "In accordance with standing order 193 the time for constituency statements has concluded", "There being no further grievances, the debate is adjourned",
                     "the time for members' statements has concluded", "The original question was that this bill be now read a second time", "Is the amendment seconded?",
                     "Do you want to seek the call again?", "The question is that the amendment be disagreed to", "The question now is that the bill be agreed to", "will resume his seat", "will resume her seat",
                     "will resume their seat") 

#### QUESTION AND ANSWER INFO AND TEXT ####
# store info and text for questions in tibble, correct variable class, add flags for question and federation chamber, extract time
sub1_q <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker")),
            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         fedchamb_flag = 0,
         question = 1,
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"))

# store info and text for answers in tibble, correct variable class, add flags for question and federation chamber, extract time
sub1_a <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.start/talker")),
            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         fedchamb_flag = 0,
         question = 0,
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"))

# merge questions and answers, arrange by time stap, flag for interjections
sub1_q_a <- rbind(sub1_q, sub1_a) %>% 
  arrange(time.stamp) %>% 
  mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))

#### QUESTION AND ANSWER INTERJECTIONS ####
# store question interjections in tibble, correct variable class, add flags for question and federation chamber
sub1_q_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.start/talker")),
                      xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         fedchamb_flag = 0,
         question = 1)

# store answer interjections in tibble, correct variable class, add flags for question and federation chamber
sub1_a_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.start/talker")),
                      xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         fedchamb_flag = 0,
         question = 0)

# merge question and answer interjections, arrange by page number
sub1_q_a_interject <- rbind(sub1_q_interject, sub1_a_interject) %>% arrange(page.no)
