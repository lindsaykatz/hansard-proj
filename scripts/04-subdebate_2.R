# Script for getting sub-debate 2 info & text
# Note: sub-debate 2 is nested within sub-debate 1

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("XML_files", "2021_11_30.xml"))

#### CHAMBER ####
# store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

# store sub-debate 2 talker info & speech in tibble, correct variable class and extract time, add flag for federation chamber
sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
         party = as.factor(party),
         fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
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

# define interjection words
interject_words <- c("Order!", "Order.", "interjecting", "Interjecting", "interjected", "Interjected", "interjections", "interjection", "Interjections", 
                     "Interjection", "The time for the discussion has concluded", "I thank the honourable member for", "I thank the honourable member for", "should withdraw that remark", 
                     "In accordance with standing order 193 the time for constituency statements has concluded", "There being no further grievances, the debate is adjourned",
                     "the time for members' statements has concluded", "The original question was that this bill be now read a second time", "Is the amendment seconded?",
                     "Do you want to seek the call again?", "The question is that the amendment be disagreed to", "The question now is that the bill be agreed to", ", on a point of order?") 

# merge chamber and federation tibbles together, arrange by page number
sub2_info <- rbind(sub2_info_chamb, sub2_info_fed) %>% 
  arrange(page.no)

# merge chamber and federation tibbles together, flag for interjections, arrange by page number and time stamp
sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_fed) %>% 
  arrange(page.no, time.stamp) %>% 
  mutate(has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))
