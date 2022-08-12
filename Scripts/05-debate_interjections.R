# Script to get speech interjection info

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("XML_files", "2021_11_30.xml"))

#### CHAMBER ####
# sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_chamb_sub1 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

# sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_chamb_sub2 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
# sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_fed_sub1 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 1)

# sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_fed_sub2 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 1)

# merge chamber and federation chamber tibbles, arrange by page number
interject_sub1 <- rbind(interject_chamb_sub1, interject_fed_sub1) %>% arrange(page.no)
interject_sub2 <- rbind(interject_chamb_sub2, interject_fed_sub2) %>% arrange(page.no)