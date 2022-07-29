# Script for getting sub-debate 2 info & text
# Note: sub-debate 2 is nested within sub-debate 1

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("hansard-proj/XML_files", "2021_11_30.xml"))

#### CHAMBER ####
# store sub-debate 2 information & text in tibble, correct variable class
subdebate2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = FALSE)

# store sub-debate 2 talker info & speech in tibble, correct variable class
subdebate2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         party = as.factor(party),
         fedchamb_flag = FALSE)

#### FEDERATION CHAMBER ####
# store sub-debate 2 information & text in tibble, correct variable class
subdebate2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                               xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = TRUE)

# store sub-debate 2 talker info & speech in tibble, correct variable class
subdebate2_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                                 xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         party = as.factor(party),
         fedchamb_flag = TRUE)

# merge chamber and federation tibbles together
subdebate2_info <- rbind(subdebate2_info_chamb, subdebate2_info_fed)
subdebate2_speech <- rbind(subdebate2_speech_chamb, subdebate2_speech_fed)