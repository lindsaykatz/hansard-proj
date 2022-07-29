# Script for getting sub-debate 1 info & text

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("hansard-proj/XML_files", "2021_11_30.xml"))

#### CHAMBER ####
# store sub-debate 1 information & text in tibble, correct variable class, add flag
subdebate1_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebateinfo")),
                               xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.text"))) %>%
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = FALSE)

# store sub-debate 1 talker info & speech in tibble, correct variable class, add flag
subdebate1_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         party = as.factor(party),
         fedchamb_flag = FALSE)

#### FEDERATION CHAMBER ####
# store sub-debate 1 information & text in tibble, correct variable class, add flag
subdebate1_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebateinfo")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.text"))) %>%
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = TRUE)

# store sub-debate 1 talker info & speech in tibble, correct variable class, add flag
subdebate1_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         party = as.factor(party),
         fedchamb_flag = TRUE)

# merge chamber and federation tibbles together
subdebate1_info <- rbind(subdebate1_info_chamb, subdebate1_info_fed)
subdebate1_speech <- rbind(subdebate1_speech_chamb, subdebate1_speech_fed)