# Script to get interjection info

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("hansard-proj/XML_files", "2021_11_30.xml"))

#### CHAMBER ####
# sub-debate 1 interjections
interject_chamb_sub1 <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         fedchamb_flag = FALSE)

# sub-debate 2 interjections
interject_chamb_sub2 <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         fedchamb_flag = FALSE)

# sub-debate 1 answer interjections (?)
interject_chamb_answer <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.start/talker")) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         fedchamb_flag = FALSE)

#### FEDERATION CHAMBER ####
# sub-debate 1 interjections
interject_fed_sub1 <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         fedchamb_flag = TRUE)

# sub-debate 2 interjections
interject_fed_sub2 <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = as.numeric(time.stamp),
         fedchamb_flag = TRUE)

# merge chamber and federation chamber tibbles
interject_sub1 <- rbind(interject_chamb_sub1, interject_fed_sub1)
interject_sub2 <- rbind(interject_chamb_sub2, interject_fed_sub2)

## note - I got a total of 113 interjections but the XML file has 119. Tried looking through but couldn't find which I missed.
# need to look into type="MemberInterjecting"