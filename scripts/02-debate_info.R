# Script for getting debate information 
# Content includes: business start, title, page number, type, body

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("XML_files", "2021_11_30.xml"))

#### CHAMBER ####
# store business start in tibble, add flag for federation chamber
bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start")), 
                          fedchamb_flag = 0)

# store debate information in tibble, correct variable class
debate_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
# store business start in tibble, add flag for federation chamber
bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")), 
                        fedchamb_flag = 1)

# store debate information in tibble, correct variable class
debate_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 1)

# merge into single business start tibble
bus_start <- rbind(bus_start_chamb, bus_start_fed)

# merge chamber & federation chamber tibbles into single debate information tibble
debate_info <- rbind(debate_info_chamb, debate_info_fed)
