# Script for getting debate information (business start, title, page number, type, body)

library(XML)
library(here)
library(tidyverse)

# parse XML
hansard_xml <- xmlParse(here("hansard-proj/XML_files", "2021_11_30.xml"))

# chamber business start, add flag for whether or not federation chamber
bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start")), 
                          fedchamb_flag = FALSE)

# federation chamber business Start, add flag for whether or not federation chamber
bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")), 
                        fedchamb_flag = TRUE)

# merge into single business start tibble
bus_start <- rbind(bus_start_chamb, bus_start_fed)

#### CHAMBER ####
# store debate information in tibble, correct variable class
debate_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debateinfo")),
                     xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = FALSE)

#### FEDERATION CHAMBER ####
# store debate information in tibble, correct variable class
debate_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = TRUE)

# merge chamber & federation chamber tibbles into single debate information tibble
debate_info <- rbind(debate_info_chamb, debate_info_fed)
