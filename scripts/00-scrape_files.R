### scraping Hansards
library(heapsofpapers)
library(tidyverse)
library(here)

# 2010-2022
# read in excel sheet with URLs
urls_df <- read.csv(here("hansard-urls-2010-2022.csv"))

# define tibble with URLs and file names
urls <- tibble(locations_are = urls_df$URL, 
                      save_here = urls_df$file_name)

# get and save files
heapsofpapers::get_and_save(data=urls,
                            links = "locations_are",
                            save_names = "save_here",
                            dir = "/Volumes/Verbatim/input",
                            dupe_strategy = "overwrite")

# 2000-2009
# read in excel sheet with URLs
urls_df <- read.csv(here("hansard-urls-2000-2009.csv"))

# two dates are missing XMLs so I left the url blank, filter those out
urls_df <- urls_df %>% filter(URL!="")

# define tibble with URLs and file names
urls <- tibble(locations_are = urls_df$URL, 
               save_here = urls_df$file_name)

# get and save files
heapsofpapers::get_and_save(data=urls,
                            links = "locations_are",
                            save_names = "save_here",
                            dir = "/Volumes/Verbatim/input",
                            dupe_strategy = "overwrite")

# issue with link from 2022 had to fix
urls2 <- tibble(locations_are = "https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25917/toc_unixml/House%20of%20Representatives_2022_07_26_Official.xml;fileType=text%2Fxml",
       save_here = "2022-07-26.xml")
heapsofpapers::get_and_save(data=urls2,
                            links = "locations_are",
                            save_names = "save_here",
                            dir = "/Volumes/Verbatim/input",
                            dupe_strategy = "overwrite")

# issue with link from 2017 had to fix
urls2 <- tibble(locations_are = "https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/7acf60a0-3455-43aa-bfd0-083a6b4252db/toc_unixml/House%20of%20Representatives_2017_06_14_5147_Official.xml;fileType=text%2Fxml",
                save_here = "2017-06-14.xml")
heapsofpapers::get_and_save(data=urls2,
                            links = "locations_are",
                            save_names = "save_here",
                            dir = "/Volumes/Verbatim/input",
                            dupe_strategy = "overwrite")




