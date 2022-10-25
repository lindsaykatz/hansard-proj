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




