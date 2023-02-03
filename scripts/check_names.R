# code to check for rows where names are missing

library(tidyverse)

files_1998 <- list.files("/Volumes/Verbatim/input/") %>% as_tibble() %>% filter(str_detect(value, "^1999-|^1998-")) %>% pull()
files_2000 <- list.files("/Volumes/Verbatim/input/") %>% as_tibble() %>% filter(str_detect(value, "^2000-|^2001-|^2002-|^2003-|^2004-|^2005-|^2006-|^2007-|^2008-|^2009-|^2010-|^2011-")) %>% filter(value <= "2011-03-24.xml") %>% pull()
files_2011 <- list.files("/Volumes/Verbatim/input/") %>% as_tibble() %>% filter(str_detect(value, "^2011-|^2012-")) %>% filter(value >= "2011-05-10.xml" & value <= "2012-06-28.xml") %>% pull(value)
files_2012 <-  list.files("/Volumes/Verbatim/input/") %>% as_tibble() %>% filter(str_detect(value, "^2012-|^2013-|^2014-|^2015-|^2016-|^2017-|^2018-|^2019-|^2020-|^2021-|^2022-")) %>% filter(value>"2012-06-28.xml") %>% pull(value)

missingNames1998 <- tibble()
missingNames2000 <- tibble()
missingNames2011 <- tibble()
missingNames2012 <- tibble()


# 1998
for (i in 1:length(files_1998)){
  
  thisDate <- str_remove(files_1998[i], "\\.xml$")
  
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-1998-1999/", thisDate, "-main.csv"), show_col_types = F)
  
  missingNames1998 <- thisFile %>% filter(is.na(name)) %>% 
    select(order, body) %>% 
    bind_cols(., thisDate %>% as_tibble()) %>% 
    rename(date = value) %>% 
    bind_rows(., missingNames1998)
  
}

# 2000
for (i in  1:length(files_2000)){
  
  thisDate <- str_remove(files_2000[i], "\\.xml$")
  
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2000-2011/", thisDate, "-main.csv"), show_col_types = F)
  
  missingNames2000 <- thisFile %>% filter(is.na(name)) %>% 
    select(order, body) %>% 
    bind_cols(., thisDate %>% as_tibble()) %>% 
    rename(date = value) %>% 
    bind_rows(., missingNames2000)
  
}

# 2011
for (i in 1:length(files_2011)){
  
  thisDate <- str_remove(files_2011[i], "\\.xml$")
  
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022/", thisDate, "-main.csv"), show_col_types = F)
  
  missingNames2011 <- thisFile %>% filter(is.na(name)) %>% 
    select(order, body) %>% 
    bind_cols(., thisDate %>% as_tibble()) %>% 
    rename(date = value) %>% 
    bind_rows(., missingNames2011)
  
}

# 2012
for (i in 1:length(files_2012)){
  
  thisDate <- str_remove(files_2012[i], "\\.xml$")
  
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022/", thisDate, "-main.csv"), show_col_types = F)
  
  missingNames2012 <- thisFile %>% filter(is.na(name)) %>% 
    select(order, body) %>% 
    bind_cols(., thisDate %>% as_tibble()) %>% 
    rename(date = value) %>% 
    bind_rows(., missingNames2012)
  
}

missingNames <- bind_rows(missingNames1998, missingNames2000, missingNames2011, missingNames2012)

write.csv(missingNames, "/Volumes/Verbatim/output/missingNames.csv", row.names = F)


parse_hansard("2001-03-05.xml"); parse_hansard("2001-02-06.xml");parse_hansard("2001-03-07.xml");parse_hansard("2002-10-23.xml");parse_hansard("2004-12-07.xml");parse_hansard("2005-05-26.xml");parse_hansard("2005-06-16.xml");parse_hansard("2006-10-09.xml");parse_hansard("2007-02-12.xml");parse_hansard("2007-02-26.xml")
dates <- missingNames2000 %>% filter(str_detect(body, "\\d$")) %>%  mutate(date=paste0(date, ".xml")) %>% unique() %>% pull(date)
dates <- missingNames2000 %>% mutate(date=paste0(date, ".xml")) %>% unique() %>% pull(date)


for (i in 1:length(dates)){
  parse_hansard(dates[i])
}
