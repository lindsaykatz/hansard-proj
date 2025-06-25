# get divisions data
library(here)
library(tidyverse)
library(xml2)
library(XML)

# code in function below from https://stackoverflow.com/questions/58492429/xml—in—r—multiple—children—with—same—name—without—loops
item_df <- function(file, path){
  # find all items and store as a list
  items <- xml_find_all(file, path)
  
  # extract all childrens names and values 
  nodenames <- xml_name(xml_children(items))
  contents <- trimws(xml_text(xml_children(items)))
  
  # need to create an index to associate the nodes/contents with each item
  itemindex <- rep(1:length(items), times=sapply(items, function(x) {length(xml_children(x))}))
  
  # store all information in data frame.
  df <- data.frame(itemindex, nodenames, contents)
  
  # Convert from long to wide format
  # sometimes titles are split onto multiple lines, so fix that up too
  df <- pivot_wider(df, id_cols = itemindex, names_from = nodenames, values_from = contents, values_fn = list) 
  
  return(df)
}

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/input/")

# define empty tibble to store data in
division_data <- tibble()

for (i in 1:length(files_all)) {
  # parse file
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", files_all[i]))
  
  # need to read the XML in in a different way for this so we can use the item_df function defined in global environment
  xml_df <- read_xml(here("/Volumes/Verbatim/input/", files_all[i]))
  
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript//division"), collectNames = F, homogeneous = T))>0){
    
    # grab header
    if (nrow(item_df(xml_df, "chamber.xscript//division/division.header"))>0 & "para" %in% names(item_df(xml_df, "chamber.xscript//division/division.header"))) {
      header <- item_df(xml_df, "chamber.xscript//division/division.header") %>% unnest(everything()) %>% 
        select(-para)
    } else if (nrow(item_df(xml_df, "chamber.xscript//division/division.header"))>0 & "body" %in% names(item_df(xml_df, "chamber.xscript//division/division.header"))) {
      header <- item_df(xml_df, "chamber.xscript//division/division.header") %>% unnest(everything()) %>% 
        mutate(time.stamp = str_extract(body, "\\d\\d:\\d\\d")) %>% 
        select(-body)
    } else {
      header <- tibble()
    }

    
    # grab result
    if (nrow(item_df(xml_df, "chamber.xscript//division/division.result"))>0 & "para" %in% names(item_df(xml_df, "chamber.xscript//division/division.result"))){
      result <- item_df(xml_df, "chamber.xscript//division/division.result") %>% unnest(everything()) %>% 
        rename(result = para)
    } else if (nrow(item_df(xml_df, "chamber.xscript//division/division.result"))>0 & "body" %in% names(item_df(xml_df, "chamber.xscript//division/division.result"))) {
      result <- item_df(xml_df, "chamber.xscript//division/division.result") %>% unnest(everything()) %>% 
        rename(result = body)
    } else {
      result <- tibble()
    }

    # ayes
    ayes <- item_df(xml_df, "chamber.xscript//division/division.data/ayes") %>% unnest(everything()) %>% 
      select(-names) %>% 
      mutate(title = ifelse(title!="AYES", "AYES", title)) %>% 
      left_join(., item_df(xml_df, "chamber.xscript//division/division.data/ayes/names") %>% 
                  rename(names = name), by="itemindex")
    
    # noes
    noes <- item_df(xml_df, "chamber.xscript//division/division.data/noes") %>% unnest(everything()) %>% 
      select(-names) %>% 
      mutate(title = ifelse(title!="NOES", "NOES", title)) %>% 
      left_join(., item_df(xml_df, "chamber.xscript//division/division.data/noes/names") %>% 
                  rename(names = name), by="itemindex")
    
    # pairs
    if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript//division/division.data/pairs"), collectNames = F, homogeneous = T))>0){
      if (nrow(item_df(xml_df, "chamber.xscript//division/division.data/pairs/names"))>0){
        pairs <- item_df(xml_df, "chamber.xscript//division/division.data/pairs") %>% unnest(everything()) %>% 
          select(-names) %>% 
          mutate(title = ifelse(title!="PAIRS", "PAIRS", title)) %>% 
          left_join(., item_df(xml_df, "chamber.xscript//division/division.data/pairs/names") %>% 
                      rename(names = name), by="itemindex")
      } else {
        pairs <- item_df(xml_df, "chamber.xscript//division/division.data/pairs") %>% unnest(everything()) %>% 
          select(-names) %>% 
          mutate(title = ifelse(title!="PAIRS", "PAIRS", title),
                 names = NA)
      }
      
    } else {
      pairs <- tibble()
    }
    
    
    # put it all together, clean up, append date
    if (("itemindex" %in% names(header)) & ("itemindex" %in% names(result))){
      division_data <- bind_rows(ayes, noes, pairs) %>% arrange(itemindex) %>% 
        left_join(header, ., by="itemindex", multiple='all') %>% 
        pivot_wider(names_from = title, values_from = c(num.votes, names)) %>% 
        left_join(., result, by="itemindex", multiple='all') %>% 
        rename(div_num = itemindex) %>% 
        mutate(date = str_remove(files_all[i], ".xml")) %>% 
        select(date, everything()) %>% 
        bind_rows(division_data, .)
    } else {
      division_data <- bind_rows(ayes, noes, pairs) %>% arrange(itemindex) %>% 
        mutate(time.stamp = NA) %>% 
        pivot_wider(names_from = title, values_from = c(num.votes, names)) %>% 
        mutate(result = NA) %>% 
        rename(div_num = itemindex) %>% 
        mutate(date = str_remove(files_all[i], ".xml")) %>% 
        select(date, everything()) %>% 
        bind_rows(division_data, .)
    }

    
    # make sure number of columns hasn't changed (due to transcription variation stuff which we want to catch)
    stopifnot(ncol(division_data) == 10)
  }
}

# save data
save(division_data, file="additional_data/division_data.rda")
readr::write_csv(division_data, "/Volumes/Verbatim/v4/hansard-supplementary-data/division_data.csv")
arrow::write_parquet(division_data, "/Volumes/Verbatim/v4/hansard-supplementary-data/division_data.parquet")

# check annual division numbers against official stats
#load("additional_data/division_data.rda")

# # read in official stats data
# divs_official_stats <- read_delim("~/Desktop/RA/div_stats.txt", delim = ",") |> 
#   rename(n_divs_official = n_divs)
# 
# # merge and compare
# division_data |> mutate(year = str_extract(date, "^\\d{4}"),
#                         year = as.numeric(year)) |> 
#   group_by(year) |> 
#   summarise(n_divs = n()) |> 
#   left_join(divs_official_stats, by="year") |> 
#   filter(year!="2022") |> 
#   filter(n_divs!=n_divs_official) |> 
#   mutate(diff = n_divs_official - n_divs,
#          relative_diff = 100*diff/n_divs)
