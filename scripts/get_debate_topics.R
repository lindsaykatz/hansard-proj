# script to get debate and sub-debate 1 info from Hansard

# read in necessary packages
library(here)
library(tidyverse)
library(xml2)

# define function which allows us to grab all XML node children that have the same name in order
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

# define function to extract debate and subdebate 1 title and page number info
get_debate_topics <- function(filename) {
  
  # parse xml file
  xml_df <- read_xml(here("/Volumes/Verbatim/input/", filename))
  
  # grab sub-debate 1 titles and page numbers, create date variable
  debate_info <- item_df(xml_df, "//debate/debateinfo | //subdebate.1/subdebateinfo") %>% 
           # replace any null values with NA so the next mutate works
    mutate(page.no =  map(page.no, `%||%`, NA),
           # sometimes there are two page numbers, where the second is wrong (i.e. a debate title or time stamp or repeated number), so only take first
           page.no = map_chr(page.no, first)) %>% 
    unnest(title) %>% 
    mutate(date = str_remove(filename, ".xml")) %>% 
    select(date, itemindex, title, page.no)
  
  return(debate_info)
}

# grab list of Hansard XML files to iterate over
files_all <- list.files("/Volumes/Verbatim/input/")

# define empty tibble to store output
all_debate_topics <- tibble()

# define for loop, iterate over all XMLs
for (i in 1:length(files_all)) {
  
  # store get_sub1_topics function output for individual file
  this_file_topics <- get_debate_topics(files_all[i])
  
  # bind rows of above to the bottom of the all_debate_topics data frame
  all_debate_topics <- bind_rows(all_debate_topics, this_file_topics)
  
}

# rename item index variable for readability, and convert page number to double
all_debate_topics <- all_debate_topics %>% rename(item_index = itemindex) %>% mutate(page.no = as.double(page.no))

# export tibble as csv and parquet files
readr::write_csv(all_debate_topics, here("~/Desktop/RA/hansard/additional_data/all_debate_topics.csv"))
arrow::write_parquet(all_debate_topics, here("~/Desktop/RA/hansard/additional_data/all_debate_topics.parquet"))
