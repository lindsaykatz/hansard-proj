### script to create a single corpus with our entire database

# read in packages
library(tidyverse)
library(arrow)
library(fs)

# set working directory to where the database is stored
# setwd("/Volumes/Verbatim/v4")

# create list of all the CSV files
all_files_csv <- dir_ls(path = "hansard_1998_to_2022-csv/")

read_csv("hansard_1998_to_2022-csv/1998-03-02.csv")

# read in and combine all CSVs, and extract just the date from each file name
full_hansard <- all_files_csv %>% 
  map_dfr(read_csv, col_types = list(name = col_character(),
                                     order = col_double(),
                                     speech_no = col_double(),
                                     page.no = col_double(),
                                     time.stamp = col_character(),
                                     name.id = col_character(),
                                     electorate = col_character(),
                                     party = col_factor(),
                                     in.gov = col_double(),
                                     first.speech = col_double(),
                                     body = col_character(),
                                     fedchamb_flag = col_factor(),
                                     question = col_factor(),
                                     answer = col_factor(),
                                     q_in_writing = col_factor(),
                                     div_flag = col_factor(),
                                     gender = col_factor(),
                                     uniqueID = col_character(),
                                     interject = col_factor()), .id = "date") %>% 
  mutate(date = str_extract(date, "(?<=hansard_1998_to_2022-csv/)\\d{4}-\\d{2}-\\d{2}"))

# export full_hansard data frame to CSV
write_csv(full_hansard, "hansard_corpus_1998_to_2022.csv")

# export full_hansard data frame to Parquet
write_parquet(full_hansard, "hansard_corpus_1998_to_2022.parquet")

# check file sizes
file_size("hansard_corpus_1998_to_2022.csv")
file_size("hansard_corpus_1998_to_2022.parquet")
