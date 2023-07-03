### script to create a single corpus with our entire database

# read in packages
library(tidyverse)
library(arrow)
library(fs)

# create list of all the CSV files
all_files_csv <- dir_ls(path = "/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/", regexp= "\\d{4}-\\d{2}-\\d{2}\\.csv")

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
write_csv(full_hansard, "/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.csv")

# export full_hansard data frame to Parquet
write_parquet(full_hansard, "/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.parquet")

# check file sizes
file_size("/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.csv")
file_size("/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.parquet")
