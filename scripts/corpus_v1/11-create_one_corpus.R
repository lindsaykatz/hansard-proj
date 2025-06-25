### script to create a single corpus with our entire database

# read in packages
library(tidyverse)
library(arrow)
library(fs)

# create list of all the CSV files
all_files_csv <- dir_ls(path = "/Volumes/Verbatim/v4/hansard-daily-csv/", regexp= "\\d{4}-\\d{2}-\\d{2}\\.csv")

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
                                     interject = col_factor(),
                                     partyfacts_id = col_double()), .id = "date") %>% 
  mutate(date = str_extract(date, "(?<=hansard-daily-csv/)\\d{4}-\\d{2}-\\d{2}"))

old <- read_csv("/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.csv",
                col_types = list(date=col_character(),
                                 name = col_character(),
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
                                 interject = col_factor(),
                                 partyfacts_id = col_double()))

# compare to old corpus to see if any difference arise that are cause for concern
setdiff(old |> select(-partyfacts_id, -party), full_hansard |> select(-partyfacts_id, -party))
setdiff(full_hansard |> select(-partyfacts_id, -party), old |> select(-partyfacts_id, -party))
# all good now, re-export

# export full_hansard data frame to CSV
write_csv(full_hansard, "/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.csv")

# export full_hansard data frame to Parquet
write_parquet(full_hansard, "/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.parquet")

# check file sizes
file_size("/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.csv")
file_size("/Volumes/Verbatim/v4/hansard-corpus/hansard_corpus_1998_to_2022.parquet")









#### OLD CODE - MANUAL FIXES ####

# detected 4 where the MP has the wrong electorate or name ID - fix those manually here
to_fix <- c("2002-08-28.csv", "2002-11-11.csv", "2003-03-06", "2008-02-20")

#### 2002-08-28 ####
hansard_2002_08_28 <- read_csv("/Volumes/Verbatim/v4/hansard-daily-csv/2002-08-28.csv", 
                               col_types = list(name = col_character(),
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
                                                interject = col_factor(),
                                                partyfacts_id = col_double()))

hansard_2002_08_28 <- hansard_2002_08_28 |> mutate(name.id = ifelse(name=="Griffin, Alan, MP", "VU5", name.id))
write_csv(hansard_2002_08_28, "/Volumes/Verbatim/v4/hansard-daily-csv/2002-08-28.csv")
write_parquet(hansard_2002_08_28, "/Volumes/Verbatim/v4/hansard-daily-parquet/2002-08-28.parquet")

#### 2002-11-11 ####
hansard_2002_11_11 <- read_csv("/Volumes/Verbatim/v4/hansard-daily-csv/2002-11-11.csv", 
                               col_types = list(name = col_character(),
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
                                                interject = col_factor(),
                                                partyfacts_id = col_double()))

hansard_2002_11_11 <- hansard_2002_11_11 |> mutate(name.id = ifelse(name=="Wilkie, Kim, MP", "84G", name.id))
write_csv(hansard_2002_11_11, "/Volumes/Verbatim/v4/hansard-daily-csv/2002-11-11.csv")
write_parquet(hansard_2002_11_11, "/Volumes/Verbatim/v4/hansard-daily-parquet/2002-11-11.parquet")

#### 2003-03-06 ####
hansard_2003_03_06 <- read_csv("/Volumes/Verbatim/v4/hansard-daily-csv/2003-03-06.csv", 
                               col_types = list(name = col_character(),
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
                                                interject = col_factor(),
                                                partyfacts_id = col_double()))

hansard_2003_03_06 <- hansard_2003_03_06 |> mutate(electorate = ifelse(name=="Swan, Wayne, MP", "Lilley", electorate))
write_csv(hansard_2003_03_06, "/Volumes/Verbatim/v4/hansard-daily-csv/2003-03-06.csv")
write_parquet(hansard_2003_03_06, "/Volumes/Verbatim/v4/hansard-daily-parquet/2003-03-06.parquet")

#### 2008-02-20 ####
hansard_2008_02_20 <- read_csv("/Volumes/Verbatim/v4/hansard-daily-csv/2008-02-20.csv", 
                               col_types = list(name = col_character(),
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
                                                interject = col_factor(),
                                                partyfacts_id = col_double()))

hansard_2008_02_20 <- hansard_2008_02_20 |> mutate(name.id = ifelse(name=="Smith, Anthony, MP", "00APG", name.id))
write_csv(hansard_2008_02_20, "/Volumes/Verbatim/v4/hansard-daily-csv/2008-02-20.csv")
write_parquet(hansard_2008_02_20, "/Volumes/Verbatim/v4/hansard-daily-parquet/2008-02-20.parquet")
