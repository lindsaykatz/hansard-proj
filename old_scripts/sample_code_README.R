### sample code for readme ###

# dependencies
library(tidyverse)
library(arrow)

### how to read in a single sitting day file ###

# set working directory to wherever you've downloaded the Hansard data
setwd("/Volumes/Verbatim/v3/")

# csv
hansard_csv <- readr::read_csv("hansard_1998_to_2022-csv/2000-06-05.csv", 
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

# parquet
hansard_parquet <- arrow::read_parquet("hansard_1998_to_2022-parquet/2000-06-05.parquet")

### use the corpus, filter for dates of interest, and split days into individual tibbles ###
hansard_corpus <- arrow::read_parquet("hansard_corpus_1998_to_2022.parquet")

# filter for dates of interest, and split each date into list of separate tibbles
hansard_1990s <- hansard_corpus %>% 
  filter(str_detect(date, "^199")) %>% 
  group_split(date)

### how to filter out stage directions ###

# filter out rows where name includes "stage direction", and drop original order variable and create new one
hansard_csv %>% 
  filter(!str_detect(name, "stage direction")) %>% 
  select(-order) %>% 
  rowid_to_column("order")

### how to merge debate topics with data

# read in Hansard file
hansard_parquet <- arrow::read_parquet("hansard_1998_to_2022-parquet/2000-06-05.parquet")

# read in topics file (can do this for CSV as well)
topics <- arrow::read_parquet("all_debate_topics.parquet")

# filter for date correspoding with your Hansard file of choice
# group by page number and summarise into list
# join with Hansard dataframe by page number, allowing topic row to match multiple of Hansard rows (as multiple Hansard rows will have the same page number)
topics |> filter(date=="2000-06-05") |> 
  group_by(page.no) |> 
  # we do this so the number of rows in the Hansard df doesn't change since multiple titles have the same page number,
  # and there's no simple way of knowing which row belongs to which debate title
  summarise(title = list(title)) |> 
  ungroup() |> 
  right_join(hansard_parquet, by = "page.no", multiple = "all")

### ADD DISCLAIMER THAT THESE MIGHT BE WRONG DUE TO TRANSCRIPTION ERRORS AND WE'VE COMBINED THEM INTO LISTS FOR IDENTICAL PAGE NUMBERS, 
### BUT IT'S HARD TO KNOW EXACTLY WHICH ROW BELONGS TO EXACTLY WHICH DEBATE TITLE