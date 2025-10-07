### preamble ------------------------------------------------------------------
# script purpose: fix question and answer flagging

### environment setup ---------------------------------------------------------
# import necessary libraries
library(arrow)
library(tidyverse)
library(xml2)
library(XML)

### data import ---------------------------------------------------------------
# read in corrected corpus exported in previous script
corpus <- read_parquet("~/Documents/RA/hansard/hansard-corpus/corpus_1998_to_2025-v031025.parquet")

### define function for parsing multiple child nodes --------------------------
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

### get new q/a flags ---------------------------------------------------------
# store list of filenames to iterate over
all_filenames <- c(list.files("/Volumes/Verbatim/input/input-2022_2025",
                              full.names = T),
                   list.files("/Volumes/Verbatim/input/input-2011_2022",
                              full.names = T))

# define empty tibbles to store corrected flags
corrected_q_and_a_flags <- tibble()

# loop through filenames
for (i in 1:length(all_filenames)) {

  # extract date from filepath for filtering later on
  this_date <- str_extract(all_filenames[[i]], "\\d{4}-\\d{2}-\\d{2}(?=\\.xml)")

  # read in XML
  xml_df <- read_xml(all_filenames[[i]])

  # if there are no questions or answers, skip this date
  if (is.na(xml_find_first(xml_df, "chamber.xscript//question/talk.text")) &
      is.na(xml_find_first(xml_df, "chamber.xscript//answer/talk.text"))) {
    next
  } else if (this_date=="2018-10-22") {
    # on this day there is only one question nested as such stating question time
    # will be tomorrow, and there is no answer, so q/a should all be 0

    next

  } else {
    # get question and answer content, add flags
    all_q_and_a <- bind_rows(
      item_df(xml_df, "chamber.xscript//question/talk.text") %>% unnest(body) %>%
        mutate(question=1, answer=0),
      item_df(xml_df, "chamber.xscript//answer/talk.text") %>% unnest(body) %>%
        mutate(question=0, answer=1))
  }


  # specific cases where the first question/answer node found is an answer that
  # is actually just part of a speech and shouldn't be flagged as an answer ---- ASK ROHAN ABOUT THIS
  if (this_date %in% c("2020-10-19", "2014-05-26", "2012-08-15", "2013-06-26",
                       "2013-11-13", "2016-09-13", "2018-03-26", "2023-10-19")) {

    # manually fix incorrectly flagged answer
    all_q_and_a <- all_q_and_a %>%
      mutate(answer = ifelse(itemindex==1 & answer==1, 0, answer))

    # this date has the first two as wrongly flagged, they are statements by
    # the deputy speakers and shouldn't be flagged as answers - manually fix
  } else if (this_date=="2017-09-13"){

    all_q_and_a <- all_q_and_a %>%
      mutate(answer = ifelse(itemindex %in% c(1,2) & answer==1, 0, answer)) %>%
      mutate(body = ifelse(itemindex %in% c(1,2),
                           str_replace(body, "\\n                    ", " "),
                           body))
  }

  # collapse into one string we can use for string detection
  if (nrow(all_q_and_a)>0){
    all_q_and_a <- all_q_and_a %>%
      select(-itemindex) %>%
      mutate(body = str_remove(body, "^.*\\(\\d{2}:\\d{2}\\):"),
             body = str_squish(body),
             body = str_sub(body, end=50),
             body = str_escape(body)) %>%
      distinct(body, question, answer) %>%
      group_by(question, answer) %>%
      summarise(body = paste0(body, collapse = "|")) %>%
      ungroup()
  }

 # create new flag and store in tibble
 corrected_q_and_a_flags <- corpus %>%
      filter(date==this_date, q_in_writing==0) %>%
      select(date, speech_no, order, body, question, answer) %>%
      mutate(
        question_new = case_when(
          str_detect(body, all_q_and_a$body[all_q_and_a$question==1]) ~ 1,
          .default = 0),
        answer_new = case_when(
          str_detect(body, all_q_and_a$body[all_q_and_a$answer==1]) ~ 1,
          .default = 0)) %>%
      select(date, speech_no, order, body, question, question_new, answer, answer_new) %>%
      filter(!(question==0 & answer==0 & question_new==0 & answer_new==0)) %>%
      bind_rows(., corrected_q_and_a_flags)
}

corrected_q_and_a_flags <- corrected_q_and_a_flags %>%
  filter(question!=question_new | answer!=answer_new)

corrected_q_and_a_flags %>% filter(question!=question_new) %>% 
  select(-answer, -answer_new) %>% filter(question==1, question_new==0)
# 
# corrected_q_and_a_flags %>% 
#   filter(str_detect(body,"I thank the member|My question is"))

################### QUESTIONS IN WRITING ###################
# identify all sitting days with q in writing, filter for q_in_writing==1 so we
# can manually inspect flagging

all_filenames <- c(list.files("/Volumes/Verbatim/input/input-2022_2025",
                              full.names = T),
                   list.files("/Volumes/Verbatim/input/input-2011_2022",
                              full.names = T),
                   list.files("/Volumes/Verbatim/input/input-1998_2011",
                              full.names = T))

dates_with_q_in_writing <- c()

#corpus %>% distinct(date, q_in_writing) %>% filter(q_in_writing==1) %>% slice(1)

for (i in 1:length(all_filenames)) {
  
  # extract date from filepath for filtering later on
  this_date <- str_extract(all_filenames[[i]], "\\d{4}-\\d{2}-\\d{2}(?=\\.xml)")
  
  # read in XML
  xml_df <- read_xml(all_filenames[[i]])
  
  if (is.na(xml_find_first(xml_df, "answers.to.questions//question")) &
      is.na(xml_find_first(xml_df, "answers.to.questions//answer"))) {
    next
  } else {
    dates_with_q_in_writing <- c(this_date, dates_with_q_in_writing)
  }
  
}

### FIXES ---------------------------------------------------------------------
# 1: identify cases where Q is not followed by an A and vice versa, manually 
#    check
invalid <- corpus %>%
  filter(q_in_writing==1) %>% 
  select(date, order, question, answer) %>% 
  mutate(type = case_when(
    question == 1 & answer == 0 ~ "Q",
    question == 0 & answer == 1 ~ "A",
    .default = "invalid"
  ),
  next_type = lead(type)) %>% 
  select(-question, -answer) %>% 
  filter(type=="invalid") %>%
  bind_rows(corpus %>%
              filter(q_in_writing==1) %>% 
              select(date, order, question, answer) %>% 
              mutate(role = case_when(
                question == 1 & answer == 0 ~ "Q",
                question == 0 & answer == 1 ~ "A",
                .default = "invalid"
              ),
              next_role = lead(role)) %>% 
              select(-question, -answer) %>% 
              filter(
                (role == "Q" & next_role != "A") |
                  (role == "A" & !is.na(next_role) & next_role != "Q"))) %>% 
  group_by(date) %>% 
  summarise(rows = paste0(order, collapse = ", ")) %>% 
  arrange(date)

invalid

# -----------------------------------------------------------------------------
# 2: identify any dates where there is q in writing content but no rows are
#    flagged as such: only one... 2019-04-02
corpus %>% filter(date %in% dates_with_q_in_writing) %>% 
  group_by(date) %>% filter(!any(q_in_writing==1)) %>% ungroup() %>% 
  distinct(date)

# -----------------------------------------------------------------------------
# 3: flag any rows starting with "My questions is to" that have question==0 as 
#    question==1


# -----------------------------------------------------------------------------