# checking the new corpus, that has additional 2022 dates. just want to make sure there are no weird changes happening after re-validating the data

library(tidyverse)

new <- read_csv("/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/hansard_corpus_1998_to_2022.csv", col_types=list(name = col_character(),
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

old <- read_csv("/Volumes/Verbatim/v4/hansard_corpus_1998_to_2022-orig.csv", col_types=list(name = col_character(),
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

new_files <- list.files("/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/") |> as_tibble() |> filter(str_detect(value, "^2022-0(2|3)")) |> pull()

new_rows <- 0

for(i in 1:length(new_files)){
  this <- read_csv(paste0("/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/", new_files[i]))
  
  new_rows <- new_rows + nrow(this)
}

nrow(new)-nrow(old) == new_rows

# check that most of the variables haven't changed for the old data
setdiff(old |> select(date, name, order, speech_no, page.no, time.stamp, in.gov:partyfacts_id), new |> filter(!str_detect(date, "^2022-0(2|3)")) |> select(date, name, order, speech_no, page.no, time.stamp, in.gov:partyfacts_id))
setdiff(new |> filter(!str_detect(date, "^2022-0(2|3)")) |> select(date, name, order, speech_no, page.no,time.stamp, in.gov:partyfacts_id), old |> select(date, name, order, speech_no, page.no, time.stamp, in.gov:partyfacts_id))

# only vars w diffs are electorate, party and name id
setdiff(old |> select(date, name, order, party, name.id, electorate), new |> filter(!str_detect(date, "^2022-0(2|3)")) |> select(date, name, order, party, name.id, electorate))
setdiff(new |> filter(!str_detect(date, "^2022-0(2|3)")) |> select(date, name, order, party, name.id, electorate), old |> select(date, name, order, party, name.id, electorate))

# manually checked that the new ones are correct and they are for all except one row, from 2002-11-11, where Wilkie, Kim was given the wrong name.id (it belongs to Julie Bishop)
# i must not have caught this earlier, so going to fix manually here
fix_2002_11_11<- read_csv(paste0("/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/2002-11-11.csv"))

fix_2002_11_11 <- fix_2002_11_11 |> mutate(name.id = ifelse(name=="Wilkie, Kim, MP", "84G", name.id),
                         party = ifelse(name=="Wilkie, Kim, MP", "ALP", party),
                         electorate = ifelse(name=="Wilkie, Kim, MP", "Swan", electorate))

write_csv(fix_2002_11_11, "/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/2002-11-11.csv")
