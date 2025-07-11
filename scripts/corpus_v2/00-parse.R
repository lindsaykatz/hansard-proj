## preamble -------------------------------------------------------------------
# script purpose: Parse XML Hansard Transcripts
# date: 09 July 2025
# author: Lindsay Katz

## environment setup ----------------------------------------------------------
# read in necessary libraries
library(tidyverse)
library(xml2)
library(XML)

# define file name to import
filename <- "2025-03-27.xml"

# read in current file as XML
hansard_xml <- read_xml(paste0("/Volumes/Verbatim/input-2022_2025/", filename))

## sanity checks to ensure structure is as expected ---------------------------
# ensure there are 3 XML children: session.header, chamber.xscript, fedchamb.xscript
stopifnot(
  xml_path(xml_find_all(hansard_xml, "/hansard/*")) == 
    c("/hansard/session.header", "/hansard/chamber.xscript", 
      "/hansard/fedchamb.xscript"))

# ensure length of business start nodeset is 1 for both chamber and fed. chamber
stopifnot(length(xml_find_all(
  hansard_xml, "./chamber.xscript/business.start/*")) == 1,
  length(xml_find_all(
    hansard_xml, "./fedchamb.xscript/business.start/*")) == 1)

# ensure the nodeset for chamber and federation chamber is structure as
# expected with only a business.start and series of debates, no unexpected nodes
# we would miss in the parsing script
stopifnot(
  xml_path(xml_find_all(hansard_xml, "./chamber.xscript/*")) %>% 
    as_tibble() %>% 
    filter(!str_detect(value, "/hansard/chamber.xscript/(business.start$|debate\\[\\d{1,2}\\]$)")) %>% 
    nrow() == 0,
  xml_path(xml_find_all(hansard_xml, "./fedchamb.xscript/*")) %>% 
    as_tibble() %>% 
    filter(!str_detect(value, "/hansard/fedchamb.xscript/(business.start$|debate\\[\\d{1,2}\\]$)")) %>% 
    nrow() == 0
)

## save dfs with info on general XML structure, for validation ----------------
# create df with number of total interjections, questions, and answers 
# that should be flagged in the final dataset (assuming no errors in hansard)
total_row_counts <- tibble(
  n_interject_chamb = length(xml_path(xml_find_all(hansard_xml, "chamber.xscript//interjection/talk.start"))),
  n_interject_fedchamb = length(xml_path(xml_find_all(hansard_xml, "fedchamb.xscript//interjection/talk.start"))),
  n_question_chamb = length(xml_path(xml_find_all(hansard_xml, "chamber.xscript//question/talk.start"))),
  n_question_fedchamb = length(xml_path(xml_find_all(hansard_xml, "fedchamb.xscript//question/talk.start"))),
  n_answer_chamb = length(xml_path(xml_find_all(hansard_xml, "chamber.xscript//answer/talk.start"))),
  n_answer_fedchamb = length(xml_path(xml_find_all(hansard_xml, "fedchamb.xscript//answer/talk.start"))))

############NOTE: not sure if these numbers for the skeleton are what we should use, since there are duplicate rows
# store skeleton structure of all talk.text that we will use to check everything
# was split correctly in the end
#chamber_all_talkers <- get_child_nodes(hansard_xml, "chamber.xscript//*/talk.start/talker") %>% unnest(everything())
#fedchamb_all_talkers <- get_child_nodes(hansard_xml, "fedchamb.xscript//*/talk.start/talker") %>% unnest(everything()) 

# create tibble with all names of MPs / people who spoke in XML
all_names <- c(
  xml_text(xml_find_all(hansard_xml, ".//span[@class='HPS-MemberInterjecting']")),
  xml_text(xml_find_all(hansard_xml, ".//span[@class='HPS-MemberSpeech']")),
  xml_text(xml_find_all(hansard_xml, ".//span[@class='HPS-MemberContinuation']")),
  xml_text(xml_find_all(hansard_xml, ".//name"))) %>% 
  as_tibble() %>% distinct()

## define function for parsing multiple children with same name ---------------
# code in function below from https://stackoverflow.com/questions/58492429/xml—in—r—multiple—children—with—same—name—without—loops
get_child_nodes <- function(file, path){
  # find all items and store as a list
  items <- xml_find_all(file, path)
  
  # extract all childrens names and values 
  nodenames <- xml_name(xml_children(items))
  contents <- trimws(xml_text(xml_children(items)))
  
  # need to create an index to associate the nodes/contents with each item
  itemindex <- rep(1:length(items), times=sapply(items, function(x) 
    {length(xml_children(x))}))
  
  # store all information in data frame.
  df <- data.frame(itemindex, nodenames, contents)
  
  # Convert from long to wide format
  # sometimes titles are split onto multiple lines, so fix that up too
  df <- pivot_wider(df, id_cols = itemindex, names_from = nodenames, 
                    values_from = contents, values_fn = list) 
  
  return(df)
}

## chamber parsing ------------------------------------------------------------
###### business start ######
bus_start_chamber <- tibble(
  body = xml_text(xml_find_all(hansard_xml,
                               "./chamber.xscript/business.start/."))) %>% 
  mutate(body = str_replace_all(body, "[[:space:]]{2,}", " "),
         date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
         name="Business start",
         time.stamp = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"),
         body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{4}(?=.{1,})"),
         fedchamb_flag = 0) %>% 
  select(date, name, time.stamp, body, fedchamb_flag)

###### talk.text ######
# parse all speeches, questions, answers and interjections 
# left join because we only want to keep the rows where there is talk.text content
chamber_text_full <- left_join(
  get_child_nodes(hansard_xml, "chamber.xscript//*/talk.text") %>% unnest(everything()),
  get_child_nodes(hansard_xml, "chamber.xscript//*/talk.start/talker") %>% unnest(everything()),
  by="itemindex") %>% 
  select(-itemindex) %>% 
  # re-assign itemindex as "speech_no" because some talk.start components do not 
  # have talk.text because they are just forming the "skeleton structure" of the 
  # debate, and we only want to keep rows which actually have text associated
  rowid_to_column(., "speech_no")

# get all talk.text questions and answers and add proper flag
chamber_questions_and_answers <- bind_rows(
  left_join(
    get_child_nodes(hansard_xml, "chamber.xscript//question/talk.text") %>% unnest(everything()),
    get_child_nodes(hansard_xml, "chamber.xscript//question/talk.start/talker") %>% unnest(everything()),
    by="itemindex") %>% 
    select(-itemindex) %>% 
    mutate(question = 1, answer = 0),
  left_join(
    get_child_nodes(hansard_xml, "chamber.xscript//answer/talk.text") %>% unnest(everything()),
    get_child_nodes(hansard_xml, "chamber.xscript//answer/talk.start/talker") %>% unnest(everything()),
    by="itemindex") %>% 
    select(-itemindex) %>% 
    mutate(question = 0, answer = 1))

# add question and answer flags to chamber_text_full
chamber_text_full <- left_join(chamber_text_full, chamber_questions_and_answers,
          by=join_by(body, page.no, time.stamp, name, name.id, electorate, 
                     party, in.gov, first.speech)) %>% 
  mutate(question = ifelse(is.na(question), 0, question),
         answer = ifelse(is.na(answer), 0, answer))

###### split interjections ######
# get list of all interjection text so we can split it out
chamber_interject <- c(
  xml_text(xml_parent(xml_find_all(hansard_xml, "//chamber.xscript//a[@type='MemberInterjecting']"))),
  xml_text(xml_parent(xml_find_all(hansard_xml, "//chamber.xscript//span[@class='HPS-GeneralInterjecting']"))),
  # not sure if this is going to fix the issue for all XMLs, but in the current on I'm looking at, all interjections
  # of the form "__ interjecting—" are stored under the "name" tag so I grabbed them that way
  get_child_nodes(hansard_xml, "//chamber.xscript//interjection/talk.start/talker") %>% 
    unnest(everything()) %>% distinct(name) %>% filter(str_detect(name, "interjecting—$")) %>% pull()) %>% 
  str_escape()

# separate rows on interjections, add flag for interjections
chamber_text <- chamber_text_full %>% 
  separate_rows(body, sep=paste0("(?=", chamber_interject, ")", collapse = "|")) %>% 
  separate_rows(body, sep=paste0("(?<=", chamber_interject, ")", collapse = "|")) %>%
  mutate(interject = ifelse(str_detect(body, paste0(chamber_interject, collapse="|")), 1, 0)) %>% 
  # comments by the speaker or deputy speaker are not flagged as interjections
  mutate(interject = ifelse(interject==1 & str_detect(body, "^(|The|Mister|Mr|Madam|Mrs|Ms)[[:space:]](DEPUTY SPEAKER|SPEAKER)\\:"),
                            0, interject)) %>% 
  # in the separate_rows process, some rows with just whitespace are separated out, drop those
  filter(body!="")

# for rows where the body is equal to one of the chamber interjections,
# remove the name, name.id, etc - those just carried over from the row split
chamber_text <- chamber_text %>% 
  mutate(across(c(name, page.no, name.id, time.stamp, electorate, party),
                ~ ifelse(str_detect(body, paste0(chamber_interject, collapse="|")), 
                         NA, .)))

##### clean up chamber text columns #####
chamber_text <- chamber_text %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
         party = {if("party" %in% names(.)) as.factor(party) else NULL},
         time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL},
         body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                  str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                  body) else NULL},
         body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                  str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                  body) else NULL},
         fedchamb_flag = 0) # add flag for federation chamber 

## federation chamber parsing -------------------------------------------------
### business start ###
bus_start_fedchamb <- tibble(
  body = xml_text(xml_find_all(hansard_xml,
                               "./fedchamb.xscript/business.start/."))) %>% 
  mutate(body = str_replace_all(body, "[[:space:]]{2,}", " "),
         date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
         name="Business start",
         time.stamp = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"),
         body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{4}(?=.{1,})"),
         fedchamb_flag = 1) %>% 
  select(date, name, time.stamp, body, fedchamb_flag)

###### talk.text ######
# parse all speeches, questions, answers and interjections 
# left join because we only want to keep the rows where there is talk.text content
fedchamb_text_full <- left_join(
  get_child_nodes(hansard_xml, "fedchamb.xscript//*/talk.text") %>% unnest(everything()),
  get_child_nodes(hansard_xml, "fedchamb.xscript//*/talk.start/talker") %>% unnest(everything()),
  by="itemindex") %>% 
  select(-itemindex) %>% 
  # re-assign itemindex as "speech_no" because some talk.start components do not 
  # have talk.text because they are just forming the "skeleton structure" of the 
  # debate, and we only want to keep rows which actually have text associated
  rowid_to_column(., "speech_no")

# get all talk.text questions and answers and add proper flag - condition on
# there being at least one question and one answer ##### TO DO - ADD CONDITIONAL HERE WITH CASES FOR 1Q, 1A, 0Q0A, OR BOTH
fedchamb_questions_and_answers <- bind_rows(
  left_join(
    get_child_nodes(hansard_xml, "fedchamb.xscript//question/talk.text") %>% unnest(everything()),
    get_child_nodes(hansard_xml, "fedchamb.xscript//question/talk.start/talker") %>% unnest(everything()),
    by="itemindex") %>% 
    select(-itemindex) %>% 
    mutate(question = 1, answer = 0),
  left_join(
    get_child_nodes(hansard_xml, "fedchamb.xscript//answer/talk.text") %>% unnest(everything()),
    get_child_nodes(hansard_xml, "fedchamb.xscript//answer/talk.start/talker") %>% unnest(everything()),
    by="itemindex") %>% 
    select(-itemindex) %>% 
    mutate(question = 0, answer = 1))

# add question and answer flags to chamber_text_full
fedchamb_text_full <- left_join(fedchamb_text_full, fedchamb_questions_and_answers,
                               by=join_by(body, page.no, time.stamp, name, name.id, electorate, 
                                          party, in.gov, first.speech)) %>% 
  mutate(question = ifelse(is.na(question), 0, question),
         answer = ifelse(is.na(answer), 0, answer))

###### split interjections ######
# get list of all interjection text so we can split it out
fedchamb_interject <- c(
  xml_text(xml_parent(xml_find_all(hansard_xml, "//fedchamb.xscript//a[@type='MemberInterjecting']"))),
  xml_text(xml_parent(xml_find_all(hansard_xml, "//fedchamb.xscript//span[@class='HPS-GeneralInterjecting']"))),
  # not sure if this is going to fix the issue for all XMLs, but in the current on I'm looking at, all interjections
  # of the form "__ interjecting—" are stored under the "name" tag so I grabbed them that way
  get_child_nodes(hansard_xml, "//fedchamb.xscript//interjection/talk.start/talker") %>% 
    unnest(everything()) %>% distinct(name) %>% filter(str_detect(name, "interjecting—$")) %>% pull()) %>% 
  str_escape()

# separate rows on interjections, add flag for interjections
fedchamb_text <- fedchamb_text_full %>% 
  separate_rows(body, sep=paste0("(?=", fedchamb_interject, ")", collapse = "|")) %>% 
  separate_rows(body, sep=paste0("(?<=", fedchamb_interject, ")", collapse = "|")) %>%
  mutate(interject = ifelse(str_detect(body, paste0(fedchamb_interject, collapse="|")), 1, 0)) %>% 
  # comments by the speaker or deputy speaker are not flagged as interjections
  mutate(interject = ifelse(interject==1 & str_detect(body, "^(|The|Mister|Mr|Madam|Mrs|Ms)[[:space:]](DEPUTY SPEAKER|SPEAKER)\\:"),
                            0, interject)) %>% 
  # in the separate_rows process, some rows with just whitespace are separated out, drop those
  filter(body!="")

# for rows where the body is equal to one of the chamber interjections,
# remove the name, name.id, etc - those just carried over from the row split
fedchamb_text <- fedchamb_text %>% 
  mutate(across(c(name, page.no, name.id, time.stamp, electorate, party),
                ~ ifelse(str_detect(body, paste0(fedchamb_interject, collapse="|")), 
                         NA, .)))

##### clean up chamber text columns #####
fedchamb_text <- fedchamb_text %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
         party = {if("party" %in% names(.)) as.factor(party) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL},
         time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL},
         body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                  str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                  body) else NULL},
         body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                  str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                  body) else NULL},
         fedchamb_flag = 1) # add flag for federation chamber 


## combine everything into single df ------------------------------------------
# combine business starts and text dfs for chamber and federation chamber

# select columns in desired order

# split out all stage directions

# add order column




#### TO DOS - div_flag, q_wo_notice, q_in_writing


