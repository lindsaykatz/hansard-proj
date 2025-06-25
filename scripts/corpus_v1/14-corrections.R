### preamble ------------------------------------------------------------------
# script purpose: fix name / ID / gender inconsistencies in Hansard corpus, and
#                 implement any additional fixes as needed.
# date of last update: 24 June 2025

### environment setup ---------------------------------------------------------
# import necessary libraries
library(tidyverse)
library(arrow)

# import corpus
corpus <- read_parquet("hansard_corpus_1998_to_2022.parquet")

### prepare lookup table for data cleaning ------------------------------------
# import Australian Politicians lookup tables
auspol_all <- AustralianPoliticians::get_auspol('all')

# import the ausPH/AusPol mapping table I made
lookup <- readxl::read_xlsx("additional_data/lookup_tables/ausPH_AusPol_mapping.xlsx")

# combine "auspol_all" and "lookup" into one
lookup_full <- left_join(lookup, auspol_all, join_by(uniqueID, surname, 
                                                     firstName, displayName, 
                                                     gender, deathDate)) %>% 
  select(uniqueID:deathDate, member, senator)

# ensure that every row with a uniqueID has a value for member and senator to
# verify the merge was successful
stopifnot(
  lookup_full %>% 
    filter(!is.na(uniqueID) & (is.na(member) | is.na(senator))) %>% 
    nrow() == 0
)

# remove individual lookup tables from environment to clear space
rm(auspol_all, lookup)

### identify issues -----------------------------------------------------------
# create and export dataframe with names/unique IDs to standardize/check
# corpus %>%
#   distinct(name, uniqueID) %>%
#   arrange(uniqueID) %>%
#   write_csv("names_to_fix.csv")

### manual fixes --------------------------------------------------------------
# single date name fixes identified in manual assessment
corpus <- corpus %>% 
  mutate(body = case_when(date=="2003-11-06" & order==35 ~ paste0(body, " Ms J.I. Bishop."),
                          date=="2004-02-16" & order==196 ~ paste0(body, " Mr Bevis."),
                          date=="2004-05-13" & order==292 ~ paste0(body, " Mr Zahra."),
                          date=="2004-12-07" & order==190 ~ paste0(body, " Mr Gibbons."),
                          .default = body)) %>% 
  mutate(name = case_when(date=="2003-11-06" & name=="Ms J.I. Bishop.Mr PYNE" ~ "Pyne, Christopher", 
                          date=="2004-02-16" & name=="Mr Bevis.Miss JACKIE KELLY" ~ "Kelly, Jackie", 
                          date=="2004-05-13" & name=="Mr Zahra.Miss JACKIE KELLY" ~ "Kelly, Jackie",
                          date=="2004-12-07" & name=="Mr Gibbons.Ms GAMBARO" ~ "Gambaro, Teresa",
                          date=="1999-12-06" & name=="Mr Wilkie" ~ "Wilkie, Kim",
                          date!="1999-12-06" & name=="Mr Wilkie" ~ "Wilkie, Andrew",
                          .default = name))

# issues where rows need to be condensed - generally due to someones name being
# mentioned in an MPs speech, and that being incorrectly separated out onto
# it's own row. these are done one date at a time for computational efficiency,
# and I free up unused memory after each one because the memory kept jumping up
# a lot while running these fixes and it became very slow.

# 2011-10-12
corpus <- corpus %>% 
  mutate(body = case_when(date=="2011-10-12" & order==306 ~ paste(body, "Mr TONY SMITH:",
                                                                  body[order==307 & date=="2011-10-12"],
                                                                  "Mr McCauley:",
                                                                  body[order==308 & date=="2011-10-12"],
                                                                  "Mr TONY SMITH:",
                                                                  body[order==309 & date=="2011-10-12"],
                                                                  "Mr McCauley:",
                                                                  body[order==310 & date=="2011-10-12"]),
                          .default = body)) %>% 
  filter(!(date=="2011-10-12" & order %in% c(307,308,309,310)))

gc()

# 2012-03-14
corpus <- corpus %>% 
  mutate(body = case_when(date=="2012-03-14" & order==425 ~ paste(body, "Mr Ciobo:",
                                                                  body[order==426 & date=="2012-03-14"],
                                                                  "Mr Anning:",
                                                                  body[order==427 & date=="2012-03-14"],
                                                                  "Mr Ciobo:",
                                                                  body[order==428 & date=="2012-03-14"],
                                                                  "Mr Anning:",
                                                                  body[order==429 & date=="2012-03-14"]),
                          .default = body)) %>% 
  filter(!(date=="2012-03-14" & order %in% c(426,427,428,429)))

gc()

# 2012-03-20
corpus <- corpus %>% 
  mutate(body = case_when(date=="2012-03-20" & order==278 ~ paste(body, "Mrs BRONWYN BISHOP:",
                                                                  body[order==279 & date=="2012-03-20"],
                                                                  "Mr Killesteyn:",
                                                                  body[order==280 & date=="2012-03-20"]),
                          .default = body)) %>% 
  filter(!(date=="2012-03-20" & order %in% c(279,280)))

gc()

# 2012-03-21
corpus <- corpus %>% 
  mutate(body = case_when(date=="2012-03-21" & order==432 ~ paste(body, "Mr McNamara:",
                                                                  body[order==433 & date=="2012-03-21"],
                                                                  "Mr McNamara:",
                                                                  body[order==434 & date=="2012-03-21"]),
                          date=="2012-03-21" & order==492 ~ paste(body, "Mr McNamara:",
                                                                  body[order==493 & date=="2012-03-21"],
                                                                  "Mr McNamara:",
                                                                  body[order==494 & date=="2012-03-21"],
                                                                  "Mr McNamara:",
                                                                  body[order==495 & date=="2012-03-21"],
                                                                  "Mr McNamara:",
                                                                  body[order==496 & date=="2012-03-21"],
                                                                  "Mr McNamara:",
                                                                  body[order==497 & date=="2012-03-21"]),
                          .default = body)) %>% 
  filter(!(date=="2012-03-21" & order %in% c(433,434,493,494,495,496,497)))

gc()

# 2012-06-25
corpus <- corpus %>% 
  mutate(body = case_when(date=="2012-06-25" & order==342 ~ paste(body, "Mr McDonald:",
                                                                  body[order==343 & date=="2012-06-25"],
                                                                  "Mr Verwer:",
                                                                  body[order==344 & date=="2012-06-25"],
                                                                  "Mr McDonald:",
                                                                  body[order==345 & date=="2012-06-25"]),
                          .default = body)) %>% 
  filter(!(date=="2012-06-25" & order %in% c(343,344,345)))

gc()

# 2013-03-20
corpus <- corpus %>% 
  mutate(body = case_when(date=="2013-03-20" & order==74 ~ paste(body, "Mr Peterson:",
                                                                 body[order==75 & date=="2013-03-20"]),
                          .default = body)) %>% 
  filter(!(date=="2013-03-20" & order %in% c(75)))

gc()

# 2013-05-27
corpus <- corpus %>% 
  mutate(body = case_when(date=="2013-05-27" & order==278 ~ paste(body, "Mr Kinley:",
                                                                  body[order==279 & date=="2013-05-27"],
                                                                  "Mr Kinley:",
                                                                  body[order==280 & date=="2013-05-27"]),
                          .default = body)) %>% 
  filter(!(date=="2013-05-27" & order %in% c(279,280)))

gc()

# 2013-05-28
corpus <- corpus %>% 
  mutate(body = case_when(date=="2013-05-28" & order==245 ~ paste(body, "Mr Constable:",
                                                                  body[order==246 & date=="2013-05-28"],
                                                                  "Mr Constable:",
                                                                  body[order==247 & date=="2013-05-28"],
                                                                  "Mr Constable:",
                                                                  body[order==248 & date=="2013-05-28"]),
                          .default = body)) %>% 
  filter(!(date=="2013-05-28" & order %in% c(246,247,248)))

gc()

# 2014-06-18
corpus <- corpus %>% 
  mutate(body = case_when(date=="2014-06-18" & order==501 ~ paste(body, "Mr Ray:",
                                                                  body[order==502 & date=="2014-06-18"]),
                          .default = body)) %>% 
  filter(!(date=="2014-06-18" & order %in% c(502)))

gc()

# 2014-09-01
corpus <- corpus %>% 
  mutate(body = case_when(date=="2014-09-01" & order==337 ~ paste(body, "Mr Warburton:",
                                                               body[order==338 & date=="2014-09-01"],
                                                               "Mr Warburton:",
                                                               body[order==339 & date=="2014-09-01"]),
                          .default = body)) %>% 
  filter(!(date=="2014-09-01" & order %in% c(338,339)))

gc()

# 2015-05-14
corpus <- corpus %>% 
  mutate(body = case_when(date=="2015-05-14" & order==444 ~ paste(body, "Ms Doyle", # no colon after name to match formatting in PDF
                                                                  body[order==445 & date=="2015-05-14"]),
                          .default = body)) %>% 
  filter(!(date=="2015-05-14" & order %in% c(445)))

gc()

# 2015-08-10
corpus <- corpus %>% 
  mutate(body = case_when(date=="2015-08-10" & order==84 ~ paste(body, "Mr Randall:",
                                                                 body[order==85 & date=="2015-08-10"],
                                                                 "Mr Randall:",
                                                                 body[order==86 & date=="2015-08-10"],
                                                                 "Mr Randall:",
                                                                 body[order==87 & date=="2015-08-10"],
                                                                 "Mr Randall:",
                                                                 body[order==88 & date=="2015-08-10"],
                                                                 "Mr Randall:",
                                                                 body[order==89 & date=="2015-08-10"]),
                          .default = body)) %>% 
  filter(!(date=="2015-08-10" & order %in% c(85,86,87,88,89)))

gc()

# 2016-02-03
corpus <- corpus %>% 
  mutate(body = case_when(date=="2016-02-03" & order==271 ~ paste(body, "Mr Hanna:",
                                                                  body[order==272 & date=="2016-02-03"],
                                                                  "Mr Skourdoumbis:",
                                                                  body[order==273 & date=="2016-02-03"],
                                                                  "Mr Hanna:",
                                                                  body[order==274 & date=="2016-02-03"]),
                          .default = body)) %>% 
  filter(!(date=="2016-02-03" & order %in% c(272,273,274)))

gc()

# 2016-04-19
corpus <- corpus %>% 
  mutate(body = case_when(date=="2016-04-19" & order==158 ~ paste(body, "Mr Tune:",
                                                                  body[order==159 & date=="2016-04-19"],
                                                                  "Mr Tune:",
                                                                  body[order==160 & date=="2016-04-19"]),
                          .default = body)) %>% 
  filter(!(date=="2016-04-19" & order %in% c(159,160)))

gc()

# 2018-02-14
corpus <- corpus %>% 
  mutate(body = case_when(date=="2018-02-14" & order==333 ~ paste(body, "Mr Thorburn:",
                                                                  body[order==334 & date=="2018-02-14"]),
                          .default = body)) %>% 
  filter(!(date=="2018-02-14" & order %in% c(334)))

gc()

# re-make order variable grouped by date now that we have condensed some rows
corpus <- corpus %>% 
  mutate(order = row_number(), .by=date)

### fix remaining name issues -------------------------------------------------
# download spreadsheet with corrected names (manually checked)
googledrive::drive_download(googledrive::as_id(
  "https://docs.google.com/spreadsheets/d/1P9bDRQnfodoLYGWWrOuwnFSjorjh5l5_jAPF9H8gvrY/edit?gid=1085792224#gid=1085792224"
  ), path = "additional_data/names_to_fix_temp.xlsx", overwrite = TRUE)

# import spreadsheet with manually corrected names
names_to_fix <- readxl::read_xlsx("additional_data/names_to_fix_temp.xlsx", 
                                  col_types = c("text","text","text","text",
                                                "text","text","text","text")) %>% 
  # drop rows flagged as manual fixes, those have all already been dealt with
  filter(is.na(manual_fix_needed)) %>% 
  # select columns of interest
  select(name, uniqueID, name_correct, not_in_auspol) %>% 
  # ensure null unique IDs are coded correctly as NA
  mutate(uniqueID = ifelse(uniqueID=="NA", NA, uniqueID))

# add "displayName" variable to merge on with AusPol table - need to remove 
# "speaker" or similar from names for merge correctly, so create new column
# with just portion of name that will match the AusPol displayName variable
names_to_fix <- names_to_fix %>% 
  mutate(displayName = case_when(
    not_in_auspol=="1" ~ NA,
    not_in_auspol=="0" & !str_detect(name_correct, "\\(The|\\(Leader") ~ name_correct,
    not_in_auspol=="0" & str_detect(name_correct, "\\(The|\\(Leader") ~ str_remove(
      name_correct, 
      " \\(The SPEAKER\\)| \\(The DEPUTY SPEAKER\\)| \\(The ACTING SPEAKER\\)| \\(Leader of the House\\)"))) %>% 
  # special fix for Sophia Panopoulos whose married name isn't reflected in her displayName
  mutate(displayName = ifelse(displayName=="Panopoulos, Sophie", 
                              "Mirabella, Sophie", displayName))

# check that all displayName values in our names_to_fix df are found in AusPol
stopifnot(setdiff(names_to_fix %>% filter(not_in_auspol==0) %>% 
                    distinct(displayName),
          lookup_full %>% select(displayName)) %>% nrow() == 0)

# merge corpus with corrected names_to_fix df
corpus_fixed <- left_join(corpus, names_to_fix, by = c("name", "uniqueID"))

# look at rows where name_correct is null - these should just be limited to
# the cases we manually fixed above:
#     Chris Pyne, Jackie Kelly, Teresa Gambaro, Kim Wilkie, and Andrew Wilkie
corpus_fixed %>% filter(is.na(name_correct)) %>% distinct(name, displayName) 
# all good - as expected

# however, this means we will need to manually assign these people a displayName
# since those would not have been captured in the names_to_fix dataframe
corpus_fixed <- corpus_fixed %>% 
  mutate(displayName = case_when(name %in% c("Wilkie, Kim", 
                                             "Pyne, Christopher",
                                             "Kelly, Jackie",
                                             "Gambaro, Teresa", 
                                             "Wilkie, Andrew") & 
                                   is.na(displayName) ~ name,
                                 .default = displayName))

# now we can assign any null "name_correct" values with the "name" cell value, 
# and then use the "name_correct" column in place of "name"
corpus_fixed <- corpus_fixed %>% 
  mutate(name_correct = ifelse(is.na(name_correct), name, name_correct)) %>% 
  # drop old name, uniqueID and gender vars, we will repopulate those using the
  # new, corrected displayName variable
  select(-name, -uniqueID, -gender) %>% 
  rename(name = name_correct) %>% 
  relocate(name, .after="date")

# merge with AusPol table on display name and re-populate unique ID and gender
corpus_fixed <- left_join(corpus_fixed, lookup_full %>% 
                            select(uniqueID, displayName, gender, member, 
                                   senator, phid), by="displayName")

### fix name ID variable ------------------------------------------------------
# manually check list of mismatched name.id / phid values to ensure that the
# PHID value is correct
# corpus_fixed %>%
#   filter(!is.na(uniqueID)) %>%
#   filter(phid!=name.id) %>%
#   # filter out general name IDs that don't actually belong to a single MP
#   filter(!name.id %in% c("UNKNOWN","10000","1000","1010000","110000")) %>%
#   distinct(name, name.id, phid)
### checked all of these, the phid values align with the parliamentary handbook

# repopulate name.id using the correct phid from the ausph / auspol lookup table
corpus_fixed <- corpus_fixed %>% 
  select(-name.id) %>% 
  rename(name.id = phid) %>% 
  relocate(name.id, .after="time.stamp")

### run additional checks -----------------------------------------------------
# only one name per unique ID - with the exception of Sophie Mirabella/Panopoulos
corpus_fixed %>% 
  filter(!is.na(uniqueID)) %>% 
  mutate(name = str_remove(name, " \\(The SPEAKER\\)| \\(The DEPUTY SPEAKER\\)| \\(The ACTING SPEAKER\\)| \\(Leader of the House\\)")) %>% 
  distinct(name, uniqueID) %>% 
  group_by(uniqueID) %>% 
  filter(n()>1) 
## all good

# one gender per unique ID
corpus_fixed %>% 
  distinct(uniqueID, gender) %>%
  group_by(uniqueID) %>% 
  filter(n()>1)

# one name ID per unique ID
corpus_fixed %>% 
  distinct(name.id, uniqueID) %>% 
  group_by(name.id) %>% 
  filter(n()>1)

### export corpus with corrections --------------------------------------------
# drop unwanted columns
corpus_fixed <- corpus_fixed %>% 
  select(-not_in_auspol, -displayName)

# export to parquet
write_parquet(corpus_fixed, "hansard_corpus_1998_to_2022-v25-06-25.parquet")
