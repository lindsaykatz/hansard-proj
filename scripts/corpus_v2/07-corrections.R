### preamble ------------------------------------------------------------------
# script purpose: fix name / ID / gender inconsistencies in Hansard corpus, and
#                 implement any additional fixes as needed.
# date of last update: 18 August 2025

### environment setup ---------------------------------------------------------
# import necessary libraries
library(tidyverse)
library(arrow)

# import corpus
corpus <- read_parquet("hansard-corpus/corpus_2022_to_2025.parquet")

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
#   write_csv("names_to_fix-2022_2025.csv")

### fix remaining name issues -------------------------------------------------
# download spreadsheet with corrected names (manually checked)
# googledrive::drive_download(googledrive::as_id(
#   "https://docs.google.com/spreadsheets/d/1P9bDRQnfodoLYGWWrOuwnFSjorjh5l5_jAPF9H8gvrY/edit?gid=2023312251#gid=2023312251"
#   ), path = "additional_data/names_to_fix_temp.xlsx", overwrite = TRUE)

# import spreadsheet with manually corrected names
names_to_fix <- readxl::read_xlsx("additional_data/names_to_fix_temp.xlsx", 
                                  col_types = c("text","text","text","text",
                                                "numeric"), sheet = 3) %>% 
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
      " \\(The SPEAKER\\)| \\(The DEPUTY SPEAKER\\)| \\(The ACTING SPEAKER\\)| \\(Leader of the House\\)")))

# check that all displayName values in our names_to_fix df are found in AusPol
stopifnot(setdiff(names_to_fix %>% filter(not_in_auspol==0) %>% 
                    distinct(displayName),
          lookup_full %>% select(displayName)) %>% nrow() == 0)

# merge corpus with corrected names_to_fix df
corpus_fixed <- left_join(corpus, names_to_fix, by = c("name", "uniqueID"))

# look at rows where name_correct is null - should be none
corpus_fixed %>% filter(is.na(name_correct)) %>% distinct(name, displayName) 
# all good - as expected

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

### manual name.id insertions for new MPs -------------------------------------
new_mps <- names_to_fix %>% 
  filter(not_in_auspol==1, 
         !str_detect(name_correct, "member|Business|Stage|CLERK|^The SPEAKER$|^The DEPUTY SPEAKER$")) %>% 
  distinct(name_correct) %>% pull()

corpus_fixed <- corpus_fixed %>% 
  mutate(displayName = case_when(
    name %in% new_mps & !str_detect(name, "\\(The|\\(Leader") ~ name,
    name %in% new_mps & str_detect(name, "\\(The|\\(Leader") ~ str_remove(
      name, " \\(The SPEAKER\\)| \\(The DEPUTY SPEAKER\\)"),
    .default = displayName))

corpus_fixed <- left_join(corpus_fixed, lookup_full %>% 
                            filter(is.na(uniqueID)) %>% 
                            select(phid, displayName, gender_to_fill=gender),
                          by="displayName")
  
stopifnot(
  corpus_fixed %>% filter(!is.na(phid) & !is.na(name.id)) %>% nrow()==0,
  corpus_fixed %>% filter(!is.na(gender) & !is.na(gender_to_fill)) %>% nrow()==0
)

corpus_fixed <-  corpus_fixed %>% 
  mutate(name.id = case_when(!is.na(phid) ~ phid,
                             .default = name.id),
         gender = case_when(!is.na(gender_to_fill) ~ gender_to_fill,
                            .default = gender)) %>% 
  select(-gender_to_fill, -phid, -not_in_auspol, -displayName)

### sept 4 fixes -------------------------------------------------------------
corpus_1998_2022 <- read_parquet("hansard-corpus/corpus_1998_to_2022.parquet")
corpus_2022_2025 <- read_parquet("hansard-corpus/corpus_2022_to_2025.parquet")
lookup <- readxl::read_xlsx("additional_data/lookup_tables/ausPH_AusPol_mapping.xlsx")

corpus_2022_2025 <- corpus_2022_2025 %>% 
  mutate(displayName = case_when(
    !str_detect(name, "\\(The|\\(Leader") ~ name,
    str_detect(name, "\\(The|\\(Leader") ~ str_remove(
      name, 
      " \\(The SPEAKER\\)| \\(The DEPUTY SPEAKER\\)| \\(The ACTING SPEAKER\\)| \\(Leader of the House\\)"))) 

names_missing_nameid <- corpus_2022_2025 %>% filter(is.na(name.id)) %>% 
  distinct(displayName) %>% pull()

corpus_2022_2025 <- left_join(corpus_2022_2025, lookup %>% 
                            select(phid, displayName, gender_use = gender) %>% 
            filter(displayName %in% names_missing_nameid), by="displayName") %>% 
  mutate(name.id = case_when(is.na(name.id) & !is.na(phid) ~ phid,
                             .default = name.id),
         gender = case_when(is.na(gender) & !is.na(gender_use) ~ gender_use,
                            .default = gender)) %>% 
  select(-phid, -displayName, -gender_use)

# same fixes for 1998-2022
corpus_1998_2022 <- corpus_1998_2022 %>% mutate(displayName = case_when(
  !str_detect(name, "\\(The|\\(Leader") ~ name,
  str_detect(name, "\\(The|\\(Leader") ~ str_remove(
    name, 
    " \\(The SPEAKER\\)| \\(The DEPUTY SPEAKER\\)| \\(The ACTING SPEAKER\\)| \\(Leader of the House\\)"))) 

names_missing_nameid <- corpus_1998_2022 %>% filter(is.na(name.id)) %>% 
  distinct(displayName) %>% pull()

corpus_1998_2022 <- left_join(corpus_1998_2022, lookup %>% 
                                select(phid, displayName, gender_use = gender) %>% 
                                filter(displayName %in% names_missing_nameid), by="displayName") %>%
  mutate(name.id = case_when(is.na(name.id) & !is.na(phid) ~ phid,
                             .default = name.id),
         gender = case_when(is.na(gender) & !is.na(gender_use) ~ gender_use,
                            .default = gender)) %>%  
  select(-phid, -displayName, -gender_use)

### fix column classes --------------------------------------------------------
corpus_2022_2025 <- corpus_2022_2025 %>% 
  mutate(date = as.Date(date),
         time.stamp = hms::as_hms(time.stamp)) %>% 
  mutate(across(c(in.gov, first.speech, gender, member, senator),
                ~ as.factor(.)))

corpus_1998_2022 <- corpus_1998_2022 %>% 
  mutate(date = as.Date(date),
         time.stamp = case_when(str_detect(time.stamp, "NaN|NA") ~ NA,
                                # recode problem time stamps as NA
                                time.stamp %in% c("29:37:00", "09:532:00", 
                                                  "09:497:00", "13:445:00",
                                                  "24:20:00", "60:20:00", 
                                                  "27:21:00","32:44:00") ~ NA,
                                .default = time.stamp),
         time.stamp = hms::as_hms(time.stamp)) %>% 
  mutate(across(c(in.gov, first.speech, gender, member, senator),
                ~ as.factor(.)))
  

### combine 2022-2025 stuff with 1998-2022 ------------------------------------
# look at rows missing name.ids for 2022-2025
corpus_2022_2025 %>% filter(is.na(name.id)) %>% distinct(name) %>% 
  filter(!str_detect(name, "Business|Member|member|CLERK$|SPEAKER$|Stage|ASSISTANT$|PRESIDENT|President|Prime Minister|GENERAL$|CHAIR$|Excellency"))
# acceptable - Michael Pezzullo is not / was not an MP

# look at rows missing name.ids for 1998-2022
corpus_1998_2022 %>% filter(is.na(name.id)) %>% distinct(name) %>% 
  filter(!str_detect(name, "Business|Member|member|CLERK$|SPEAKER$|Stage|ASSISTANT$|PRESIDENT|President|Prime Minister|GENERAL$|CHAIR$|Excellency"))
# acceptable - the Crosio, Janice and O'Connor, Gavan case is unique because they were recorded as interjecting together

corpus_full_for_export <- bind_rows(corpus_1998_2022, corpus_2022_2025)

stopifnot(nrow(corpus_full_for_export)==
            nrow(corpus_1998_2022)+nrow(corpus_2022_2025))

### export corpus with corrections --------------------------------------------
# export to parquet on local folder
write_parquet(corpus_1998_2022, "hansard-corpus/corpus_1998_to_2022.parquet")
write_parquet(corpus_2022_2025, "hansard-corpus/corpus_2022_to_2025.parquet")
write_parquet(corpus_full_for_export, "hansard-corpus/corpus_1998_to_2025.parquet")

# export to parquet on external drive
write_parquet(corpus_1998_2022, "/Volumes/Verbatim/hansard-corpus/corpus_1998_to_2022.parquet")
write_parquet(corpus_2022_2025, "/Volumes/Verbatim/hansard-corpus/corpus_2022_to_2025.parquet")
write_parquet(corpus_full_for_export, "/Volumes/Verbatim/hansard-corpus/corpus_1998_to_2025.parquet")
