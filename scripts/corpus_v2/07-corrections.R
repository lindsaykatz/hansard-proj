### preamble ------------------------------------------------------------------
# script purpose: fix name / ID / gender inconsistencies in Hansard corpus, and
#                 implement any additional fixes as needed.
# date of last update: 18 August 2025

### environment setup ---------------------------------------------------------
# import necessary libraries
library(tidyverse)
library(arrow)

# import corpus
corpus <- read_parquet("/Volumes/Verbatim/hansard-corpus/hansard_corpus_2022_to_2025.parquet")

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

### fix column classes --------------------------------------------------------
corpus_fixed <- corpus_fixed %>% 
  mutate(date = as.Date(date),
         time.stamp = as_hms(time.stamp)) %>% 
  mutate(across(c(in.gov, first.speech, gender, member, senator),
                ~ as.factor(.)))

### export corpus with corrections --------------------------------------------
# export to parquet on local folder
write_parquet(corpus_fixed, "hansard-corpus/hansard_corpus_2022_to_2025.parquet")

# export to parquet on external drive
write_parquet(corpus_fixed, "/Volumes/Verbatim/hansard-corpus/hansard_corpus_2022_to_2025.parquet")
write_csv(corpus_fixed, "/Volumes/Verbatim/hansard-corpus/hansard_corpus_2022_to_2025.csv")
