# series of tests to validate our data
library(tidyverse)

# read in ausPH and auspol dataset
ausPH <- ausPH::getIndividuals()

# for some reason ausPH wasn't working so i got data myself
#ausPH <-rjson::fromJSON(file="/Volumes/Verbatim/lookup_tables/individuals.json")[["value"]]|>dplyr::bind_rows()

all <- AustralianPoliticians::get_auspol('all')
party <- AustralianPoliticians::get_auspol('allbyparty')
mps <- AustralianPoliticians::get_auspol('mps')

# read in my lookup table
lookup <- read_csv("additional_data/lookup_tables/member_lookup.csv", show_col_types = F)

# grab list of file names
files_all <- list.files("/Volumes/Verbatim/output/main-filled-csv-v2")

##### test 1 - date in filename must match one date in our list of dates from all session headers

# grab session header csv - code to make this is in 01-session_info.R script
session_headers <- read_csv("additional_data/session_info_all.csv", show_col_types = F) %>% pull(date)

# extract dates from file names, convert to date
dates_all <- files_all %>% as_tibble() %>% mutate(value = as.Date(str_remove(value, "-main-v2.csv"))) %>% pull(value)

# check that dates of our filenames match dates from session headers
setdiff(as.character(dates_all), as.character(session_headers))

# one difference - 2009-06-03
# our filename is right, there is an error in the session header - it was transcribed as 2009-06-04, I checked official release to confirm we're right

##### test 2 - checks whether two lines that immediately follow each other have the same "body" - NEW
test2 <- tibble()

for(i in 1:length(files_all)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)

  test2 <-thisFile %>% filter(body == lead(body) | body == lag(body)) %>% 
    select(name, order, body) %>% 
    filter(body!="interjecting") %>% 
    mutate(date = str_remove(files_all[i], "-main-v2.csv")) %>% 
    bind_rows(test2, .)
  
}

##### test 3 - time expired at end of body
# define empty tibble
test3 <- tibble()

# loop over all files
for (i in 1:length(files_all)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # check that file has rows where time expired is followed by something in body
  if (thisFile %>% filter(str_detect(body, "\\(Time (E|e)xpired\\)[[:space:]]{0,2}\\..{1,}|\\(Time (E|e)xpired\\)[[:space:]]{0,2}(?!\\.)([[:alpha:]]|\\()")) %>% nrow()>0)

    # grab the text that follows time expired so we can see what wasn't split properly
    test3 <- thisFile %>% filter(str_detect(body, "\\(Time (E|e)xpired\\)[[:space:]]{0,2}\\..{1,}|\\(Time (E|e)xpired\\)[[:space:]]{0,2}(?!\\.)([[:alpha:]]|\\()")) %>% 
      select(order, body) %>% 
      mutate(date = str_remove(files_all[i], "-main-v2.csv"),
             body = str_extract(body, "\\(Time (E|e)xpired\\).+$")) %>% 
      bind_rows(., test3)
  
}

# now dealing with the NAs in test3 body resulting from newlines following (Time expired)
test3_newlines <- tibble()

# grab dates where body is NA
test3_dates<- test3 %>% filter(is.na(body)) %>% select(date) %>% mutate(date = paste0(date, "-main-v2.csv")) %>% pull

# change regex to capture the entire body so we can look manually
for (i in 1:length(test3_dates)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-2/", test3_dates[i]), show_col_types = F)
  
  # check that file has rows where time expired is followed by something in body
  if (thisFile %>% filter(str_detect(body, "\\(Time (E|e)xpired\\)[[:space:]]{0,2}\\..{1,}|\\(Time (E|e)xpired\\)[[:space:]]{0,2}(?!\\.)([[:alpha:]]|\\()")) %>% nrow()>0)
    
    # grab the text that follows time expired so we can see what wasn't split properly
    test3_newlines <- thisFile %>% filter(str_detect(body, "\\(Time (E|e)xpired\\)[[:space:]]{0,2}\\.{0,1}\\n|\\(Time (E|e)xpired\\)[[:space:]]{0,2}\\.{0,1}\\t")) %>% 
      select(order, body) %>% 
      mutate(date = str_remove(test3_dates[i], "-main-v2.csv")) %>% 
      bind_rows(., test3_newlines)
  
}

##### test 4 - one unique party and electorate per person per sitting day

# define empty tibble
test4 <- tibble()

# loop over all files
for (i in 1:length(files_all)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # select name ids where individual has more than one unique electorate or party on a sitting day
  test4 <- thisFile %>% select(name.id, party, electorate) %>% 
    unique() %>% 
    filter(!is.na(party) & !is.na(electorate)) %>% 
    group_by(name.id) %>% 
    mutate(n=n()) %>% 
    filter(n!=1) %>% 
    mutate(date = str_remove(files_all[i], "-main-v2.csv")) %>% 
    bind_rows(., test4)
}

test4 <- test4 %>% filter(!is.na(name.id) & name.id!="UNKNOWN" & name.id!="10000" & name.id!="1000")

# let's fix the issues
# this is likely due to my own filling in with the lookup table that doesn't account for the date (these things can change)
# using newly created party and electorate lookup tables with dates

party_lookup <- read_csv("additional_data/lookup_tables/party_lookup.csv", show_col_types = F)
electorate_lookup <- read_csv("additional_data/lookup_tables/electorate_lookup.csv", show_col_types = F)

# grab dates that failed test 4 so we can fix them
test4_dates <- test4 %>% ungroup() %>% select(date) %>% unique() %>% mutate(date = paste0(date, "-main-v2.csv")) %>% pull()

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/output/main-filled-csv-v2")

# subset for ones we need to fix
test4_dates <- files_all[files_all %in% test4_dates]

for (i in 1:length(test4_dates)) {
  
  # define filename
  filename <- test4_dates[i]
  
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", filename), show_col_types = F)
  
  # grab the name ids that were caught in test above
  nameIDs <- test4 %>% filter(date == str_remove(filename, "-main-v2.csv")) %>% select(name.id) %>% unique() %>% ungroup()
  
  #electorates <- test4 %>% filter(date == str_remove(filename, "-main-v2.csv")) %>% select(name.id, electorate) %>% unique() %>% ungroup()
  
  # join those we caught with our party lookup table, filter date range to match date of current file, and select the party associated with each name id
  party_toFix <- left_join(nameIDs, party_lookup %>% rename(name.id = phid), by="name.id") %>% 
    filter(partyFrom <= str_remove(filename, "-main-v2.csv") | is.na(partyFrom)) %>% 
    filter(partyTo > str_remove(filename, "-main-v2.csv") | is.na(partyTo)) %>% 
    rename(party_use=partyAbbrev) %>% 
    select(name.id, party_use)
  
  # same as above but for electorates and using mp membership date range
  electorate_toFix <- left_join(nameIDs, electorate_lookup %>% rename(name.id = phid), by="name.id") %>% 
    filter(mpFrom <= str_remove(filename, "-main-v2.csv") | is.na(mpFrom)) %>% 
    filter(mpTo > str_remove(filename, "-main-v2.csv") | is.na(mpTo)) %>% 
    rename(electorate_use = division) %>% 
    select(name.id, electorate_use)
  
  # check that there is one party/electorate associated with each unique name ID we need to fix
  stopifnot(nrow(nameIDs) == nrow(party_toFix))
  stopifnot(nrow(nameIDs) == nrow(electorate_toFix))
  
  # grab the correct name id party and electorate combos to fill the data file with
  toFix <- left_join(party_toFix, electorate_toFix, by="name.id")
  
  # fix electorates and parties
  thisFile_fixed <- left_join(thisFile, toFix, by = "name.id") %>% 
    mutate(party = ifelse(!is.na(party_use), party_use, party),
           electorate = ifelse(!is.na(electorate_use), electorate_use, electorate)) %>% 
    select(-c(party_use, electorate_use))
  
  # check dimensions are the same
  stopifnot(dim(thisFile) == dim(thisFile_fixed))
  
  # export file
  write.csv(thisFile_fixed, paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", test4_dates[i]), row.names = F) 
  
}

# after this i re-ran the test on the updated files and everything passed :)
  
##### test 5 - name id must exist in ausPH
# define empty tibble
test5 <- tibble()

# loop over all files
for (i in 1:length(files_all)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # grab name IDs that aren't in ausPH
  test5 <- thisFile %>% select(name, name.id) %>% 
    unique() %>% 
    filter(!is.na(name.id) & !str_detect(name, "SPEAKER$") 
           & !str_detect(name.id, "^10000$|^1000$|^0000$|^UNKNOWN$|^110000$|^1010000$|^10001$")) %>% 
    filter(!(name.id %in% ausPH$PHID)) %>% 
    mutate(date = str_remove(files_all[i], "-main-v2.csv")) %>% 
    bind_rows(., test5)
}

# remove people who are not MPs
test5 <- test5 %>% filter(str_detect(name, "MP"))

# fix wrong name IDs
test5 <- test5 %>% 
  mutate(name.id_use = case_when(name=="Georgiou, Petro, MP" ~ "HM5",
                                 name=="Dutton, Peter, MP" ~ "00AKI",
                                 name=="Debus, Bob, MP" ~ "8IS",
                                 name=="Nelson, Dr Brendan, MP" ~ "RW5",
                                 name=="Smith, Anthony, MP" ~ "00APG",
                                 name=="Gibbons, Steve, MP" ~ "83X",
                                 name=="Ciobo, Steven, MP" ~ "00AN0",
                                 name=="Pyne, Chris, MP" ~ "9V5",
                                 name=="Griffin, Alan, MP" ~ "VU5",
                                 name=="Byrne, Anthony, MP" ~ "008K0",
                                 name=="Anthony, Larry, MP" ~ "XJ6",
                                 name=="Andrews, Kevin, MP" ~ "HK5",
                                 name=="O'Connor, Gavan, MP" ~ "WU5",
                                 name=="Bailey, Fran, MP" ~ "JT4",
                                 name=="Joyce, Barnaby MP" ~ "E5D",
                                 name=="Randall, Don, MP" ~ "PK6",
                                 name=="katter, Bob, MP" ~ "HX4",
                                 name=="dutton, Peter, MP" ~ "00AKI",
                                 name=="johnson, Michael, MP" ~ "00AMX")) %>% 
  unique()

# grab list of dates where we need to fix the CSVs
test5_dates <- test5 %>% select(date) %>% unique() %>% mutate(date = paste0(date, "-main-v2.csv")) %>% pull(date)

# loop over those dates we just grabbed to fix name ID issues
for (i in 1:length(test5_dates)){
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", test5_dates[i]), show_col_types = F) 
  
  # grab dim of file to check after merge
  dim_now <- dim(thisFile)
  
  # merge, fill in correct name IDs
  thisFile <- left_join(thisFile, test5 %>% select(name, name.id, name.id_use) %>% unique(), by=c("name.id", "name")) %>% 
    mutate(name.id = ifelse(!is.na(name.id_use), name.id_use, name.id)) %>% 
    select(-name.id_use)
  
  # check dim didn't change
  stopifnot(dim_now==dim(thisFile))
  
  # export file
  write.csv(thisFile, paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", test5_dates[i]), row.names = F) 
  
}

 # I re-ran the test after fixing the files and all we're left with are people who aren't actually MPs - visiting politicians from other countries
# In this test I have ignored the general name IDs given to speakers/deputy speakers. we can loop through and make them all the same if we want.

##### test 6 - all individuals from sitting day have been born and are not dead
# define empty tibble
test6 <- tibble()

# loop over all files
for (i in 1:length(files_all)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # grab name IDs where 
  test6 <- thisFile %>% 
    select(uniqueID) %>% 
    filter(!is.na(uniqueID)) %>% 
    unique() %>% 
    mutate(date = as.Date(str_remove(files_all[i], "-main-v2.csv"))) %>% 
    left_join(., all, by="uniqueID") %>% 
    select(uniqueID, date, birthDate, deathDate) %>% 
    filter(!is.na(birthDate)) %>% #some ppl have missing birthdates
    mutate(pass_test = case_when(is.na(deathDate) & date>=birthDate ~ 1,
                                 !is.na(deathDate) & date>=birthDate & date<=deathDate ~ 1)) %>% 
    mutate(pass_test = ifelse(is.na(pass_test), 0, pass_test)) %>% 
    filter(pass_test==0) %>% 
    bind_rows(., test6)
}

##### test 7 - all individuals speaking are actually members of parliament on sitting day
# define empty tibble
test7 <- tibble()

# loop over all files
for (i in 1:length(files_all)) {
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # check that all MPs who spoke that day are actually in parliament on that sitting day
  # idea: if they're still an mp mpTo is NA, so pass test, if they served more than one term and they're in parliament for at least one, pass test
  test7 <- thisFile %>% 
    select(uniqueID) %>% 
    filter(!is.na(uniqueID)) %>% 
    unique() %>% 
    mutate(date = as.Date(str_remove(files_all[i], "-main-v2.csv"))) %>% 
    left_join(., mps, by="uniqueID") %>% 
    select(uniqueID, date, mpFrom, mpTo) %>% 
    filter(!is.na(mpFrom)) %>%  # some uniqueIDs aren't in mps dataset (only one I found so far is Connelly1978)
    mutate(pass_test = case_when(is.na(mpTo) & date>=mpFrom ~ 1,
                                 !is.na(mpTo) & date>=mpFrom & date<=mpTo ~ 1)) %>% 
    group_by(uniqueID) %>% 
    mutate(pass_test = ifelse(n()>1 & any(pass_test==1), 1, pass_test),
           pass_test = ifelse(is.na(pass_test), 0, pass_test)) %>% 
    ungroup() %>% 
    filter(pass_test==0) %>% 
    bind_rows(., test7)
}

# there are 9 people who didn't pass this test Lawrence1948, Rudd1957, MacTiernan1953, Alexander1951, Feeney1970, Scott1977, Hart1960, Keating1944, Moore1936

# manually checking these
# Alexander1951 is an AusPol dates in parliament error
# Feeney1970 also a dates error, "Mr Feeney interjecting" appears in both dates flagged
# same for Moore1936 and Scott1977 (Ms Scott interjecting-)
# fixed Keating by adding better lookarounds when grabbing names
# fixed Lawrence by adding "Ms Lawrence" to repeated surnames list b/c the ones that were an issue was Tania Lawrence who isn't in AusPol yet, so code was filling with Carmen Lawrence

# filter out people who we manually confirmed are not an error on our end
test7 <- test7 %>% filter(!(uniqueID %in% c("Alexander1951", "Moore1936", "Feeney1970", "Scott1977", "Lawrence1948")))



###### old code that we're not using
##### test 2 - equal number of flagged questions and answers - THIS ASSUMPTION WAS WRONG- test not being used
# define empty tibble
# test2 <- tibble()
# 
# # loop over all files
# for (i in 1:length(files_all)) {
#   # read in file
#   thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv/", files_all[i]), show_col_types = F)
#   
#   # grab total qs and as, check if equal, add date
#   test2 <- thisFile %>% summarise(total_q = sum(question),
#                                   total_a = sum(answer),
#                                   equal = total_q==total_a) %>% 
#     mutate(date = str_remove(files_all[i], "-main-v2.csv")) %>% 
#     bind_rows(test2)
#   
# }
# 
# # filter for days where test failed
# test2 <- test2 %>% filter(equal==FALSE) %>% select(-equal)
