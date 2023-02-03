# script to maximize completeness of hansard data by using AusPol lookup table
# main goal is to replace short names with full names, and fill in any missing party, electorate, gender, or uniqueID details
library(tidyverse)

# read in master lookup table
master_orig <- read_csv("/Volumes/Verbatim/member_lookup.csv", show_col_types = F) 
speaker_list <- read_csv("/Volumes/Verbatim/speaker_lookup.csv", show_col_types = F) %>% unique()

# setdiff(ausPH %>% select(phid) %>% unique(), master %>% select(phid) %>% unique())

# rename phid to match with hansard, and change parties to hansard abbreviations for consistency
# these are from: https://www.aph.gov.au/Parliamentary_Business/Committees/Senate/woc/wocjanjun08/a02
master_orig <- master_orig %>% 
  rename(name.id = phid) %>% 
  mutate(party = case_when(party=="Australian Democrats" ~ "AD",
                           party=="Australian Greens" ~ "AG",
                           party=="Australian Labor Party" ~ "ALP",
                           party=="Independent" ~ "Ind",
                           party=="Liberal Party of Australia" ~ "LP",
                           party=="The Nationals" ~ "NATS",
                           party=="National Party of Australia" ~ "NATS"))

# do same for speaker list
speaker_list <- speaker_list %>% 
  rename(name.id = phid) %>% 
  mutate(party = case_when(party=="Australian Democrats" ~ "AD",
                           party=="Australian Greens" ~ "AG",
                           party=="Australian Labor Party" ~ "ALP",
                           party=="Independent" ~ "Ind",
                           party=="Liberal Party of Australia" ~ "LP",
                           party=="National Party of Australia" ~ "NATS"))

# define file-name
#filename <- "2022-09-07-main.csv"

fill_main <- function(filename) {
  
  # let's filter out people who have died prior to the Hansard date we're working with
  # this will help reduce the number of people who share a surname and title
  master <- master_orig %>% filter(deathDate >= paste0(str_remove(filename, "-main.csv")) | is.na(deathDate))
  
  # get list of people who share a title and surname (summarized by looking at form2 and form4)
  # adding a few for ppl who got their seats in 2022 and aren't in AusPol yet
  repeat_forms <- master %>% 
    separate_rows(title, sep="\\|") %>% 
    group_by(surname, title) %>% 
    mutate(n=n()) %>% 
    filter(n>1) %>% 
    ungroup() %>% 
    select(form2, form4) %>% 
    unique() %>% 
    pivot_longer(form2:form4, names_to = "type", values_to = "form") %>%  
    pull(form) %>% 
    c(., "Mr Reid", "Mr REID", "Ms Lawrence", "Ms LAWRENCE")
  
  # read in file
  if (str_remove(filename, "-main.csv") > "2011-03-24") {
    # 2011-2022 (everything post name change from main committee to federation chamber)
    thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022/", filename), show_col_types = F)
  } else if (str_remove(filename, "-main.csv") <= "2011-03-24" & str_remove(filename, "-main.csv") >= "2000-02-15") {
    # 2000-2011
    thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2000-2011/", filename), show_col_types = F)
  } else if (str_remove(filename, "-main.csv") <= "1999-12-09") {
    # 1998-1999
    thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-1998-1999/", filename), show_col_types = F)
  }
  
  # grab dim of thisFile so we can use it for checks throughout code, want to ensure it is the same before we export
  thisFile_dim <- dim(thisFile)
  
  # issue specific here b/c super short sitting and name ID col is numeric cause all are missing except one which is 10000
  if (filename == "2002-10-24-main.csv") {
    thisFile$name.id <- as.character(thisFile$name.id)
  }
  
  # first, use name id to fill in missing uniqueID, electorate and party
  # rename things in master list to fill with, left join, if the variable is missing in the original csv, and we have one to fill with, fill it in
  # drop the fill variables so we're left with the same number of variables as before, just a more complete dataset
  thisFile <- master %>% 
    select(name.id, uniqueID, gender, electorate, party) %>% 
    rename(uniqueID_use = uniqueID,
           gender_use = gender,
           electorate_use = electorate,
           party_use = party) %>% 
    left_join(thisFile, ., by="name.id") %>% 
    mutate(uniqueID = ifelse(is.na(uniqueID) & !is.na(uniqueID_use), uniqueID_use, uniqueID),
           gender = ifelse(is.na(gender) & !is.na(gender_use), gender_use, gender),
           party = ifelse(is.na(party) & !is.na(party_use), party_use, party),
           electorate = ifelse(is.na(electorate) & !is.na(electorate_use), electorate_use, electorate)) %>%
    select(-c(uniqueID_use, gender_use, electorate_use, party_use))
  
  # add check to ensure dimensions of original data-set and filled in data-set are equal
  stopifnot(dim(thisFile) == thisFile_dim)
  
  # now, let's fill things in for people who's name is not complete, and who's name does not match one of the repeated ones from the master list
  # first, grab list of these names to match for
  # filter out the names which are just general statements or stage directions
  short_names <- thisFile %>% filter(is.na(name.id) & !str_detect(name, "business|stage|SPEAKER|member")) %>% 
    select(name) %>% 
    unique() %>% 
    filter(!str_detect(name, paste0(repeat_forms, collapse = "|"))) %>% 
    pull()
  
  # filter master list for people in short names list, paste full name form to match Hansard (aka use displayName and add ", MP" at end)
  # then, rename variables for merge, perform merge, fill things in, and drop extra variables
  thisFile <- master %>% unite("all_forms", form1:last_col(), sep="*") %>% # use asteriks for easy separation in next step
    separate_rows(all_forms, sep="\\*") %>% # separate them so we can extract all matches in the next step (multiple variations of the same name, want to account for and fill all of these)
    mutate(name = str_extract(paste0(short_names, collapse = "|"), all_forms)) %>% 
    filter(!is.na(name)) %>% 
    mutate(fullName = paste0(displayName, " MP")) %>% 
    select(fullName, name, name.id, uniqueID, gender, electorate, party) %>% 
    rename(name.id_use = name.id, 
           uniqueID_use = uniqueID,
           gender_use = gender,
           electorate_use = electorate,
           party_use = party) %>% 
    left_join(thisFile, ., by="name") %>% 
    mutate(name.id = ifelse(is.na(name.id) & !is.na(name.id_use), name.id_use, name.id), 
           uniqueID = ifelse(is.na(uniqueID) & !is.na(uniqueID_use), uniqueID_use, uniqueID),
           gender = ifelse(is.na(gender) & !is.na(gender_use), gender_use, gender),
           party = ifelse(is.na(party) & !is.na(party_use), party_use, party),
           electorate = ifelse(is.na(electorate) & !is.na(electorate_use), electorate_use, electorate)) %>%
    mutate(name = ifelse(str_detect(name, paste0("^", short_names, "$", collapse = "|")) & !is.na(fullName), fullName, name)) %>% 
    select(-c(uniqueID_use, gender_use, electorate_use, party_use, name.id_use, fullName))
  
  # next step is to deal with the people who's forms are repeated in our master list - let's find the full name it matches with (if we have one in the hansard) and use that name id to back out the correct person
  # first grab the short names which are repeated in our master list
  repeat_short_names <- thisFile %>% filter(is.na(name.id) & !str_detect(name, "business|stage|SPEAKER|member")) %>% 
    select(name) %>% 
    unique() %>% 
    filter(str_detect(name, paste0(repeat_forms, collapse = "|"))) %>% 
    pull()
  
  # now grab the display name versions of those short names, so we can match for the full/complete names in the hansard
  if (length(repeat_short_names)>0){
    repeat_full_names <- master %>% unite("all_forms", form1:last_col(), sep="*") %>% # use asteriks for easy separation in next step
      separate_rows(all_forms, sep="\\*") %>% # separate them so we can extract all matches in the next step (multiple variations of the same name, want to account for and fill all of these)
      mutate(name = str_extract(paste0(repeat_short_names, collapse = "|"), all_forms)) %>% 
      filter(!is.na(name)) %>% 
      select(displayName) %>% 
      unique() %>% 
      pull()
    
    if (length(repeat_full_names)>0){
      # now find the full version of those short names in the current hansard
      # TO DO - add check so the num of unique name IDs is equal to the num of unique repeat short names (make all cap then unique for count)
      repeat_name.id <- thisFile %>% filter(str_detect(name, paste0(repeat_full_names, collapse = "|"))) %>% 
        select(name.id) %>% 
        unique() %>% 
        na.omit() %>% 
        pull()
      
      if (length(repeat_name.id)>0){
        # filter master list for the name id's we grabbed in previous step, filtering out the correct people with repeated surnames based on the one that exists in the hansard
        # unite and separate rows for full matching (different cases, want to match everything)
        # then paste full name to fill with, rename everything
        # left join with main dataset, fill things in accordingly, and drop extra variables
        thisFile <- master %>% 
          filter(str_detect(name.id, paste0("^", repeat_name.id, "$", collapse = "|"))) %>% 
          unite("all_forms", form1:last_col(), sep="*") %>% # use asteriks for easy separation in next step
          separate_rows(all_forms, sep="\\*") %>% # separate them so we can extract all matches in the next step (multiple variations of the same name, want to account for and fill all of these)
          mutate(name = str_extract(paste0(repeat_short_names, collapse = "|"), all_forms)) %>% 
          filter(!is.na(name)) %>% 
          mutate(fullName = paste0(displayName, " MP")) %>% 
          select(fullName, name, name.id, uniqueID, gender, electorate, party) %>% 
          rename(name.id_use = name.id, 
                 uniqueID_use = uniqueID,
                 gender_use = gender,
                 electorate_use = electorate,
                 party_use = party) %>% 
          left_join(thisFile, ., by="name") %>% 
          mutate(name.id = ifelse(is.na(name.id) & !is.na(name.id_use), name.id_use, name.id), 
                 uniqueID = ifelse(is.na(uniqueID) & !is.na(uniqueID_use), uniqueID_use, uniqueID),
                 gender = ifelse(is.na(gender) & !is.na(gender_use), gender_use, gender),
                 party = ifelse(is.na(party) & !is.na(party_use), party_use, party),
                 electorate = ifelse(is.na(electorate) & !is.na(electorate_use), electorate_use, electorate)) %>%
          mutate(name = ifelse(str_detect(name, paste0("^", repeat_short_names, "$", collapse = "|")) & !is.na(fullName), fullName, name)) %>% 
          select(-c(uniqueID_use, gender_use, electorate_use, party_use, name.id_use, fullName))
      }
    }
  }
  
  # add check to ensure dimensions of original data-set and filled in data-set are equal
  stopifnot(dim(thisFile) == thisFile_dim)
  
  # if file is from 1998-1999, often names when used after the first time are just the last name in caps, with the name id filled in
  # for completeness, lets left join to fill in those where the name is just the surname in all caps
  # remove title from form4 and use that, b/c the surname is already capitalized correctly (i.e. MacTIERAN special cases corrected for in this form)
  if (str_detect(filename, "^199")) {
    thisFile <- master %>% mutate(title = str_replace_all(title, "\\|", "\\\\|"),
                      surname_caps = str_remove(form4, paste0("\\({0,1}", title, "\\){0,1}", " "))) %>% 
      select(surname_caps, name.id, uniqueID, displayName) %>% 
      rename(name = surname_caps) %>% 
      left_join(thisFile, ., by=c("name", "name.id", "uniqueID")) %>% 
      mutate(name = ifelse(!is.na(displayName), paste0(displayName, ", MP"), name)) %>% 
      select(-displayName)
    
    # add check to ensure dimensions of original data-set and filled in data-set are equal
    stopifnot(dim(thisFile) == thisFile_dim)
  }
  
  # fill in info of deputy speaker or speaker with title in name, using speaker lookup table
  thisFile <- speaker_list %>% mutate(fullName = paste0(displayName, " MP")) %>% 
    select(fullName, name, name.id, uniqueID, gender, electorate, party) %>% 
    rename(name.id_use = name.id, 
           uniqueID_use = uniqueID,
           gender_use = gender,
           electorate_use = electorate,
           party_use = party) %>% 
    left_join(thisFile, ., by="name") %>% 
    mutate(name.id = ifelse(is.na(name.id) & !is.na(name.id_use), name.id_use, name.id), 
           uniqueID = ifelse(is.na(uniqueID) & !is.na(uniqueID_use), uniqueID_use, uniqueID),
           gender = ifelse(is.na(gender) & !is.na(gender_use), gender_use, gender),
           party = ifelse(is.na(party) & !is.na(party_use), party_use, party),
           electorate = ifelse(is.na(electorate) & !is.na(electorate_use), electorate_use, electorate)) %>%
    mutate(name = ifelse(str_detect(name, paste0("^", repeat_short_names, "$", collapse = "|")) & !is.na(fullName), fullName, name)) %>% 
    select(-c(uniqueID_use, gender_use, electorate_use, party_use, name.id_use, fullName))
  
  stopifnot(dim(thisFile) == thisFile_dim)
  
  # export csv, include v2 in name of csv to clarify this is for version 2 of the data set
  write.csv(thisFile, paste0("/Volumes/Verbatim/output/main-filled-csv/", str_remove(filename, "-main.csv"), "-main-v2.csv"), row.names = FALSE)

}

dates_now <- as_tibble(c("2005-06-23.xml", "2007-05-09.xml", "2007-09-19.xml", "2008-11-24.xml", "2008-12-01.xml", "2009-02-04.xml", "2010-03-10.xml", "2010-10-28.xml", "2011-03-21.xml")) %>% 
  mutate(value = paste0(str_remove(value, ".xml"), "-main.csv")) %>% 
  pull

dates_now <- dates_now %>% as_tibble() %>% mutate(value=paste0(str_remove(value, ".xml"), "-main.csv")) %>% pull()

for(i in 1:length(dates_now)) {
  fill_main(dates_now[i])
}


# grab list of all file names
files_all <- c(list.files("/Volumes/Verbatim/output/main-2011-2022"), list.files("/Volumes/Verbatim/output/main-2000-2011"), list.files("/Volumes/Verbatim/output/main-1998-1999"))

files_all <- files_all %>% as_tibble() %>% filter(!str_detect(value, "^2011")) %>% pull()

for(i in 1:length(files_all)){
  fill_main(files_all[i])
}


# now, we want to fix any discrepancies in the way that parties are specified, for example NATS vs Nationals
# let's use the party abbreviations using in the APH for consistency
# define empty tibble
partyList <- tibble()

# go through all files, grab unique party names and compile a list
# this will allow us to see where these discrepancies exist
for(i in 1:length(files_all)){
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled/", filename), show_col_types = F)
  
  partyList <- thisFile %>% select(party) %>% bind_rows(., partyList) %>% unique()
}

# next fill in role of members who were ministers
ministers <- ausPH::getCommitteeMembers()



