library(tidyverse)

# read in auspol dataset
all_auspol <- AustralianPoliticians::get_auspol('all')

# read in ausPH dataset
all_ausPH <- ausPH::getIndividuals()

# from auspol dataset, filter for members of the house, and select variables we want, and add name variables in full capitals - this is for merging later
auspol <- all_auspol %>% filter(member==1) %>% 
  select(uniqueID, surname, firstName, commonName, allOtherNames, displayName, earlierOrLaterNames, gender, title, deathDate) %>% 
  mutate(surname_cap = str_to_upper(surname),
         firstName_cap = str_to_upper(firstName),
         commonName_cap = str_to_upper(commonName),
         allOtherNames_cap = str_to_upper(allOtherNames),
         displayName_cap = str_to_upper(displayName),
         earlierOrLaterNames_cap = str_to_upper(earlierOrLaterNames))

# select variables we want from ausPH, grab surnames, create surname_gender variable to match things with in the next step
ausPH <- all_ausPH %>% select(PHID, DisplayName, Gender, Electorate, Party) %>% 
  rename(phid = PHID,
         displayName = DisplayName,
         gender = Gender) %>% 
  mutate(surname = str_extract(displayName, "^[[:alpha:]]+(?=,)|^[[:upper:]][[:alpha:]]+-[[:upper:]][[:alpha:]]+(?=,)|^[[:upper:]]+[[:space:]][[:upper:]]+(?=,)|^[[:upper:]]'[[:alpha:]]+(?=,)")) %>% 
  mutate(surname = ifelse(is.na(surname) & str_detect(displayName, "\\(formerly"),
                          str_extract(displayName, "^[[:alpha:]]+(?= \\(formerly)"),
                          surname)) %>% 
  mutate(surname = ifelse(surname=="COOK" & str_detect(displayName, "Hume"),
                          "HUME COOK",
                          surname)) %>% 
  mutate(gender = ifelse(gender=="Male", "male", gender),
         gender = ifelse(gender=="Female", "female", gender)) 

# capitalization issue we need to fix manually to ensure successful merge
# remove 2nd bob brown b/c it isn't in AusPol and causing issues when we merge later
# keep the one with AM suffix b/c that's the one who matches the birth year in AusPol
# same idea with Don James Cameron, want the one born in 1917 to match AusPol
ausPH <- ausPH %>% mutate(displayName=ifelse(displayName=="ASTON, the Hon. Sir WillIam John, KCMG",
                                    "ASTON, the Hon. Sir William John, KCMG",
                                    displayName),
                 displayName=ifelse(displayName=="ATKINSON, the Hon. Lllewelyn",
                                    "ATKINSON, the Hon. Llewellyn",
                                    displayName),
                 displayName=ifelse(displayName=="COOK, the Hon. James Newton Haxton Hume",
                                    "HUME COOK, the Hon. James Newton Haxton",
                                    displayName)) %>% 
  filter(displayName!="BROWN, Robert (Bob) James") %>% 
  filter(displayName!="CAMERON, the Hon. Donald James")

# grab everything after surname for first name, and then clean it up by extracting the suffix
ausPH <- ausPH %>% mutate(firstName = ifelse(!str_detect(displayName, "\\(formerly"), str_remove(displayName, paste0(surname, ", ")), NA),
                          firstName = ifelse(is.na(firstName), str_extract(displayName, paste0("(?<=\\), ).+$")), firstName)) %>% 
  mutate(prefix = str_extract(firstName, "^the Hon\\. Dr |^the Hon\\. Sir |^the Rt\\. Hon\\. Sir |^the Rt\\. Hon\\. |^the Rt\\ Hon\\. |^Dr\\. |^Dr |^the Hon\\. Rev\\. |^Sir |^the Hon\\. "),
         firstName = str_remove(firstName, paste0("^", prefix))) %>% 
  mutate(suffix = str_extract(firstName, ", .{1,25}$| AC$| KCMG$|\\, OAM$| OAM$"),
         firstName = str_remove(firstName, paste0(suffix, "$")),
         suffix = str_remove(suffix, "^, |^\\. |^ ")) %>% 
  mutate(commonName = str_extract(firstName, "(?<=\\().+(?=\\))"))

ausPH <- ausPH %>% 
  mutate(allOtherNames = str_remove(firstName, "\\([[:alpha:]]+[[:space]]{0,1}\\)")) %>% 
  mutate(firstName = str_extract(firstName, "^[[:alpha:]]+(?=[[:space]]{0,1})")) %>% 
  rename(fullName = displayName,
         surname_cap = surname) %>% 
  mutate(surname_cap = str_to_upper(surname_cap)) %>% 
  mutate(displayName = paste0(surname_cap, ", ", firstName),
         displayName_cap = str_to_upper(displayName)) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(across(where(is.character), ~str_replace_all(., "  ", " ")))

# fix inconsistencies in allOtherNames between AusPH and ausPol, using AusPol version
ausPH <- ausPH %>% mutate(allOtherNames=ifelse(allOtherNames=="Julia" & surname_cap=="BANKS",
                                      "Julia Helen",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Frank" & surname_cap=="BRENNAN",
                                      "Francis",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Kevin Michael Kiernan" & surname_cap=="CAIRNS",
                                      "Kevin Michael",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="William" & surname_cap=="COLEMAN",
                                      "William Peter",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Francis Daniel" & surname_cap=="CREAN",
                                      "Frank",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Janice Ann" & surname_cap=="CROSIO",
                                      "Janice Anne",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="John" & surname_cap=="CURTIN",
                                      "John Joseph Ambrose",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Alexander James" & surname_cap=="FORBES",
                                      "Alexander James de Burgh",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Celia Monica" & surname_cap=="HAMMOND",
                                      "Celia",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="William" & surname_cap=="JOHNSON",
                                      "William Elliot",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="Barnaby Thomas Gerard" & surname_cap=="JOYCE",
                                      "Barnaby Thomas Gerrard",
                                      allOtherNames),
                 allOtherNames=ifelse(allOtherNames=="William" & surname_cap=="HODGMAN",
                                      "William Michael",
                                      allOtherNames)) %>% 
  mutate(allOtherNames = ifelse(surname_cap=="HUME COOK", 
                                "James Newton Haxton Hume", 
                                allOtherNames)) %>% 
  mutate(allOtherNames=ifelse(allOtherNames=="Gerardine Mary" & surname_cap=="KEARNEY",
                              "Gerardine",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Denis James KCMG" & surname_cap=="KILLEN",
                              "Denis James",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="David Kelly" & surname_cap=="LITTLEPROUD",
                              "David",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Dame Enid Muriel" & surname_cap=="LYONS",
                              "Enid Muriel",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Kimberley William" & surname_cap=="WILKIE",
                              "Kim William",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Deane McMillan" & surname_cap=="WELLS",
                              "Deane MacMillan",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Meryl Jane" & surname_cap=="SWANSON",
                              "Meryl",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Kenneth Vincent" & surname_cap=="TICEHURST",
                              "Ken",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="James William" & surname_cap=="STEVENS",
                              "James William Stevens",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="George Hugh Alexander" & surname_cap=="MACKAY",
                              "George Hugh",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Edward Lynam" & surname_cap=="O'BRIEN",
                              "Ted Lynam",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Paul John." & surname_cap=="ZAMMIT",
                              "Paul John",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Rebekha Carina Che" & surname_cap=="SHARKIE",
                              "Rebekha Carina",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Edgar Hughes Degenaart" & surname_cap=="RUSSELL",
                              "Edgar Hughes Deg",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Gavin Bruce" & surname_cap=="PEARCE",
                              "Gavin",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Brendan Patrick John" & surname_cap=="O'CONNOR",
                              "Brendan Patrick",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Bernard Fernando" & surname_cap=="RIPOLL",
                              "Bernard Fernand",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Dr Herbert Vere" & surname_cap=="EVATT",
                              "Herbert Vere",
                              allOtherNames),
         allOtherNames=ifelse(allOtherNames=="Alannah Joan Geraldine" & surname_cap=="MACTIERNAN",
                              "Alannah Joan Geraldine Cecilia",
                              allOtherNames))

# create lookup table by mergine
# The Jenkins thing is b/c there are two Harry Jenkins and I had to manually specify which phid and uniqueID matched based on birth year (Sr and Jr, not specified in ausPH)
lookup <- left_join(auspol, ausPH, by=c("surname_cap", "allOtherNames", "gender")) %>% 
  filter(!(phid=="DRW" & uniqueID=="Jenkins1952" | phid=="HH4" & uniqueID=="Jenkins1925")) %>% 
  select(uniqueID, phid, prefix, suffix, Electorate, Party) %>% 
  rename(electorate = Electorate,
         party = Party) %>% 
  left_join(auspol, ., by="uniqueID") %>% 
  select(uniqueID, phid, surname, firstName, displayName, gender, title, prefix, suffix, electorate, party, deathDate) %>% 
  mutate(electorate = ifelse(electorate=="", NA, electorate))

# fill in the correct title to precede the name (Dr/Mr/Mrs/Ms) and fix a couple displayNames manually to match Hansard
# this is based on manual checks
lookup <- lookup %>% mutate(title = ifelse(str_detect(prefix, "Dr"), "Dr", title),
                  title = ifelse(gender=="male" & is.na(title), "Mr", title),
                  title = ifelse(gender=="female" & is.na(title), "Ms|Mrs", title)) %>% 
  mutate(displayName = ifelse(displayName=="Katter, Bob (Jr)", "Katter, Bob", displayName),
         displayName = ifelse(displayName=="O'Neill, Deborah", "O'Neill, Deb", displayName),
         displayName = ifelse(displayName=="Oakeshott, Rob", "Oakeshott, Robert", displayName),
         displayName = ifelse(displayName=="Somlyay, Alex", "Somlyay, Alexander", displayName),
         displayName = ifelse(displayName=="Pyne, Christopher", "Pyne, Chris", displayName),
         displayName = ifelse(displayName=="Anthony, Larry (Lawrence)", "Anthony, Larry", displayName))

# if someone's title is Dr, add all possible titles based on gender to ensure we capture everything
lookup <- lookup %>% mutate(title = ifelse(title=="Dr" & gender=="male", "Dr|Mr", title),
                  title = ifelse(title=="Dr" & gender=="female", "Dr|Ms|Mrs", title))

# now we can paste the various name forms
lookup <- lookup %>% mutate(form1 = paste0("(", title, ") ", firstName, " ", surname),
                  form2 = paste0("(", title, ") ", surname),
                  form3 = ifelse(str_detect(surname, "^[[:upper:]][[:lower:]]{1,2}[[:upper:]]"), 
                           paste0("(", title, ") ", str_to_upper(firstName), " ", str_extract(surname, "^[[:upper:]][[:lower:]]{1,2}"), 
                                  str_to_upper(str_remove(surname, "^[[:upper:]][[:lower:]]{1,2}"))), 
                           paste0("(", title, ") ", str_to_upper(firstName), " ", str_to_upper(surname))),
            form4 = ifelse(str_detect(surname, "^[[:upper:]][[:lower:]]{1,2}[[:upper:]]"), 
                           paste0("(", title, ") ", str_extract(surname, "^[[:upper:]][[:lower:]]{1,2}"), str_to_upper(str_remove(surname, "^[[:upper:]][[:lower:]]{1,2}"))), 
                           paste0("(", title, ") ", str_to_upper(surname))),
            form5 = ifelse(str_detect(surname, "^[[:upper:]][[:lower:]]{1,2}[[:upper:]]"), 
                           paste0("(", title, ") ", firstName, " ", str_extract(surname, "^[[:upper:]][[:lower:]]{1,2}"), 
                                  str_to_upper(str_remove(surname, "^[[:upper:]][[:lower:]]{1,2}"))), 
                           paste0("(", title, ") ", firstName, " ", str_to_upper(surname)))) %>% 
  mutate(across(c(form1:last_col()), ~ ifelse(!str_detect(., "\\|"), str_remove(., "\\("), .)),
         across(c(form1:last_col()), ~ ifelse(!str_detect(., "\\|"), str_remove(., "\\)"), .)))

# export lookup table
write.csv(lookup, "/Volumes/Verbatim/member_lookup.csv", row.names = FALSE)

# make speaker table, filtering only for speakers or deputy speakers (based on parliamentary handbook website)
speaker_lookup <- lookup %>% 
  filter(phid %in% c("KHI", "L0K", "KQP", "K99","KXG","KFK",
                     "KLL","KIT","JOS","JVR","L08","JTY",
                     "KSC","JMF","K5L","5J4","DQF",
                     "HF4","4I4","8I4","3H4","5E4","HE4",
                     "8H4","HH4","0V5","83S","SE4","00APG",
                     "265967","53517", "AE4","4K6","83S",
                     "0V5","YT4","HWN","218019","265991","248181",
                     "ZI4","HH4","YT4","DZY","M3E","74046")) %>% 
  mutate(speaker = ifelse(phid %in% c("KHI", "L0K", "KQP", "K99","KXG","KFK",
                                    "KLL","KIT","JOS","JVR","L08","JTY",
                                    "KSC","JMF","K5L","5J4","DQF",
                                    "HF4","4I4","8I4","3H4","5E4","HE4",
                                    "8H4","HH4","0V5","83S","SE4","00APG",
                                    "265967","53517"), 1, 0),
       deputySpeaker = ifelse(phid %in% c("AE4","4K6","83S","0V5","YT4","HWN","218019","265991","248181"), 1, 0),
       deputySpeaker2 = ifelse(phid %in% c("ZI4","HH4","YT4","DZY","M3E","74046"), 1, 0))

# go through all mains and grab different forms of deputy speaker names so we can paste correct forms in our lookup table
# grab list of all file names
files_2022 <- list.files("/Volumes/Verbatim/output/main-2011-2022")
files_2011 <- list.files("/Volumes/Verbatim/output/main-2000-2011")
files_1999 <- list.files("/Volumes/Verbatim/output/main-1998-1999")

list <- tibble()

for (i in 1:length(files_2022)) {
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022/", files_2022[i]), show_col_types = F)
  
  list <- thisFile %>% select(name, name.id, electorate, party, uniqueID) %>% 
    filter(str_detect(name, "SPEAKER|Speaker")) %>% 
    unique() %>% 
    filter(name!="The SPEAKER" & name!="The DEPUTY SPEAKER") %>% 
    bind_rows(., list)
}

for (i in 1:length(files_2011)) {
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-2000-2011/", files_2011[i]), show_col_types = F)
  
  if (files_2011[i] == "2002-10-24-main.csv") {
    thisFile$name.id <- as.character(thisFile$name.id)
  }
  
  list <- thisFile %>% select(name, name.id, electorate, party, uniqueID) %>% 
    filter(str_detect(name, "SPEAKER|Speaker")) %>% 
    unique() %>% 
    bind_rows(., list)
}

for (i in 1:length(files_1999)) {
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-1998-1999/", files_1999[i]), show_col_types = F)
  
  list <- thisFile %>% select(name, name.id, electorate, party, uniqueID) %>% 
    filter(str_detect(name, "SPEAKER|Speaker")) %>% 
    unique() %>% 
    bind_rows(., list)
}


# filter out complete rows, and filter out general names
list <- list %>% filter(is.na(name.id) | is.na(uniqueID)) %>% 
  unique() %>% 
  filter(name!="Mr SPEAKER" & name!="Mr DEPUTY SPEAKER" & name!="ACTING SPEAKER, Mr" & name!="DACTING SPEAKER, Mr" & name!="DEPUTY SPEAKER, Madam" & name!="ACTING SPEAKER, The" & 
           name!="Madam DEPUTY SPEAKER" & name!="SPEAKER" & name!="SPEAKER, The" & name!="DEPUTY SPEAKER" & name!="ACTING SPEAKER" & name!="DEPUTY SPEAKER," & name!="DEPUTY SPEAKER, The"& 
           name!="DEPUTY SPEAKER, Tee" & name!="Mr SPEAKER," & name!="The SPEAKER" & name!="DEPUTY SPEAKER, Mr" &
           name!="SPEAKER, Mr" & name!="The DEPUTY SPEAKER" & name!="SPEAKER, Madam" & name!="The SPEAKER," & name!="The DEPUTY SPEAKER, The" & name!="Mr Speaker—Mr ABBOTT")

# extract everyone's surnames
list <- list %>% mutate(surname = ifelse(str_detect(name, "[[:lower:]] \\(The DEPUTY SPEAKER\\)$"), str_extract(name, "^.{1,15}(?=, )"), NA),
                surname = ifelse(str_detect(name, "[[:lower:]] \\(Madam DEPUTY SPEAKER\\)$"), str_extract(name, "^.{1,15}(?=, )"), surname),
                surname = ifelse(str_detect(name, "[[:lower:]] \\(Mr DEPUTY SPEAKER\\)$"), str_extract(name, "^.{1,15}(?=, )"), surname),
                surname = ifelse(str_detect(name, "[[:lower:]] \\(Mr ACTING SPEAKER\\)$"), str_extract(name, "^.{1,15}(?=, )"), surname),
                surname = ifelse(str_detect(name, "DEPUTY SPEAKER \\("), str_extract(name, "(?<![[:upper:]]')[[:upper:]][[:alpha:]]{1,30}(?=\\))"), surname),
                surname = ifelse(str_detect(name, " MP \\("), str_extract(name, "^[[:alpha:]]+"), surname),
                surname = ifelse(name=="DEPUTY SPEAKER, Mr (Mr Causley)", "Causley", surname),
                surname = ifelse(name=="The DEPUTY SPEAKER (Mr Llew O'Brien)", "O'Brien", surname),
                surname = ifelse(name=="The DEPUTY SPEAKER (Ms O'Neill)", "O'Neill", surname),
                surname = ifelse(name=="The DEPUTY SPEAKER (Mrs D'Ath)", "D'Ath", surname),
                surname = ifelse(str_detect(name, "’Ath, Yvette"), "D'Ath", surname))

# filter death date for time frame we're looking at to remove repeated surname (McLeay)
# join list with speaker lookup to get uniqueID etc info for each person
# add flag for elected deputy speakers, because the rest of people on this list were just on the speaker panel
list <- speaker_lookup %>% filter(deathDate>="1998-01-01" | is.na(deathDate)) %>% 
  left_join(list %>% select(name, surname), ., by="surname") %>% 
  select(name, firstName, surname, uniqueID, phid, displayName, party, electorate, gender, title, deathDate) %>% 
  mutate(electedDeputy = ifelse(!is.na(uniqueID), 1, 0))

# only keep completed, unique rows
list <- list %>% filter(!is.na(uniqueID)) %>% unique()

# export lookup table
write.csv(list, "/Volumes/Verbatim/speaker_lookup.csv", row.names = FALSE)

################# new stuff - read in lookup table #################
member_lookup <- read_csv("/Volumes/Verbatim/member_lookup.csv", show_col_types = F)

# i need to make a table that has the unique ID, phid, and party with the dates so that we can account for changes over time
party <- AustralianPoliticians::get_auspol('allbyparty')
mps <- AustralianPoliticians::get_auspol('mps')

# create party lookup by filtering out partyTo earlier than our earliest Hansard
# also filter out people who died before our earliest Hansard
party_lookup <- party %>% select(uniqueID, partyAbbrev, partyName, partyFrom, partyTo) %>% 
  left_join(., member_lookup %>% select(uniqueID, phid), by="uniqueID") %>% 
  filter(!is.na(phid)) %>% 
  filter(partyTo >= "1998-01-01" | is.na(partyTo)) %>% 
  left_join(., select(all, c(deathDate, uniqueID)), by="uniqueID") %>% 
  filter(deathDate>="1998-01-01" | is.na(deathDate))

electorate_lookup <- mps %>% select(uniqueID, division, mpFrom, mpTo) %>% 
  left_join(., member_lookup %>% select(uniqueID, phid), by="uniqueID") %>% 
  filter(!is.na(phid)) %>% 
  filter(mpTo >= "1998-01-01" | is.na(mpTo)) %>% 
  left_join(., select(all, c(deathDate, uniqueID)), by="uniqueID") %>% 
  filter(deathDate>="1998-01-01" | is.na(deathDate))

# export lookup tables
write.csv(party_lookup, "/Volumes/Verbatim/party_lookup.csv", row.names = FALSE)
write.csv(electorate_lookup, "/Volumes/Verbatim/electorate_lookup.csv", row.names = FALSE)
