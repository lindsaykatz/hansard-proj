### preamble ------------------------------------------------------------------
# script purpose: fix any outstanding issues in the data set that were
#                 identified with manual checks of the corpus.

### environment setup ---------------------------------------------------------
# import necessary libraries
library(tidyverse)
library(arrow)

### data import ---------------------------------------------------------------
# import corpus
corpus <- read_parquet("hansard-corpus/corpus_1998_to_2025.parquet")

# read in AusPol dataset
aus_pol <- AustralianPoliticians::get_auspol('all')

# read in ausPH dataset, filter for people in corpus, select vars of interest
aus_ph <- ausPH::getIndividuals() %>% 
  filter(PHID %in% corpus$name.id) 

# add parties to aus_ph
aus_ph <- left_join(aus_ph %>% select(name.id=PHID, party_ausPH=Party, 
                                      electorate_ausPH=Electorate), 
                    ausPH::getParties() %>% 
                      select(party_ausPH=Name, party_ausPH_abbrev=Abbrev),
                    by="party_ausPH")

### ensure all uniqueIDs are assigned before merge ----------------------------
# identify list of people in corpus with missing uniqueID but non-missing gender
names_no_uniqueID <- corpus %>% 
  filter(is.na(uniqueID), !is.na(gender)) %>% 
  mutate(name = str_remove(name, " \\(The DEPUTY SPEAKER\\)")) %>% 
  distinct(name) %>% 
  pull(name)

# there are 4 people missing a uniqueID who should have one
aus_pol %>% 
  filter(displayName %in% names_no_uniqueID)

# fix member/senator flags and unique ID for those 4 people
# also identified that Anne Urquhart and Ben Small were elected as members this year
# need to update the flags for them
corpus <- corpus %>% 
  mutate(
    uniqueID = case_when(
      str_detect(name, "Marino, Nola") ~ "Marino1954",
      str_detect(name, "Small, Ben") ~ "Small1988",
      str_detect(name, "Payne, Marise") ~ "Payne1964",
      str_detect(name, "Urquhart, Anne") ~ "Urquhart1957",
      .default = uniqueID),
    member = case_when(
      str_detect(name, "Marino, Nola") ~ as.factor(1),
      str_detect(name, "Small, Ben") ~ as.factor(1),
      str_detect(name, "Payne, Marise") ~ as.factor(0),
      str_detect(name, "Urquhart, Anne") ~ as.factor(1),
      .default = member),
    senator = case_when(
      str_detect(name, "Marino, Nola") ~ as.factor(0),
      str_detect(name, "Small, Ben") ~ as.factor(1),
      str_detect(name, "Payne, Marise") ~ as.factor(1),
      str_detect(name, "Urquhart, Anne") ~ as.factor(1),
      .default = senator))

### add clean displayName column ----------------------------------------------
# add clean displayName column to corpus based on uniqueID
corpus <- left_join(corpus, aus_pol %>% select(uniqueID, displayName), 
                    by="uniqueID")

# look at cases where name.id is not NA and displayName is NA
corpus %>% filter(is.na(displayName) & !is.na(name.id)) %>% 
  distinct(name, name.id)

# all the new MPs from 2022 onward, can safely remove The DEPUTY SPEAKER and
# paste names in displayName column
corpus <- corpus %>% 
  mutate(displayName = case_when(
    is.na(displayName) & !is.na(name.id) ~ str_squish(
      str_remove(name, "\\(The DEPUTY SPEAKER\\)")),
    .default = displayName))

### manual fixes --------------------------------------------------------------
# fix Lidia Thorpe's name
corpus <- corpus %>% 
  mutate(name = case_when(name=="Lidia Thorpe" ~ "Thorpe, Lidia",
                          .default = name),
         displayName = case_when(displayName=="Lidia Thorpe" ~ "Thorpe, Lidia",
                                 .default = displayName))

# ensure consistent capitalization of "member"
corpus <- corpus %>% 
  mutate(name = case_when(
    str_detect(name, "Member") ~ str_replace(name, "Member", "member"),
    .default = name)) 

# 2000-08-30, Janice Crosio called "Madam SPEAKER", when she is Deputy Speaker
corpus <- corpus %>% 
  mutate(name = case_when(
    date=="2000-08-30" & name=="Madam SPEAKER" ~ "Madam DEPUTY SPEAKER",
    .default = name))

# 1998-05-26, wrong name ID for Kim Beazley, need to manually fix
corpus <- corpus %>% 
  mutate(name = case_when(name=="Beazley, Kim (Sr)" & 
                            date=="1998-05-26" ~ "Beazley, Kim (Jr)",
                          .default = name),
         displayName = case_when(displayName=="Beazley, Kim (Sr)" &
                                   date=="1998-05-26" ~ "Beazley, Kim (Jr)",
                                 .default = displayName),
         uniqueID = case_when(uniqueID=="Beazley1917" & 
                                date=="1998-05-26" ~ "Beazley1948",
                              .default = uniqueID),
         name.id = case_when(uniqueID=="Beazley1948" ~ "PE4",
                             .default = name.id),
         electorate = case_when(name.id=="PE4" & date=="1998-05-26" ~ "Brand",
                                .default = electorate),
         party = case_when(date=="1998-05-26" & name.id=="PE4" ~ "ALP",
                           .default = party))

# 2023-02-14 someone was quoting Michael Pezzullo and it was incorrectly split
# as if part of the speech, need to fix that manually

# these are the rows involved
corpus %>% 
  filter(date=="2023-02-14" & speech_no==69 & order %in% c(218, 219, 220, 221))

# combine body into one row (all part of Anthony Albanese's speech)
pezzullo_row <- corpus %>% 
  filter(date=="2023-02-14" & speech_no==69 & order %in% c(218, 219, 220)) %>% 
  mutate(body = paste0(body, collapse = " ")) %>% 
  filter(!(date=="2023-02-14" & order %in% c(219,220)))

# add corrected row, update order variable
corpus <- corpus %>%
  filter(!(date=="2023-02-14" & order %in% c(218,219,220))) %>%
  # add row back in correct spot
  add_row(pezzullo_row, .before = which(.$date=="2023-02-14" & .$order==221)) %>% 
  # update ordering to reflect two less rows
  mutate(order = case_when(date=="2023-02-14" & order>218 ~ order-2, 
                           .default = order))

# ensure we fixed the issues above
stopifnot(corpus %>% filter(str_detect(name, "Pezzullo")) %>% nrow()==0,
          corpus %>% filter(str_detect(name, "Beazley, Kim \\(Sr\\)")) %>% nrow()==0)

# rows where the name is "The SPEAKER" but the body starts with "Mr Speaker,..."
corpus %>% 
  filter(name=="The SPEAKER" & str_detect(body, "^Mr Speaker,")) %>% 
  distinct(date, order, body)

# fix all those identified above
corpus <- corpus %>% 
  mutate(name = case_when(order=="509" & date=="2002-06-20" ~ "Abbott, Tony",
                          order=="439" & date=="2003-08-20" ~ "Cameron, Ross",
                          order=="255" & date=="2004-03-11" ~ "Abbott, Tony",
                          order=="239" & date=="2004-08-09" ~ "Nelson, Brendan",
                          order=="317" & date=="2006-10-17" ~ "Truss, Warren",
                          order=="259" & date=="2007-08-16" ~ "Ruddock, Philip",
                          order=="300" & date=="2010-02-03" ~ "Emerson, Craig",
                          .default = name),
         displayName = case_when(order=="509" & date=="2002-06-20" ~ "Abbott, Tony",
                                 order=="439" & date=="2003-08-20" ~ "Cameron, Ross",
                                 order=="255" & date=="2004-03-11" ~ "Abbott, Tony",
                                 order=="239" & date=="2004-08-09" ~ "Nelson, Brendan",
                                 order=="317" & date=="2006-10-17" ~ "Truss, Warren",
                                 order=="259" & date=="2007-08-16" ~ "Ruddock, Philip",
                                 order=="300" & date=="2010-02-03" ~ "Emerson, Craig",
                                 .default = displayName),
         uniqueID = case_when(order=="509" & date=="2002-06-20" ~ "Abbott1957",
                              order=="439" & date=="2003-08-20" ~ "Cameron1965",
                              order=="255" & date=="2004-03-11" ~ "Abbott1957",
                              order=="239" & date=="2004-08-09" ~ "Nelson1958",
                              order=="317" & date=="2006-10-17" ~ "Truss1948",
                              order=="259" & date=="2007-08-16" ~ "Ruddock1943",
                              order=="300" & date=="2010-02-03" ~ "Emerson1954",
                              .default = uniqueID),
         name.id = case_when(order=="509" & date=="2002-06-20" ~ "EZ5",
                             order=="439" & date=="2003-08-20" ~ "3K6",
                             order=="255" & date=="2004-03-11" ~ "EZ5",
                             order=="239" & date=="2004-08-09" ~ "RW5",
                             order=="317" & date=="2006-10-17" ~ "GT4",
                             order=="259" & date=="2007-08-16" ~ "0J4",
                             order=="300" & date=="2010-02-03" ~ "83V",
                             .default = name.id),
         electorate = case_when(order=="509" & date=="2002-06-20" ~ "Warringah",
                                order=="439" & date=="2003-08-20" ~ "Parramatta",
                                order=="255" & date=="2004-03-11" ~ "Warringah",
                                order=="239" & date=="2004-08-09" ~ "Bradfield",
                                order=="317" & date=="2006-10-17" ~ "Wide Bay",
                                order=="259" & date=="2007-08-16" ~ "Berowra",
                                order=="300" & date=="2010-02-03" ~ "Rankin",
                                .default = electorate),
         party = case_when(order=="509" & date=="2002-06-20" ~ "LP",
                           order=="439" & date=="2003-08-20" ~ "LP",
                           order=="255" & date=="2004-03-11" ~ "LP",
                           order=="239" & date=="2004-08-09" ~ "LP",
                           order=="317" & date=="2006-10-17" ~ "Nats",
                           order=="259" & date=="2007-08-16" ~ "LP",
                           order=="300" & date=="2010-02-03" ~ "ALP",
                           .default = party),
         partyfacts_id = case_when(order=="509" & date=="2002-06-20" ~ 486,
                                   order=="439" & date=="2003-08-20" ~ 486,
                                   order=="255" & date=="2004-03-11" ~ 486,
                                   order=="239" & date=="2004-08-09" ~ 486,
                                   order=="317" & date=="2006-10-17" ~ 1743,
                                   order=="259" & date=="2007-08-16" ~ 486,
                                   order=="300" & date=="2010-02-03" ~ 424,
                                   .default = partyfacts_id),
         gender = case_when(order=="509" & date=="2002-06-20" ~ "male",
                            order=="439" & date=="2003-08-20" ~ "male",
                            order=="255" & date=="2004-03-11" ~ "male",
                            order=="239" & date=="2004-08-09" ~ "male",
                            order=="317" & date=="2006-10-17" ~ "male",
                            order=="259" & date=="2007-08-16" ~ "male",
                            order=="300" & date=="2010-02-03" ~ "male",
                            .default = gender),
         member = case_when(order=="509" & date=="2002-06-20" ~ factor(1),
                            order=="439" & date=="2003-08-20" ~ factor(1),
                            order=="255" & date=="2004-03-11" ~ factor(1),
                            order=="239" & date=="2004-08-09" ~ factor(1),
                            order=="317" & date=="2006-10-17" ~ factor(1),
                            order=="259" & date=="2007-08-16" ~ factor(1),
                            order=="300" & date=="2010-02-03" ~ factor(1),
                            .default = member),
         senator = case_when(order=="509" & date=="2002-06-20" ~ factor(0),
                             order=="439" & date=="2003-08-20" ~ factor(0),
                             order=="255" & date=="2004-03-11" ~ factor(0),
                             order=="239" & date=="2004-08-09" ~ factor(0),
                             order=="317" & date=="2006-10-17" ~ factor(0),
                             order=="259" & date=="2007-08-16" ~ factor(0),
                             order=="300" & date=="2010-02-03" ~ factor(0),
                             .default = senator))

# 2025-02-12 two rows requiring manual split identified
# identify order and speech numbers associated with first error (Ross Vasta)
corpus %>% filter(date=="2025-02-12", 
                  str_detect(body,"^Prior to me speaking on the Early Childhood Education and Care")) %>% 
  select(order, speech_no)

# recode interject as a factor
corpus <- corpus %>% mutate(interject=as.factor(interject))

# for all rows after that one, we need to add a value of 1 to the speech_no and
# order values, since we will be adding a new row/speech that was missed
corpus <- corpus %>% 
  mutate(order = case_when(date=="2025-02-12" & order>213 ~ order+1, 
                           .default = order),
         speech_no = case_when(date=="2025-02-12" & speech_no>117 ~ speech_no+1,
                               .default = speech_no)) %>% 
  # add row for Ross Vasta's comment that was missed due to incorrect XML nesting
  add_row(date=as.Date("2025-02-12"),
          name="Vasta, Ross (The DEPUTY SPEAKER)",
          order=214, 
          speech_no = 118, 
          page.no = 845,
          time.stamp = hms::as_hms("19:30:00"),
          name.id="E0D",
          electorate="Bonner",
          party="LP",
          in.gov=NA, first.speech=NA,
          body="It being now 7.30 pm, I propose the question: That the House do now adjourn.",
          fedchamb_flag=factor(0), question=factor(0), answer=factor(0),
          q_in_writing=factor(0), div_flag=factor(0),
          partyfacts_id=486,
          uniqueID="Vasta1966",
          gender="male",
          member=factor(1), senator=factor(0), interject=as.factor(0),
          displayName="Vasta, Ross",
          .after = which(corpus$date=="2025-02-12" & corpus$order==213))

# same approach to separate the row with body "Madam Deputy Speaker-  A division having been called in the House of Representatives-\n                    Sitting suspended from 10:23 to 10:29"
# identify order and speech numbers associated with second error
corpus %>% filter(date=="2025-02-12", 
                  str_detect(body,"Madam Deputy Speaker-  A division having been called in the House of Representatives-\n                    Sitting suspended from 10:23 to 10:29")) %>% 
  select(order, speech_no)

# add two new rows, fix body of original one
corpus <- corpus %>% 
  # won't be treated as a new speech so don't need to change the speech number
  mutate(order = case_when(date=="2025-02-12" & order>237 ~ order+2, 
                           .default = order)) %>% 
  # add rows for stage directions to be manually separated out
  add_row(date=c(as.Date("2025-02-12"),as.Date("2025-02-12")),
          name=c("Stage direction","Stage direction"),
          order=c(238,239), 
          speech_no = c(137,137), 
          page.no = c(859, 859),
          time.stamp = NA,
          name.id=NA,
          electorate=NA,
          party=NA,
          in.gov=NA, first.speech=NA,
          body=c("A division having been called in the House of Representatives-",
                 "Sitting suspended from 10:23 to 10:29"),
          fedchamb_flag=c(factor(1),factor(1)), 
          question=c(factor(0),factor(0)), 
          answer=c(factor(0),factor(0)),
          q_in_writing=c(factor(0),factor(0)),
          div_flag=c(factor(0),factor(0)),
          partyfacts_id=NA,
          uniqueID=NA,
          gender=NA,
          member=NA, senator=NA, interject=as.factor(0),
          displayName=NA,
          .after = which(corpus$date=="2025-02-12" & corpus$order==237)) %>% 
  mutate(body = case_when(
    date=="2025-02-12" & order==237 ~ "Madam Deputy Speaker-",
    .default = body))

# same approach to separate the row with body "Sitting suspended from 10:36 to 10:53"
# identify order and speech numbers associated with second error
corpus %>% filter(date=="2025-02-12", 
                  str_detect(body,"Sitting suspended from 10:36 to 10:53")) %>% 
  select(order, speech_no)

# add two new rows, fix body of original one
corpus <- corpus %>% 
  # won't be treated as a new speech so don't need to change the speech number
  mutate(order = case_when(date=="2025-02-12" & order>240 ~ order+2, 
                           .default = order)) %>% 
  # add rows for stage directions to be manually separated out
  add_row(date=c(as.Date("2025-02-12"),as.Date("2025-02-12")),
          name=c("Stage direction","Stage direction"),
          order=c(241,242), 
          speech_no = c(137,137), 
          page.no = c(859, 859),
          time.stamp = NA,
          name.id=NA,
          electorate=NA,
          party=NA,
          in.gov=NA, first.speech=NA,
          body=c("A division having been called in the House of Representatives-",
                 "Sitting suspended from 10:36 to 10:53"),
          fedchamb_flag=c(factor(1),factor(1)), 
          question=c(factor(0),factor(0)), 
          answer=c(factor(0),factor(0)),
          q_in_writing=c(factor(0),factor(0)),
          div_flag=c(factor(0),factor(0)),
          partyfacts_id=NA,
          uniqueID=NA,
          gender=NA,
          member=NA, senator=NA, interject=as.factor(0),
          displayName=NA,
          .after = which(corpus$date=="2025-02-12" & corpus$order==240)) %>% 
  mutate(body = case_when(
    date=="2025-02-12" & order==240 ~ str_remove(body, "  A division having been called in the House of Representatives-\n                    Sitting suspended from 10:36 to 10:53"),
    .default = body))

# 2020-06-12, part of a question in questions in writing is incorrectly nested
# within the corresponding answer. need to fix that manually

# store combined question body in one object to paste
# 2020-06-12, part of a question in questions in writing is incorrectly nested
# within the corresponding answer. need to fix that manually
body_20200612_burns <- corpus %>% 
  filter(date=="2020-06-12", q_in_writing==1) %>% 
  slice(1:2) %>% summarise(body = paste0(body, collapse = " ")) %>% pull(body)

# note: since q in writing is the end of the day, we can update the order for
# just the effected row and it won't impact the rest of the day's order
#corpus %>% filter(date=="2020-06-12") %>% tail()

# manually fix issue defined above
corpus <- corpus %>% 
  mutate(body = case_when(date=="2020-06-12" & order==283 ~ body_20200612_burns,
                          .default = body)) %>% 
  filter(!(date=="2020-06-12" & order==284)) %>% 
  mutate(order = case_when(date=="2020-06-12" & order==285 ~ 284,
                           .default = order),
         answer = case_when(date=="2020-06-12" & order==284 ~ as.factor(1),
                            .default = answer))

### fix party column ----------------------------------------------------------
auspol_party <- AustralianPoliticians::get_auspol("allbyparty") %>% 
  filter(uniqueID %in% unique(corpus$uniqueID)) %>% 
  select(uniqueID, partyAbbrev, partyName, partyFrom, partyTo) %>% 
  mutate(partyAbbrev = ifelse(partyAbbrev=="NPA", "NP", partyAbbrev),
         partyName = ifelse(partyAbbrev=="NP", "The Nationals", partyName))

# MPs who have only ever been in one party
mps_one_party <- auspol_party %>% 
  distinct(uniqueID, partyAbbrev, partyName) %>% 
  group_by(uniqueID) %>% filter(n()==1) %>% ungroup() %>% 
  select(uniqueID, partyAbbrev, partyName)

# MPs who have been in more than one party
mps_multi_party <- auspol_party %>% 
  group_by(uniqueID) %>% filter(n_distinct(partyAbbrev)>1) %>% ungroup()

# add correct/standardized party name and abbr columns to corpus
corpus_fixed <- corpus %>% 
  left_join(., mps_one_party, by="uniqueID") %>% 
  mutate(partyAbbrev = case_when(
    # Banks
    uniqueID=="Banks1962" & date<"2018-11-27" ~ "LIB",
    uniqueID=="Banks1962" & date>="2018-11-27" ~ "IND",
    # Bradford
    uniqueID=="Bradford1946" & date<"1998-04-01" ~ "LIB",
    uniqueID=="Bradford1946" & date>="1998-04-01" ~ "CDP",
    # Campbell
    uniqueID=="Campbell1939" & date<"1995-12-01" ~ "ALP",
    uniqueID=="Campbell1939" & date>="1995-12-01" ~ "IND",
    # Filing
    uniqueID=="Filing1955" & date<"1995-06-01" ~ "LIB",
    uniqueID=="Filing1955" & date>="1995-06-01" ~ "IND",
    # Hanson
    uniqueID=="Hanson1954" & date<"1997-06-01" ~ "IND",
    uniqueID=="Hanson1954" & date>="1997-06-01" ~ "PHON",
    # Hicks
    uniqueID=="Hicks1940" & date<"1982-10-17" ~ "NCP",
    uniqueID=="Hicks1940" & date>="1982-10-17" ~ "NP",
    # Johnson
    uniqueID=="Johnson1970" & date<"2010-05-01" ~ "LIB",
    uniqueID=="Johnson1970" & date>="2010-05-01" ~ "IND",
    # Katter
    uniqueID=="Katter1945" & date<"2001-07-01" ~ "NP",
    uniqueID=="Katter1945" & date>="2001-07-01" & date<"2011-09-27" ~ "IND",
    uniqueID=="Katter1945" & date>="2011-09-27" ~ "KAP",
    # Kernot
    uniqueID=="Kernot1948" & date<"1997-10-15" ~ "AD",
    uniqueID=="Kernot1948" & date>="1997-10-15" ~ "ALP",
    # King
    uniqueID=="King1952" & date<"2004-09-01" ~ "LIB",
    uniqueID=="King1952" & date>="2004-09-01" ~ "IND",
    # McGauran
    uniqueID=="McGauran1957" & date<"2003-10-11" ~ "NP",
    uniqueID=="McGauran1957" & date>="2003-10-11" & date<"2006-02-01" ~ "NP",
    uniqueID=="McGauran1957" & date>="2006-02-01" ~ "LIB",
    # O’Connor
    uniqueID=="OConnor1947" & date<"2007-10-01" ~ "ALP",
    uniqueID=="OConnor1947" & date>="2007-10-01" ~ "IND",
    # Quick
    uniqueID=="Quick1941" & date<"2007-08-01" ~ "ALP",
    uniqueID=="Quick1941" & date>="2007-08-01" ~ "IND",
    # Rocher
    uniqueID=="Rocher1936" & date<"1995-08-01" ~ "LIB",
    uniqueID=="Rocher1936" & date>="1995-08-01" ~ "IND",
    # Sharkie
    uniqueID=="Sharkie1972" & date<"2018-04-10" ~ "NXT",
    uniqueID=="Sharkie1972" & date>="2018-04-10" ~ "CA",
    # Sinclair
    uniqueID=="Sinclair1929" & date<"1975-05-03" ~ "CP",
    uniqueID=="Sinclair1929" & date>="1975-05-03" & date<"1982-10-17" ~ "NCP",
    uniqueID=="Sinclair1929" & date>="1982-10-17" ~ "NP",
    # Slipper
    uniqueID=="Slipper1950" & date<"1987-07-11" ~ "NP",
    uniqueID=="Slipper1950" & date>="1987-07-11" & date<"2011-11-24" ~ "LIB",
    uniqueID=="Slipper1950" & date>="2011-11-24" ~ "IND",
    # Smith
    uniqueID=="Smith1950" & date<"1998-05-01" ~ "LIB",
    uniqueID=="Smith1950" & date>="1998-05-01" ~ "IND",
    # Theophanous
    uniqueID=="Theophanous1946" & date<"2000-04-01" ~ "ALP",
    uniqueID=="Theophanous1946" & date>="2000-04-01" ~ "IND",
    # Thomson
    uniqueID=="Thomson1964" & date<"2012-04-01" ~ "ALP",
    uniqueID=="Thomson1964" & date>="2012-04-01" ~ "IND",
    # Zammit
    uniqueID=="Zammit1941" & date<"1998-02-01" ~ "LIB",
    uniqueID=="Zammit1941" & date>="1998-02-01" ~ "IND",
    .default = partyAbbrev
  )) %>% 
  group_by(partyAbbrev) %>% 
  fill(partyName, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(partyName = case_when(
    partyAbbrev=="CA" ~ "Centre Alliance",
    partyAbbrev=="CDP" ~ "Christian Democratic Party",
    partyAbbrev=="PHON" ~ "Pauline Hansons One Nation",
    partyAbbrev=="KAP" ~ "Katters Australian Party",
    partyAbbrev=="NXT" ~ "Nick Xenophon Team",
    .default = partyName)) 

# the new MPs not in AusPol yet needed to be dealt with manually - I checked
# each one to ensure only ever in one party
corpus_fixed <- corpus_fixed %>% 
  mutate(
    partyAbbrev = case_when(
      displayName == "Abdo, Basem" ~ "ALP",
      displayName == "Aldred, Mary" ~ "LIB",
      displayName == "Ambihaipahar, Ash" ~ "ALP",
      displayName == "Ananda-Rajah, Michelle" ~ "ALP",
      displayName == "Bates, Stephen" ~ "GRN",
      displayName == "Batt, David" ~ "NP",
      displayName == "Belyea, Jodie" ~ "ALP",
      displayName == "Berry, Carol" ~ "ALP",
      displayName == "Birrell, Sam" ~ "NP",
      displayName == "Boele, Nicolette" ~ "IND",
      displayName == "Boyce, Colin" ~ "NP",
      displayName == "Briskey, Jo" ~ "ALP",
      displayName == "Burnell, Matt" ~ "ALP",
      displayName == "Byrnes, Alison" ~ "ALP",
      displayName == "Caldwell, Cameron" ~ "LIB",
      displayName == "Campbell, Julie-Ann" ~ "ALP",
      displayName == "Chaffey, Jamie" ~ "NP",
      displayName == "Chandler-Mather, Max" ~ "GRN",
      displayName == "Chaney, Kate" ~ "IND",
      displayName == "Charlton, Andrew" ~ "ALP",
      displayName == "Clutterham, Claire" ~ "ALP",
      displayName == "Coffey, Renee" ~ "ALP",
      displayName == "Comer, Emma" ~ "ALP",
      displayName == "Cook, Kara" ~ "ALP",
      displayName == "Cook, Trish" ~ "ALP",
      displayName == "Daniel, Zoe" ~ "IND",
      displayName == "Doyle, Mary" ~ "ALP",
      displayName == "Fernando, Cassandra" ~ "ALP",
      displayName == "France, Ali" ~ "ALP",
      displayName == "French, Tom" ~ "ALP",
      displayName == "Garland, Carina" ~ "ALP",
      displayName == "Gregg, Matt" ~ "ALP",
      displayName == "Holzberger, Rowan" ~ "ALP",
      displayName == "Jarrett, Madonna" ~ "ALP",
      displayName == "Jordan-Baird, Alice" ~ "ALP",
      displayName == "Kennedy, Simon" ~ "LIB",
      displayName == "Lawrence, Tania" ~ "ALP",
      displayName == "Laxale, Jerome" ~ "ALP",
      displayName == "Le, Dai" ~ "IND",
      displayName == "Lim, Sam" ~ "ALP",
      displayName == "Marino, Nola" ~ "LIB",
      displayName == "Mascarenhas, Zaneta" ~ "ALP",
      displayName == "McKenzie, Zoe" ~ "LIB",
      displayName == "Miller-Frost, Louise" ~ "ALP",
      displayName == "Moncrieff, David" ~ "ALP",
      displayName == "Ng, Gabriel" ~ "ALP",
      displayName == "Payne, Marise" ~ "LIB",
      displayName == "Penfold, Alison" ~ "NP",
      displayName == "Pike, Henry" ~ "LIB",
      displayName == "Rae, Sam" ~ "ALP",
      displayName == "Rebello, Leon" ~ "LIB",
      displayName == "Reid, Gordon" ~ "ALP",
      displayName == "Repacholi, Dan" ~ "ALP",
      displayName == "Roberts, Tracey" ~ "ALP",
      displayName == "Ryan, Monique" ~ "IND",
      displayName == "Scamps, Sophie" ~ "IND",
      displayName == "Scrymgour, Marion" ~ "ALP",
      displayName == "Sitou, Sally" ~ "ALP",
      displayName == "Small, Ben" ~ "LIB",
      displayName == "Smith, Matt" ~ "ALP",
      displayName == "Soon, Zhi" ~ "ALP",
      displayName == "Spender, Allegra" ~ "IND",
      displayName == "Teesdale, Jess" ~ "ALP",
      displayName == "Tink, Kylea" ~ "IND",
      displayName == "Urquhart, Anne" ~ "ALP",
      displayName == "Venning, Tom" ~ "LIB",
      displayName == "Violi, Aaron" ~ "LIB",
      displayName == "Ware, Jenny" ~ "LIB",
      displayName == "Watson-Brown, Elizabeth" ~ "GRN",
      displayName == "White, Rebecca" ~ "ALP",
      displayName == "Willcox, Andrew" ~ "NP",
      displayName == "Witty, Sarah" ~ "ALP",
      displayName == "Wolahan, Keith" ~ "LIB",
      .default = partyAbbrev)) %>% 
  group_by(partyAbbrev) %>% 
  fill(partyName, .direction = "downup") %>% 
  ungroup()

### anna burke issue found ----------------------------------------------------
# identified cases after her retirement where her name is used incorrectly in 
# the place of Tony Burke
corpus_fixed <- corpus_fixed %>% 
  mutate(name = case_when(name=="Burke, Anna" & date>"2016-05-09" ~ "Burke, Tony",
                          .default = name)) %>% 
  mutate(name.id = case_when(name=="Burke, Tony" ~ "DYW",
                             .default = name.id),
         electorate = case_when(name=="Burke, Tony" ~ "Watson",
                                .default = electorate),
         uniqueID = case_when(name=="Burke, Tony" ~ "Burke1969",
                              .default = uniqueID),
         gender = case_when(name=="Burke, Tony" ~ "male",
                            .default = gender),
         member = case_when(name=="Burke, Tony" ~ factor(1),
                            .default = member),
         senator = case_when(name=="Burke, Tony" ~ factor(0),
                             .default = senator),
         displayName = case_when(name=="Burke, Tony" ~ "Burke, Tony",
                                 .default = displayName),
         partyAbbrev = case_when(name=="Burke, Tony" ~ "ALP",
                                 .default = partyAbbrev),
         partyName = case_when(name=="Burke, Tony" ~ "Australian Labor Party",
                               .default = partyName))

### fix electorate column -----------------------------------------------------
# recode UNKNOWN electorates to NA
corpus_fixed <- corpus_fixed %>% 
  mutate(electorate = ifelse(electorate=="UNKNOWN", NA, electorate))

# approach: create lookup table with correct electorate for every MP, except for
# MPs who have been affiliated with more than one distinct electorate over time

# import lookup table manually created
electorates_corrected <- readxl::read_xlsx(
  "additional_data/multiple_electorates_to_fix.xlsx") %>% 
  # filter for people only ever associated with one electorate
  distinct(displayName, correct_electorate) %>% 
  group_by(displayName) %>% 
  filter(n()==1 & correct_electorate!="CHECK DATE") %>% 
  ungroup()

# for MPs with two distinct electorates in hansard where one is NA, fill those,
# and add to list of MPs only ever associated with one electorate
electorates_corrected <- corpus_fixed %>% 
  distinct(displayName, electorate) %>% 
  filter(!is.na(displayName)) %>% 
  group_by(displayName) %>% 
  filter(n()==2 & any(is.na(electorate))) %>% 
  fill(electorate, .direction = "downup") %>% 
  ungroup() %>% 
  rename(correct_electorate = electorate) %>% 
  bind_rows(electorates_corrected) %>% 
  distinct() %>% 
  arrange(displayName)

# lastly, we need to consider MPs affiliated with more than one electorate in
# their career. export a table of those which will be manually filled in
# corpus_fixed %>%
#   distinct(displayName, electorate, date) %>%
#   filter(!is.na(displayName)) %>%
#   left_join(electorates_corrected, by="displayName") %>%
#   arrange(displayName) %>%
#   filter(is.na(correct_electorate)) %>%
#   group_by(displayName) %>%
#   filter(n_distinct(electorate)>1) %>%
#   group_by(displayName, electorate) %>%
#   summarise(dates_found = paste0(date, collapse="; ")) %>%
#   writexl::write_xlsx("multiple_electorates_with_dates.xlsx")

# import completed table exported above
multiple_electorates_with_dates <- readxl::read_xlsx(
  "additional_data/multiple_electorates_with_dates.xlsx",
  col_types = c("text","text","text","text")) %>% 
  separate_rows(dates_found, sep="; ") %>% 
  select(everything(), date=dates_found) %>% 
  mutate(date = as.Date(date))

# check that there is no overlap in displayName between electorates_corrected
# and multiple_electorates_with_dates
stopifnot(
  intersect(multiple_electorates_with_dates %>% distinct(displayName),
            electorates_corrected %>% distinct(displayName)) %>% 
    nrow() == 0,
  intersect(electorates_corrected %>% distinct(displayName),
            multiple_electorates_with_dates %>% distinct(displayName)) %>% 
    nrow() == 0
) ## all good

# next, left join multiple_electorates_with_dates with corpus
corpus_correct_electorates <- left_join(corpus_fixed, electorates_corrected, 
          by="displayName") %>% 
  left_join(., multiple_electorates_with_dates %>% select(-electorate) %>% distinct(), 
            by=c("displayName", "date")) 

# before proceeding, make sure there are no rows with a value for both
# correct_electorate and correct_electorate_for_that_date
stopifnot(
  corpus_correct_electorates %>% 
    filter(!is.na(correct_electorate) & 
             !is.na(correct_electorate_for_that_date)) %>% 
    nrow() == 0)

# combine correct_electorate and correct_electorate_for_that_date into one
corpus_correct_electorates <- corpus_correct_electorates %>% 
  mutate(correct_electorate = case_when(
    is.na(correct_electorate) & 
      !is.na(correct_electorate_for_that_date) ~ correct_electorate_for_that_date,
    is.na(correct_electorate) & 
      is.na(correct_electorate_for_that_date) &
      !is.na(electorate) ~ electorate,
    .default = correct_electorate)) %>% 
  select(-correct_electorate_for_that_date) 

# check for any MPs who do not have an electorate
corpus_correct_electorates %>% 
  distinct(displayName, electorate, correct_electorate) %>% 
  filter(is.na(correct_electorate))

# correct those
corpus_correct_electorates <- corpus_correct_electorates %>% 
  mutate(correct_electorate = case_when(
    uniqueID=="Kemp1944" ~ "Victoria",
    uniqueID=="Macdonald1945" ~ "Queensland",
    uniqueID=="Minchin1953" ~ "South Australia",
    uniqueID=="Nettle1973" ~ "New South Wales",
    uniqueID=="Payne1964" ~ "New South Wales",
    uniqueID=="Thorpe1973" ~ "Victoria",
    .default = correct_electorate))

# check again - all resolved
corpus_correct_electorates %>% 
  distinct(displayName, electorate, correct_electorate) %>% 
  filter(is.na(correct_electorate))

# identify anyone who has a displayName but does not have a uniqueID
names_missing_flags <- corpus_correct_electorates %>% 
  filter(!is.na(displayName) & is.na(uniqueID)) %>% 
  distinct(uniqueID, displayName, member, senator, electorate, correct_electorate) %>% 
  filter(is.na(member) | is.na(senator)) %>% 
  pull(displayName)

# check if any of the people identified above are in AusPol and for some reason
# are missing data - none, all good
aus_pol %>% filter(displayName %in% names_missing_flags) %>% 
  select(uniqueID, displayName, member, senator)

# check if there is anyone missing a correct electorate - none, all good
corpus_correct_electorates %>% 
  distinct(displayName, uniqueID, name.id, gender, member, senator, partyAbbrev, 
           correct_electorate) %>% 
  filter(!is.na(displayName)) %>% 
  arrange(displayName) %>% 
  filter(is.na(correct_electorate))

# look at cases where electorate is PO - these should all be changed to NA, not MPs
corpus_correct_electorates %>% filter(correct_electorate=="PO") %>% distinct(name)

# fix above
corpus_correct_electorates <- corpus_correct_electorates %>% 
  mutate(correct_electorate = case_when(correct_electorate=="PO" ~ NA,
                                        .default = correct_electorate))

# drop "electorate", use correct_electorate in it's place
corpus_correct_electorates <- corpus_correct_electorates %>% 
  select(-electorate) %>% 
  rename(electorate = correct_electorate) %>% 
  relocate(electorate, .after="name.id")

# same thing for party
corpus_correct_electorates  <- corpus_correct_electorates %>% 
  select(-party, -partyfacts_id) %>% 
  relocate(c(partyAbbrev, partyName), .after = "electorate")

# put displayName before name
corpus_correct_electorates <- corpus_correct_electorates %>% 
  relocate(displayName, .before = name)

### quick q flag fixes --------------------------------------------------------
corpus_correct_electorates <- corpus_correct_electorates %>% 
  mutate(
    question = case_when(
      question==0 & 
        str_detect(body, "^My question(s{0,1})( is| to| therefore| goes to| without notice is| again is|, again, is| also is|, which is addressed| refers to the| without notice goes| are directed| go to| relate to| are to)") ~ as.factor(1),
    .default = question),
    answer = case_when(
      answer==1 & 
        str_detect(body, "^My question(s{0,1})( is| to| therefore| goes to| without notice is| again is|, again, is| also is|, which is addressed| refers to the| without notice goes| are directed| go to| relate to| are to)") ~ as.factor(0),
      .default = answer))

### quick interject flag fixes ------------------------------------------------
corpus_correct_electorates <- corpus_correct_electorates %>% 
  mutate(
    interject = case_when(
      interject==1 & 
        str_detect(body, "^(Mr |Mrs |Dr ).{1,20} to move\\: That this House|^to move\\: That this House|^to present a Bill|^(Mr |Mrs |Dr ).{1,20} to present a Bill") ~ as.factor(0),
      .default = interject)
  )

### page number issue ---------------------------------------------------------
# identify rows where the previous page number is bigger than the current one
# corpus_correct_electorates %>%
#   select(date:page.no, body) %>%
#   group_by(date) %>%
#   mutate(previous_page = lag(page.no)) %>%
#   filter(previous_page > page.no) %>%
#   write_csv("page_number_issues.csv")

# to be dealt with later on, but for now, NA the page.no values for those rows
corpus_correct_electorates <- corpus_correct_electorates %>% 
  group_by(date) %>% 
  mutate(page.no_issue = ifelse(page.no < lag(page.no), 1, 0)) %>% 
  ungroup() %>% 
  mutate(page.no = case_when(page.no_issue==1 & !is.na(page.no) ~ NA,
                             .default = page.no)) %>% 
  select(-page.no_issue)

### manual name.id fixes -----------------------------------------------------
# check which name.ids in the corpus aren't in ausPH
corpus_correct_electorates %>% distinct(name.id, displayName) %>% 
  filter(!(name.id %in% c(ausPH::getIndividuals() %>% pull(PHID))))

# manually assign correct one
corpus_correct_electorates <- corpus_correct_electorates %>% 
  mutate(name.id = case_when(name.id=="80000" ~ "8E4",
                             name.id=="50000" ~ "5E4",
                             name.id=="30000" ~ "3E4",
                             .default = name.id))

### fix column classes --------------------------------------------------------
corpus_correct_electorates <- corpus_correct_electorates %>% 
  select(-c(in.gov, first.speech)) %>% 
  mutate(date = as.Date(date),
         time.stamp = hms::as_hms(time.stamp)) %>% 
  mutate(across(c(partyAbbrev, question, answer, q_in_writing, div_flag, gender, 
                  member, senator, fedchamb_flag, interject),
                ~ as.factor(.)))

### validation that all MPs present are actually members at that time ---------
# import auspol MPS
mps <-AustralianPoliticians::get_auspol("mps") %>% 
  distinct(uniqueID, mpFrom, mpTo) %>% 
  filter(uniqueID %in% unique(corpus_correct_electorates$uniqueID))

# fix error with John Alexander's second term dates
mps <- mps %>% 
  mutate(mpFrom = case_when(uniqueID=="Alexander1951" & 
                              mpFrom=="2018-12-16" ~ as.Date("2017-12-15"),
                            .default = mpFrom),
         mpTo = case_when(uniqueID=="Alexander1951" & 
                            is.na(mpTo) ~ as.Date("2022-04-10"),
                          .default = mpTo))

# look at cases where date detected is not in the mpFrom - mpTo date range
corpus_correct_electorates %>% 
  distinct(uniqueID, name, displayName, date, name.id) %>% 
  filter(!is.na(uniqueID)) %>% 
  left_join(., mps, by="uniqueID", relationship = "many-to-many") %>% 
  arrange(uniqueID) %>% 
  mutate(correct = (date >= mpFrom & date <= mpTo)) %>% 
  group_by(uniqueID, name, displayName, name.id, date) %>% 
  filter(!any(correct)) %>% ungroup()

### manually checked all of these
# John Moore is there as the Minister for Defence - correct
# Fiona Scott is wrong - should be Emma Husar
# Bob Brown is wrong - should be the other Bob Brown with uniqueID Brown1944
# David Feeney is wrong - should be Ged Kearney

### implement fixes identified above ------------------------------------------
corpus_correct_electorates<- corpus_correct_electorates %>% 
  mutate(name = case_when(
    date=="2016-09-15" & name=="Scott, Fiona" ~ "Husar, Emma",
    date=="2003-10-23" & name=="Brown, Bob, MP" ~ "Brown, Bob, Senator",
    date %in% c("2018-06-20", "2018-09-11") & name=="Feeney, David" ~ "Kearney, Ged",
    .default = name)) %>% 
  mutate(electorate = case_when(
    name=="Husar, Emma" ~ "Lindsay",
    name=="Brown, Bob, Senator" ~ "Tasmania",
    name=="Kearney, Ged" ~ "Batman",
    .default = electorate),
    uniqueID = case_when(
      name=="Husar, Emma" ~ "Husar1980",
      name=="Brown, Bob, Senator" ~ "Brown1944",
      name=="Kearney, Ged" ~ "Kearney1963",
      .default = uniqueID),
    gender = case_when(
      name=="Husar, Emma" ~ "female",
      name=="Brown, Bob, Senator" ~ "male",
      name=="Kearney, Ged" ~ "female",
      .default = gender), 
    member = case_when(
      name=="Husar, Emma" ~ factor(1),
      name=="Brown, Bob, Senator" ~ factor(0),
      name=="Kearney, Ged" ~ factor(1),
      .default = member), 
    senator = case_when(
      name=="Husar, Emma" ~ factor(0),
      name=="Brown, Bob, Senator" ~ factor(1),
      name=="Kearney, Ged" ~ factor(0),
      .default = senator), 
    displayName = case_when(
      name=="Husar, Emma" ~ "Husar, Emma",
      name=="Brown, Bob, Senator" ~ "Brown, Bob, Senator",
      name=="Kearney, Ged" ~ "Kearney, Ged",
      .default = displayName), 
    partyAbbrev = case_when(
      name=="Husar, Emma" ~ "ALP",
      name=="Brown, Bob, Senator" ~ "GRN",
      name=="Kearney, Ged" ~ "ALP",
      .default = partyAbbrev), 
    partyName = case_when(
      name=="Husar, Emma" ~ "Australian Labor Party",
      name=="Brown, Bob, Senator" ~ "Australian Greens",
      name=="Kearney, Ged" ~ "Australian Labor Party",
      .default = partyName))

### clean up environment -----------------------------------------------------
rm(aus_ph, aus_pol, auspol_party, corpus, corpus_fixed, electorates_corrected, 
   mps, mps_multi_party, mps_one_party, multiple_electorates_with_dates, 
   pezzullo_row, body_20200612_burns, names_missing_flags, names_no_uniqueID)
gc()

### found 23 rows with NA body value - resolve those --------------------------
corpus_correct_electorates %>% 
  group_by(date, speech_no) %>% 
  filter(any(is.na(body))) %>% 
  ungroup() %>% 
  select(date:speech_no, body)

# for 2007-02-26, 2007-02-28, 2008-05-29, 2011-03-05 and 2017-12-04, these can
# be safely filtered out. The other ones require manual fixes.
corpus_final_export <- corpus_correct_electorates %>% 
  filter(!(is.na(body) & date %in% c("2007-02-26", "2007-02-28", "2008-05-29", 
                                     "2011-03-03", "2017-12-04")))
  
# now deal with the other ones identified that need to be manually fixed
corrected_rows_missing_body <- corpus_final_export %>% 
  filter(is.na(body) | lag(is.na(body))) %>% 
  select(date, name, order, speech_no, body) %>% 
  rowwise() %>% 
  mutate(body = case_when(
    name %in% c("An honourable member",
                "Honourable members") ~ paste(name, na.omit(body)),
    .default = body)) %>% 
  mutate(body = case_when(str_detect(body, "^Honourable members !$") ~ "Honourable members!",
                          str_detect(body,"^Honourable members $") ~ "Honourable members.",
                          .default = body)) %>% 
  group_by(date, speech_no) %>% 
  fill(body, .direction = "up") %>% 
  mutate(drop = ifelse(name %in% c("An honourable member",
                                    "Honourable members"), TRUE, FALSE)) %>% 
  rename(body_use=body) %>% 
  ungroup()

corpus_final_export <- left_join(corpus_final_export, 
                                 corrected_rows_missing_body,
          by=c("order","speech_no","date","name")) %>% 
  filter(!drop | is.na(drop)) %>% 
  mutate(body = case_when(is.na(body) & !is.na(body_use) ~ body_use,
                          .default = body)) %>% 
  select(-body_use)

# correct issue I just found
corpus_final_export <- corpus_final_export %>% 
  mutate(displayName = case_when(
    name=="Ripoll, Bernie and Husic, Ed" ~ NA,
    name=="Opposition members" ~ NA,
    .default = displayName
  ))

# ensure the difference in number of rows is correct
nrow(corpus_correct_electorates)-nrow(corpus_final_export) == 
  nrow(corrected_rows_missing_body %>% filter(drop))

# reassign order variable
corpus_final_export <- corpus_final_export %>% 
  select(-order) %>% 
  group_by(date) %>% 
  mutate(order = row_number()) %>% 
  relocate(order, .after = "name")

### data export ---------------------------------------------------------------
write_parquet(corpus_final_export, 
              "hansard-corpus/corpus_1998_to_2025-v081025.parquet")

############## FUTURE FIXES ##############
### bigger interject fixes ----------------------------------------------------
# corpus_correct_electorates %>% 
#   filter(str_detect(body,"—(Mr |Mrs |Dr ).{1,20} interjecting"))

### bigger manual Q/A flag fixes ----------------------------------------------

### page number checks --------------------------------------------------------

### q in writing, 2019-04-02 issue --------------------------------------------
##########################################
