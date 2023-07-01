### script to add party facts column to our documents
### code based on https://partyfacts.herokuapp.com/download/

library(tidyverse)
library(arrow)
library(here)

# download and read Party Facts mapping table
file_name <- "partyfacts-mapping.csv"

# download party facts mapping file
if(!file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}

# read in party facts csv
# note - there are parsing error warnings due to missingness
partyfacts_raw <- read_csv(file_name, guess_max = 50000, show_col_types = F)

# filter for only those for Australia, with a non-null party facts ID
partyfacts_aus <- partyfacts_raw %>% filter(!is.na(partyfacts_id) & country=="AUS")

# link datasets (select only linked parties) - using code from https://partyfacts.herokuapp.com/download/
dataset_1 <- partyfacts_aus %>%  filter(dataset_key == "manifesto")
dataset_2 <- partyfacts_aus %>% filter(dataset_key == "parlgov")
link_table <- dataset_1 %>% inner_join(dataset_2, by = c("partyfacts_id", "country"))

# select variables of interest
link_table <- link_table %>% select(c(partyfacts_id, name_short.x, name_short.y, name.x, name.y))

# pivot link table so we have all distinct name and abbreviations on separate rows
pf_data <- link_table |> pivot_longer(name_short.x:name_short.y, names_to = "source", values_to = "name_short") |> 
  pivot_longer(name.x:name.y, names_to = "name_source", values_to = "name") |> 
  select(partyfacts_id, name_short, name) |> 
  group_by(partyfacts_id, name) |> 
  # this is b/c the abbreviation for Australian Greens is missing from one row
  fill(name_short, .direction = "downup") |> 
  ungroup() |> 
  distinct() |> 
  rename(name_short_pf = name_short,
         name_pf = name)

### Manual spelling/abbreviation fixes and filtering ###
# a number of parties have two spelling variations in the party facts data, so let's only keep the ones that match AusPol spelling
# and modify spelling if needed to match AusPol

# fix Katter's name to match that in AusPol so we can merge, and only keep row where abbreviation is "KAT"
pf_data <- pf_data |> mutate(name_pf = ifelse(name_pf=="Katter's Australian Party", str_remove(name_pf, "'"), name_pf)) |> 
  filter(name_short_pf!="Katter")

# also only keep the one row for the NPA party that matches the AusPol format b/c there are 4 different ones 
pf_data <- pf_data |> filter(partyfacts_id==1743 & name_pf=="National Party of Australia" & name_short_pf=="NPA" | partyfacts_id!=1743)

# for Liberal National Party of Queensland - only keep row with LNP abbreviation
pf_data <- pf_data |> filter(partyfacts_id==285 & name_short_pf=="LNP" | partyfacts_id!=285)

# fix Country Liberal Party name to match that in AusPol
pf_data <- pf_data |> mutate(name_pf = ifelse(name_pf=="Country Liberal Party", "Country Liberal Party (Northern Territory)", name_pf)) |> 
  filter(partyfacts_id==143 & name_short_pf=="CLP" | partyfacts_id!=143)

# only keep row for Democratic Labor Party where it is spelled "Labor" - there is one with a U that doesn't match AusPol spelling
pf_data <- pf_data |> filter(partyfacts_id==1540 & name_pf=="Democratic Labor Party" | partyfacts_id!=1540)

# read in auspol dataset, get distinct party names and abbreviations - this will be our gold standard
auspol_data <- AustralianPoliticians::get_auspol('allbyparty') |> 
  select(partyAbbrev, partyName) |> 
  distinct() |> 
  # rename to match style in pf_data
  rename(name_short_auspol = partyAbbrev,
         name_auspol = partyName)

# merge auspol and partyfacts data together
party_table <- full_join(auspol_data, pf_data, by = c("name_auspol" = "name_pf"), multiple="all") |> select(name_auspol, name_short_auspol, partyfacts_id)

# read in our Hansard corpus as of v3 (before we add party facts)
corpus <- read_parquet(here("additional_data/hansard_corpus_1998_to_2022.parquet"))
  
# grab list of all unique party names found in corpus
corpus_parties <- corpus %>% 
  select(party) %>% 
  filter(!is.na(party) & party!="UNKNOWN") %>% 
  distinct()

# add party full names to the distinct ones we have in our Hansard data, in accordance with the AusPol data
corpus_parties <- corpus_parties %>% 
  mutate(party_name = case_when(party == "LP" | party == "LIB" ~ "Liberal Party of Australia",
                                party == "ALP" ~ "Australian Labor Party",
                                party == "AG" ~ "Australian Greens",
                                party == "IND" | party=="Ind." | party=="Ind" ~ "Independent",
                                party == "CA" ~ "Centre Alliance",
                                party == "CLP" ~ "Country Liberal Party (Northern Territory)",
                                party == "KAP" ~ "Katters Australian Party",
                                party == "LNP" ~ "Liberal National Party of Queensland",
                                party == "NATS" | party=="Nats" ~ "Nationalist Party",
                                party == "NatsWA" ~ "National Party of Australia (WA)",
                                party == "PUP" ~ "Palmer United Party",
                                party == "UAP" ~ "United Australia Party",
                                party == "NP" | party=="NPA" ~ "National Party of Australia",
                                party == "NXT" ~ "Nick Xenophon Team"))

# rename variables for clarity
corpus_parties <- corpus_parties |> rename(name_hansard = party,
                                           name_short_hansard = party)

# merge party table with unique Hansard party data, and only keep rows relevant to the Hansard data (i.e. where Hansard party name is non null)
party_table_final <- left_join(party_table, corpus_parties, by=c("name_auspol"="party_name"), multiple='all') |> 
  filter(!is.na(name_short_hansard)) |> 
  # rename vars for clarity when added to Hansard data
  rename(party_name_auspol = name_auspol,
         party_abb_auspol = name_short_auspol,
         party_abb_hansard = name_short_hansard) |> 
  select(partyfacts_id, party_abb_hansard, party_abb_auspol, party_name_auspol)

# export clean party table to have
# write_csv(party_table_final, "additional_data/party_table.csv")
party_table_final <- read_csv("additional_data/PartyFacts_map.csv", show_col_types = F)

# grab list of filenames to iterate over
all_csvs <- list.files("/Volumes/Verbatim/v3/hansard_1998_to_2022-csv")

# now that we have our party table finalized, we can loop through each Hansard data file and add the new variables
#notice when exporting I am also removing sub1_flag and sub2_flag variables - these are not relevant for users and will cause confusion
# NOTE - splitting this up into a few hundred at a time is best to avoid weird errors / the session aborting
# i found when i tried to do this for the full csv list it always crashed halfway through and gave me really weird errors, restarting R helped
# adding sys.sleep didn't resolve the issue anyway

for (i in 1:length(all_csvs)){
  
  # read in file, specify column types
  thisFile <- readr::read_csv(paste0("/Volumes/Verbatim/v3/hansard_1998_to_2022-csv/", all_csvs[i]), 
                                 col_types = list(name = col_character(),
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
                                                  interject = col_factor())) |> 
    # deselect sub1 flag and sub2 flag variables
    select(-c(sub1_flag, sub2_flag))
  
  # merge with party table final
  thisFile_new <- left_join(thisFile, party_table_final, by= c("party" = "party_abb_hansard")) |> 
    select(-c(party_abb_auspol, party_name_auspol))
  
  # check nrow hasn't changed
  stopifnot(nrow(thisFile) == nrow(thisFile_new))
  
  # export updated file as CSV and Parquet
  readr::write_csv(thisFile_new, paste0("/Volumes/Verbatim/v4/hansard_1998_to_2022-csv/", all_csvs[i]))
  
  arrow::write_parquet(thisFile_new, paste0("/Volumes/Verbatim/v4/hansard_1998_to_2022-parquet/", str_remove(all_csvs[i], "csv"), "parquet"))
  

}



