### environment setup ---------------------------------------------------------
# import necessary libraries
library(tidyverse)
library(arrow)

### manual interject fixes ----------------------------------------------------
# import corpus exported in script 08
corpus <- read_parquet("hansard-corpus/corpus_1998_to_2025-v081025.parquet")

# get patterns for rows that still have someone interjecting
unsplit_interjections <- corpus %>% 
  mutate(interjection_content = str_extract_all(
    body, "(?<=—)\\s{0,2}[[:upper:]][[:lower:]].{1,50} interjecting—"))

unsplit_interjections <- unsplit_interjections %>% 
  unnest(interjection_content)

# separate rows with multiple interjections
unsplit_interjections <- unsplit_interjections %>% 
  distinct(date, interjection_content) %>% 
  unnest(interjection_content) %>% 
  separate_rows(interjection_content, sep="(?<=interjecting—)") %>% 
  filter(interjection_content!="")

# export so I can produce a lookup table with MPs info
#writexl::write_xlsx(unsplit_interjections, "unsplit_interjections_lookup.xlsx")

# import lookup table
unsplit_interjections_lookup <- readxl::read_xlsx(
  "additional_data/unsplit_interjections_lookup.xlsx") %>% 
  mutate(date = as.Date(date))

# create one pattern per date
unsplit_interjections <- unsplit_interjections %>% 
  group_by(date) %>% 
  summarise(full_interject_pattern = paste0("(?=", interjection_content, ")", 
                                            collapse = "|")) %>% 
  ungroup()

# have a look at the unsplit interjections - all looks good
unsplit_interjections

# store dates with unsplit interjections to iterate over
dates_unsplit <- unique(unsplit_interjections$date)

# split corpus by date, more computationally efficient
corpus_by_date <- corpus %>% 
  group_split(date, .keep = T) %>% 
  set_names(unique(corpus$date))

# coerce all factors to characters for now to avoid issues
corpus_by_date <- map(corpus_by_date, ~ .x %>% 
                        mutate(across(where(is.factor), as.character)))

# tibble to store new rows
new_rows <- tibble()
separated <- tibble()

# separate the rows
for (i in 1:length(dates_unsplit)) {
  
  # define index of dataframe to be split
  index <- which(names(corpus_by_date)==dates_unsplit[[i]])
  
  # pull interjection pattern for that date
  interject_pattern <- unsplit_interjections %>% 
    filter(date==dates_unsplit[[i]]) %>% pull(full_interject_pattern)
  
  separated <- corpus_by_date[[index]] %>% 
    filter(str_detect(body, interject_pattern)) %>% 
    separate_rows(body, sep=interject_pattern) %>% 
    bind_rows(., separated)
  
  # store new rows
  new_rows <- corpus_by_date[[index]] %>% 
    separate_rows(body, sep=interject_pattern) %>% 
    mutate(body = str_squish(body)) %>% 
    filter(!str_detect(body, "^\\s{1,2}$|^$")) %>% 
    left_join(unsplit_interjections_lookup %>% filter(date==dates_unsplit[[i]]),
              by=c("date","body")) %>% 
    mutate(name = case_when(!is.na(name_use) ~ name_use, .default = name),
           displayName = case_when(!is.na(displayName_use) ~ displayName_use,
                                   is.na(displayName_use) & !is.na(name_use) ~ NA,
                                   .default = displayName)) %>% 
    mutate(interject = case_when(!is.na(name_use) ~ "1",
                                 .default = interject)) %>% 
    mutate(across(c(order, time.stamp, name.id, electorate, partyAbbrev,
                    partyName, uniqueID, gender, member, senator), 
                  ~case_when(!is.na(name_use) ~ NA, .default = .x))) %>% 
    mutate(name.id = case_when(!is.na(name.id_use) ~ name.id_use,
                               .default = name.id),
           electorate = case_when(!is.na(electorate_use) ~ electorate_use,
                                  .default = electorate),
           partyAbbrev = case_when(!is.na(partyAbbrev_use) ~ partyAbbrev_use,
                                   .default = partyAbbrev),
           partyName = case_when(!is.na(partyName_use) ~ partyName_use,
                                 .default = partyName),
           uniqueID = case_when(!is.na(uniqueID_use) ~ uniqueID_use,
                                .default = uniqueID),
           gender = case_when(!is.na(gender_use) ~ gender_use,
                              .default = gender),
           member = case_when(!is.na(member_use) ~ member_use,
                              .default = member),
           senator = case_when(!is.na(senator_use) ~ senator_use,
                               .default = senator)) %>% 
    filter(!is.na(name_use)) %>% 
    select(date, displayName, name, speech_no, name.id, electorate, partyAbbrev, 
           partyName, body, uniqueID, gender, member, senator, interject) %>% 
    bind_rows(., new_rows)
  
  # split rows, fill in correct name with lookup table, and make other ID-related
  # columns null for now to be filled in later on
  corpus_by_date[[index]] <- corpus_by_date[[index]] %>% 
    separate_rows(body, sep=interject_pattern) %>% 
    mutate(body = str_squish(body)) %>% 
    filter(!str_detect(body, "^\\s{1,2}$|^$")) %>% 
    left_join(unsplit_interjections_lookup %>% filter(date==dates_unsplit[[i]]),
              by=c("date","body")) %>% 
    mutate(name = case_when(!is.na(name_use) ~ name_use, .default = name),
           displayName = case_when(!is.na(displayName_use) ~ displayName_use,
                                   .default = displayName)) %>% 
    mutate(interject = case_when(!is.na(name_use) ~ "1",
                                 .default = interject)) %>% 
    mutate(across(c(order, time.stamp, name.id, electorate, partyAbbrev,
                    partyName, uniqueID, gender, member, senator), 
                  ~case_when(!is.na(name_use) ~ NA, .default = .x))) %>% 
    mutate(name.id = case_when(!is.na(name.id_use) ~ name.id_use,
                               .default = name.id),
           electorate = case_when(!is.na(electorate_use) ~ electorate_use,
                                  .default = electorate),
           partyAbbrev = case_when(!is.na(partyAbbrev_use) ~ partyAbbrev_use,
                                   .default = partyAbbrev),
           partyName = case_when(!is.na(partyName_use) ~ partyName_use,
                                 .default = partyName),
           uniqueID = case_when(!is.na(uniqueID_use) ~ uniqueID_use,
                                .default = uniqueID),
           gender = case_when(!is.na(gender_use) ~ gender_use,
                              .default = gender),
           member = case_when(!is.na(member_use) ~ member_use,
                              .default = member),
           senator = case_when(!is.na(senator_use) ~ senator_use,
                               .default = senator)) %>% 
    select(-c(order, ends_with("_use"))) %>% 
    rowid_to_column("order") %>% relocate(order, .after = name)
  
}

# look at new rows
new_rows

# bind full corpus back together
corpus_final <- bind_rows(corpus_by_date) %>% 
  mutate(date = as.Date(date),
         time.stamp = hms::as_hms(time.stamp)) %>% 
  mutate(across(c(partyAbbrev, question, answer, q_in_writing, div_flag, gender, 
                  member, senator, fedchamb_flag, interject),
                ~ as.factor(.)))

# left_join(corpus_final %>% group_by(date) %>%
#             count(name="new_row_count") %>% ungroup(),
#           corpus %>% group_by(date) %>% 
#             count(name="old_row_count") %>% ungroup(),
#             by="date") %>% 
#   mutate(diff = new_row_count-old_row_count) %>% 
#   filter(diff!=0) %>% 
#   left_join(., unsplit_interjections_lookup %>% group_by(date) %>% 
#               count(name="n_interjections")) %>% filter(diff!=n_interjections) %>% View
# 

# ensure nothing changed among the dates not involved in the splitting process
stopifnot(
  setdiff(corpus %>% filter(!(date %in% dates_unsplit)),
          corpus_final %>% filter(!(date %in% dates_unsplit))) %>% nrow()==0,
  setdiff(corpus_final %>% filter(!(date %in% dates_unsplit)),
          corpus %>% filter(!(date %in% dates_unsplit))) %>% nrow()==0 
)

### fix column classes --------------------------------------------------------
corpus_final <- corpus_final %>% 
  select(-drop) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date),
         time.stamp = hms::as_hms(time.stamp)) %>% 
  mutate(across(c(partyAbbrev, question, answer, q_in_writing, div_flag, gender, 
                  member, senator, fedchamb_flag, interject),
                ~ as.factor(.)))

### data export ---------------------------------------------------------------
write_parquet(corpus_final, 
              "hansard-corpus/corpus_1998_to_2025-v081025.parquet")
