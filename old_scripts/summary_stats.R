# script to get summary statistics for Hansard
library(tidyverse)
library(here)
library(arrow)

# read in corpus
corpus <- read_parquet("/Volumes/Verbatim/v4/hansard_1998_to_2022-parquet/hansard_corpus_1998_to_2022.parquet")

# read in party table
party_table_final <- read_csv("additional_data/PartyFacts_map.csv", show_col_types = F)

### TOTAL DISTINCT SPEECHES BY PARTY ###
# only for those parties with a party facts ID
s_by_party <- corpus |> select(date, speech_no, partyfacts_id, party) |> 
  mutate(party = as.character(ifelse(str_detect(party, "^Ind"), "IND", as.character(party)))) |> 
  group_by(date, partyfacts_id) |> 
  summarise(n_distinct_speeches = n_distinct(speech_no, na.rm=T)) |> 
  group_by(partyfacts_id) |> 
  summarise(total_distinct_speeches = sum(n_distinct_speeches)) |> 
  filter(!is.na(partyfacts_id)) |> 
  left_join(party_table_final |> 
              select(-party_abb_hansard, -party_abb_auspol) |> 
              distinct() |> 
              # b/c Centre Alliance is current name and they have the same party facts ID
              filter(party_name_auspol!="Nick Xenophon Team"), 
            by="partyfacts_id")

# rename vars, kable
s_by_party |> select(party_name_auspol, everything()) |> 
  rename("Party Name" = party_name_auspol,
         "PartyFacts ID" = partyfacts_id,
         "# Distinct Speeches" = total_distinct_speeches) |> 
  kableExtra::kable()

### TOTAL DAILY DISTINCT SPEECHES ###
s_by_day <- corpus %>% select(date, speech_no, fedchamb_flag) %>%
  group_by(date, fedchamb_flag) %>% 
  summarise(n_distinct_speeches = n_distinct(speech_no, na.rm=T))

# plot
ggplot(s_by_day, aes(x=as.Date(date), y=n_distinct_speeches)) + 
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~fedchamb_flag, labeller = as_labeller(c(`0` = "Chamber", `1` = "Federation Chamber"))) +
  labs(x = "date", y = "# distinct speeches")

### TOTAL YEARLY DISTINCT SPEECHES ###
s_by_year <- corpus %>% select(date, speech_no, fedchamb_flag) %>%
  group_by(date, fedchamb_flag) %>% 
  summarise(n_distinct_speeches = n_distinct(speech_no, na.rm=T)) %>% 
  mutate(year = str_extract(date, "^\\d{4}")) %>% 
  group_by(year, fedchamb_flag) %>% 
  summarise(total_speeches = sum(n_distinct_speeches))

# plot
ggplot(s_by_year, aes(x=as.numeric(year), y=total_speeches)) + 
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~fedchamb_flag, labeller = as_labeller(c(`0` = "Chamber", `1` = "Federation Chamber"))) +
  labs(x = "year", y = "# distinct speeches")

### TOTAL SPEECHES BY IN GOV STATUS ###
s_by_ingov <- corpus |> group_by(date, in.gov) |> 
  summarise(n_distinct_speeches = n_distinct(speech_no, na.rm=T)) |> 
  filter(!is.na(in.gov))

# plot
ggplot(s_by_ingov, aes(x=as.Date(date), y=n_distinct_speeches)) +
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~in.gov, labeller = as_labeller(c(`0` = "Not In Government", `1` = "In Government"))) +
  labs(x = "date", y = "# distinct speeches")

### SPEECHES BY ELECTORATE ###
s_by_electorate <- corpus |> group_by(date, electorate) |> 
  summarise(n_distinct_speeches = n_distinct(speech_no, na.rm=T)) |> 
  group_by(electorate) |> 
  summarise(total_speeches_electorate = sum(n_distinct_speeches)) |> 
  filter(!is.na(electorate))

# look at avg
s_by_electorate |> summarise(avg = mean(total_speeches_electorate))

# look at min and max
s_by_electorate |> filter(total_speeches_electorate==min(total_speeches_electorate) | total_speeches_electorate==max(total_speeches_electorate))

### INTERJECTIONS BY GENDER, ANNUALLY ###
interject_yr_sex <- corpus %>% select(date, gender, interject, fedchamb_flag) %>% 
  mutate(year = str_extract(date, "^\\d{4}"),
         gender = as.character(gender),
         gender = ifelse(is.na(gender), "unknown", gender)) %>% 
  group_by(year, gender, fedchamb_flag, interject) %>% 
  summarise(n = n()) |> 
  group_by(year, fedchamb_flag) |> 
  mutate(total_rows = sum(n)) |> 
  filter(interject==1) |> 
  group_by(year, gender, fedchamb_flag) |> 
  summarise(prop_interject = 100*n/total_rows) %>% 
  ungroup()

# plot
ggplot(interject_yr_sex, aes(x=as.numeric(year), y=prop_interject, color=as.factor(gender))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~fedchamb_flag, labeller = as_labeller(c(`0` = "Chamber", `1` = "Federation Chamber"))) +
  labs(y = "Percent Interjections", x = "Year", color = "Gender")

### INTERJECTIONS BY GENDER, DAILY ###
interject_day_sex <- corpus %>% select(date, gender, interject, fedchamb_flag) %>% 
  mutate(gender = as.character(gender),
         gender = ifelse(is.na(gender), "unknown", gender)) %>% 
  group_by(date, gender, fedchamb_flag, interject) %>% 
  summarise(n = n()) %>% 
  group_by(date, fedchamb_flag) |> 
  mutate(total_rows = sum(n)) |> 
  filter(interject==1) |> 
  group_by(date, gender, fedchamb_flag) |> 
  summarise(prop_interject = 100*n/total_rows) %>% 
  ungroup()

# plot
ggplot(interject_day_sex, aes(x=as.Date(date), y=prop_interject, color=as.factor(gender))) +
  #geom_point(alpha=0.25) +
  geom_smooth(method="loess") +
  facet_wrap(~fedchamb_flag, labeller = as_labeller(c(`0` = "Chamber", `1` = "Federation Chamber")), scales = "free_y") +
  labs(y = "Percent Interjections", x = "Date", color = "Gender")

## DAILY WORD COUNT ##
daily_word_count <- corpus |> 
  group_by(date, fedchamb_flag) |> 
  summarise(word_count = sum(str_count(body)))

# plot
ggplot(daily_word_count, aes(x=as.Date(date), y=word_count, color = fedchamb_flag, fill=fedchamb_flag)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(x="Date", y="Word Count", color=NULL, fill=NULL) +
  scale_color_discrete(labels=c("Chamber", "Federation Chamber")) +
  scale_fill_discrete(labels=c("Chamber", "Federation Chamber"))

## DAILY # UNIQUE NAMES ##
daily_unique_names <- corpus |> 
  select(date, name, fedchamb_flag) |> 
  filter(name!="business start" & name!="stage direction" & !str_detect(name, "SPEAKER")) |> 
  group_by(date, name, fedchamb_flag) |> 
  distinct() |> 
  group_by(date, fedchamb_flag) |> 
  summarise(n=n())

# plot
ggplot(daily_unique_names, aes(x=as.Date(date), y = n, color=fedchamb_flag, fill=fedchamb_flag)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(x="Date", y="# Unique Names", color=NULL, fill=NULL) +
  scale_color_discrete(labels=c("Chamber", "Federation Chamber")) +
  scale_fill_discrete(labels=c("Chamber", "Federation Chamber"))
