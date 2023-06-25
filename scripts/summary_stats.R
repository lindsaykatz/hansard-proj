# script to get summary statistics for Hansard
library(tidyverse)
library(here)
library(arrow)

# read in corpus
corpus <- read_parquet("hansard_corpus_1998_to_2022.parquet")

corpus |> select(date, name, order, speech_no, party_name_auspol) |> 
  group_by(date, party_name_auspol) |> 
  summarise(n_distinct_speeches = n_distinct(speech_no, na.rm=T)) |> 
  group_by(party_name_auspol) |> 
  summarise(total_distinct_speeches = sum(n_distinct_speeches))

# speeches per year
speeches_per_year <- corpus %>% select(date, speech_no) %>%
  mutate(year = str_extract(date, "^\\d{4}")) %>% 
  distinct() %>% 
  group_by(date) %>% 
  filter(speech_no == max(speech_no, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarise(total_speeches = sum(speech_no))

# speeches per party
### note - this will require some cleaning b/c of some variation in party abbrevation - may be better to do after party facts ID are added
speeches_per_party <- corpus |> 
  group_by(date, party_name_auspol) |> 
  filter(speech_no == max(speech_no, na.rm = TRUE)) |> 
  group_by(party_name_auspol) |> 
  summarise(total_speeches_party = sum(speech_no))

# speeches in gov
corpus |> group_by(date) |> 
  filter(speech_no == max(speech_no, na.rm = TRUE)) |> 
  group_by(in.gov) |> 
  summarise(total_speeches_in_gov = sum(speech_no))

# speeches by electorate
corpus |> group_by(date) |> 
  filter(speech_no == max(speech_no, na.rm = TRUE)) |> 
  group_by(electorate) |> 
  summarise(total_speeches_electorate = sum(speech_no)) |> arrange(desc(total_speeches_electorate))

# interjections by men vs by women per year
interject_yr_sex <- corpus %>% select(date, gender, interject, fedchamb_flag) %>% 
  mutate(year = str_extract(date, "^\\d{4}"),
         gender = as.character(gender),
         gender = ifelse(is.na(gender), "unknown", gender)) %>% 
  filter(interject==1) %>% 
  group_by(year, gender, fedchamb_flag) %>% 
  summarise(n = n()) %>% 
  ungroup()

interject_day_sex <- corpus %>% select(date, gender, interject, fedchamb_flag) %>% 
  mutate(gender = as.character(gender),
         gender = ifelse(is.na(gender), "unknown", gender)) %>% 
  group_by(date, gender, fedchamb_flag, interject) %>% 
  summarise(n_interject = n()) %>% 
  group_by(date, gender, fedchamb_flag) %>% 
  mutate(total_interject = sum(n_interject)) %>% 
  ungroup() %>% 
  filter(interject==1) %>% 
  mutate(pct_interject = 100*n_interject/total_interject)
  



interject_data %>% 
  group_by(date) %>% 
  summarise(pct_interject = 100*(n_interject[interject==1]/sum(n_interject))) %>% 
  ggplot(aes(x = date, y = pct_interject)) +
  geom_smooth() +
  geom_point(alpha = 0.4)+
  theme_bw() +
  labs(x = "Date", y = "Percent interjections")


ggplot(interject_yr_sex, aes(x=as.numeric(year), y=n, color=as.factor(gender))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~fedchamb_flag)

ggplot(interject_day_sex, aes(x=as.Date(date), y=pct_interject, color=as.factor(gender))) +
  geom_smooth() +
  facet_wrap(~fedchamb_flag)
