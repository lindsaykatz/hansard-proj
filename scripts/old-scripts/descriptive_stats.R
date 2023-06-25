# descriptive stats
library("quanteda")

# read in file
this_df <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv/", "2005-08-18-main-v2.csv"), show_col_types = F)

# most common words
hansard_corpus <- thisFile %>% select(order, body) %>% corpus(., docid_field = "order", text_field = "body")

hansard_corpus %>% tokens() %>% dfm()

stopwords(source="snowball")

# average length of speeches

# frequency of interjections by gender


# average number of rows?



# first grab list of all main CSVs
all_dfs <- list.files("/Volumes/Verbatim/output/main")

# define empty tibbles to store things in
interject_data <- tibble()
fedchamb_rows <- tibble()
unique_data <- tibble()

for(i in 1:length(all_dfs)){
  # define df
  this_df <- read_csv(paste0("/Volumes/Verbatim/output/main/", all_dfs[i]), show_col_types = F)
  
  # number of interjections
  this_interject_data <- this_df %>% group_by(interject, gender) %>%
    summarise(n_interject=n()) %>%
    mutate(date = as.Date(str_extract(all_dfs[i], "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d"))) %>% 
    select(date, interject, n_interject)
  
  # number of rows in chamb and fedchamb
  this_fedchamb_rows <- this_df %>% group_by(fedchamb_flag) %>% 
    summarise(n_rows = n()) %>% 
    mutate(date = as.Date(str_extract(all_dfs[i], "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d"))) %>% 
    select(date, fedchamb_flag, n_rows)
  
  # number of unique things (NAs omitted) - need to fix a tiny bit more but better for now
  this_nameid <- this_df %>% select(name.id, fedchamb_flag) %>% 
    group_by(fedchamb_flag) %>% 
    unique() %>% 
    na.omit() %>% 
    summarise(n_name.id = n())
  
  this_electorate <- this_df %>% select(electorate, fedchamb_flag) %>% 
    group_by(fedchamb_flag) %>% 
    unique() %>% 
    na.omit() %>% 
    summarise(n_electorate = n())
  
  this_party <- this_df %>% select(party, fedchamb_flag) %>% 
    group_by(fedchamb_flag) %>% 
    unique() %>% 
    na.omit() %>% 
    summarise(n_party = n())
  
  this_name <- this_df %>% select(name, fedchamb_flag) %>% 
    group_by(fedchamb_flag) %>% 
    unique() %>% 
    filter(name!="business start" & name!="stage direction" & !str_detect(name, "member") & name!="The DEPUTY SPEAKER" & name!="The SPEAKER") %>% 
    na.omit() %>% 
    mutate(name = str_remove(name, "^Mrs[[:space:]]|^Mr[[:space:]]|^Ms[[:space:]]|^Dr[[:space:]]"),
           surname = str_extract(name, "^.{1,20}(?=\\,[[:space:]][[:upper:]][[:lower:]])"),
           surname = ifelse(is.na(surname) & !str_detect(name, "^The DEPUTY SPEAKER"), name, surname),
           surname = ifelse(is.na(surname) & str_detect(name, "^The DEPUTY SPEAKER"), str_extract(name, "(?<=\\(Mrs[[:space:]]|\\(Mr[[:space:]]|\\(Ms[[:space:]]|\\(Dr[[:space:]]).{1,20}(?=\\))"), surname),
           surname = str_to_title(surname),
           first_name = str_extract(name, "(?<=\\,[[:space:]]).{1,20}(?=\\,[[:space:]]MP)|(?<=\\,[[:space:]]).{1,20}(?=[[:space:]]MP \\(The DEPUTY SPEAKER\\))|(?<=\\,[[:space:]]).{1,20}(?=[[:space:]]MP)"),
           first_name = ifelse(is.na(first_name) & str_detect(name, "\\(The"), str_extract(name, "(?<=\\,[[:space:]]).{1,20}(?=[[:space:]]\\(The)"), first_name)) %>%
    group_by(surname) %>% 
    fill(first_name, .direction = "updown") %>% select(-name) %>% 
    unique() %>% 
    ungroup() %>% 
    group_by(fedchamb_flag) %>% 
    summarise(n_names=n())
  
  this_unique_data <- merge(this_nameid, this_electorate, by="fedchamb_flag") %>% 
    merge(., this_party, by="fedchamb_flag") %>% 
    merge(., this_name, by="fedchamb_flag") %>% 
    as_tibble() %>% 
    mutate(date = as.Date(str_extract(all_dfs[i], "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d")))
  
  # bind new stuff onto existing tibble
  interject_data <- rbind(interject_data, this_interject_data)
  fedchamb_rows <- rbind(fedchamb_rows, this_fedchamb_rows)
  unique_data <- rbind(unique_data, this_unique_data)
}