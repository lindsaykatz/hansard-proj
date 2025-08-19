# script to detect bug in time stamps in our Hansard database
library(tidyverse)

# grab list of all file names
all_files <- list.files("/Volumes/Verbatim/output_filled-2022_2025")

# define empty tibble to store loop output in
times <- tibble()

# define for loop to iterate over all our database files
for (i in 1:length(all_files)) {
  
  # read in file
  thisFile <- readr::read_csv(paste0("/Volumes/Verbatim/output_filled-2022_2025/", all_files[i]), show_col_types = FALSE)
  
  # grab distinct time stamps and append date
  thisFile_times <- thisFile %>% 
    select(time.stamp) %>% 
    filter(!is.na(time.stamp)) %>% 
    #distinct() %>% 
    mutate(date = str_remove(all_files[i], "\\.csv"),
           time.stamp = as.character(time.stamp))
  
  # add above to tibble
  times <- bind_rows(times, thisFile_times)
  
}

# filter to only keep time stamps that do not fit the correct format hh:mm:ss
wrong_times <- times %>% filter(!str_detect(time.stamp, "^\\d\\d:\\d\\d:\\d\\d$"))

### no wrong times detected, all good.