# script to detect bug in time stamps in our Hansard database
library(tidyverse)

# grab list of all file names
all_files <- list.files("/Volumes/Verbatim/v3/hansard_1998_to_2022-csv")

# define empty tibble to store loop output in
times <- tibble()

# define for loop to iterate over all our database files
for (i in 1:length(all_files)) {
  
  # read in file
  thisFile <- readr::read_csv(paste0("/Volumes/Verbatim/v3/hansard_1998_to_2022-csv/", all_files[i]), show_col_types = FALSE)
  
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

# grab random sample of 25% of wrong times to check
sample_times <- slice_sample(wrong_times, prop = 0.25)

### checked entire sample, all are confirmed transcription errors 
### that is, these are all errors inherent to the XML files, not the result of our database construction process
