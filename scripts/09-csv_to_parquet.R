# script to convert all CSV files (version 2, filled) to parquet files, with correct variable classes
library(arrow)
library(tidyverse)

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/output/main-filled-csv-v2")

files_all[i]
fill_main(str_remove(files_all[i], "-v2"))

# to loop through all our files
for (i in 1:length(files_all)) {
  
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # modify variable classes
  thisFile <- thisFile %>% mutate(party = as.factor(party),
                                  gender = as.factor(gender))
  
  stopifnot(ncol(thisFile)==21 & c("name", "order","speech_no","page.no", "time.stamp", "name.id", "electorate", "party", "in.gov", "first.speech",
                                   "body", "fedchamb_flag", "sub1_flag", "sub2_flag","question", "answer", "q_in_writing", "div_flag", "gender", "uniqueID", "interject") %in% names(thisFile))
  
  stopifnot(str_detect(thisFile$order, "^\\d+$"))
  
  # export as parquet file
  write_parquet(thisFile, paste0("/Volumes/Verbatim/output/main-filled-parquet-v2/", str_remove(files_all[i], "csv"), "parquet"))
  
}


# just a final check that all column names are right etc
# to loop through all our files
for (i in 1:length(files_all)) {
  
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-v2/", files_all[i]), show_col_types = F)
  
  # modify variable classes
  thisFile <- thisFile %>% mutate(party = as.factor(party),
                                  gender = as.factor(gender))
  
  stopifnot(ncol(thisFile)==21 & c("name", "order","speech_no","page.no", "time.stamp", "name.id", "electorate", "party", "in.gov", "first.speech",
                                   "body", "fedchamb_flag", "sub1_flag", "sub2_flag","question", "answer", "q_in_writing", "div_flag", "gender", "uniqueID", "interject") %in% names(thisFile))
  
  stopifnot(str_detect(thisFile$order, "^\\d+$"))
  
}
