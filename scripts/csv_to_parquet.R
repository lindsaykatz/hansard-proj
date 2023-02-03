# script to convert all CSV files (version 2, filled) to parquet files, with correct variable classes
library(arrow)
library(tidyverse)

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/output/main-filled-csv")

# to loop through all our files
for (i in 1:length(files_all)) {
  
  # read in file
  thisFile <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv/", files_all[i]), show_col_types = F)
  
  # modify variable classes
  thisFile <- thisFile %>% mutate(party = as.factor(party),
                                  gender = as.factor(gender))
  
  # export as parquet file
  write_parquet(thisFile, paste0("/Volumes/Verbatim/output/main-filled-parquet/", str_remove(files_all[i], "csv"), "parquet"))
  
}

