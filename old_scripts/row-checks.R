# script to check that number of rows hasn't changed as a result of new parsing method for 2011-2022

new_method_filled <- list.files("/Volumes/Verbatim/output/main-filled-csv-2") %>% 
  as_tibble() %>%
  filter(value > "2012-06-28") %>% 
  pull(value)

old_method_filled <- list.files("/Volumes/Verbatim/output/main-filled-csv") %>% 
  as_tibble() %>%
  filter(value > "2012-06-28") %>% 
  pull(value)

row_check_filled <- tibble()

for(i in 1:length(new_method_filled)){
  thisFile_new <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-2/", new_method_filled[i]), show_col_types = F)
  thisFile_old <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv/", old_method_filled[i]), show_col_types = F)
  
  row_check_filled <- tibble(nrow_new = nrow(thisFile_new),
         nrow_old = nrow(thisFile_old),
         date = str_extract(new_method_filled[i], "\\d{4}-\\d{2}-\\d{2}")) %>% 
    bind_rows(row_check_filled, .)
}


row_check_filled <- row_check_filled %>% filter(nrow_new!=nrow_old)

row_check_filled <- row_check_filled %>% filter(!(date %in% row_check$date))

# THESE HAVE BEEN CHECKED, ALL GOOD
rc_filled_new_less <- row_check_filled %>% filter(nrow_new < nrow_old)
  
rc_filled_new_more <- row_check_filled %>% filter(nrow_new > nrow_old)

thisFile_new <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv-2/", rc_filled_new_more$date[64], "-main-v2.csv"), show_col_types = F)
thisFile_old <- read_csv(paste0("/Volumes/Verbatim/output/main-filled-csv/", rc_filled_new_more$date[64], "-main-v2.csv"), show_col_types = F)

nrow(thisFile_new)-nrow(thisFile_old)

setdiff(select(thisFile_new, c(body)),select(thisFile_old, c(body)))
setdiff(select(thisFile_old, c(body)),select(thisFile_new, c(body)))

thisFile_new %>% filter(name=="stage direction") %>% nrow; thisFile_old %>% filter(name=="stage direction") %>% nrow

setdiff(select(thisFile_new, c(name)),select(thisFile_old, c(name)))
setdiff(select(thisFile_old, c(name)),select(thisFile_new, c(name)))

fill_main("2019-12-02-main.csv")

# to reparse- house adjourned issue 12, 13, 14, 15

### now for the non-filled files

new_method <- list.files("/Volumes/Verbatim/output/main-2011-2022-NEW-v2") %>% 
  as_tibble() %>%
  filter(value > "2012-06-28") %>% 
  pull(value)

old_method <- list.files("/Volumes/Verbatim/output/main-2011-2022-NEW") %>% 
  as_tibble() %>%
  filter(value > "2012-06-28") %>% 
  pull(value)

new_method <- new_method %>% as_tibble() %>% filter(value %in% old_method) %>% pull()

row_check <- tibble()

for(i in 1:length(new_method)){
  thisFile_new <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022-NEW-v2/", new_method[i]), show_col_types = F)
  thisFile_old <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022-NEW/", old_method[i]), show_col_types = F)
  
  row_check <- tibble(nrow_new = nrow(thisFile_new),
                             nrow_old = nrow(thisFile_old),
                             date = str_extract(new_method[i], "\\d{4}-\\d{2}-\\d{2}")) %>% 
    bind_rows(row_check, .)
}

#@ to do - fix these and figure out diffs
row_check <- row_check %>% filter(nrow_new!=nrow_old)

# check 2011-05-25
thisFile_new <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022-NEW-v2/", row_check$date[23], "-main.csv"), show_col_types = F)
thisFile_old <- read_csv(paste0("/Volumes/Verbatim/output/main-2011-2022-NEW/", row_check$date[23], "-main.csv"), show_col_types = F)

setdiff(select(thisFile_new, c(body)),select(thisFile_old, c(body)))
setdiff(select(thisFile_old, c(body)),select(thisFile_new, c(body)))
setdiff(select(thisFile_new, c(name)),select(thisFile_old, c(name)))
setdiff(select(thisFile_old, c(name)),select(thisFile_new, c(name)))
