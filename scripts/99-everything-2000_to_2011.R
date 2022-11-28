# works from 15 February 2000 10 May 2011
library(here)
library(tidyverse)
library(xml2)
library(XML)
library(hms)

# code in function below from https://stackoverflow.com/questions/58492429/xml-in-r-multiple-children-with-same-name-without-loops
item_df <- function(file, path){
  # find all items and store as a list
  items <- xml_find_all(file, path)
  
  # extract all childrens names and values 
  nodenames <- xml_name(xml_children(items))
  contents <- trimws(xml_text(xml_children(items)))
  
  # need to create an index to associate the nodes/contents with each item
  itemindex <- rep(1:length(items), times=sapply(items, function(x) {length(xml_children(x))}))
  
  # store all information in data frame.
  df <- data.frame(itemindex, nodenames, contents)
  
  # Convert from long to wide format
  # sometimes titles are split onto multiple lines, so fix that up too
  df <- pivot_wider(df, id_cols = itemindex, names_from = nodenames, values_from = contents, values_fn = list) 
  
  return(df)
}

# grab "all" dataset from AusPol package
all <- AustralianPoliticians::get_auspol('all')

hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", "1999-02-08.xml"))
xml_df <- read_xml(here("/Volumes/Verbatim/input/", "1999-02-08.xml"))

filename<-"1999-02-08.xml"

parse_hansard <- function(filename){ 
 
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))
  xml_df <- read_xml(here("/Volumes/Verbatim/input/", filename))
  
  ######### BUSINESS START #########
  # chamber
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start"), homogeneous = T, collectNames = F))>0) {
    bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start"), homogeneous = T, collectNames = F), 
                              fedchamb_flag = 0, .name_repair = "unique") %>% 
      rename(date = day.start)
    
    if ("para" %in% names(bus_start_chamb)) {
      bus_start_chamb <- bus_start_chamb %>% 
        rename(body = para) %>% 
        mutate(body = ifelse(body=="—————", NA, body)) %>% 
        select(fedchamb_flag, everything()) %>% 
        unite("body", c(fedchamb_flag:last_col(), -fedchamb_flag), na.rm = T, sep=" ") %>% 
        mutate(date = str_extract(body, "\\d{4}-\\d{2}-\\d{2}"),
               date = as.Date(date),
               body = str_remove(body, paste0(date)),
               body = str_remove(body, "[[:space:]]{0,2}10000SPEAKER, Mr|[[:space:]]{0,2}10000SPEAKER, Mrs|[[:space:]]{0,2}10000SPEAKER, Ms|[[:space:]]{0,2}10000SPEAKER, The"),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:space:][:lower:]\\.[:lower:]\\.|[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:]\\.[:lower:]\\."))
      
    } else if ("para...2" %in% names(bus_start_chamb)) {
      bus_start_chamb <- bus_start_chamb %>% 
        rename(body = para...2) %>% 
        mutate(body = ifelse(body=="—————", NA, body)) %>% 
        select(fedchamb_flag, everything()) %>% 
        unite("body", c(fedchamb_flag:last_col(), -fedchamb_flag), na.rm = T, sep=" ") %>% 
        mutate(date = str_extract(body, "\\d{4}-\\d{2}-\\d{2}"),
               date = as.Date(date),
               body = str_remove(body, paste0(date)),
               body = str_remove(body, "[[:space:]]{0,2}10000SPEAKER, Mr|[[:space:]]{0,2}10000SPEAKER, Mrs|[[:space:]]{0,2}10000SPEAKER, Ms|[[:space:]]{0,2}10000SPEAKER, The"),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:]\\.[:lower:]\\.|[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:]\\.[:lower:]\\.[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:lower:]\\.[:lower:]\\."))
    } else if ("para...3" %in% names(bus_start_chamb)) {
      bus_start_chamb <- bus_start_chamb %>% rename(body = para...3) %>% 
        select(date, fedchamb_flag, everything()) %>% 
        unite("body", c(fedchamb_flag:last_col(), -fedchamb_flag), na.rm = T, sep=" ") %>% 
        mutate(date = str_extract(body, "\\d{4}-\\d{2}-\\d{2}"),
               date = as.Date(date),
               body = str_remove(body, paste0(date)),
               body = str_remove(body, "[[:space:]]{0,2}10000SPEAKER, Mr|[[:space:]]{0,2}10000SPEAKER, Mrs|[[:space:]]{0,2}10000SPEAKER, Ms|[[:space:]]{0,2}10000SPEAKER, The"),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:space:][:lower:]\\.[:lower:]\\.|[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:]\\.[:lower:]\\."))
    } else {
      # case when no text with business start 2008-02-12
      bus_start_chamb <- bus_start_chamb %>%
        mutate(date = as.Date(date),
               body=NA,
               day_of_week=strftime(date, "%A"),
               start_time=NA) %>% 
        select(date, fedchamb_flag, body, day_of_week, start_time)
    }
    
    
    if ("interjection" %in% names(bus_start_chamb)) {
      bus_start_chamb <- bus_start_chamb %>% unite("body", c(body, interjection), na.rm = T, sep=" ")
    }
    
    bus_start_chamb <- bus_start_chamb %>% select(date, body, fedchamb_flag, day_of_week, start_time)
    
  } else {
    bus_start_chamb <- tibble()
  }
  
  # federation chamber
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/business.start"), homogeneous = T, collectNames = F))>0) {
    bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/business.start"), homogeneous = T, collectNames = F), 
                              fedchamb_flag = 1, .name_repair = "unique") %>% 
      rename(date = day.start) 
    
    if ("para" %in% names(bus_start_fed)) {
      bus_start_fed <- bus_start_fed %>% 
        rename(body = para) %>% 
        mutate(body = ifelse(body=="—————", NA, body)) %>% 
        select(fedchamb_flag, everything()) %>% 
        unite("body", c(fedchamb_flag:last_col(), -fedchamb_flag), na.rm = T, sep=" ") %>% 
        mutate(date = str_extract(body, "\\d{4}-\\d{2}-\\d{2}"),
               date = as.Date(date),
               body = str_remove(body, paste0(date)),
               body = str_remove(body, "[[:space:]]{0,2}10000SPEAKER, Mr|[[:space:]]{0,2}10000SPEAKER, Mrs|[[:space:]]{0,2}10000SPEAKER, Ms|[[:space:]]{0,2}10000SPEAKER, The"),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:space:][:lower:]\\.[:lower:]\\.|[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:]\\.[:lower:]\\."))
      
    } else if ("para...2" %in% names(bus_start_fed)) {
      bus_start_fed <- bus_start_fed %>% 
        rename(body = para...2) %>% 
        mutate(body = ifelse(body=="—————", NA, body)) %>% 
        select(fedchamb_flag, everything()) %>% 
        unite("body", c(fedchamb_flag:last_col(), -fedchamb_flag), na.rm = T, sep=" ") %>% 
        mutate(date = str_extract(body, "\\d{4}-\\d{2}-\\d{2}"),
               date = as.Date(date),
               body = str_remove(body, paste0(date)),
               body = str_remove(body, "[[:space:]]{0,2}10000SPEAKER, Mr|[[:space:]]{0,2}10000SPEAKER, Mrs|[[:space:]]{0,2}10000SPEAKER, Ms|[[:space:]]{0,2}10000SPEAKER, The"),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:][:lower:]|[:digit:]{1,2}[:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:space:][:lower:]\\.[:lower:]\\.|[:digit:]{1,2}\\.[:digit:][:digit:][:space:][:lower:]\\.[:lower:]\\.|\\d{1,2}\\.\\d\\d[:lower:]\\.[:lower:]\\."))
    } else {
      # case when no text with business start 2008-02-12
      bus_start_fed<- bus_start_fed %>%
        mutate(date = as.Date(date),
               body=NA,
               day_of_week=strftime(date, "%A"),
               start_time=NA) %>% 
        select(date, fedchamb_flag, body, day_of_week, start_time)
    }
    

    if ("interjection" %in% names(bus_start_fed)) {
      bus_start_fed <- bus_start_fed %>% unite("body", c(body, interjection), na.rm = T, sep=" ")
    }
    
    bus_start_fed <- bus_start_fed %>% select(date, body, fedchamb_flag, day_of_week, start_time)
    
  } else {
    bus_start_fed <- tibble()
  }
  
  # combine business starts and prepare to add to main at the end
  if (nrow(rbind(bus_start_chamb, bus_start_fed)) > 0) {
    bus_start <- rbind(bus_start_chamb, bus_start_fed) %>%
      rename(time.stamp = start_time) %>%
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d\\:"), paste0("0", time.stamp), time.stamp),
             time.stamp = ifelse(str_detect(time.stamp, "^\\d\\."), str_replace(time.stamp, "(?<=^\\d).", ":"), time.stamp),
             body = str_replace(body, "\\n[[:space:]]{1,20}", " ")) %>% 
      select(c(body, time.stamp, fedchamb_flag)) %>%
      mutate(name = "business start",
             name_short = NA,
             page.no = NA,
             name.id = NA,
             party = NA,
             electorate = NA,
             in.gov = "",
             first.speech = "",
             role = NA,
             speech_no = NA,
             itemindex = NA,
             sub1_flag = 0,
             sub2_flag = 0) %>%
      select(c(itemindex, speech_no, page.no, time.stamp, name, name_short, name.id, electorate, party, in.gov, first.speech, role, body, fedchamb_flag, sub1_flag, sub2_flag))
    
    # convert time to 24hour time
    bus_start <- bus_start %>% mutate(time.stamp = str_replace_all(time.stamp, "(?<=\\d)\\.", "\\:"),
                                      time.stamp = ifelse(str_detect(time.stamp, "^\\d\\d[[:space:]][[:alpha:]]"), paste0(time.stamp), time.stamp)) %>% 
      mutate(time.stamp = ifelse(str_detect(time.stamp, "a\\.m\\."), str_replace(time.stamp, "a.m.", "AM"),time.stamp),
             time.stamp = ifelse(str_detect(time.stamp, "p\\.m\\."), str_replace(time.stamp, "p.m.", "PM"),time.stamp),
             time.stamp = ifelse(str_detect(time.stamp, "am\\."), str_replace(time.stamp, "am.", "AM"),time.stamp),
             time.stamp = ifelse(str_detect(time.stamp, "pm\\."), str_replace(time.stamp, "pm.", "PM"),time.stamp),
             time.stamp = ifelse(str_detect(time.stamp, "(?<=\\d\\d)(?=[[:alpha:]]{2})"), str_replace(time.stamp, "(?<=\\d\\d)(?=[[:alpha:]]{2})", " "), time.stamp)) %>% 
      mutate(time.stamp = ifelse(str_detect(time.stamp, "\\d{1,2}\\:\\d{2}"), format(strptime(time.stamp, "%I:%M %p"), format="%H:%M:%S"), time.stamp),
             time.stamp = ifelse(str_detect(time.stamp, "\\d{1,2}[[:space:]][[:alpha:]]"), format(strptime(time.stamp, "%I %p"), format="%H:%M:%S"), time.stamp))
  }
  
  # grab all content - CHAMBER
  debate_text_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate"), homogeneous = T, collectNames = F), .name_repair = "unique") %>% 
    unite("body", c(debateinfo:last_col(), -debateinfo), na.rm = T, sep=" ") %>% 
    rowid_to_column("itemindex") %>% 
    left_join(item_df(xml_df, "//chamber.xscript/debate/debateinfo") %>% 
                select(itemindex, page.no) %>% 
                unnest(page.no), ., by="itemindex") %>% 
    select(itemindex, page.no, body) %>% 
    mutate(fedchamb_flag = 0,
           page.no = as.numeric(page.no))
  
  # grab all content  - FEDERATION CHAMBER
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/debateinfo"), homogeneous = T, collectNames = F))>0){
    debate_text_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate"), homogeneous = T, collectNames = F), .name_repair = "unique") %>% 
      unite("body", c(debateinfo:last_col(), -debateinfo), na.rm = T, sep=" ") %>% 
      rowid_to_column("itemindex") %>% 
      left_join(item_df(xml_df, "//maincomm.xscript/debate/debateinfo") %>% 
                  select(itemindex, page.no) %>% 
                  unnest(page.no), ., by="itemindex") %>% 
      select(itemindex, page.no, body) %>% 
      mutate(fedchamb_flag = 1,
             page.no = as.numeric(page.no))
    
  } else {
    debate_text_fed <- tibble() 
  }
  
  # merge all text
  debate_text_all <- bind_rows(debate_text_chamb, debate_text_fed)
  
  # going to try to grab the combo of things that are found at the beginning of each statement so we can build regex list
  # first, main debates (includes speeches and interjections)
  # sometimes the beginning of "para" has the deputy speakers name in brackets, and that's part of the pattern, so extract it and unite it with the talker pattern
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//talk.start"), homogeneous = T, collectNames = F))>0) {
    patterns_text <- item_df(xml_df, "//talk.start") %>% 
      unnest(everything()) %>% 
      mutate(para = ifelse(str_detect(para, "^\\(.{1,35}\\)—[[:upper:]]") ,
                           str_extract(para, "^\\(.{1,35}\\)(?=—[[:upper:]])"),
                           NA)) %>% 
      rename(first_pattern = talker)
    
    # sometimes the para should be merged with a space between, other times not, so let's duplicate the rows with a para and add a space before so we can detect both patterns later
    patterns_text <- patterns_text %>% filter(!is.na(para)) %>% mutate(para = paste0(" ", para)) %>% 
      bind_rows(., patterns_text) %>% 
      unite("first_pattern", c(first_pattern, para), sep="", na.rm = T) %>% 
      select(first_pattern) %>% 
      unique() %>% 
      pull()
    
  } else {
    patterns_text <- NULL
  }
  
  # next add motionnospeech stuff pasted correct
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//motionnospeech"), homogeneous = T, collectNames = F))>0) {
    patterns_text <- item_df(xml_df, "//motionnospeech") %>% 
      select(name, electorate, role, time.stamp) %>% 
      unnest(everything()) %>% 
      unite("first_pattern", everything(), sep="") %>% 
      unique() %>% 
      pull() %>% 
      c(., patterns_text)
  }
  
  # remove 0000 name id pattern
  patterns_text <- patterns_text %>% as_tibble() %>% filter(value!="0000Mr SPEAKERMr SPEAKER" & value!="0000SPEAKER, MrMr SPEAKER") %>% pull()
  
  # in one case a pattern started with a "\" so remove that b\c gonna cause a lot of issues with regex matches later
  patterns_text <- patterns_text %>% as_tibble() %>% mutate(value = ifelse(str_detect(value, "^\\\\"),
                                                          str_replace(value, "^\\\\", ""),
                                                          value))
  
  # escape all special characters in patterns so the separations happen correctly
  # this is stored separately b/c when separating rows we need the one without the backslashes
  patterns_text_esc <- patterns_text %>% as_tibble() %>% mutate(value = ifelse(str_detect(value, "\\("),
                                                                               str_replace_all(value, "\\(", "\\\\("),
                                                                               value),
                                                                value = ifelse(str_detect(value, "\\)"),
                                                                               str_replace_all(value, "\\)", "\\\\)"),
                                                                               value),
                                                                value = ifelse(str_detect(value, "\\."),
                                                                               str_replace_all(value, "\\.", "\\\\."),
                                                                               value)) %>% pull(value)
  
if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//talk.start/talker"), homogeneous = T, collectNames = F))>0) {
  
  # first, extract all talk.start talker info, and unnest names so we have full name and display name
  patterns_data <- item_df(xml_df, "//talk.start/talker") %>% 
    mutate(name = map(name, c)) %>% 
    unnest_wider(name, names_sep = "_")
  
  # usually there are two names. one full, and one short - we want both
  # merge the talker info with the pattern, so we can use this as a look up table to merge with main
  # this will let us fill in the right speaker details based on the pattern preceding the speech
  if ("name_2" %in% names(patterns_data)) {
    patterns_data <- patterns_data %>% rename(name = name_1,
                                              name_short = name_2) %>% 
      unnest(everything()) %>% 
      merge(.,  item_df(xml_df, "//talk.start") %>% 
              unnest(everything()),
            by = "itemindex") %>% 
      as_tibble()
    
    # usually the deputy speaker has an additional title included in the beginning of "para" in brackets
    # we want to capture these b/c they're included in that pattern we want to match with/extract
    # we can also use these as their name_short to replace the general "The DEPUTY SPEAKER" name short
    patterns_data <- patterns_data %>% mutate(para = ifelse(str_detect(para, "^\\(.{1,35}\\)—[[:upper:]]") & str_detect(name, "DEPUTY"),
                                                            str_extract(para, "^\\(.{1,35}\\)(?=—[[:upper:]])"),
                                                            NA)) %>%
      mutate(name_short = ifelse(!is.na(para), paste(str_remove_all(para, "^\\(|\\)$")),
                                 name_short)) %>% 
      rename(first_pattern = talker)
    
    # similar to what we did with patterns_text, sometimes para is pasted with space between so add extra rows with that space before we unite - so we have both pattern variations to match for
    patterns_data <- patterns_data %>% filter(!is.na(para)) %>% mutate(para = paste0(" ", para)) %>% 
      bind_rows(., patterns_data) %>% 
      unite("first_pattern", c(first_pattern, para), sep="", na.rm = T) %>% 
      mutate(role = {if("role" %in% names(.)) role else NA},
             page.no = {if("role" %in% names(.)) page.no else NA},
             time.stamp = {if("role" %in% names(.)) time.stamp else NA},
             electorate = {if("role" %in% names(.)) electorate else NA},
             party = {if("role" %in% names(.)) party else NA},
             in.gov = {if("role" %in% names(.)) in.gov else NA},
             first.speech = {if("role" %in% names(.)) first.speech else NA}) %>% 
      # need to filter out any page numbers which contain text that were wrongly transcribed by hansard editors
      filter(is.na(page.no) | str_detect(page.no, "\\d")) %>% 
      select(page.no, time.stamp, name, name_short, name.id, electorate, party, role, in.gov, first.speech, first_pattern) %>% 
      group_by(name) %>% 
      fill(c(electorate, party, name.id), .direction = "downup") %>% 
      ungroup() %>% 
      unique()
  } else if ("name_1" %in% names(patterns_data)) {
    patterns_data <- patterns_data %>% rename(name = name_1) %>% 
      mutate(name_short = NA) %>% 
      unnest(everything()) %>% 
      merge(.,  item_df(xml_df, "//talk.start") %>% 
              select(itemindex, talker) %>% 
              unnest(talker),
            by = "itemindex") %>% 
      as_tibble() %>% 
      mutate(para = ifelse(str_detect(para, "^\\(.{1,35}\\)—[[:upper:]]") & str_detect(name, "DEPUTY"),
                           str_extract(para, "^\\(.{1,35}\\)(?=—[[:upper:]])"),
                           NA)) %>% 
      mutate(name_short = ifelse(!is.na(para), paste(str_remove_all(para, "^\\(|\\)$")),
                                 name_short)) %>% 
      rename(first_pattern = talker) %>% 
      mutate(role = {if("role" %in% names(.)) role else NA},
             page.no = {if("role" %in% names(.)) page.no else NA},
             time.stamp = {if("role" %in% names(.)) time.stamp else NA},
             electorate = {if("role" %in% names(.)) electorate else NA},
             party = {if("role" %in% names(.)) party else NA},
             in.gov = {if("role" %in% names(.)) in.gov else NA},
             first.speech = {if("role" %in% names(.)) first.speech else NA}) %>% 
      select(page.no, time.stamp, name, name_short, name.id, electorate, party, role, in.gov, first.speech, first_pattern) %>% 
      group_by(name) %>% 
      fill(c(electorate, party, name.id), .direction = "downup") %>% 
      ungroup() %>% 
      unique()
  }
} else {
  patterns_data <- tibble()
}
  
  # add motionnospeech data as well, remove repeated rows
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//motionnospeech"), homogeneous = T, collectNames = F))>0) {
    patterns_data <- item_df(xml_df, "//motionnospeech") %>% 
      select(name, electorate, role, time.stamp) %>% 
      unnest(everything()) %>% 
      mutate(first_pattern = paste0(name, electorate, role, time.stamp),
             name.id = NA,
             party= NA,
             in.gov=NA,
             first.speech=NA,
             page.no=NA,
             role = {if("role" %in% names(.)) role else NA}) %>% 
      select(page.no, time.stamp, name, name.id, electorate, party, role, in.gov, first.speech, first_pattern) %>% 
      bind_rows(., patterns_data) %>% 
      unique()
  }
  
  # saw this on 2002-05-16 - caused issues with merge later on b/c two full name versions
  patterns_data <- patterns_data %>% mutate(name = ifelse(name=="Mr MARTIN FERGUSON,AM, MP", "Ferguson, Martin, MP", name))
  
  # saw on 2010-06-21 that a page number was captured as a time by accident? should've been time stamp - i need to come back to this
  patterns_data <- patterns_data %>% filter(!grepl("\\.", page.no))
  
  # saw this on 2005-05-31 - two page numbers, let's go with bigger one (checked and this is correct in XML based on preceding/following statement page numbers)
  patterns_data <-  patterns_data %>% group_by(first_pattern) %>% 
    mutate(page.no=as.numeric(page.no)) %>% 
    filter(n()>1 & page.no==max(page.no) | n()==1) %>% 
    ungroup()
  
  # sometimes the party or electorate of the speaker is given but sometimes not, and when we merge with first pattern we'll get a row for both if both exist
  # so, lets just keep those as NA for the speaker b/c they can be backed out later on
  patterns_data <- patterns_data %>% mutate(electorate = ifelse(str_detect(name, "^SPEAKER"), NA, electorate),
                           party = ifelse(str_detect(name, "^SPEAKER"), NA, party)) %>% unique()
  
  # if there is a duplicated first pattern due to different name_short, take the longer one
  if (nrow(patterns_data %>% filter(duplicated(first_pattern)))>0) {
    patterns_data <- patterns_data %>% group_by(first_pattern) %>% 
      filter(n()>1 & nchar(name_short)==max(nchar(name_short)) | n()==1) %>% 
      ungroup()
  }
  
  # checking that there are no duplicated first patterns that would cause extra rows when we merge
  stopifnot(group_by(patterns_data, first_pattern) %>% summarise(n=n()) %>% filter(n!=1) %>% nrow() == 0)
  
  # need debate info bc often talk start details are preceded by this
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1 | //maincomm.xscript/debate/subdebate.1"), homogeneous = T, collectNames = F))>0) {
    sub1_patterns_info <- item_df(xml_df, "//subdebate.1")
    
    # also grab titles - sometimes there are extra title nodes and we want to remove these as well
    if ("title" %in% names(sub1_patterns_info)) {
      sub1_patterns_info <- sub1_patterns_info %>% 
        select(subdebateinfo, title) %>% 
        unnest(c(subdebateinfo, title)) %>% 
        unite("subdebateinfo", c(subdebateinfo, title), na.rm = T, sep="") %>% 
        mutate(subdebateinfo = str_replace_all(subdebateinfo, "\\(","\\\\("),
               subdebateinfo = str_replace_all(subdebateinfo, "\\)","\\\\)"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\:","\\\\:"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\.","\\\\."),
               subdebateinfo = str_replace_all(subdebateinfo, "\\?","\\\\?"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\*","\\\\*"))%>% 
        unique() %>% 
        pull()
    } else {
      sub1_patterns_info <- sub1_patterns_info %>% select(subdebateinfo) %>% 
        unnest(subdebateinfo) %>% 
        mutate(subdebateinfo = str_replace_all(subdebateinfo, "\\(","\\\\("),
               subdebateinfo = str_replace_all(subdebateinfo, "\\)","\\\\)"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\:","\\\\:"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\.","\\\\."),
               subdebateinfo = str_replace_all(subdebateinfo, "\\?","\\\\?"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\*","\\\\*"))%>% 
        unique() %>% 
        pull()
    }
  } else {
    sub1_patterns_info <- NULL
  }
  
  
  # separate for sub2 b/c those are preceded by both the sub1 and sub2 titles (in that order), rather than just the sub1 title
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2 | //maincomm.xscript/debate/subdebate.1/subdebate.2"), homogeneous = T, collectNames = F))>0) {
    sub2_patterns_info <- item_df(xml_df, "//subdebate.2")
    
    if ("title" %in% names(sub2_patterns_info)) {
      sub2_patterns_info <- sub2_patterns_info %>% 
        select(subdebateinfo, title) %>% 
        unnest(c(subdebateinfo, title)) %>% 
        unite("subdebateinfo", c(subdebateinfo, title), na.rm = T, sep="") %>% 
        mutate(subdebateinfo = str_replace_all(subdebateinfo, "\\(","\\\\("),
               subdebateinfo = str_replace_all(subdebateinfo, "\\)","\\\\)"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\:","\\\\:"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\.","\\\\."),
               subdebateinfo = str_replace_all(subdebateinfo, "\\?","\\\\?"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\*","\\\\*")) %>% 
        unique() %>% 
        pull()
    } else {
      sub2_patterns_info <-sub2_patterns_info %>%  select(subdebateinfo) %>% 
        unnest(subdebateinfo) %>% 
        mutate(subdebateinfo = str_replace_all(subdebateinfo, "\\(","\\\\("),
               subdebateinfo = str_replace_all(subdebateinfo, "\\)","\\\\)"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\:","\\\\:"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\.","\\\\."),
               subdebateinfo = str_replace_all(subdebateinfo, "\\?","\\\\?"),
               subdebateinfo = str_replace_all(subdebateinfo, "\\*","\\\\*")) %>% 
        unique() %>% 
        pull()
    }
  } else {
    sub2_patterns_info <- NULL
  }
  
  # need to split questions without notice and answers so that they have their own speech number 
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//question"), homogeneous = T, collectNames = F))>0) {
    question_patterns <- item_df(xml_df, "//question") %>% 
      select(talk.start) %>% 
      unnest(talk.start) %>% 
      rename(q_pattern = talk.start) %>% 
      mutate(q_pattern = str_replace_all(q_pattern, "\\(","\\\\("),
             q_pattern = str_replace_all(q_pattern, "\\)","\\\\)"),
             q_pattern = str_replace_all(q_pattern, "\\.","\\\\."),
             q_pattern = str_replace_all(q_pattern, "\\?","\\\\?"),
             q_pattern = str_replace_all(q_pattern, "\\$","\\\\$"),
             q_pattern = str_replace_all(q_pattern, "\\*","\\\\*")) %>%
      pull()
    
    answer_patterns <- item_df(xml_df, "//answer") %>% 
      select(talk.start) %>% 
      unnest(talk.start) %>% 
      rename(a_pattern = talk.start) %>% 
      mutate(a_pattern = str_replace_all(a_pattern, "\\(","\\\\("),
             a_pattern = str_replace_all(a_pattern, "\\)","\\\\)"),
             a_pattern = str_replace_all(a_pattern, "\\.","\\\\."),
             a_pattern = str_replace_all(a_pattern, "\\?","\\\\?"),
             a_pattern = str_replace_all(a_pattern, "\\$","\\\\$"),
             a_pattern = str_replace_all(a_pattern, "\\*","\\\\*")) %>%
      pull() 
  } else {
    question_patterns <- NULL
    answer_patterns <- NULL
  }
  
  # on 2005-08-17 one subdebate1 title was literally just a space and that will cause so many problems when we separate rows
  # also remove any extra backslashes that shouldnt be there- remove that b\c gonna cause a lot of issues with regex matches later
  # so let's filter those out in case
  if (length(sub1_patterns_info)>0){
    sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% 
      filter(value!="") %>% 
      mutate(value = ifelse(str_detect(value, "\\\\(?=[[:digit:]])|\\\\(?=[[:alpha:]])"),
                            str_replace(value, "\\\\", ""),
                            value)) %>% 
      pull()
  }
  if (length(sub2_patterns_info)>0){
    sub2_patterns_info <- sub2_patterns_info %>% as_tibble() %>% 
      filter(value!="") %>% mutate(value = ifelse(str_detect(value, "\\\\(?=[[:digit:]])|\\\\(?=[[:alpha:]])"),
                                                  str_replace(value, "\\\\", ""),
                                                  value)) %>% 
      pull()
  }
  if (length(question_patterns)>0){
    question_patterns <- question_patterns %>% as_tibble() %>% 
      filter(value!="") %>% mutate(value = ifelse(str_detect(value, "\\\\(?=[[:digit:]])|\\\\(?=[[:alpha:]])"),
                                                  str_replace(value, "\\\\", ""),
                                                  value)) %>% 
      pull()
  }
  if (length(answer_patterns)>0){
    answer_patterns <- answer_patterns %>% as_tibble() %>% 
      filter(value!="") %>% 
      mutate(value = ifelse(str_detect(value, "\\\\(?=[[:digit:]])|\\\\(?=[[:alpha:]])"),
                            str_replace(value, "\\\\", ""),
                            value)) %>% 
      pull()
  }
  
  
  # let's grab all the talk starts that are nested in a speech node - these will allow us to perform our first split the text
  # this will in turn allow us to correctly define the beginning of each speech
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//speech"), homogeneous = T, collectNames = F))>0) {
    speech_start <- item_df(xml_df, "//speech/talk.start") %>% 
      select(talker) %>% 
      unnest(talker) %>% 
      mutate(talker = str_replace_all(talker, "\\(","\\\\("),
             talker = str_replace_all(talker, "\\)","\\\\)"),
             talker = str_replace_all(talker, "\\:","\\\\:"),
             talker = str_replace_all(talker, "\\.","\\\\."),
             talker = str_replace_all(talker, "\\?","\\\\?"),
             talker = str_replace_all(talker, "\\*","\\\\*")) %>% 
      unique() %>% 
      pull()
  } else {
    speech_start <- tibble()
  }
  
  
  # First eparate rows at each sub debate pattern, so we know where each unique debate starts
  # sometimes when we separate rows on a pattern at the beginning of a body, an empty body is left behind, so just filter those out
  if (length(sub1_patterns_info)>0){
    debate_text_all <- separate_rows(debate_text_all, body, sep=paste0("(?=", paste0(sub1_patterns_info, collapse = "|"), ")")) %>% 
      filter(body!="")
  }
  
  
  # info stuff only necessary for very first part of speech - clean those up
  if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)>0) {
    main <- debate_text_all %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             str_match(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             NA),
                                       sub2_pattern = ifelse(str_detect(body, paste0(sub2_patterns_info, collapse = "|")),
                                                             str_match(body, paste0(sub2_patterns_info, collapse = "|")),
                                                             NA),
                                       sub2_pattern = ifelse(!is.na(sub1_pattern) & !is.na(sub2_pattern) 
                                                             & str_detect(body, paste0(sub1_pattern, sub2_pattern)),
                                                             str_match(body, paste0(sub1_pattern, sub2_pattern)),
                                                             NA)) %>% 
      mutate(sub1_pattern = str_replace_all(sub1_pattern, "\\(","\\\\("),
             sub1_pattern = str_replace_all(sub1_pattern, "\\)","\\\\)"),
             sub1_pattern = str_replace_all(sub1_pattern, "\\:","\\\\:"),
             sub1_pattern = str_replace_all(sub1_pattern, "\\.","\\\\."),
             sub1_pattern = str_replace_all(sub1_pattern, "\\?","\\\\?"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\(","\\\\("),
             sub2_pattern = str_replace_all(sub2_pattern, "\\)","\\\\)"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\:","\\\\:"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\.","\\\\."),
             sub2_pattern = str_replace_all(sub2_pattern, "\\?","\\\\?"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\*","\\\\*")) %>% 
      mutate(body = ifelse(!is.na(sub2_pattern) & str_detect(body, paste0("^", sub2_pattern)),
                           str_remove(body, paste0("^", sub2_pattern)),
                           body),
             body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0("^", sub1_pattern)),
                           str_remove(body, paste0("^", sub1_pattern)),
                           body)) %>% 
      mutate(sub1_flag = ifelse(!is.na(sub1_pattern) & is.na(sub2_pattern), 1, 0),
             sub2_flag = ifelse(!is.na(sub2_pattern), 1, 0)) %>% 
      select(-sub1_pattern, -sub2_pattern)
  } else if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)==0) {
    main <- debate_text_all %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             str_match_all(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             NA),
                                       body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0("^", sub1_pattern)),
                                                     str_remove(body, paste0("^", sub1_pattern)),
                                                     body)) %>% 
      select(-sub1_pattern)
  } else {
    main <- debate_text_all
  }
  
  # split on speech start patterns, question and answer patterns, flag questions and answers
  # add new speech_no
  if (length(question_patterns)>0 & length(answer_patterns)>0 & length(speech_start)>0) {
    # sometimes there are too many patterns and the regex is too long causing the script to break
    # in these cases, we can just separate on subsets of the patterns
    if (length(question_patterns)>100 & length(answer_patterns)>100){
      main <- main %>% 
        separate_rows(., body, sep=paste0("(?=", question_patterns[1:100], ")", collapse = "|")) %>% 
        separate_rows(., body, sep=paste0("(?=", answer_patterns[1:100], ")", collapse = "|")) %>%
        separate_rows(., body, sep=paste0("(?=", speech_start, ")", collapse = "|")) %>% 
        filter(body!="") %>% 
        filter(body!=" ") %>% 
        filter(!str_detect(body, "^\\d{1,2}$")) %>% 
        mutate(question = ifelse(str_detect(body, paste0(question_patterns[1:100], collapse = "|")), 1, 0),
               answer = ifelse(str_detect(body, paste0(answer_patterns[1:100], collapse = "|")), 1, 0))
      
      main <- main %>% 
        separate_rows(., body, sep=paste0("(?=", question_patterns[101:length(question_patterns)], ")", collapse = "|")) %>% 
        separate_rows(., body, sep=paste0("(?=", answer_patterns[101:length(answer_patterns)], ")", collapse = "|")) %>% 
        filter(body!="") %>% 
        filter(body!=" ") %>% 
        filter(!str_detect(body, "^\\d{1,2}$")) %>% 
        mutate(question = ifelse(str_detect(body, paste0(question_patterns[101:length(question_patterns)], collapse = "|")), 1, 0),
               answer = ifelse(str_detect(body, paste0(answer_patterns[101:length(answer_patterns)], collapse = "|")), 1, 0))
      
      main <- main %>% rowid_to_column("speech_no")
      
    } else if (length(question_patterns)>100) {
      main <- main %>% 
        separate_rows(., body, sep=paste0("(?=", question_patterns[1:100], ")", collapse = "|")) %>% 
        separate_rows(., body, sep=paste0("(?=", answer_patterns, ")", collapse = "|")) %>%
        separate_rows(., body, sep=paste0("(?=", speech_start, ")", collapse = "|")) %>% 
        filter(body!="") %>% 
        filter(body!=" ") %>% 
        filter(!str_detect(body, "^\\d{1,2}$")) %>% 
        mutate(question = ifelse(str_detect(body, paste0(question_patterns[1:100], collapse = "|")), 1, 0),
               answer = ifelse(str_detect(body, paste0(answer_patterns, collapse = "|")), 1, 0))
      
      main <- main %>% 
        separate_rows(., body, sep=paste0("(?=", question_patterns[101:length(question_patterns)], ")", collapse = "|")) %>% 
        filter(body!="") %>% 
        filter(body!=" ") %>% 
        filter(!str_detect(body, "^\\d{1,2}$")) %>% 
        mutate(question = ifelse(str_detect(body, paste0(question_patterns[101:length(question_patterns)], collapse = "|")), 1, 0))
      
      main <- main %>% rowid_to_column("speech_no")
      
    } else {
      main <- main %>% 
        separate_rows(., body, sep=paste0("(?=", question_patterns, ")", collapse = "|")) %>% 
        separate_rows(., body, sep=paste0("(?=", answer_patterns, ")", collapse = "|")) %>%
        separate_rows(., body, sep=paste0("(?=", speech_start, ")", collapse = "|")) %>% 
        filter(body!="") %>% 
        filter(body!=" ") %>% 
        filter(!str_detect(body, "^\\d{1,2}$")) %>% 
        mutate(question = ifelse(str_detect(body, paste0(question_patterns, collapse = "|")), 1, 0),
               answer = ifelse(str_detect(body, paste0(answer_patterns, collapse = "|")), 1, 0)) %>% 
        rowid_to_column("speech_no")
    }
    
  } else if (length(speech_start)>0) {
    main <- main %>% 
      separate_rows(., body, sep=paste0("(?=", speech_start, ")", collapse = "|")) %>% 
      filter(body!="") %>% 
      filter(!str_detect(body, "^\\d{1,2}$")) %>% 
      mutate(question = 0,
             answer = 0) %>% 
      rowid_to_column("speech_no")
  } else {
    main <- main %>% 
      filter(body!="") %>% 
      mutate(question = 0,
             answer = 0) %>% 
      rowid_to_column("speech_no")
  }
  
  # grab nrow to check in after merge
  main_row_here <- nrow(main)
  
  # now, detect first pattern match so we can merge with pattern data tibbles allowing us to have the page number, electorate, etc
  main <- main %>% mutate(first_pattern = ifelse(str_detect(body, paste0("^", patterns_text_esc, collapse = "|")),
                                                 str_match(body, paste0("^", patterns_text_esc, collapse = "|")),
                                                 NA)) %>% 
    merge(., patterns_data, by="first_pattern", all.x = T) %>% 
    as_tibble() %>% 
    arrange(speech_no)
  
  # need to deal with rows where page number x and y contradict - we want the later one but also want to make sure we maintain order
  main <- main %>% mutate(page.no = ifelse(page.no.x > page.no.y & !is.na(page.no.x) & !is.na(page.no.x), 
                                           page.no.x, 
                                           page.no.y),
                          page.no = ifelse(is.na(page.no) & is.na(page.no.x), 
                                           page.no.y, 
                                           page.no),
                          page.no = ifelse(is.na(page.no) & is.na(page.no.y), 
                                           page.no.x, 
                                           page.no)) %>% 
    select(itemindex, speech_no, page.no, time.stamp, name, name_short, name.id, electorate, party, role, in.gov, first.speech, body, fedchamb_flag, first_pattern) %>% 
    unique()
  
  # now check that row number was fixed with merge and page number fix up
  stopifnot(nrow(main_row_here) == nrow(main))
  
  # if the row before has a bigger page number, fill next row with the maximum page number - this will cause problems later with ordering
  main <- main %>% group_by(itemindex) %>% 
    mutate(page.no = ifelse(page.no < max(page.no) & speech_no > speech_no[which.max(page.no)], max(page.no), page.no)) %>% 
    ungroup()
  
  # lets make sure the first pattern matches the name and time stamp that we merged it with
  # idea: if the first pattern exists and doesn't contain the time stamp and name in that row, throw an error
  stopifnot(nrow(filter(main, !is.na(first_pattern) & !str_detect(first_pattern, paste0(time.stamp)) & !is.na(time.stamp) & !str_detect(first_pattern, paste0(name)))) == 0)
  
  # now, let's remove the first pattern from the body to clean things
  main <- main %>% 
    mutate(first_pattern = ifelse(str_detect(first_pattern, "\\("), str_replace_all(first_pattern, "\\(", "\\\\("), first_pattern),
           first_pattern = ifelse(str_detect(first_pattern, "\\)"), str_replace_all(first_pattern, "\\)", "\\\\)"), first_pattern),
           first_pattern = ifelse(str_detect(first_pattern, "\\."), str_replace_all(first_pattern, "\\.", "\\\\."), first_pattern)) %>% 
    mutate(body = ifelse(!is.na(first_pattern), str_remove(body, paste0(first_pattern)), body),
           body = ifelse(str_detect(body, "^—"), str_remove(body, "^—"), body)) %>% 
    select(-first_pattern)
  
  # the 0000 name id for speaker is causing issues b/c usually 10000 is used - fix that up
  main <- main %>% mutate(body = ifelse(str_detect(body, "(?<!1)0000Mr SPEAKERMr SPEAKER"),
                                        str_replace_all(body, "(?<!1)0000Mr SPEAKERMr SPEAKER", "10000SPEAKER, MrMr SPEAKER"),
                                        body),
                          body = ifelse(str_detect(body, "(?<!1)0000SPEAKER, MrMr SPEAKER"),
                                        str_replace_all(body, "(?<!1)0000SPEAKER, MrMr SPEAKER", "10000SPEAKER, MrMr SPEAKER"),
                                        body))
  
  # separate rows on patterns
  # sometimes it's too long - we need to split on subsets of the patterns if this is the case
  if (length(patterns_text_esc)>400) {
    main <- separate_rows(main, body, sep=paste0("(?=", patterns_text_esc[1:400], ")", collapse = "|")) %>% 
      separate_rows(., body, sep=paste0("(?=", patterns_text_esc[401:length(patterns_text_esc)], ")", collapse = "|")) %>% 
      filter(body!="")
  } else {
    main <- separate_rows(main, body, sep=paste0("(?=", patterns_text_esc, ")", collapse = "|")) %>% filter(body!="")
  }
  
  # 2002 09 16 I saw some rows where the body was just the name id - this happened b/c of varying patterns for the same person, and there's a bit that wasn't removed
  # let's filter out any rows where the body is the name id
  main <- main %>% filter(!str_detect(body, paste0("^", name.id, "$")))
  
  # define general interjections to split on
  interject_general <- c("Opposition members interjecting—", "Government members interjecting—", 
                         "Honourable members interjecting—", "An opposition member interjecting—", 
                         "An honourable member interjecting—", "A government member interjecting—",
                         "Honourable members and senators interjecting—",
                         "Honourable member and senators interjecting—")
  
  # separate rows on general interjections
  main <- separate_rows(main, body, sep=paste0("(?=", interject_general, ")(?!", interject_general, "[[:space:]]on[[:space:]]my)(?!", interject_general, "[[:space:]]having)(?!", interject_general, "[[:space:]]standing)(?!", interject_general, "[[:space:]]will )(?!", interject_general, "[[:space:]]can )",collapse="|"))
  
  # define stage notes
  stage_notes <- c("Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.",
                   "Debate interrupted\\.",
                   "Message from the .{0,100}[[:space:]]announced\\.",
                   "Question agreed to\\.",
                   "Question unresolved\\.",
                   "Debate adjourned",
                   "Debate .{1,50} adjourned\\.",
                   "The House divided\\.",
                   "Question put\\.",
                   "Question negatived\\.",
                   "House adjourned at \\d\\d\\:\\d\\d",
                   "House adjourned at \\d{1,2}\\.\\d\\d.{0,6}\\.",
                   "Federation Chamber adjourned at \\d\\d\\:\\d\\d",
                   "House adjourned \\d\\d\\:\\d\\d",
                   "Federation Chamber adjourned \\d\\d\\:\\d\\d",
                   "Main Committee adjourned at \\d\\d\\:\\d\\d",
                   "Main Committeer adjourned \\d\\d\\:\\d\\d",
                   "Main Committee adjourned at \\d{1,2}\\:\\d\\d.{0,6}",
                   "Main Committee adjourned at \\d{1,2}\\.\\d\\d.{0,6}",
                   "Leave not granted\\.",
                   "Leave granted\\.",
                   "A division having been called in the House of Representatives\\-",
                   "Honourable members having stood in their places-",
                   "Honourable members standing in their places-",
                   "The Speaker having seated himself in the chair-",
                   "The bells having been rung and a ballot having been taken-",
                   "A division having been called and the bells having been rung-",
                   "Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d",
                   "The member for [[:alpha:]]{0,50} then left the chamber.",
                   "More than the number of members required by the standing orders having risen in their places-",
                   "Members and senators rising and applauding,.{1,50}left the chamber\\.",
                   "Proposed expenditure agreed to\\.",
                   "Bill agreed to\\.",
                   "Bill returned from Main Committee without amendment; certified copy of the bill presented\\.",
                   "Bill returned from Main Committee without amendment, appropriation message having been reported; certified copy of the bill presented\\.",
                   "Bill returned from Main Committee for further consideration\\; certified copy of the bill presented\\.Ordered that this bill be considered at a later hour this day\\.",
                   "Bill, explanatory memorandum and the report of the Committee for the Review of Parliamentary Entitlements presented by.{1,35}\\.",
                   "Order of the day returned from Main Committee for further consideration; certified copy of the motion presented\\.",
                   "Order of the day returned from Main Committee for further consideration; certified copy presented\\.",
                   "Bill and explanatory memorandum presented by .{1,35}\\.",
                   "Ordered that the order of the day be considered immediately\\.",
                   "Bill returned from the Senate with amendments\\.",
                   "Consideration resumed.{0,40}\\.",
                   "Debate resumed.{0,100}\\.",
                   "Debate resumed.{0,100}\\:",
                   "The following notice was given\\:",
                   "Message received from the Senate returning the bills without amendment or request\\.",
                   "Ordered that this bill be considered immediately\\.",
                   "Question put:That the motion.{1,50}be agreed to\\.",
                   "The following notices were given\\:",
                   "Bill presented by .{1,35}, and read a first time\\.",
                   "Proposed expenditure \\$.{1,20}\\.",
                   "Proposed expenditure, \\$.{1,20}\\.",
                   "Proposed expenditure\\$.{1,30}",
                   "Proposed expenditure,\\$.{1,30}",
                   "Bill—by leave—taken as a whole\\.",
                   "Bill \\(on motion by .{1,35}\\)—by leave—read a third time\\.",
                   "Consideration resumed from .{1,30}, on motion by .{1,35}\\:That the bill be now read a .{3,7} time\\.",
                   "Leave granted for .{3,7} reading to be moved forthwith\\.Bill \\(on motion by .{1,35}\\) read a .{3,7} time\\.",
                   "Bill \\(on motion by .{1,35}\\).{1,20}read a [[:alpha:]]{1,10} time\\.",
                   "Motion \\(by .{1,35}\\) proposed: That the House do now adjourn\\.",
                   "The following bill was returned from the Senate without amendment or request: .{1,50}",
                   "The following bills were returned from the Senate without amendment or request:.{1,100}",
                   "Debate—by leave—adjourned\\.",
                   "Leave granted for third reading to be moved forthwith\\.",
                   "Bill returned from the Senate with amendments\\. Ordered that the amendments be considered at the next sitting\\.",
                   "Question proposed: That grievances be noted\\.",
                   "Messages from the Governor-General reported informing the House of assent to the following bills:.{1,200}\\.",
                   "Bill presented by.{1,35}\\.",
                   "Bills presented by.{1,35}\\.",
                   "Bill received from the Senate, and read a .{1,10} time\\.",
                   "Bills received from the Senate, and read a .{1,10} time\\.",
                   "Messages from the Governor-General reported informing the House of assent to the bills\\.",
                   "Ordered that the amendment be considered at a later hour this day\\.",
                   "Ordered that the .{1,15} reading be made an order of the day for the next sitting\\.",
                   "Bill returned from the Senate with an amendment\\.",
                   "Bill returned from Main Committee with an amendment; certified copy of the bill and schedule of amendments presented\\.",
                   "Message received from the Senate returning the bill without amendment or request\\.") 
  
  # split on stage notes
  # sometimes they're preceeded by a timestamp, and we want to capture those so in the next step we can use them for the time stamp
  # other times they're not though, so split those out afterward with a negative lookbehind to avoid double-splitting on those with a timestamp
  main <- separate_rows(main, body, sep=paste0("(?=\\d\\d\\:\\d\\d\\:\\d\\d", stage_notes, ")", collapse = "|")) %>% 
    separate_rows(., body, sep=paste0("(?<!\\d\\d\\:\\d\\d\\:\\d\\d)(?=", stage_notes, ")", collapse = "|")) %>% filter(body!="") %>% filter(body!=" ")
  
  # remove details for stage direction that followed from separate_rows
  main <- main %>% mutate(name = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), "stage direction", name),
                          name.id = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), NA, name.id),
                          electorate = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), NA, electorate),
                          party = as.factor(ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), NA, as.character(party))),
                          in.gov = ifelse(name == "stage direction", NA, in.gov),
                          first.speech = ifelse(name == "stage direction", NA, first.speech),
                          role = ifelse(name == "stage direction", NA, role),
                          name_short = ifelse(name=="stage direction", NA, name_short),
                          time.stamp = ifelse(name=="stage direction", NA, time.stamp))
  
  # now fill in the time stamp using what's at the beginning of the body
  main <- main %>% mutate(time.stamp = ifelse(str_detect(body, "^\\d\\d\\:\\d\\d\\:\\d\\d"),
                                      str_extract(body, "^\\d\\d\\:\\d\\d\\:\\d\\d"),
                                      time.stamp),
                          body = ifelse(str_detect(body, "^\\d\\d\\:\\d\\d\\:\\d\\d"),
                                        str_remove(body, "^\\d\\d\\:\\d\\d\\:\\d\\d"),
                                        body))
  
  # remove details for split statements, so we can fill in with the correct ones
  main <- main %>% mutate(name = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, name),
                          name_short = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, name_short),
                          name.id = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, name.id),
                          role = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, role),
                          in.gov = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, in.gov),
                          first.speech = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, first.speech),
                          electorate = ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, electorate),
                          party = as.factor(ifelse(grepl(paste0("^", c(patterns_text_esc, interject_general), collapse = "|"), body), NA, as.character(party))))
  
  
  
  # if general interjection, paste who is interjecting in name column
  main <- main %>% mutate(name = ifelse(is.na(name) & !str_detect(body, "took the chair"), 
                                        str_extract(main$body, paste0("^", str_remove(interject_general, " interjecting—$"), collapse = "|")), 
                                        name))
  
  # now that we've split all the rows, let's add an order column so we can keep track of exact order of things
  # need original speech_no column though to keep track of which interjections belong to which speech (will be useful to flag interjections later)
  main <- main %>% rowid_to_column("order") %>% select(-itemindex) %>% select(order, everything())
  
  # grab first pattern again so we can fill things in
  # now, detect first pattern match so we can merge with pattern data tibbles allowing us to have the page number, electorate, etc
  patterns_data <- patterns_data %>% 
    rename(page.no_use = page.no,
           time.stamp_use = time.stamp,
           name_use = name,
           name_short_use = name_short,
           name.id_use = name.id,
           electorate_use = electorate,
           party_use = party,
           role_use = role,
           in.gov_use = in.gov,
           first.speech_use = first.speech) %>% 
    unique()

  # grab first pattern and merge with pattern data table
  main <- main %>% mutate(first_pattern = ifelse(str_detect(body, paste0("^", patterns_text_esc, collapse = "|")),
                                                 str_match(body, paste0("^", patterns_text_esc, collapse = "|")),
                                                 NA)) %>% 
    merge(., patterns_data, by="first_pattern", all.x = T) %>% 
    as_tibble()
  
  # if the entire body is just equal to the first_pattern, paste in "interjecting" b/c that's the case when the interjection is documented 
  # but there's no text associated with it
  main <- main %>% mutate(body = ifelse(body==paste(first_pattern), paste0(first_pattern, "interjecting"), body))
  
  # fill in appropriate info, remove pattern
  main <- main %>% 
    mutate(page.no = ifelse(is.na(page.no_use), page.no, page.no_use),
           name = ifelse(is.na(name_use), name, name_use),
           name_short = ifelse(is.na(name_short_use), name_short, name_short_use),
           electorate = ifelse(is.na(electorate_use), electorate, electorate_use),
           name.id = ifelse(is.na(name.id_use), name.id, name.id_use),
           role = ifelse(is.na(role_use), role, role_use),
           in.gov = ifelse(is.na(in.gov_use), in.gov, in.gov_use),
           first.speech = ifelse(is.na(first.speech_use), first.speech, first.speech_use),
           party = as.factor(ifelse(is.na(party_use), as.character(party), as.character(party_use)))) %>% 
    select(-c(name_use, party_use, electorate_use, name.id_use, name_short_use, in.gov_use, first.speech_use, time.stamp_use, role_use, page.no_use, first_pattern)) %>% 
    arrange(order) %>% 
    mutate(body = ifelse(str_detect(body, paste0("^", patterns_text_esc, collapse = "|")),
                         str_remove(body, paste0("^", patterns_text_esc, collapse = "|")),
                         body),
           body = ifelse(str_detect(body, "^—|^-"),
                         str_remove(body, "^—|^-"),
                         body)) %>% 
    mutate(role = ifelse(str_detect(role, "^—"), str_remove(role, "^—"), role),
           role = ifelse(str_detect(role, "\\)$"), str_remove(role, "\\)$"), role),
           electorate = ifelse(str_detect(electorate, "^\\("), str_remove(electorate, "^\\("), electorate),
           electorate = ifelse(str_detect(electorate, "\\)$"), str_remove(electorate, "\\)$"), electorate))
  
  # if name is interjecting, we would've removed that when we took out the pattern, so let's paste it back in
  main <- main %>% mutate(body = ifelse(body==" interjecting—" & !is.na(name_short), paste0(name_short, body), body))
  
  # extract short name version of deputy speaker and remove it from body
  main <- main %>% mutate(name_short = ifelse(str_detect(name_short, "DEPUTY SPEAKER") & str_detect(body, "^\\(Ms.{1,35}\\)|^\\(Mrs.{1,35}\\)|^\\(Mr.{1,35}\\)|^\\(Dr.{1,35}\\)|^\\(Hon\\. .{1,35}\\)"),
                                                        str_extract(body, "(?<=^\\()Ms.{1,35}(?=\\))|(?<=^\\()Mrs.{1,35}(?=\\))|(?<=^\\()Mr.{1,35}(?=\\))|(?<=^\\()Dr.{1,35}(?=\\))|(?<=^\\()Hon\\. .{1,35}(?=\\))"),
                                                        name_short),
                                    body = ifelse(str_detect(body, "^\\(Ms.{1,35}\\)|^\\(Mrs.{1,35}\\)|^\\(Mr.{1,35}\\)|^\\(Dr.{1,35}\\)|^\\(Hon\\. .{1,35}\\)"),
                                                  str_remove(body, "^\\(Ms.{1,35}\\)|^\\(Mrs.{1,35}\\)|^\\(Mr.{1,35}\\)|^\\(Dr.{1,35}\\)|^\\(Hon\\. .{1,35}\\)"),
                                                  body),
                                    body = ifelse(str_detect(body, "^—|^-"),
                                                  str_remove(body, "^—|^-"),
                                                  body))

  # when people give notices, their name_short is usually preceded by the notice but removed in the steps above b/c it's detected as part of the pattern
  # so, let's paste that back in for clarity
  main <- main %>% mutate(body = ifelse(str_detect(body, "^ to present a Bill|^ to  present a Bill for|^ to move\\:"), paste0(name_short, body), body))
  
  # remove any leftover subdebate 1 or 2 patterns that we forgot to remove
  if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)>0) {
    main <- main %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             str_match(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             NA),
                                       sub2_pattern = ifelse(str_detect(body, paste0(sub2_patterns_info, collapse = "|")),
                                                             str_match(body, paste0(sub2_patterns_info, collapse = "|")),
                                                             NA),
                                       sub2_pattern = ifelse(!is.na(sub1_pattern) & !is.na(sub2_pattern) 
                                                             & str_detect(body, paste0(sub1_pattern, sub2_pattern)),
                                                             str_match(body, paste0(sub1_pattern, sub2_pattern)),
                                                             NA)) %>% 
      mutate(sub1_pattern = str_replace_all(sub1_pattern, "\\(","\\\\("),
             sub1_pattern = str_replace_all(sub1_pattern, "\\)","\\\\)"),
             sub1_pattern = str_replace_all(sub1_pattern, "\\:","\\\\:"),
             sub1_pattern = str_replace_all(sub1_pattern, "\\.","\\\\."),
             sub1_pattern = str_replace_all(sub1_pattern, "\\?","\\\\?"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\(","\\\\("),
             sub2_pattern = str_replace_all(sub2_pattern, "\\)","\\\\)"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\:","\\\\:"),
             sub2_pattern = str_replace_all(sub2_pattern, "\\.","\\\\."),
             sub2_pattern = str_replace_all(sub2_pattern, "\\?","\\\\?")) %>% 
      mutate(body = ifelse(!is.na(sub2_pattern) & str_detect(body, paste0(sub2_pattern)),
                           str_remove(body, paste0(sub2_pattern)),
                           body),
             body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0(sub1_pattern)),
                           str_remove(body, paste0(sub1_pattern)),
                           body)) %>% 
      mutate(sub1_flag = ifelse(!is.na(sub1_pattern) & is.na(sub2_pattern), 1, 0),
             sub2_flag = ifelse(!is.na(sub2_pattern), 1, 0)) %>% 
      select(-sub1_pattern, -sub2_pattern)
  } else if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)==0) {
    main <- main %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             str_match_all(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             NA),
                                       body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0(sub1_pattern)),
                                                     str_remove(body, paste0(sub1_pattern)),
                                                     body)) %>% 
      mutate(sub1_flag = ifelse(!is.na(sub1_pattern), 1, 0),
             sub2_flag = 0) %>% 
      select(-sub1_pattern)
  }
  
  ######### BELOW IS COMMENTED OUT B/C CAUSING ISSUES - this is something to deal with later, look at 2002-09-26 row 17-18 for example
  # # sometimes new statements begin with the short name, followed by their electorate and role, and the time stamp
  # # we want to separate rows on these, and put the name in the name variable, so let's grab a list of matches from main
  # phrases_split <- str_extract_all(main$body, "M.{1,2}[[:space:]].{0,20}[[:space:]]\\(.{0,250}\\)[[:space:]]{0,2}\\(\\d{1,2}\\.\\d\\d [[:alpha:]]\\.[[:alpha:]]\\.\\)—") %>% unlist() %>% na.omit()
  # 
  # # escape special characters
  # phrases_split <- phrases_split %>% as_tibble() %>% mutate(value=str_replace_all(value,"\\(", "\\\\("),
  #                                          value=str_replace_all(value,"\\)", "\\\\)"),
  #                                          value=str_replace_all(value,"\\.", "\\\\.")) %>% pull()
  # 
  # # split on phrases, filter out empty rows from split
  # main <- separate_rows(main, body, sep=paste0("(?=", phrases_split, ")", collapse = "|")) %>% filter(body!="")
  # 
  # # if a line begins with a phrase we split on, extract the name and paste it in - do this for name short as well since they're the same
  # main <- main %>% mutate(name = ifelse(str_detect(body, paste0("^", phrases_split, collapse = "|")),
  #                               str_extract(body, "^M.{1,2}[[:space:]].{0,20}(?=[[:space:]]\\(.{0,250}\\)[[:space:]]{0,2}\\(\\d{1,2}\\.\\d\\d [[:alpha:]]\\.[[:alpha:]]\\.\\)—)"),
  #                               name),
  #                         name_short = ifelse(str_detect(body, paste0("^", phrases_split, collapse = "|")),
  #                                       str_extract(body, "^M.{1,2}[[:space:]].{0,20}(?=[[:space:]]\\(.{0,250}\\)[[:space:]]{0,2}\\(\\d{1,2}\\.\\d\\d [[:alpha:]]\\.[[:alpha:]]\\.\\)—)"),
  #                                       name_short),
  #                         body = str_remove(body, paste0("^", phrases_split, collapse = "|")))
  
  ######### ANSWERS TO QUESTIONS #########
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions"))))>0) {
    
    # debate info
    a_to_q_deb_info <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/debateinfo"),
                              collectNames = F, homogeneous = T), .name_repair = "unique")
    
    # sometimes page number is written twice so multiple of the same column, just grab the first one
    if ("page.no...2" %in% names(a_to_q_deb_info)) {
      a_to_q_deb_info <- a_to_q_deb_info %>% rename(page.no = page.no...2) %>% 
        select(c(title, page.no, id.no)) %>% 
        mutate(page.no = as.numeric(page.no))
    }
    
    # sub-debate info
    a_to_q_sub_info <- item_df(xml_df,"//answers.to.questions/debate/subdebate.1/subdebateinfo") %>% 
      unnest(everything())
      
      #tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/subdebateinfo"), collectNames = F, homogeneous = T), .name_repair = "unique")
    
    # sometimes page number is written twice so multiple of the same column, just grab the first one
    if ("page.no...2" %in% names(a_to_q_sub_info)) {
      a_to_q_sub_info <- a_to_q_sub_info %>% rename(page.no = page.no...2) %>% 
        select(c(title, page.no, id.no)) %>% 
        mutate(page.no = as.numeric(page.no))
    }
    
    # all question info, cleaning stuff up and fixing spacing issue as best as possible
    a_to_q_speech_qs <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.start/para"), 
                                              collectNames = F, homogeneous = T), .name_repair = "unique") %>% 
      cbind(item_df(xml_df, "//answers.to.questions/debate/subdebate.1/question/talk.start/talker"), .) %>% 
      mutate(name = map(name, c)) %>% 
      unnest_wider(name, names_sep = "_") %>% rename(name = name_1,
                                                name_short = name_2) %>% 
       unnest(everything()) %>% 
      mutate(first.speech = {if("first.speech" %in% names(.)) first.speech else NA},
             role = {if("role" %in% names(.)) role else NA}) %>% 
      select(-itemindex) %>% 
      select(page.no, name, name.id, electorate, party, in.gov, first.speech, everything()) %>% 
      unite("body", c(first.speech:last_col(), -first.speech), sep=" ", na.rm = T) %>% 
      cbind(., tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question"), 
                                     collectNames = F, homogeneous = T), .name_repair = "unique")) %>% 
      select(-talk.start) %>% 
      unite("body", c(body:last_col()), sep=" ", na.rm = T) %>% 
      mutate(body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                           body),
             body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                           body),
             body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\;[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\;(?=[[:upper:]])", "; "),
                           body),
             body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\?[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\?(?=[[:upper:]])", "? "),
                           body),
             question = 1,
             answer = 0)
    
    # all answer info, cleaning stuff up and fixing spacing issue as best as possible
    a_to_q_speech_as <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.start/para"), 
                                              collectNames = F, homogeneous = T), .name_repair = "unique") %>% 
      cbind(item_df(xml_df, "//answers.to.questions/debate/subdebate.1/answer/talk.start/talker"), .) %>% 
      mutate(name = map(name, c)) %>% 
      unnest_wider(name, names_sep = "_") %>% rename(name = name_1,
                                                     name_short = name_2) %>% 
      unnest(everything()) %>% 
      mutate(first.speech = {if("first.speech" %in% names(.)) first.speech else NA},
             role = {if("role" %in% names(.)) role else NA}) %>% 
      select(-itemindex) %>% 
      select(page.no, name, name.id, electorate, party, in.gov, first.speech, role, everything()) %>% 
      unite("body", c(first.speech:last_col(), -first.speech), sep=" ", na.rm = T) %>% 
      mutate(body = ifelse(str_detect(body, "^—"), str_remove(body, "^—"), body)) %>% 
      cbind(., tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer"), 
                                     collectNames = F, homogeneous = T), .name_repair = "unique")) %>% 
      mutate(first.speech = {if("first.speech" %in% names(.)) first.speech else NA},
             role = {if("role" %in% names(.)) role else NA}) %>% 
      select(-talk.start) %>% 
      select(page.no, name, name.id, electorate, party, role, in.gov, body, everything()) %>% 
      unite("body", body:last_col(), sep=" ", na.rm = T) %>% 
      mutate(body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                           body),
             body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                           body),
             body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\;[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\;(?=[[:upper:]])", "; "),
                           body),
             body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\?[[:upper:]]"), 
                           str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\?(?=[[:upper:]])", "? "),
                           body),
             question = 0,
             answer = 1)
    
    # combine them in the right order, add flags
    a_to_q_speech <- lst(a_to_q_speech_qs, a_to_q_speech_as) %>% 
      map(rowid_to_column) %>% 
      bind_rows() %>% 
      arrange(rowid) %>% 
      select(-rowid, -role) %>% 
      mutate(fedchamb_flag = 0,
             q_in_writing = 1,
             page.no = as.numeric(page.no))
    
  } else {
    a_to_q_deb_info <- tibble()
    a_to_q_sub_info <- tibble()
    a_to_q_speech <- tibble()
  }
  
  # add questions in writing to end of main
  main <- main %>% mutate(page.no = as.numeric(page.no)) %>% 
    arrange(order) %>% 
    select(-order) %>% 
    mutate(question = 0,
           answer = 0,
           q_in_writing = 0) %>% 
    bind_rows(., a_to_q_speech)
  
  # add business start to main
  if (sum(is.na(bus_start$time.stamp))==0){
    main <- bind_rows(bus_start, main) %>% 
      group_by(fedchamb_flag) %>% 
      mutate(page.no = ifelse(is.na(page.no), min(page.no, na.rm=TRUE), page.no)) %>% 
      ungroup() %>% 
      arrange(fedchamb_flag)
  } else {
    main <- bind_rows(bus_start, main) %>% 
      group_by(fedchamb_flag) %>% 
      mutate(page.no = ifelse(is.na(page.no), min(page.no, na.rm=TRUE), page.no)) %>% 
      ungroup() %>% 
      arrange(fedchamb_flag)
  }
  
  # grab adjournment, add all flags, select things in right order to be added to main - CHAMBER
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment"),collectNames = F, homogeneous = T), .name_repair = "unique"))==1){
    
    # sometimes there is no page number and time.stamp (i.e. adjournment info), so we need to add an extra if-else for that case
    if (nrow(item_df(xml_df, "//chamber.xscript/adjournment/adjournmentinfo"))>0){
      adjournment_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment/adjournmentinfo"),
                                                 collectNames = F, homogeneous = T), .name_repair = "unique")
      
      # sometimes page number is written twice so multiple of the same column, just grab the first one
      if ("page.no...2" %in% names(adjournment_chamb)) {
        adjournment_chamb <- adjournment_chamb %>% rename(page.no = page.no...1) %>% 
          select(c(page.no, time.stamp)) %>% 
          mutate(page.no = as.numeric(page.no))
      }
      
      adjournment_chamb <- cbind(adjournment_chamb, xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment/para"))) %>% 
        as_tibble() %>% 
        rename(body = last_col()) %>% 
        mutate(name = "stage direction",
               name.id = NA,
               electorate = NA,
               party = NA,
               in.gov = NA,
               question = 0,
               answer = 0,
               sub1_flag = 0,
               sub2_flag = 0,
               fedchamb_flag = 0,
               q_in_writing = 0,
               page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NA},
               time.stamp = {if("time.stamp" %in% names(.)) time.stamp else NA}) %>% 
        select(page.no, time.stamp, name, name.id, electorate, party, in.gov, body, question, answer, sub1_flag, sub2_flag, fedchamb_flag, q_in_writing)
    } else {
      # case when there is no adjournment info, it's just empty
      adjournment_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment"),
                                                 collectNames = F, homogeneous = T), .name_repair = "unique") %>% 
        rename(body = last_col()) %>% 
        mutate(name = "stage direction",
               name.id = NA,
               electorate = NA,
               party = NA,
               in.gov = NA,
               question = 0,
               answer = 0,
               sub1_flag = 0,
               sub2_flag = 0,
               fedchamb_flag = 0,
               q_in_writing = 0,
               page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NA},
               time.stamp = {if("time.stamp" %in% names(.)) time.stamp else NA}) %>% 
        select(page.no, time.stamp, name, name.id, electorate, party, in.gov, body, question, answer, sub1_flag, sub2_flag, fedchamb_flag, q_in_writing)
    }
    
    
    
  } else if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment"),collectNames = F, homogeneous = T), .name_repair = "unique"))>1) {
    # sometimes there's a random extra empty line in the adjournment, we need to get rid of that
    adjournment_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment/adjournmentinfo"))) %>% filter(!is.na(time.stamp))
    
    # sometimes page number is written twice so multiple of the same column, just grab the first one
    if ("page.no...2" %in% names(adjournment_chamb)) {
      adjournment_chamb <- adjournment_chamb %>% rename(page.no = page.no...1) %>% 
        select(c(page.no, time.stamp)) %>% 
        mutate(page.no = as.numeric(page.no))
    }
    
    adjournment_chamb <- cbind(adjournment_chamb, xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment/para")) %>% filter(!is.na(text))) %>% 
      as_tibble() %>% 
      select(page.no, time.stamp, text) %>% 
      rename(body = last_col()) %>% 
      mutate(name = "stage direction",
             name.id = NA,
             electorate = NA,
             party = NA,
             in.gov = NA,
             question = 0,
             answer = 0,
             sub1_flag = 0,
             sub2_flag = 0,
             fedchamb_flag = 0,
             q_in_writing = 0,
             page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NA},
             time.stamp = {if("time.stamp" %in% names(.)) time.stamp else NA}) %>% 
      select(page.no, time.stamp, name, name.id, electorate, party, in.gov, body, question, answer, sub1_flag, sub2_flag, fedchamb_flag, q_in_writing)
    
  } else {
    adjournment_chamb <- tibble()
  }
  
  # grab adjournment, add all flags, select things in right order to be added to main - FEDERATION CHAMBER
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/adjournment/adjournmentinfo"),collectNames = F, homogeneous = T), .name_repair = "unique"))>0){
    adjournment_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/adjournment/adjournmentinfo"),
                                               collectNames = F, homogeneous = T), .name_repair = "unique")
    
    # sometimes page number is written twice so multiple of the same column, just grab the first one
    if ("page.no...2" %in% names(adjournment_fed)) {
      adjournment_fed <- adjournment_fed %>% rename(page.no = page.no...1) %>% 
        select(c(page.no, time.stamp)) %>% 
        mutate(page.no = as.numeric(page.no))
    }
    
    # bind adjournment details with associated text, add empty cols to prepare for merge with main
    adjournment_fed <- cbind(adjournment_fed, xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/adjournment/para"))) %>% 
      as_tibble() %>% 
      rename(body = last_col()) %>% 
      mutate(name = "stage direction",
             name.id = NA,
             electorate = NA,
             party = NA,
             in.gov = NA,
             question = 0,
             answer = 0,
             sub1_flag = 0,
             sub2_flag = 0,
             fedchamb_flag = 1,
             q_in_writing = 0,
             page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NA},
             time.stamp = {if("time.stamp" %in% names(.)) time.stamp else NA}) %>% 
      select(page.no, time.stamp, name, name.id, electorate, party, in.gov, body, question, answer, sub1_flag, sub2_flag, fedchamb_flag, q_in_writing)
  } else {
    adjournment_fed <- tibble()
  }
  
  # bind adjournments together
  adjournment <- rbind(adjournment_chamb, adjournment_fed)
  
  # manually fix flag in case maincommittee adjournment is nested in chamber.xscript
  if (nrow(adjournment)>0) {
    adjournment <- adjournment %>% mutate(fedchamb_flag = ifelse(str_detect(body, "Main Committee"), 1, 0))
  }
  
  # add adjournment to main
  if (nrow(adjournment)>0){
    main <- bind_rows(main,adjournment) %>% 
      group_by(fedchamb_flag) %>% 
      mutate(page.no = ifelse(is.na(page.no), max(page.no, na.rm=TRUE), page.no)) %>% 
      ungroup() %>% 
      arrange(fedchamb_flag)
  }
  
  # issue specific to 2005-09-08 row 287
  main <- main %>% mutate(name = ifelse(str_detect(body, "^DANBY, Mr Michael"), "Danby, Michael, MP", name),
                          time.stamp = ifelse(str_detect(body, "^DANBY, Mr Michael, Melbourne Ports14812\\.31 pmDanby, Michael, MPWF6Melbourne PortsALP00Mr DANBY\\(Melbourne Ports\\)\\(12\\.31 pm\\)—"),
                                              "12:31:00", time.stamp),
                          body = ifelse(str_detect(body, "^DANBY, Mr Michael, Melbourne Ports14812\\.31 pmDanby, Michael, MPWF6Melbourne PortsALP00Mr DANBY\\(Melbourne Ports\\)\\(12\\.31 pm\\)—"),
                                        str_remove(body, "^DANBY, Mr Michael, Melbourne Ports14812\\.31 pmDanby, Michael, MPWF6Melbourne PortsALP00Mr DANBY\\(Melbourne Ports\\)\\(12\\.31 pm\\)—"),
                                        body))
  
  # now that everything is in the main, group by fedchamb flag and fill time stamp down, then ungroup
  main <- main %>% group_by(fedchamb_flag) %>% fill(time.stamp, .direction = "down") %>% ungroup()
  
  # sometimes if a pattern is missing a couple digits then it won't be removed from the body, they'll just be left on their own lines, so filter those out
  main <- main %>% filter(!str_detect(body, "^\\d{1,2}$"))
  
  # if a row's entire body is just a sub-debate title preceded by a single digit (this happened on 2007-05-08), remove the contents b/c we don't want to keep this row
  # filter it out in next step
  main <- main %>% mutate(body = ifelse(str_detect(body, paste0("^\\d", sub2_patterns_info, "$", collapse = "|")),
                                        "", body))
  
  # get rid of any random empty rows
  main <- main %>% filter(body!="") %>% filter(body!=" ")
  
  # add order variable and select variables in the order we want
  main <- main %>% rowid_to_column("order") %>% 
      select(order, speech_no, page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag, question, answer, q_in_writing, name_short, role)
  
  # add flag for divisions
  main <- main %>% mutate(div_flag = ifelse(str_detect(body, "The House divided\\."), 1, 0))
  
  # need to extract any short names which precede statement and weren't captured in the original pattern extraction process
  main <- main %>% mutate(name_short = ifelse(name_short=="", NA, name_short), 
                  name_short = ifelse(str_detect(body, "^Mrs.{1,20}(?=—.{1,})|^Mr.{1,20}(?=—.{1,})|^Ms.{1,20}(?=—.{1,})|^Dr.{1,20}(?=—.{1,})") & 
                                        is.na(name_short) & 
                                        str_detect(name, str_to_title(str_extract(body, "(?<=^Mr ).{1,20}(?=—.{1,})"))),
                                      str_extract(body, "^Mrs.{1,20}(?=—.{1,})|^Mr.{1,20}(?=—.{1,})|^Ms.{1,20}(?=—.{1,})|^Dr.{1,20}(?=—.{1,})"),
                                      name_short),
                  name_short = ifelse(str_detect(body, "^[[:space:]]{0,2}Mrs.{1,20}(?=\\(.{1,})|^[[:space:]]{0,2}Mr.{1,20}(?=\\(.{1,})|^[[:space:]]{0,2}Ms.{1,20}(?=\\(.{1,})|^[[:space:]]{0,2}Dr.{1,20}(?=\\(.{1,})") & 
                                        is.na(name_short),
                                        str_extract(body, "^[[:space:]]{0,2}Mrs.{1,20}(?=\\(.{1,})|^Mr.{1,20}(?=\\(.{1,})|^Ms.{1,20}(?=\\(.{1,})|^Dr.{1,20}(?=\\(.{1,})"),
                                      name_short),
                  name_short = ifelse(str_detect(body, "^[[:space:]]{0,3}\\(Mrs.{1,20}\\)|^[[:space:]]{0,3}\\(Mr.{1,20}\\)|^[[:space:]]{0,3}\\(Ms.{1,20}\\)|^[[:space:]]{0,3}\\(Dr.{1,20}\\)") &
                                        is.na(name_short),
                                      str_extract(body, "(?<=^[[:space:]]{0,3}\\()Mrs.{1,20}(?=\\))|(?<=^[[:space:]]{0,3}\\()Ms.{1,20}(?=\\))|(?<=^[[:space:]]{0,3}\\()Mr.{1,20}(?=\\))|(?<=^[[:space:]]{0,3}\\()Dr.{1,20}(?=\\))"),
                                      name_short))
  
  # if someones name is missing but we have a name_short, paste it in the name column
  main <- main %>% mutate(name = ifelse(is.na(name) & !is.na(name_short), name_short, name))
  
  # grab date of sitting day from filename so we can filter people who are alive in next step
  thisDate <- as.Date(str_remove(filename, "\\.xml$"))
  
  # on 2000-08-31 Zahra Christian was given Dr Nelson's name_short and name.id - we need to manually fix this
  main <- main %>% mutate(name_short = ifelse(name=="Zahra, Christian, MP" & name_short=="Dr NELSON" & name.id=="RW5",
                                        "Mr Zahra", name_short),
                    name.id = ifelse(name=="Zahra, Christian, MP" & name.id=="RW5", "84H", name.id))
  
  # 2000-12-04 same issue for Kelly Jackie
  main <- main %>% mutate(name_short = ifelse(name=="Kelly, Jackie, MP" & name_short=="Mr CREAN" & name.id=="DT4",
                                              "Ms Kelly Jackie", name_short),
                          name.id = ifelse(name=="Kelly, Jackie, MP" & name.id=="DT4", "GK6", name.id))
  
  # 2000-04-13 mr slipper given mr speaker's name as name_short, need to fix
  main <- main %>% mutate(name = ifelse(name=="SPEAKER, Mr" & name_short=="Mr Slipper" & name.id=="0V5",
                                              "Slipper, Peter, MP", name))
  
  # from 2002-09-18
  main <- main %>% mutate(name = ifelse(name=="Mr McMULLAN,MP" & name.id=="5I4",
                                        "McMullan, Bob, MP", name))
  
  main <- main %>% mutate(name= ifelse(name=="Mr CADMAN,MP" & name.id=="SD4",
                                       "Cadman, Alan, MP", name))
  
  main <- main %>% mutate(name= ifelse(name=="Mr McGAURAN,MP" & name.id=="XH4",
                                       "McGauran, Peter, MP", name))
  
  # from 2005-09-07
  main <- main %>% mutate(name = ifelse(name=="McGauran, and the Peter, MP","McGauran, Peter, MP", name))
  
  # from 2003-03-06
  main <- main %>% mutate(name = ifelse(name=="Abbott, Tony, MP" & name_short =="Mr SWAN" & name.id=="2V5", "Swan, Wayne, MP", name))
  
  # from 2004-03-22
  main <- main %>% mutate(name = ifelse(name=="Latham, Mark, MPis the Prime Minister now expecting", "Latham, Mark, MP", name))
  
  # from 1999-02-08
  main <- main %>% mutate(name = ifelse(name=="Hoare, Kelly, MP" & name_short=="Mr SPEAKER" & name.id=="10000",
                                              "Mr SPEAKER", name),
                          party = ifelse(name=="Mr SPEAKER" & party=="ALP", NA, party),
                          electorate = ifelse(name=="Mr SPEAKER" & electorate=="Charlton", NA, electorate))
  
  main <- main %>% mutate(name = ifelse(name=="Beazley, Kim, MP" & name_short=="Mr SPEAKER" & name.id=="10000",
                                        "Mr SPEAKER", name),
                          party = ifelse(name=="Mr SPEAKER" & party=="ALP", NA, party),
                          electorate = ifelse(name=="Mr SPEAKER" & electorate=="Brand", NA, electorate))
  
  # 2000-06-20 issue b/c sometimes name short and name ID are unique but other times they're just the general ones, and this will give us extra rows in the lookup table
  # let's just keep it general b/c it's easier to do and then just back out later
  main <- main %>% mutate(name.id = ifelse(name=="SPEAKER, The" & name.id!="10000", "10000", name.id),
                  name_short = ifelse(name=="SPEAKER, The" & name_short!="The SPEAKER", "The SPEAKER", name_short))
  
  # 2008-06-23
  main <- main %>% mutate(name = ifelse(name ==", Bob, MP" & name.id=="8IS", "Debus, Bob, MP", name))
  
  # trying to keep things general for the speaker so there aren't merging issues with the lookup table - ex 1999-02-08
  main <- main %>% mutate(name.id = ifelse(name_short=="Mr SPEAKER", "10000", name.id),
                          electorate = ifelse(name_short=="Mr SPEAKER", NA, electorate),
                          party = ifelse(name_short=="Mr SPEAKER", NA, party))
  
  #1998-03-04
  # causing issues
  #main <- main %>% mutate(name = ifelse(name_short=="Mr SPEAKER" & name.id=="10000" & !str_detect(name, "SPEAKER"), "Mr SPEAKER", name))
  
  # creating lookup table to fill name party electorate and name ID with
  # sometimes the full name given is just the short name, so
  # if name column contains just the prefix and the last name / first and last name, paste that into name_short b/c it isn't the full name
  # if electorate or party or name ID is just a space, make it NA
  lookup <- main %>% select(name, name_short, party, electorate, name.id, role) %>% 
    unique() %>% 
    mutate(name_short = ifelse(is.na(name_short) & str_detect(name, "^Mrs|^Ms|^Mr|^Dr"), name, name_short),
           name_short = ifelse(name_short=="", NA, name_short)) %>% 
    filter(!str_detect(name, "business start|stage direction|member")) %>%
    mutate(electorate = ifelse(electorate=="", NA, electorate),
           party = ifelse(party=="", NA, party),
           name.id = ifelse(name.id=="", NA, name.id),
           name.id = ifelse(name.id=="UNKNOWN", NA, name.id)) %>% 
    group_by(name_short) %>% 
    mutate(name = ifelse(n()>1 & name==name_short & !is.na(name_short), NA, name)) %>% 
    fill(name, .direction = "updown") %>% 
    ungroup() %>% 
    group_by(name) %>% 
    fill(c(party, name.id, electorate), .direction = "updown") %>% 
    ungroup() %>% 
    group_by(name.id) %>% 
    fill(c(party, electorate), .direction = "updown") %>% 
    ungroup()
  
  # escape any parentheses in the role so there aren't issues with str_detect in next step
  lookup <- lookup %>% mutate(role = str_replace_all(role, "\\(", "\\\\("),
                              role = str_replace_all(role, "\\)", "\\\\)"))
  
  # if someones name contains their role in brackets at the end, remove that and add ", MP" for consistency
  lookup <- lookup %>% mutate(name = ifelse(str_detect(name, paste0(" \\(", role, "\\)$")), 
                                            str_replace(name, paste0(" \\(", role, "\\)$"), ", MP"),
                                            name)) %>% 
    select(-role)
  
  # if someone's prefix is included in the full name, remove that
  lookup <- lookup %>% mutate(name = ifelse(str_detect(name, "^Mrs |^Mr |^Ms |^Dr ") & !str_detect(name, "SPEAKER"),
                                  str_remove(name, "^Mrs |^Mr |^Ms |^Dr "),
                                  name))
  
  # special case - his last name is capitalized and it's causing issues b/c other times it's not in all caps
  # fix for consistency
  lookup <- lookup %>% mutate(name = ifelse(name=="REITH, Peter, MP", "Reith, Peter, MP", name))
  
  # if someones name short is Mr SPEAKER but their name doesn't include SPEAKER, drop those (will cause merging issues)
  lookup <-  lookup %>% filter(!(!is.na(name_short) & str_detect(name_short, "Mr SPEAKER") & !str_detect(name, "SPEAKER")))
  
  # lets extract the first and last name so we can create display names which will match those of AusPol pkg
  # extract title/prefix as well
  # notice we're also removing "Dr" from names, that's because when ppl have a Dr title it's often included in their full name before their first name
  # this wouldn't be in the display name so we don't want to extract it by accident with the first name
  lookup <- lookup %>% mutate(full_name = name,
                              name = str_remove(full_name, "[:space:],[:space:]MP|[:space:]MP|,MP"),
                              name = str_remove(name, ",$"),
                              name = ifelse(str_detect(name, "\\(.{1,35}\\)$") & !str_detect(name,"SPEAKER\\)$"),
                                            str_remove(name, "\\(.{1,35}\\)$"), name),
                              name = ifelse(str_detect(name_short, "^Dr[[:space:]]") & str_detect(name, ",[[:space:]]Dr[[:space:]][[:alpha:]]"),
                                            str_remove(name, "(?<=,[[:space:]])Dr[[:space:]](?=[[:alpha:]])"), name),
                              first_name = ifelse(!str_detect(name, "SPEAKER\\)$"), str_extract(name, "(?<=\\,).{0,50}$"), NA),
                              first_name = str_replace_all(first_name, "^[:blank:]", ""),
                              last_name = str_extract(name, "^[:alpha:]{0,35},|^[:alpha:]{0,35}[:space:][:alpha:]{0,35},|^[:alpha:]{0,35}[:punct:][:alpha:]{0,35},|^[:alpha:]{1,35}[[:space:]]{0,3}$"),
                              last_name = str_replace_all(last_name, "[:punct:]$", ""),
                              first_name = ifelse(str_detect(name, "DEPUTY") & is.na(first_name),
                                                  str_extract(name, "(?<=,[[:space:]]).{1,50}(?=[[:space:]]\\()"),
                                                  first_name)) %>% 
    mutate(title = ifelse(str_detect(name_short, "Mrs "), "Mrs", NA),
           title = ifelse(is.na(title) & str_detect(name_short, "Mr "), "Mr", title),
           title = ifelse(is.na(title) & str_detect(name_short, "Ms "), "Ms", title),
           title = ifelse(is.na(title) & str_detect(name_short, "Dr "), "Dr", title),
           title = ifelse(is.na(title) & str_detect(name_short, "Madam "), "Ms", title),
           title = ifelse(is.na(title) & str_detect(name_short, "Miss "), "Ms", title))
  
  # if any names start or end with whitespace, remove that
  lookup <- lookup %>% mutate(name = str_trim(name, side="both"),
                              first_name = str_trim(first_name, side="both"),
                              last_name = str_trim(last_name, side="both"),
                              name_short = str_trim(name_short, side="both"),
                              full_name = str_trim(full_name, side="both"))
  
  # if someones full name is literally just their last name, remove that
  # this is happening because sometimes all we have for someone is the short version of their name, and we remove the prefix leaving us just with their last name
  # we don't want these b/c we can just paste their name_short in for their name later
  # keeping them is causing some issues and not filling right
  lookup <- lookup %>% filter(full_name!=last_name)
  
  # if last name is all caps, make it title style
  lookup <- lookup %>% mutate(last_name = ifelse(grepl("^[[:upper:]]+$", last_name),
                                       str_to_title(last_name),
                                       last_name))
  
  
  # in case full_name is equal to name_short but the actual full name exists, we want to use that
  # also fill stuff by grouping by name id
  # rename name to full name
  lookup <- lookup %>% group_by(name_short) %>% 
    mutate(name = ifelse(!is.na(name) & !is.na(name_short) & name_short==name & n()>1, NA, name)) %>% 
    ungroup() %>% 
    group_by(name.id) %>% 
    fill(c(first_name, last_name, electorate, name, party, title), .direction = "updown") %>% 
    ungroup()
  
  # combine short name forms into one cell
  # need to ensure those referred to both with and without deputy speaker title are kept separate, as title may not be correct to have for all of their speeches
  lookup <- lookup %>%  
    mutate(deputy_flag = str_detect(full_name, "DEPUTY SPEAKER")) %>% 
    group_by(last_name, first_name, deputy_flag) %>%
    filter(n()>1 & !is.na(name_short) | n()==1) %>% 
    mutate(name_short = paste0(name_short, collapse = "|")) %>% 
    ungroup() %>% 
    select(-deputy_flag) %>% 
    unique()
  
  # add display name to match AusPol displayName variable
  lookup <- lookup %>%  mutate(displayName = case_when(!str_detect(first_name, "[[:space:]]") & !str_detect(full_name, "\\,[[:space:]]Dr[[:space:]]") & str_detect(full_name, "\\,[[:space:]]MP$|[[:space:]]MP$|\\,[[:space:]]MP[[:space:]]\\(The|[[:space:]]\\(The DEPUTY SPEAKER\\)$") ~ 
                                                         str_extract(full_name, ".{1,50}(?=\\,[[:space:]]MP|[[:space:]]MP|[[:space:]]\\(The DEPUTY SPEAKER\\))"),
                                                       str_detect(first_name, "[[:space:]]") & str_detect(full_name, "\\,[[:space:]]MP$|[[:space:]]MP$|\\,[[:space:]]MP[[:space:]]\\(The|[[:space:]]\\(The DEPUTY SPEAKER\\)$") ~
                                                         str_extract(full_name, ".{1,30}(?=[[:space:]].{1,20}\\, MP|[[:space:]].{1,20}[[:space:]]MP|[[:space:]].{1,20}[[:space:]]\\(The DEPUTY SPEAKER\\))"),
                                                       !is.na(first_name) & !is.na(last_name) ~ paste0(last_name, ", ", first_name)),
                               displayName = ifelse(str_detect(displayName, "\\,$"), str_remove(displayName, "\\,$"), displayName))
  
  
  # this will be the master list of names for Hansard
  # fixing up women with title "Mrs" based on aph.gov.au website using gender search tool (these have prefixes)
  # keeping original prefix based on "all" b/c these may have changed at some point in time
  # filtering for people who are still alive on this specific sitting day
  master_list <- all %>% filter(deathDate > thisDate | is.na(deathDate)) %>% 
    select(c(uniqueID, surname, allOtherNames, firstName, commonName, displayName, title, gender)) %>% 
    mutate(title = ifelse(gender=="male" & is.na(title), "Mr", title),
           title = ifelse(gender=="female" & is.na(title), "Ms", title)) %>% 
    #select(-gender) %>% 
    rename(last_name = surname) %>% 
    mutate(title = ifelse(displayName=="Archer, Bridget", "Ms|Mrs", title),
           title = ifelse(displayName=="May, Margaret", "Ms|Mrs", title),
           title = ifelse(displayName=="Hull, Kay", "Ms|Mrs", title),
           title = ifelse(displayName=="McIntosh, Melissa", "Ms|Mrs", title),
           title = ifelse(displayName=="Phillips, Fiona", "Ms|Mrs", title),
           title = ifelse(displayName=="Haines, Helen", "Ms|Dr", title),
           title = ifelse(displayName=="Chalmers, Jim", "Mr|Dr", title),
           title = ifelse(displayName=="Freelander, Mike", "Mr|Dr", title),
           title = ifelse(displayName=="Gillespie, David", "Mr|Dr", title),
           title = ifelse(displayName=="Leigh, Andrew", "Mr|Dr", title),
           title = ifelse(displayName=="Wicks, Lucy", "Mrs|Ms", title),
           title = ifelse(displayName=="Kelly, De-Anne", "Mrs|Ms", title),
           title = ifelse(displayName=="Southcott, Andrew", "Mr|Dr", title),
           title = ifelse(displayName=="Kemp, David", "Mr|Dr", title),
           title = ifelse(displayName=="Washer, Mal", "Mr|Dr", title),
           title = ifelse(displayName=="Wooldridge, Michael", "Mr|Dr", title),
           title = ifelse(displayName=="Stone, Sharman", "Ms|Dr", title),
           title = ifelse(displayName=="Lawrence, Carmen", "Ms|Dr", title),
           title = ifelse(displayName=="Vale, Danna", "Mrs|Ms", title),
           title = ifelse(displayName=="Draper, Trish", "Mrs|Ms", title),
           title = ifelse(displayName=="Theophanous, Andrew", "Mr|Dr", title)) %>% 
    separate_rows(title, sep="\\|") %>% 
    mutate(displayName = ifelse(displayName=="Katter, Bob (Jr)", "Katter, Bob", displayName),
           displayName = ifelse(displayName=="O'Neill, Deborah", "O'Neill, Deb", displayName),
           displayName = ifelse(displayName=="Oakeshott, Rob", "Oakeshott, Robert", displayName),
           displayName = ifelse(displayName=="Somlyay, Alex", "Somlyay, Alexander", displayName),
           displayName = ifelse(displayName=="Pyne, Christopher", "Pyne, Chris", displayName),
           displayName = ifelse(displayName=="Anthony, Larry (Lawrence)", "Anthony, Larry", displayName))
  
  # merge lookup with master list by surname, displayname and title - this will get us the gender and uniqueID of everyone
  # don't group by name ID b/c deputy speaker and speakers all have 1000 and this will cause issues if we try to fill that way
  lookup <- left_join(lookup, master_list, by=c("last_name", "displayName", "title")) %>% 
    select(c(name_short, full_name, first_name, last_name, title, displayName, gender, uniqueID, name.id,electorate, party)) %>% 
    mutate(gender = ifelse(is.na(gender) & title=="Mr", "male", gender),
           gender = ifelse(is.na(gender) & title=="Mrs", "female", gender),
           gender = ifelse(is.na(gender) & title=="Ms", "female", gender))
  
  # remove rows where name_short is literally just "The/Mr/Madam Deputy Speaker" b/c we don't want to modify those since the deputy speaker can change throughout proceedings
  # also remove rows where there is no name_short (b/c nothing to fill in)
  # also remove rows where last name is just "SPEAKER" again don't wanna mess with speaker info, this can be easily backed out later
  lookup <- lookup %>% filter(name_short!="NA" & !is.na(name_short) & 
                                !str_detect(last_name, "SPEAKER") &
                                !str_detect(name_short, "SPEAKER$"))
  
  # filling in gender and unique ID
  lookup <- lookup %>% 
    group_by(first_name, last_name) %>% 
    fill(c(gender, uniqueID), .direction = "downup") %>% 
    ungroup()
  
  # if there happens to be a shorter and longer form of the name (ex. Coulton, Mark and Coulton, Mark MP), take the longer one
  lookup <- lookup %>% 
    group_by(last_name, first_name, displayName) %>% 
    filter(n()>1 & !str_detect(full_name, "\\)") & nchar(full_name)==max(nchar(full_name)) | n()==1 | str_detect(full_name, "\\)")) %>% 
    ungroup()
  
  # 04-04-2000 Dr Wooldridge attributed two electorates (Chisholm and Casey) - this is wrong it should just be Casey - fix
  if (thisDate=="2000-04-04") {
    lookup <- lookup %>% mutate(electorate = ifelse(uniqueID=="Wooldridge1956", "Casey", electorate)) %>% 
      unique()
  }
  
  # issue on 2000-09-04 where Anderson is Acting Speaker and "Acting" is being transcribed in his party like (NPActing) - let's fix this
  lookup <- lookup %>% mutate(party = ifelse(str_detect(party, "Acting$"), str_remove(party, "Acting$"), party))
  
  # for now, if someone is a deputy speaker and their name is duplicated b/c the name ID, electorate and party are full in one row but empty in another, let's just keep it general
  # by this i mean make name id 1000 and electorate/party NA - this is a general approach that will remove a lot of issues as we continue to parse, otherwise the code will keep breaking on each
  # individual where this is the case
  # TO DO - come back to this and make things more complete. just doing this now b/c of time crunch
  lookup <- lookup %>% group_by(full_name) %>% mutate(name.id = ifelse(duplicated(full_name) & str_detect(full_name, "DEPUTY SPEAKER") & name.id!="10000", 10000, name.id),
                                            party = ifelse(duplicated(full_name) & str_detect(full_name, "DEPUTY SPEAKER") & !is.na(party), NA, party),
                                            electorate = ifelse(duplicated(full_name) & str_detect(full_name, "DEPUTY SPEAKER") & !is.na(electorate), NA, electorate)) %>% 
    ungroup() %>% 
    unique()
  
  # clean up / modify lookup table for merge with main
  lookup <- lookup %>% 
    select(-c(first_name, last_name)) %>% 
    rename(name_use = full_name,
           electorate_use = electorate,
           name.id_use = name.id,
           party_use = party,
           name = name_short) %>% 
    separate_rows(name, sep="\\|") %>% 
    filter(name!=name_use & !str_detect(name, "MP$")) %>% 
    distinct()
  
  # fill stuff in 
  lookup <- lookup %>% 
    group_by(name_use) %>% 
    fill(c(uniqueID, gender, name.id_use, electorate_use, party_use), .direction = "updown") %>% 
    ungroup() %>% unique()
  
  # merge main with lookup table, replace names with correct name (as needed) and fill in missing info (name ID/party/electorate)
  # store nrow of main pre-merge to check on after merge
  nrow_main_before <- nrow(main)
  
  # perform merge
  main <- merge(main, lookup, by="name", all.x = T) %>%
    mutate(name = ifelse(is.na(name_use), name, name_use),
           electorate = ifelse(is.na(electorate_use), electorate, electorate_use),
           name.id = ifelse(is.na(name.id_use), name.id, name.id_use),
           party = as.factor(ifelse(is.na(party_use), as.character(party), as.character(party_use)))) %>% 
    mutate(name_short_esc = name_short,
           name_short_esc = str_replace_all(name_short_esc, "\\)","\\\\)"),
           name_short_esc = str_replace_all(name_short_esc, "\\(","\\\\(")) %>% 
    mutate(body = ifelse(str_detect(body, paste0("^", name_short_esc, "—")),
                         str_remove(body, paste0("^", name_short_esc, "—")),
                         body)) %>% 
    select(-c(name_use, party_use, electorate_use, name.id_use, name_short, displayName, role, title, name_short_esc)) %>% arrange(order)
  
  # check number of rows didn't change from merge
  stopifnot(nrow_main_before == nrow(main))
  
  # fill in gender
  main <- main %>% group_by(name) %>% 
    fill(c(gender, uniqueID), .direction = "downup") %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender) & str_detect(name, "^Mr[[:space:]]"), "male", gender),
           gender = ifelse(is.na(gender) & str_detect(name, "^Mrs[[:space:]]"), "female", gender),
           gender = ifelse(is.na(gender) & str_detect(name, "^Ms[[:space:]]"), "female", gender),
           gender = ifelse(is.na(gender) & str_detect(name, "^The DEPUTY SPEAKER \\(Ms[[:space:]]"), "female", gender),
           gender = ifelse(is.na(gender) & str_detect(name, "^The DEPUTY SPEAKER \\(Mrs[[:space:]]"), "female", gender),
           gender = ifelse(is.na(gender) & str_detect(name, "^The DEPUTY SPEAKER \\(Mr[[:space:]]"), "male", gender))
  
  # create gender and unique ID list to fill main with
  gender_uniqueID_list <- main %>% filter(is.na(gender) & is.na(uniqueID) & name!="business start" & name!="stage direction" 
                                          & name!="The SPEAKER" & name!="The DEPUTY SPEAKER" & !str_detect(name, "member")) %>% 
    select(c(name, name.id, party, electorate)) %>% 
    unique() %>% 
    mutate(displayName = ifelse(str_detect(name, "[[:lower:]]\\, MP$|[[:lower:]] MP$"), str_extract(name, ".{1,35}(?=\\, MP$| MP$)"), NA),
           displayName = ifelse(!is.na(displayName), str_remove(displayName, "\\,$"), displayName),
           displayName = ifelse(str_detect(displayName, "\\, Dr [[:alpha:]]"), str_remove(displayName, " Dr"), displayName),
           displayName = ifelse(name=="The DEPUTY SPEAKER (Hon. Peter Slipper)", "Slipper, Peter", displayName)) %>% 
    left_join(., master_list, by=c("displayName")) %>% 
    select(c(name, gender, uniqueID)) %>% 
    rename(gender_use = gender,
           uniqueID_use = uniqueID) %>% 
    unique()
  
  # merge
  main <- merge(main, gender_uniqueID_list, by="name", all.x = T) %>% 
    mutate(gender = ifelse(is.na(gender_use), gender, gender_use),
           uniqueID = ifelse(is.na(uniqueID_use), uniqueID, uniqueID_use)) %>% 
    select(-c(uniqueID_use, gender_use)) %>% arrange(order)
  
  # check number of rows didn't change from merge
  stopifnot(nrow_main_before == nrow(main))
  
  # flag for interjections
  # since question in writing parts don't have a speech number, we want to make sure they aren't accidentally flagged as interjections since we're grouping by speech number and they all have NA
  main <- main %>% group_by(speech_no) %>% arrange(order) %>% 
    mutate(interject = case_when(order == min(order) ~ 0,
                                 str_detect(name, "SPEAKER|stage direction") ~ 0,
                                 is.na(speech_no) & q_in_writing==1 ~ 0)) %>% 
    ungroup() %>% 
    group_by(name, speech_no) %>%
    fill(interject, .direction = "down") %>% 
    ungroup() %>% 
    mutate(interject = ifelse(is.na(interject), 1, interject))
  
  # fix up full stop issue that came with parsing
  main <- main %>% mutate(body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                        body),
                          body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                        body),
                          body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\?[[:upper:]]"),
                                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\?(?=[[:upper:]])", "? "),
                                        body),
                          body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\![[:upper:]]"),
                                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\!(?=[[:upper:]])", "! "),
                                        body),
                          body = ifelse(str_detect(body, "[[:lower:]][[:lower:]];[[:upper:]]"),
                                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]]);(?=[[:upper:]])", "; "),
                                        body))
  
  # trim any excess whitespace
  main <- main %>% mutate(body = str_trim(body, side="both"))

  # add space between by leave-I move - for word count purposes
  # two different types of dashes
  main <- main %>% mutate(body = ifelse(str_detect(body, "by leave\\—[[:upper:]]"), str_replace(body, "(?<=by leave)—", "- "), body))
  main <- main %>% mutate(body = ifelse(str_detect(body, "by leave\\-[[:upper:]]"), str_replace(body, "(?<=by leave)-", "- "), body))
  
  # also add space between "on motion byMr Baird" type of pattern
  main <- main %>% mutate(body = ifelse(str_detect(body, "on motion byM"), str_replace(body, "on motion by(?=M)", "on motion by "), body))
  
  # clean up body of debate text (removing name, title in brackets, and time stamp from body)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{1,2}\\.[:digit:]{2}[[:space:]]{0,1}[[:alpha:]]\\.[[:alpha:]]\\.\\)-")
  
  # same as above but in case time stamp has single digit to begin (e.g. 9:01 instead of 09:01)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{1}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when no colon, just two spaces then text
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)[:space:]{2}")
  
  # case when there is no title, just name and time stamp to be removed from body
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when there is no title, just name and time stamp to be removed from body, with spaces before time stamp
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                  \\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when name is followed by new line and 18 spaces
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                  \\(.{0,250}\\)\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when name and title is followed by new line and 20 spaces
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\\(.{0,250}\\)\n                    \\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when name is followed by newline and 20 spaces, then title and timestamp
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n[:space:]{16,24}\\(.{0,250}\\)[[:space:]]\\([:digit:]{2}:[:digit:]{2}\\)\\:[[:space:]]{0,5}")
  
  # case when name is followed by title then newline and spaces, then timestamp
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\\(.{0,250}\\)\n[:space:]{16,24}\\([:digit:]{2}:[:digit:]{2}\\)\\:[[:space:]]{0,5}")
   
  # case when name is followed by and then newline and spaces then timestamp
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                \\([:digit:]{2}:[:digit:]{2}\\)\\:[[:space:]]{0,5}")
  
  # case when name is followed by newline then title then newline then time
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                  \\(.{0,250}\\)\n                  \\([:digit:]{2}:[:digit:]{2}\\)\\:[[:space:]]{0,5}")
  
  # case when name is followed by title then time in brackets
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\\(.{0,250}\\)[[:space:]]{0,2}\\(\\d\\.\\d\\d [[:alpha:]]\\.[[:alpha:]]\\.\\)—")
  
  # same as above just two digits for hour
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\\(.{0,250}\\)[[:space:]]{0,2}\\(\\d\\d\\.\\d\\d [[:alpha:]]\\.[[:alpha:]]\\.\\)—")
  
  # 2002-09-26 speaker title put at the beginning of body needed to be extracted, and put into the name variable b/c name was missing
  main <- main %>% mutate(name = ifelse(is.na(name) & str_detect(body, "^The SPEAKER  \\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                str_extract(body, "^The SPEAKER(?=  \\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—)"),
                                name),
                  body = ifelse(str_detect(body, "^The SPEAKER  \\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                str_remove(body, "^The SPEAKER  \\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                body))
  
  # export CSV
  write.csv(main, paste0("/Volumes/Verbatim/output/main-pre-2011/", str_remove(filename, ".xml"), "-main.csv"), row.names = FALSE)
  
}

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/input/")

# grab a couple years
files_get <- files_all %>%
  as_tibble() %>%
  filter(str_detect(value, "^1999")) %>% 
  #filter(value >="2002-12-18.xml") %>% 
  pull(value)

#"^2000|^2001-|^2002-|^2003-|^2004-|^2005-|^2006-|^2007-|^2008-|^2009-|^2010-|^2011-"


for(i in 1:length(files_get)){
  parse_hansard(files_get[i])
}
 