# works from 15 February 2000 to 10 May 2011
library(here)
library(tidyverse)
library(xml2)
library(XML)
library(hms)

# code in function below from https://stackoverflow.com/questions/58492429/xml—in—r—multiple—children—with—same—name—without—loops
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
#all <- read.csv("/Volumes/Verbatim/all.csv")

################### temp filename
filename <- "2011-03-21.xml"

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
      # case when no text with business start 2008—02—12
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
      # case when no text with business start 2008—02—12
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
             sub2_flag = 0,
             question = 0,
             answer = 0,
             q_in_writing = 0) %>%
      select(c(itemindex, speech_no, page.no, time.stamp, name, name_short, name.id, electorate, party, in.gov, first.speech, role, body, fedchamb_flag, sub1_flag, sub2_flag, question, answer, q_in_writing))
    
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
  
  # grab all content — CHAMBER
  debate_text_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate"), homogeneous = T, collectNames = F), .name_repair = "unique") %>% 
    unite("body", c(debateinfo:last_col(), -debateinfo), na.rm = T, sep=" ") %>% 
    rowid_to_column("itemindex") %>% 
    left_join(item_df(xml_df, "//chamber.xscript/debate/debateinfo") %>% 
                select(itemindex, page.no) %>% 
                unnest(page.no), ., by="itemindex") %>% 
    select(itemindex, page.no, body) %>% 
    mutate(fedchamb_flag = 0,
           page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
  
  # grab all content  — FEDERATION CHAMBER
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/debateinfo"), homogeneous = T, collectNames = F))>0){
    debate_text_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate"), homogeneous = T, collectNames = F), .name_repair = "unique") %>% 
      unite("body", c(debateinfo:last_col(), -debateinfo), na.rm = T, sep=" ") %>% 
      rowid_to_column("itemindex") %>% 
      left_join(item_df(xml_df, "//maincomm.xscript/debate/debateinfo") %>% 
                  select(itemindex, page.no) %>% 
                  unnest(page.no), ., by="itemindex") %>% 
      select(itemindex, page.no, body) %>% 
      mutate(fedchamb_flag = 1,
             page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
    
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
      bind_rows(., patterns_text %>% filter(!is.na(para)) %>% mutate(para = paste0("", para))) %>% 
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
  
  # specific issue happening b/c no page number in subdebateinfo so the pattern is missing the page number and it's not matching right
  # one-off case
  if (filename=="2006-02-07.xml") {
    patterns_text <- patterns_text %>% as_tibble() %>% mutate(value = ifelse(value=="814:27:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD",
                                                                     "14:27:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD",
                                                                     value)) %>% pull()
  }
  
  # another issue specific to 2000 03 16 - a couple instances where name ID of speaker is P10000 or 110000 instead of 10000 - remove those
  if (filename=="2000-03-16.xml"){
    patterns_text <- patterns_text %>% as_tibble() %>% filter(!str_detect(value, "^P10000") & !str_detect(value, "^110000")) %>% pull()
  }
  
  # weird transcription thing for this one- need to manually add pattern
  if (filename=="2007-06-20.xml"){
    patterns_text <- patterns_text %>% add_row(value="BALDWIN, Mr Robert Charles, Paterson19011.12 amBaldwin, Robert, MPLL6PatersonLP10Mr BALDWIN(Paterson—Parliamentary Secretary to the Minister for Industry, Tourism and Resources)(11.12 am)") %>% pull()
  }
  
  # weird transcription thing for this one- need to manually add pattern
  if (filename=="2005-09-12.xml"){
    patterns_text <- patterns_text %>% add_row(value="JENKINS, Mr Harry Alfred, Scullin15110.39 pmJenkins, Harry, MPHH4ScullinALP00Mr JENKINS(Scullin)(10.39 pm)") %>% pull()
  }
  
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
  
  # let's arrange the patterns_text and patterns_text_esc vectors so the longest patterns are first
  # this way, the first match will be the longest match and the pattern is more likely to capture everything
  # example - if we have two patterns that are similar but one is longer and the shorter one is first, the shorter one will match and then some leftover bits that we would've wanted to remove from the pattern will be left over
  patterns_text <- patterns_text %>% as_tibble() %>% arrange(desc(str_length(value))) %>% pull()
  patterns_text_esc <- patterns_text_esc %>% as_tibble() %>% arrange(desc(str_length(value))) %>% pull()
  
if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//talk.start/talker"), homogeneous = T, collectNames = F))>0) {
  
  # first, extract all talk.start talker info, concatenate names, and unnest names so we have full name and display name
  patterns_data <- item_df(xml_df, "//talk.start/talker") %>% 
    mutate(name = map(name, c)) %>% 
    unnest_wider(name, names_sep = "_")
  
  # usually there are two names - one full, and one short — we want both
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
    patterns_data <- patterns_data %>% mutate(para = ifelse((str_detect(para, "^\\(.{1,35}\\)—[[:upper:]]") & str_detect(name, "SPEAKER")) | (str_detect(para, "^\\(.{1,35}\\)—[[:upper:]]") & str_detect(name_short, "DEPUTY SPEAKER")) | (str_detect(para, "^\\(.{1,35}\\)\\(.{1,10}\\)—[[:upper:]]")),
                                                            str_extract(para, "^\\(.{1,35}\\)(?=—[[:upper:]])"),
                                                            NA)) %>%
      mutate(name_short = ifelse(!is.na(para), paste(str_remove_all(para, "^\\(|\\)$")),
                                 name_short)) %>% 
      rename(first_pattern = talker)
    
    # need to fix a wrong name given to Peter Slipper - specific case
    if (filename == "2004-06-24.xml"){
      patterns_data <- patterns_data %>% mutate(name = ifelse(name.id=="0V5" & name_short=="Mr SLIPPER" & name=="O'Connor, Brendan, MP", "Slipper, Peter, MP", name))
    }
    
    # wrong name - checked pdf
    if (filename=="2004-12-07.xml") {
      patterns_data <- patterns_data %>% mutate(name = ifelse(first_pattern=="10000Melham, Daryl, MPThe DEPUTY SPEAKER", "Wilkie, Kim (The DEPUTY SPEAKER)", name))
    }
    
    if (filename=="2002-03-13.xml"){
      patterns_data <- patterns_data %>% mutate(name = ifelse(name.id=="0V5" & name_short=="Mr SLIPPER" & name=="Swan, Wayne, MP", "Slipper, Peter, MP", name))
    }
    
    # specific issue to this date - name id was transcribed as jen10000Jenkins, Harry (The DEPUTY SPEAKER)The DEPUTY SPEAKER (Mr Jenkins)—
    if (filename=="2004-03-02.xml"){
      patterns_data <- patterns_data %>% mutate(name.id = ifelse(name.id=="jen10000Jenkins, Harry (The DEPUTY SPEAKER)The DEPUTY SPEAKER (Mr Jenkins)—", "10000", name.id))
    }
    
    # similar to what we did with patterns_text, sometimes para is pasted with space between so add extra rows with that space before we unite — so we have both pattern variations to match for
    patterns_data <- patterns_data %>% filter(!is.na(para)) %>% mutate(para = paste0(" ", para)) %>% 
      bind_rows(., patterns_data %>% filter(!is.na(para)) %>% mutate(para = paste0("", para))) %>% 
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
  
  # specific issue happening b/c no page number in subdebateinfo so the pattern is missing the page number and it's not matching right
  # one-off case
  if (filename=="2006-02-07.xml") {
    patterns_data <- patterns_data %>% mutate(first_pattern = ifelse(first_pattern=="814:27:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD",
                                                                     "14:27:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD",
                                                                     first_pattern))
  }
  
  # another issue specific to 2000 03 16 - a couple instances where name ID of speaker is P10000 instead of 10000 - remove those
  if (filename=="2000-03-16.xml"){
    patterns_data <- patterns_data %>% filter(!str_detect(first_pattern, "^P10000") & !str_detect(first_pattern, "^110000")) 
  }
  
  # one-off case, name ID was put in wrong in one place causing problems
  if (filename=="2000-08-17.xml") {
    patterns_data <- patterns_data %>% filter(first_pattern!="T4Ronaldson, Michael, MPMr RONALDSON") %>% unique()
    patterns_text <- patterns_text %>% filter(value!="T4Ronaldson, Michael, MPMr RONALDSON")
  }
  
  # weird transcription issue, add row manually
  if (filename=="2007-06-20.xml"){
    patterns_data <- patterns_data %>% add_row(page.no = "190", 
                                               time.stamp = "11:12:00", 
                                               name = "Baldwin, Robert, MP",
                                               name.id = "LL6",
                                               electorate = "Paterson",
                                               party = "LP",
                                               role = "Parliamentary Secretary to the Minister for Industry, Tourism and Resources",
                                               in.gov = "1",
                                               first.speech = "0",
                                               first_pattern = "BALDWIN, Mr Robert Charles, Paterson19011.12 amBaldwin, Robert, MPLL6PatersonLP10Mr BALDWIN(Paterson—Parliamentary Secretary to the Minister for Industry, Tourism and Resources)(11.12 am)")
  }
  
  # same as above, different date
  if (filename=="2005-09-12.xml"){
    patterns_data <- patterns_data %>% add_row(page.no = "151", 
                                               time.stamp = "10:39:00", 
                                               name = "Jenkins, Harry, MP",
                                               name.id = "HH4",
                                               electorate = "Scullin",
                                               party = "ALP",
                                               role = NA,
                                               in.gov = "0",
                                               first.speech = "0",
                                               first_pattern = "JENKINS, Mr Harry Alfred, Scullin15110.39 pmJenkins, Harry, MPHH4ScullinALP00Mr JENKINS(Scullin)(10.39 pm)")
  }
  
  
  # saw this on 2002—05—16 — caused issues with merge later on b/c two full name versions
  patterns_data <- patterns_data %>% mutate(name = ifelse(name=="Mr MARTIN FERGUSON,AM, MP", "Ferguson, Martin, MP", name))
  
  # saw this on 2002—08—19 — caused issues with merge later on b/c two full name versions
  patterns_data <- patterns_data %>% mutate(name = ifelse(name=="Mr WILLIAMS,AM, QC, MP", "Williams, Daryl, MP", name))
  
  if (filename=="2002-05-15.xml"){
    patterns_data <- patterns_data %>% mutate(name = ifelse(name=="Mossfield, Frank, MP" & name.id=="0V5" & name_short=="Mr SLIPPER",
                                           "Slipper, Peter, MP",
                                           name),
                             party = ifelse(name=="Slipper, Peter, MP", "LP", as.character(party)),
                             electorate = ifelse(name=="Slipper, Peter, MP", "Fisher", electorate))
  }
  
  # saw on 2010—06—21 that a page number was captured as a time by accident? should've been time stamp — i need to come back to this
  patterns_data <- patterns_data %>% filter(!grepl("\\.", page.no))
  
  # if we have two of the same patterns for deputy speaker where only diff in row is the name short, take the shorter one that doesnt have "DEPUTY SPEAKER" in it
  patterns_data <- patterns_data %>% group_by(first_pattern) %>% filter(!(n()>1 & str_detect(name, "DEPUTY") & str_detect(name_short, "DEPUTY"))) %>% ungroup()
  
  # saw this on 2005—05—31 — two page numbers, let's go with bigger one (checked and this is correct in XML based on preceding/following statement page numbers)
  # third condition of filer is for cases where there are two or more of the same first pattern but there are no page numbers for any of them
  patterns_data <-  patterns_data %>% group_by(first_pattern) %>% 
    mutate(page.no=as.numeric(page.no)) %>% 
    filter(n()>1 & !is.na(page.no) & page.no==max(page.no) | n()==1 | n()>1 & is.na(page.no)) %>% 
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
    
    # also grab titles — sometimes there are extra title nodes and we want to remove these as well
    if ("title" %in% names(sub1_patterns_info)) {
      sub1_patterns_info <- sub1_patterns_info %>% 
        select(subdebateinfo, title) %>% 
        unnest(subdebateinfo) %>% 
        unnest_wider(title) %>% # added dec 12 b/c 2000-09-04 multiple titles - didn't do it for sub2 yet in case of issues
        unite("subdebateinfo", subdebateinfo:last_col(), na.rm = T, sep="") %>% 
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
  
  # on 2005—08—17 one subdebate1 title was literally just a space and that will cause so many problems when we separate rows
  # also remove any extra backslashes that shouldnt be there— remove that b\c gonna cause a lot of issues with regex matches later
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
  
  
  # let's grab all the talk starts that are nested in a speech node — these will allow us to perform our first split the text
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
  
  # doing this b/c of 2008—06—02 where a pattern was just "Iraq" — this causes so many issues when we split on this word
  # we can filter out patterns like this that will cause problems by removing those that only contain letters
  if (length(sub1_patterns_info)>0) {
    if (nrow(as_tibble(sub1_patterns_info) %>% filter(str_detect(value, "^[[:alpha:]]+$")))>0) {
      sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% 
        filter(!str_detect(value, "^[[:alpha:]]+$")) %>% 
        pull(value)
    }
    
    # same idea as above for 2006-02-07. pattern is "United Nations Oil for Food Program"
    # we need to just keep those patterns that have page numbers b/c otherwise there will be splitting issues
    # if the pattern is in the middle of a speech, it'll be separated there
    if (nrow(as_tibble(sub1_patterns_info) %>% filter(!str_detect(value, "\\d")))>0) {
      
      # need this extra conditional b/c sometimes nodes are named differently, and want to make sure we have everything
      if ("subdebateinfo" %in% names(item_df(xml_df, "//subdebate.1")) & "subdebate.2" %in% names(item_df(xml_df, "//subdebate.1"))) {
        # going to grab the patterns that are just words, and check if they have subdebate2 patterns associated, and then paste those together to replace the pattern we would've filtered out
        sub1_patterns_info <- item_df(xml_df, "//subdebate.1") %>% select(subdebateinfo, subdebate.2) %>% 
          unnest(everything()) %>% 
          filter(!is.na(subdebate.2)) %>% 
          filter(!str_detect(subdebateinfo, "\\d")) %>% 
          mutate(subdebate.2 = str_match(subdebate.2, paste0(sub2_patterns_info, collapse = "|"))) %>% 
          unite("value", everything(),  sep="") %>% pull(value) %>% c(sub1_patterns_info, .) %>% 
          as_tibble() %>% 
          filter(str_detect(value, "\\d")) %>% 
          pull(value)
      } else {
        # else, just remove the patterns with no page numbers to avoid splitting issues
        # will deal with any leftover pattern problems manually
        sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% 
          filter(str_detect(value, "\\d")) %>% 
          pull(value)
      }
    }
  }
  
  # specific issue where pattern is someones role followed by their in.gov value which we don't want to split on b/c that'll mess up the speech start patterns later on
  # this one shouldn't be included in sub1 patterns - remove it
  if (filename=="2003-08-19.xml") {
   sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% filter(value!="Minister for Regional Services, Territories and Local Government1") %>% pull(value)
  }
  
  # same idea as a above, for diff day
  # need to add full pattern version so we capture what we need without splitting on wrong things
  if (filename=="2007-05-28.xml") {
    sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% filter(value!="Legal and Constitutional Affairs Committee1") %>% 
      add_row(value="Legal and Constitutional Affairs Committee1Report1") %>% 
      pull(value)
  }
  
  # two subdebate1 titles, needs special pasting together
  if (filename=="2003-11-04.xml"){
    sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% filter(value!="Communications, Information Technology and the Arts Committee21939" & value!="Treaties Committee21938") %>% 
      add_row(value = "Treaties Committee21938 Communications, Information Technology and the Arts Committee21939") %>% 
      pull(value)
  }
  
  # this was removed above but we need to add back in and won't be an issue b/c doesn't exist in the middle of any speeches (checked)
  if (filename=="2008-10-13.xml"){
    sub1_patterns_info <- c(sub1_patterns_info, "Sydney Airport\\: East-West Runway")
  }
  
  # this pattern is missing because editors forgot a page number so it was filtered out, let's add it manually
  if (filename=="2006-03-01.xml"){
    sub1_patterns_info <- c(sub1_patterns_info, "Oil for Food Program67")
  }
  
  if (filename=="2006-12-07.xml"){
    sub1_patterns_info <- c(sub1_patterns_info, "Reserve Bank of Australia76")
  }
  
  if (filename=="2007-03-27.xml"){
    sub1_patterns_info <- c(sub1_patterns_info, "Report19")
  }
  
  if (filename=="2007-05-10.xml"){
    sub1_patterns_info <- c(sub1_patterns_info, "Economy73")
  }
  
  # missing space in pattern, need to manually fix
  if (filename=="2002-08-26.xml"){
    sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% 
      mutate(value = str_replace(value, 
                                 "Public Works Committee Foreign Affairs, Defence and Trade Committee5636Foreign Affairs, Defence and Trade Committee", 
                                 "Public Works Committee Foreign Affairs, Defence and Trade Committee5636 Foreign Affairs, Defence and Trade Committee")) %>% 
      add_row(value = "VETERANS' AFFAIRS LEGISLATION AMENDMENT BILL \\(No\\. 1\\) 2002 Second Reading5658") %>% 
      filter(value!="Second Reading5658") %>% 
      pull(value)
  }
  
  if (filename=="2005-05-30.xml"){
    sub1_patterns_info <- sub1_patterns_info %>% as_tibble() %>% filter(value!="Budget 2005-0636") %>% pull(value)
  }
  
  # First separate rows at each sub debate pattern, so we know where each unique debate starts
  # sometimes when we separate rows on a pattern at the beginning of a body, an empty body is left behind, so just filter those out
  # add "(?!, | |\\?|\\. |\\.Th)" to avoid splitting when pattern is part of sentence
  if (length(sub1_patterns_info)>0){
    debate_text_all <- separate_rows(debate_text_all, body, sep=paste0("(?=", paste0("(?<!the |our |so—called |Minister for )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|"), ")")) %>% 
      filter(body!="")
  }
  
  # from 2010-10-26 - sub1 patterns had "Hon\\. Kenneth Shaw WriedtReport from Main Committee1579"  and "Report from Main Committee1579"
  # after splitting as we did above, the "Hon\\. Kenneth Shaw Wriedt" wasn't captured so it was just left on it's own row
  # get rid of that
  if (filename=="2010-10-26.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Hon. Kenneth Shaw Wriedt")
  }
  
  # same idea as above, with "Condolences"
  if (filename=="2011-02-21.xml" | filename=="2007-05-08.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Condolences: ")
  }
  
  if (filename=="2000-09-07.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Minister for Health and Aged Care: ")
  }
  
  if (filename=="2001-06-07.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Hansard: ")
  }
  
  if (filename=="2002-06-27.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Parliament: ")
  }
  
  if (filename=="2003-11-26.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Consideration of ")
  }
  
  if (filename=="2006-12-04.xml"){
    debate_text_all <- debate_text_all %>% filter(body!="Hon. Kim Beazley")
  }
  
  # pattern issue - this one question's subdebateinfo was missing a page number, causing an issue when we tried to remove the pattern later
  # i fixed this manually by adding the correct page number, 8, that will allow for correct splitting and removal of the pattern
  if (filename=="2007-02-08.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^Water814:15:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD—My question is also to the Treasurer"),
                                             str_replace(body, "Water814:15:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD—My question is also to the Treasurer", "Water8814:15:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD—My question is also to the Treasurer"),
                                             body))
  }
  
  # same idea as above - missing page number, handle it manually
  if (filename=="2006-03-01.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^Oil for Food Program6714:07:00Beazley, Kim, MPPE4BrandALP0Mr BEAZLEY—My question is to the Deputy Prime Minister"),
                                                                str_replace(body, "Oil for Food Program6714:07:00Beazley, Kim, MPPE4BrandALP0Mr BEAZLEY—My question is to the Deputy Prime Minister", "Oil for Food Program676714:07:00Beazley, Kim, MPPE4BrandALP0Mr BEAZLEY—My question is to the Deputy Prime Minister"),
                                                                body))
  }
  
  if (filename=="2006-12-07.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^Reserve Bank of Australia7614:20:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD—My question is to the Prime Minister\\. It refers to the Treasurer’s last answer"),
                                                                str_replace(body, "Reserve Bank of Australia7614:20:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD—My question is to the Prime Minister\\. It refers to the Treasurer’s last answer", "Reserve Bank of Australia767614:20:00Rudd, Kevin, MP83TGriffithALP0Mr RUDD—My question is to the Prime Minister. It refers to the Treasurer’s last answer"),
                                                                body))
  }
  
  if (filename=="2007-03-27.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^Report1915:20:00Pyne, Chris, MP9V5SturtLPMinister for Ageing10Mr PYNE—Mr Speaker, on indulgence"),
                                                                str_replace(body, "Report1915:20:00Pyne, Chris, MP9V5SturtLPMinister for Ageing10Mr PYNE—Mr Speaker, on indulgence", "Report191915:20:00Pyne, Chris, MP9V5SturtLPMinister for Ageing10Mr PYNE—Mr Speaker, on indulgence"),
                                                                body))
  }
  
  if (filename=="2007-05-10.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^Economy7314:00:00Swan, Wayne, MP2V5LilleyALP0Mr SWAN—My question is directed to the Treasurer"),
                                                                str_replace(body, "Economy7314:00:00Swan, Wayne, MP2V5LilleyALP0Mr SWAN—My question is directed to the Treasurer", "Economy737314:00:00Swan, Wayne, MP2V5LilleyALP0Mr SWAN—My question is directed to the Treasurer"),
                                                                body))
  }
  
  if (filename=="2009-02-11.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^884Suspension of Standing and Sessional OrdersSuspension of Standing and Sessional Orders 88410:13:00Albanese, Anthony, MPR36GrayndlerALPLeader of the House10"),
                                                                str_replace(body, "884Suspension of Standing and Sessional OrdersSuspension of Standing and Sessional Orders 88410:13:00Albanese, Anthony, MPR36GrayndlerALPLeader of the House10", "88410:13:00Albanese, Anthony, MPR36GrayndlerALPLeader of the House10"),
                                                                body))
  }
  
  if (filename=="2008-11-25.xml"){
    debate_text_all <- debate_text_all %>% mutate(body = ifelse(str_detect(body, "^Returned from the SenateMessage received from the Senate returning the bills without amendment or request\\.$"),
                                                                str_replace(body, "Returned from the SenateMessage received from the Senate returning the bills without amendment or request\\.", "Message received from the Senate returning the bills without amendment or request."),
                                                                body))
  }
  
  # info stuff only necessary for very first part of speech — clean those up by matching for them and removing them
  # when sub2 exists, it's nested in sub1, so it'll have both debate titles
  # needed to add (?!, | |\\?|\\. |\\.Th) b/c sometimes a pattern is actually used in a sentence (2009—02—04 example) and we don't want to remove those
  if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)>0) {
    main <- debate_text_all %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0("(?<!the |our |so—called |Minister for )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|")),
                                                             str_match(body, paste0("(?<!the |our |so—called |Minister for )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|")),
                                                             NA),
                                       sub2_pattern = ifelse(str_detect(body, paste0(sub2_patterns_info, collapse = "|")),
                                                             str_match(body, paste0(sub2_patterns_info, collapse = "|")),
                                                             NA),
                                       sub1_pattern = str_replace_all(sub1_pattern, "\\(","\\\\("),
                                       sub1_pattern = str_replace_all(sub1_pattern, "\\)","\\\\)"),
                                       sub1_pattern = str_replace_all(sub1_pattern, "\\:","\\\\:"),
                                       sub1_pattern = str_replace_all(sub1_pattern, "\\.","\\\\."),
                                       sub1_pattern = str_replace_all(sub1_pattern, "\\?","\\\\?"),
                                       sub2_pattern = str_replace_all(sub2_pattern, "\\(","\\\\("),
                                       sub2_pattern = str_replace_all(sub2_pattern, "\\)","\\\\)"),
                                       sub2_pattern = str_replace_all(sub2_pattern, "\\:","\\\\:"),
                                       sub2_pattern = str_replace_all(sub2_pattern, "\\.","\\\\."),
                                       sub2_pattern = str_replace_all(sub2_pattern, "\\?","\\\\?"),
                                       sub2_pattern = str_replace_all(sub2_pattern, "\\*","\\\\*"),
                                       sub2_pattern = ifelse(!is.na(sub1_pattern) & !is.na(sub2_pattern) 
                                                             & str_detect(body, paste0(sub1_pattern, sub2_pattern)),
                                                             paste0(sub1_pattern, sub2_pattern),
                                                             NA)) %>% 
      mutate(body = ifelse(!is.na(sub2_pattern) & str_detect(body, paste0("^", sub2_pattern)),
                           str_remove(body, paste0("^", sub2_pattern)),
                           body),
             body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0("^", sub1_pattern, "(?!, | |\\?|\\. |\\.Th)")),
                           str_remove(body, paste0("^", sub1_pattern, "(?!, | |\\?|\\. |\\.Th)")),
                           body)) %>% 
      mutate(sub1_flag = ifelse(!is.na(sub1_pattern) & is.na(sub2_pattern), 1, 0),
             sub2_flag = ifelse(!is.na(sub2_pattern), 1, 0)) %>% 
      select(-sub1_pattern, -sub2_pattern)
  } else if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)==0) {
    main <- debate_text_all %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             str_match_all(body, paste0(sub1_patterns_info, collapse = "|")),
                                                             NA)) %>% 
      mutate(sub1_pattern = str_replace_all(sub1_pattern, "\\(","\\\\("),
             sub1_pattern = str_replace_all(sub1_pattern, "\\)","\\\\)"),
             sub1_pattern = str_replace_all(sub1_pattern, "\\:","\\\\:"),
             sub1_pattern = str_replace_all(sub1_pattern, "\\.","\\\\."),
             sub1_pattern = str_replace_all(sub1_pattern, "\\?","\\\\?")) %>% 
      mutate(body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0("^", sub1_pattern)),
                           str_remove(body, paste0("^", sub1_pattern)), body)) %>% 
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
  
  # specific pattern issue - fix manually
  if (filename=="2003-06-24.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "^Reference17311Mr LLOYD \\(Robertson\\) \\(4\\.24 p\\.m\\.\\)—by leave—I move:That the following order of the day"),
                                          str_replace(body, "^Reference17311Mr LLOYD \\(Robertson\\) \\(4\\.24 p\\.m\\.\\)—by leave—I move:That the following order of the day", "Mr LLOYD (Robertson) (4.24 p.m.)—by leave—I move:That the following order of the day"),
                                          body))
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
  
  # need to deal with rows where page number x and y contradict — we want the later one but also want to make sure we maintain order
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
  
  # if the row before has a bigger page number, fill next row with the maximum page number — this will cause problems later with ordering
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
  
  # the 0000 name id for speaker is causing issues b/c usually 10000 is used — fix that up
  main <- main %>% mutate(body = ifelse(str_detect(body, "(?<!1)0000Mr SPEAKERMr SPEAKER"),
                                        str_replace_all(body, "(?<!1)0000Mr SPEAKERMr SPEAKER", "10000SPEAKER, MrMr SPEAKER"),
                                        body),
                          body = ifelse(str_detect(body, "(?<!1)0000SPEAKER, MrMr SPEAKER"),
                                        str_replace_all(body, "(?<!1)0000SPEAKER, MrMr SPEAKER", "10000SPEAKER, MrMr SPEAKER"),
                                        body))
  
  # issue with name id on one statement, messing up pattern, need to fix manually
  if (filename=="2000-08-17.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "84HZahra, Christian, MPT4Ronaldson, Michael, MPMr RONALDSON—I think I might take the interjection from the member for McMillan"),
                                          str_replace(body, "84HZahra, Christian, MPT4Ronaldson, Michael, MPMr RONALDSON—I think I might take the interjection from the member for McMillan",
                                                      "84HZahra, Christian, MPXT4Ronaldson, Michael, MPMr RONALDSON—I think I might take the interjection from the member for McMillan"),
                                          body))
    
    patterns_text <- patterns_text %>% as_tibble() %>% filter(value!="T4Ronaldson, Michael, MPMr RONALDSON") %>% pull()
    patterns_text_esc <- patterns_text_esc %>% as_tibble() %>% filter(value!="T4Ronaldson, Michael, MPMr RONALDSON") %>% pull()
  }
  
  # messed up pattern b/c name id of speaker is sometimes P10000 or 110000, need to fix manually
  if (filename=="2000-03-16.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "to whip up is going to have the desired effect\\.P10000Nehl, Garry \\(Mr DEPUTY SPEAKER\\)Mr DEPUTY SPEAKER \\(Mr Nehl\\)"),
                                  str_replace(body, "to whip up is going to have the desired effect\\.P10000Nehl, Garry \\(Mr DEPUTY SPEAKER\\)Mr DEPUTY SPEAKER \\(Mr Nehl\\)", "to whip up is going to have the desired effect.10000Nehl, Garry (Mr DEPUTY SPEAKER)Mr DEPUTY SPEAKER (Mr Nehl)"),
                                  body),
                    body = ifelse(str_detect(body, "asking for an opinion from the minister and, as such, should be ruled out of order\\.P10000SPEAKER, MrMr SPEAKER—The member for Farrer makes real what is a difficult"),
                                  str_replace(body, "asking for an opinion from the minister and, as such, should be ruled out of order\\.P10000SPEAKER, MrMr SPEAKER—The member for Farrer makes real what is a difficult", "asking for an opinion from the minister and, as such, should be ruled out of order.10000SPEAKER, MrMr SPEAKER—The member for Farrer makes real what is a difficult"),
                                  body),
                    body = ifelse(str_detect(body, "PE4Beazley, Kim, MP110000SPEAKER, MrMr SPEAKER—Leader of the Opposition\\!"),
                                  str_replace(body, "PE4Beazley, Kim, MP110000SPEAKER, MrMr SPEAKER—Leader of the Opposition\\!", "PE4Beazley, Kim, MP10000SPEAKER, MrMr SPEAKER—Leader of the Opposition!"),
                                  body),
                    body = ifelse(str_detect(body, "rules which apply—CE4O'Keefe, Neil, MP110000SPEAKER, MrMr SPEAKER—The Treasurer will resume his seat"),
                                  str_replace(body, "rules which apply—CE4O'Keefe, Neil, MP110000SPEAKER, MrMr SPEAKER—The Treasurer will resume his seat", "rules which apply—CE4O'Keefe, Neil, MP10000SPEAKER, MrMr SPEAKER—The Treasurer will resume his seat"),
                                  body))
  }
  
  # separate rows on patterns
  # sometimes it's too long — we need to split on subsets of the patterns if this is the case
  if (length(patterns_text_esc)>400) {
    main <- separate_rows(main, body, sep=paste0("(?=", patterns_text_esc[1:400], ")", collapse = "|")) %>% 
      separate_rows(., body, sep=paste0("(?=", patterns_text_esc[401:length(patterns_text_esc)], ")", collapse = "|")) %>% 
      filter(body!="")
  } else {
    main <- separate_rows(main, body, sep=paste0("(?=", patterns_text_esc, ")", collapse = "|")) %>% filter(body!="")
  }
  
  # 2002 09 16 I saw some rows where the body was just the name id — this happened b/c of varying patterns for the same person, and there's a bit that wasn't removed
  # let's filter out any rows where the body is the name id
  main <- main %>% filter(!str_detect(body, paste0("^", name.id, "$")))
  
  # 2010-06-21 saw a row that was just a sub2 pattern - remove this
  main <- main %>% filter(!str_detect(body, paste0("^", sub2_patterns_info, "[[:space:]]{0,1}$", collapse = "|")))
  
  # same idea but for sub1 pattern
  main <- main %>% filter(!str_detect(body, paste0("^", sub1_patterns_info, "[[:space:]]{0,1}$", collapse = "|")))
  
  # define general interjections to split on
  # dec 13 - removed The Clerk- b/c causing issues since the clerk is often written like "10000The ClerkThe Clerk—" and it was separating this in half (tried neg. lookbehind but caused issues later)
  interject_general <- c("Opposition members interjecting—", "Government members interjecting—",
                         "Government member interjecting—",
                         "Honourable members interjecting—", "An opposition member interjecting—", 
                         "An honourable member interjecting—", "Honourable member interjecting—", 
                         "A government member interjecting—", "Opposition member interjecting—",
                         "Honourable members and senators interjecting—",
                         "Honourable member and senators interjecting—",
                         "A government member—", "Government member—", "Government members—", "Honourable members—", "Honourable member—", "Honourable members and senators—",
                         "Honourable member and senators—", "Opposition member—", "Opposition members—")
  
  # separate rows on general interjections
  main <- separate_rows(main, body, sep=paste0("(?=", interject_general, ")(?!", interject_general, "[[:space:]]on[[:space:]]my)(?!", interject_general, "[[:space:]]having)(?!", interject_general, "[[:space:]]standing)(?!", interject_general, "[[:space:]]will )(?!", interject_general, "[[:space:]]can )",collapse="|"))
  
  # define stage notes
  stage_notes <- c("Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.",
                   "Bill, by leave, taken as a whole\\.",
                   "Debate interrupted\\.",
                   "Message from the .{0,100}[[:space:]]announced\\.",
                   "Question agreed to\\.",
                   "Question unresolved\\.",
                   "Debate adjourned",
                   "Debate .{1,50} adjourned\\.",
                   "The House divided\\.",
                   "The House divided\\.     ",
                   "Question put\\.",
                   "Ordered that the report be made a parliamentary paper",
                   "Question put:[[:space:]]{0,1}That this bill be now read a second time",
                   "Question put:That this bill be now read a second time\\.",
                   "Cognate bills:",
                   "Cognate bill:",
                   "Message No\\. 135 of 23 October 2002 received from the Senate acquainting the House that, in accordance with",
                   "Question negatived\\.",
                   "Question proposed\\. That grievances be noted\\.",
                   "Original question agreed to\\.",
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
                   "Leave granted for the [[:alpha:]]{1,10} reading to be moved forthwith\\.",
                   "A division having been called in the House of Representatives\\—",
                   "Honourable members having stood in their places—",
                   "Honourable members standing in their places—",
                   "The Speaker having seated himself in the chair—",
                   "The bells having been rung and a ballot having been taken—",
                   "A division having been called and the bells having been rung—",
                   "Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d",
                   "Sitting suspended from \\d{1,2}\\.\\d\\d .{2,4} to \\d{1,2}\\.\\d\\d .{2,4}",
                   "The member for [[:alpha:]]{0,50} then left the chamber.",
                   "More than the number of members required by the standing orders having risen in their places—",
                   "Members and senators rising and applauding,.{1,50}left the chamber\\.",
                   "Proposed expenditure agreed to\\.",
                   "Bill agreed to\\.",
                   "Bill and explanatory memorandum presented by",
                   "Bill and explanatory memorandum and—by leave—a regulation",
                   "The following papers were deemed to have been presented on \\d{1,2} [[:alpha:]]{1,10} \\d{4}\\:",
                   "Bill returned from Main Committee with amendments; certified copy of bill and schedule of amendments presented\\.Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee without amendment; certified copy presented\\.Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee without amendment; certified copy presented\\. Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee with an amendment; certified copy of the bill and schedule of amendments presented\\.",
                   "Bill returned from Main Committee with an amendment; certified copy of bill and schedule of amendment presented\\.Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee with amendments and an amended title; certified copy of the bill and schedule of amendments presented\\.Ordered that this bill be considered forthwith\\.",
                   "Bill returned from Main Committee without amendment; certified copy of the bill presented\\.",
                   "Bill returned from Main Committee for further consideration; certified copy of presented\\.",
                   "Bill returned from Main Committee with amendments, appropriation message having been reported; certified copy of bill and schedule of amendments presented\\.Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee without amendment, appropriation message having been reported; certified copy of the bill presented\\.",
                   "Bill returned from Main Committee for further consideration\\; certified copy of the bill presented\\.Ordered that this bill be considered at a later hour this day\\.",
                   "Bill, explanatory memorandum and the report of the Committee for the Review of Parliamentary Entitlements presented by.{1,35}\\.",
                   "Bill returned from Main Committee without having been fully considered; certified copy presented\\.Ordered that the bill be taken into consideration forthwith\\.",
                   "Order of the day returned from Main Committee for further consideration; certified copy of the motion presented\\.",
                   "Order of the day returned from Main Committee for further consideration;",
                   "Order of the day reported from Main Committee; certified copy of the report presented\\.",
                   "Bill returned from Main Committee with amendments, message from the Governor-General recommending appropriation for proposed amendments having been reported; certified copy of bill and schedule of the amendments presented\\.Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill—by leave—and explanatory memorandum presented by .{1,35}\\.",
                   "Bill and explanatory memorandum presented by .{1,35}\\.",
                   "Bill and explanatory memorandum presented by\\n.{1,35}\\.",
                   "Bill and explanatory memorandum presented by\\n.{1,35}\\nfor .{1,35}\\.",
                   "Bill and explanatory memorandum—by leave—presented by",
                   "Bill, explanatory memorandum and regulation impact statement presented",
                   "Bill, explanatory memorandum and regulatory impact statement presented by",
                   "Ordered that the order of the day be considered immediately\\.",
                   "Order of the day returned from Main Committee with an unresolved question\\.Ordered that the matter be taken into consideration forthwith\\.Unresolved question\\—That further proceedings be conducted in the House\\.Question resolved in the [[:alpha:]]{5,15}\\.Ordered that further consideration of the matter be set down as an order of the day for the next sitting\\.",
                   "Order of the day returned from the Main Committee",
                   "Bill returned from the Senate with amendments\\.",
                   "Consideration resumed.{0,40}\\.",
                   "Consideration resumed from \\d{1,2} [[:alpha:]]{1,10}",
                   "Debate resumed.{0,150}[[:punct:]]{0,1}",
                   "Debate resumed from.{1,20}, on motion by\\n.{1,35}\\:That this bill be now read a .{1,10} time\\.",
                   "Debate resumed from.{1,20}, on motion by.{1,35}\\:That this bill be now read a .{1,10} time\\.",
                   "Debate resumed from.{1,20}, on motion by.{1,35}\\:That the House take note of the document\\.",
                   "Debate resumed from.{1,20}, on motion by.{1,35}\\:That\\:.{1,1500}\\.",
                   "Debate resumed from.{1,20}, on motion by.{1,35}\\:That this House.{0,20}\\:.{1,1500}\\.",
                   "The following notice was given\\:",
                   "Message received from the Senate returning the bills without amendment or request\\.",
                   "Messages received from the Senate returning the bills without amendment or request\\.",
                   "Message received from the Senate requesting the House to consider immediately the .{1,200}\\.",
                   "Message received from the Senate acquainting the House that the Senate has agreed to a resolution extending until .{1,500}\\.",
                   "Message from Governor-General recommending.{1,250}\\.",
                   "Ordered that this bill be considered immediately\\.",
                   "Question put:That the motion.{1,50}be agreed to\\.",
                   "The following notices were given\\:",
                   "The following notices were given",
                   "The following\\nnotices were\\ngiven\\:",
                   "Bill presented by",
                   "Bill presented—by leave—by",
                   "Proposed expenditure \\$.{1,30}",
                   "Proposed expenditure, \\$.{1,30}",
                   "Proposed expenditure\\$.{1,30}",
                   "Proposed expenditure,\\$.{1,30}",
                   "Bill—by leave—taken as a whole\\.",
                   "Bill \\(on motion by .{1,35}\\)—by leave—read a third time\\.",
                   "Bill \\(on motion by .{1,35}\\)—by leave—agreed to\\.",
                   "Consideration resumed from .{1,30}, on motion by .{1,35}\\:That the bill be now read a .{3,7} time\\.",
                   "Leave granted for .{3,7} reading to be moved forthwith\\.Bill \\(on motion by .{1,35}\\) read a .{3,7} time\\.",
                   "Bill \\(on motion by .{1,35}\\).{1,20}read a [[:alpha:]]{1,10} time\\.",
                   "Motion \\(by .{1,35}\\) proposed: That the House do now adjourn\\.",
                   "The following bill was returned from the Senate without amendment:.{1,50}",
                   "The following bill was returned from the Senate without amendment or request: .{1,50}",
                   "The following bills were returned from the Senate without amendment or request:.{1,100}",
                   "Debate—by leave—adjourned\\.",
                   "Bill—by leave—presented by .{1,35} and read a [[:alpha:]]{1,10} time\\.",
                   "Leave granted for third reading to be moved forthwith\\.",
                   "Bill returned from the Senate with amendments\\. Ordered that the amendments be considered at the next sitting\\.",
                   "Bill returned from the Senate with a request for a further amendment\\.Ordered that the requested amendment be taken into consideration forthwith\\.",
                   "Bill returned from the Senate with requested amendments\\.[[:space:]]{0,2}Ordered that the requested amendments be taken into consideration at the next sitting\\.",
                   "Bill returned from the Senate with a requested amendment\\.[[:space:]]{0,2}Ordered that the requested amendment be taken into consideration at the next sitting\\.",
                   "Bill returned from the Senate with an amendment made in place of the amendment disagreed to by the House\\.[[:space:]]{0,2}Ordered that the amendment be taken into consideration forthwith\\.",
                   "Bill returned from the Senate with a request for amendments\\.[[:space:]]{0,2}Ordered that the requested amendments be taken into consideration forthwith\\.",
                   "Question proposed: That grievances be noted\\.",
                   "Messages from the Governor-General.{0,20} reported informing the House of assent to the following bills:",
                   "Messages from the Governor-General and the Deputy of the Governor-General reported informing the House of assent to the bills",
                   "Bill presented by.{1,150}\\.",
                   "Bills presented by.{1,150}\\.",
                   "Bill received from the Senate\\.",
                   "Bill received from the Senate, and read a .{1,10} time\\.",
                   "Bill received from the Senate and read a .{1,10} time\\.",
                   "Bills received from the Senate, and read a .{1,10} time\\.",
                   "Bills received from the Senate and read a .{1,10} time\\.",
                   "Bill returned from the Senate with requests for amendments and amendments\\.Ordered that the requested amendments and amendments be taken into consideration at a later hour this day\\.",
                   "Bill returned from the Senate with requests for amendments and amendments\\.Ordered that the amendments be taken into consideration at the next sitting\\.",
                   "Bill returned from the Senate with requests for amendments and amendments\\. Ordered that the amendments be taken into consideration at the next sitting\\.",
                   "Bill returned from the Senate with requests for amendments and amendments\\.Ordered that the requested amendments and amendments be taken into consideration at the next sitting.",
                   "Bill returned from the Senate with requests for amendments and amendments\\. Ordered that the requested amendments and amendments be taken into consideration at the next sitting.",
                   "Bill returned from the Senate with amendments\\.Ordered that the amendments be taken into consideration forthwith\\.",
                   "Bill returned from the Senate with requests for amendments\\.Ordered that the requested amendments be taken into consideration at the next sitting\\.",
                   "Bill returned from the Senate with a request for an amendment\\.[[:space:]]{0,2}Ordered that the amendment be taken into consideration forthwith\\.",
                   "Ordered that the amendments be taken into consideration forthwith\\.",
                   "Messages from the Governor-General reported informing the House of assent to the bills\\.",
                   "Message from the Governor-General reported informing the House of assent to the bills\\.",
                   "Message from the Governor-General reported informing the House of assent to the bill\\.",
                   "Messages from the Governor-General and the Administrator reported informing the House of assent to the bills\\.",
                   "Message from the Governor-General and the Administrator reported informing the House of assent to the bills\\.",
                   "Message from the Governor-General and the Administrator reported informing the House of assent to the bill\\.",
                   "Messages from the Administrator and the Governor-General reported informing the House of assent to the bills",
                   "Messages from the Governor-General/Administrator.{1,250}\\.",
                   "Message from the Governor-General recommending.{1,500} announced\\.",
                   "Ordered that the amendment be considered at a later hour this day\\.",
                   "Ordered that the .{1,15} reading be made an order of the day for the next sitting\\.",
                   "Bill returned from the Senate with an amendment\\.",
                   "Bill returned from Main Committee for further consideration; certified copy presented\\.",
                   "Message received from the Senate returning the bill without amendment or request\\.",
                   "Message received from the Senate acquainting the House that the Senate does not insist on its amendments.{0,50} disagreed to by the House\\.",
                   "Message received from the Senate acquainting the House that the Senate does not insist upon its amendments.{0,50} disagreed to by the House\\.",
                   "Message received from the Senate acquainting the House that the Senate has agreed to the amendments made by the House\\.",
                   "Message received from the Senate returning the bills without amendment or requests\\.",
                   "Message received from the Senate returning the bills without amendment\\.",
                   "Message received from the Senate returning the bill and acquainting the House that the Senate insists on the amendments made by the Senate\\.",
                   "Bill returned from the Senate with requested amendments\\.Ordered that the requested amendments be taken into consideration\\.",
                   "Petition received\\.",
                   "Motion \\(by .{1,35}\\) agreed to:.{5,400}\\.{0,1}",
                   "Motion \\(by .{1,35}\\)—agreed to:.{5,400}\\.{0,1}",
                   "Motion \\(by .{1,35}\\)—by leave—agreed to:.{5,400}\\.{0,1}",
                   "Motion \\(by .{1,35}\\)—by leave—proposed\\:.{5,400}\\.{0,1}",
                   "Motion \\(by .{1,35}\\) proposed[[:punct:]][[:space:]]{0,1}That the Main Committee do now adjourn\\.",
                   "Message received from the Senate returning the bill and informing the House that the Senate has agreed to the amendments made by the house\\.",
                   "Message received from the Senate returning the bill and informing the House that the Senate does not insist upon its amendments .{1,100}, disagreed to by the House.{0,150}\\.",
                   "Message received from the Senate returning the bill and informing the House that the Senate does not insist upon its amendment disagreed to by the House\\.",
                   ".{1,35}, for the committee elected to prepare an address—in—reply to the speech of Her Excellency the Governor-General, presented the proposed address, which was read by the Clerk\\. The proposed address read as follows:",
                   "Honourable members attended accordingly and having returned—",
                   "Honourable members attended accordingly, and having returned—",
                   "The Usher of the Black Rod, having been announced, was admitted, and delivered the message that the Deputy of the Governor-General for the opening of the parliament desired the attendance of honourable members in the Senate chamber\\.",
                   "The Usher of the Black Rod, having been announced, was admitted and delivered the message that the Deputy of the Governor-General for the opening of the parliament desired the attendance of honourable members in the Senate chamber\\.",
                   "The Deputy authorised by the Governor-General to administer the oath or affirmation entered the chamber\\.",
                   "The Clerk read the authority authorising the Hon\\. Robert Shenton French AC, Chief Justice of the High Court of Australia, to administer the oath or affirmation of allegiance to the Queen required by the Constitution to be taken or made by members of the House of Representatives\\.",
                   "The Clerk laid on the table duly endorsed returns to the writs for the election of members of the House of Representatives held on \\d{1,2} [[:alpha:]]{2,15} \\d{4}\\.",
                   "The following honourable members made and subscribed the oath or affirmation of allegiance:",
                   "The Usher of the Black Rod, having been announced, was admitted and delivered a message that Her Excellency the Governor-General desired the attendance of honourable members in the Senate chamber forthwith\\.",
                   "The Usher of the Black Rod, having been announced, was admitted, and delivered a message that His Excellency the Governor-General desired the attendance of honourable members in the Senate chamber immediately\\.",
                   "The Usher of the Black Rod, being announced, was admitted, and delivered a message that His Excellency the Governor-General desired the attendance of honourable members in the Senate chamber",
                   "The Usher of the Black Rod, having been announced, was admitted, and delivered a message that His Excellency the Governor-General",
                   "The Speaker and honourable members attended accordingly, and having returned—",
                   "The House met at \\d{1,2}\\.\\d\\d [[:alpha:]]{2}, pursuant to the proclomation of Her Excellency the Goveror-General.[[:space:]]{0,2}The Clerk read the Proclomation\\.",
                   "The House met at \\d{1,2}\\.\\d\\d [[:alpha:]]{2}, pursuant to the proclamation of Her Excellency the Governor-General.[[:space:]]{0,2}The Clerk read the Proclamation\\.",
                   "The House met at .{2,15} pursuant to the proclamation of His Excellency the Governor-General\\.[[:space:]]{0,2}The Clerk read the proclamation\\.",
                   "The House met at 10\\.30 a\\.m\\. pursuant to the proclamation of His Excellency the Governor-General\\.The Clerk read the proclamation\\.",
                   "His Excellency, Dr Susilo Bambang Yudhoyono, having been announced and escorted into the chamber—",
                   "Message received from the Senate acquainting the House that the Senate does not insist on its amendments disagreed to by the House and has agreed to the amendments made by the House in place of them\\.",
                   "(Ms|Mr|Mrs|Dr) .{5,35} made and subscribed the oath of allegiance\\.",
                   "(Ms|Mr|Mrs|Dr) .{5,35} made and subscribed the oath of allegiance:",
                   "A message from the Senate has been received advising the House that",
                   "Message received from the Senate returning the bill and informing the House that the Senate has agreed to the amendments made by the House\\.",
                   "The Rt Hon\\. Tony Blair having been announced and escorted into the chamber—",
                   "The Speaker has received a message from the Senate transmitting a resolution agreed to by the Senate",
                   "Message received from the Senate returning the bill and acquainting the House that the Senate insists on amendments Nos .{1,30} disagreed to by the House and desires the reconsideration of the bill by the House in respect of the amendments\\. Ordered that consideration of the message be made an order of the day for the next sitting\\.",
                   "Bill returned from Main Committee without amendment[[:punct:]] appropriation message having been reported; certified copy presented\\.[[:space:]]{0,2}Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee without having been fully considered[[:punct:]] certified copy presented\\.[[:space:]]{0,2}Ordered that the bill be taken into consideration at the next sitting\\.",
                   "Bill returned from Main Committee with an unresolved question; certified copy of bill and schedule of unresolved question presented\\.[[:space:]]{0,2}Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee with an amendment[[:punct:]].{0,150} certified copy of bill and schedule of amendment presented\\.[[:space:]]{0,2}Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee without amendment[[:punct:]].{0,300} certified copy.{0,20} presented\\.[[:space:]]{0,2}Ordered that the bill be taken into consideration .{1,50}\\.",
                   "Bill returned from Main Committee without having been fully considered; certified copy presented\\.[[:space:]]{0,2}Ordered that further proceedings of the bill be taken into consideration at the next sitting\\.",
                   "Bill returned from Main Committee with an unresolved question; certified copy of the bill and schedule of unresolved question presented\\.[[:space:]]{0,2}Ordered that the bill be taken into consideration forthwith\\.",
                   "Bill returned from Main Committee for further consideration; certified copy of the bill presented\\.",
                   "Bill returned from Main Committee having been considered",
                   "Bill returned from Main Committee with amendments[[:punct:]]",
                   "Bill returned from Main Committee with an amendment[[:punct:]]",
                   "Bill returned from Main Committee with an unresolved question[[:punct:]]",
                   "Bill returned from Main Committee with unresolved questions[[:punct:]]",
                   "Bill returned from Main Committee with an unresolved question and amendments",
                   "Bill returned from Main Committee without amendment[[:punct:]]",
                   "Bill returned from Main Committee without having been fully considered[[:punct:]]",
                   "Bill returned from Main Committee; certified copy of the bill presented\\.",
                   "Bill returned from the Senate with[[:space:]]{1,3}a request for an amendment",
                   "Bill returned from the Senate with a request for amendments\\.",
                   "Bill returned from the Senate with a requested amendment\\.",
                   "Bill returned from the Senate with amendment\\.",
                   "Bill returned from the Senate with further amendments\\.",
                   "Bill returned from the Senate with request[[:lower:]]{0,1} for amendments\\.",
                   "Bill returned from the Senate with requested amendments\\.",
                   "Bill returned from the Senate, with amendments\\.",
                   "Message[[:lower:]]{0,1} received from the Senate ",
                   "Message[[:lower:]]{0,1} .{0,20}from the Governor-General reported informing the House of assent to the following bill[[:lower:]]{0,1}",
                   "Message received from the Governor-General informing the House of assent to the following bills:",
                   "Consideration resumed, on motion by",
                   "Bill—by leave—taken as a whole",
                   "Billby leavetaken as a whole\\.",
                   "Motion \\(by .{1,45}\\){0,1}—by leave—put:",
                   "Motion \\(by .{1,45}\\){0,1} put:",
                   "Motion \\(by .{1,45}\\){0,1} agreed.{0,5}:",
                   "Motion \\(by .{1,45}\\) proposed:",
                   "Motion by \\(.{1,45}\\) proposed",
                   "Bills returned from the Senate with amendments",
                   "Bills returned from the Senate with a request for [[:space:]]{0,2}amendments and amendments",
                   "Bills returned from Main Committee without amendment;",
                   "Schedule \\d{1,2}, items \\d{1,2} to \\d{1,2}—by leave—taken together, and agreed to",
                   "Schedule \\d{1,2} agreed to\\.[[:space:]]{0,2}Schedule \\d{1,2}, items \\d{1,2} and \\d{1,2}—by leave—taken together, and agreed to\\.",
                   "Schedule \\d{1,2}, item \\d{1,2}\\.",
                   "The following bill was returned from the Senate without.{0,30} request",
                   "Mr Allan Agapitos Morris made and subscribed the affirmation of allegiance\\.",
                   "The Clerk laid on the table duly endorsed return to the writ for the supplementary election for the division of Newcastle, in the state of New South Wales\\.",
                   "Order of the day returned from Main Committee\\.",
                   "Order of the day returned from Main Committee[[:punct:]]",
                   "Bill—by leave—taken as whole\\.",
                   "Schedule \\d{1,2}—by leave—taken as a whole\\.",
                   "On 7 December 2000, at page 23642, the incorrect Senate amendments were inserted into Hansard for the States Grants \\(Primary and Secondary Education Assistance\\) Bill 2000\\. The correct text should read as follows:",
                   "Messages from the Administrator reported informing the House of assent to the following bills",
                   "The following papers were deemed to be presented on \\d{1,2} [[:alpha:]]{1,15} \\d{4}",
                   "The following paper was deemed to have been presented on \\d{1,2} [[:alpha:]]{1,15} \\d{4}",
                   "Mr Cameron Thompson, for the committee appointed to prepare an address-in-reply to the speech of His Excellency the Governor-General, presented the proposed address, which was read by the Clerk\\.",
                   "The Honourable George Walker Bush having been announced and escorted into the chamber—",
                   "The Rt Hon\\. Stephen Harper having been announced and escorted into the chamber—",
                   "His Excellency Hu Jintao having been announced and escorted into the chamber—",
                   "Ms O’Neill, for the committee elected to prepare an address-in-reply to the speech of Her Excellency the Governor-General, presented the proposed address, which was read by the Clerk\\.",
                   "A message has been received from the Senate acquainting the House that",
                   "The following bills were returned from the Senate without amendment",
                   "Message from the Senate returning the bill and acquainting the House that",
                   "Message from the Administrator reported informing the House of assent to the following bills",
                   "Mrs Louise Markus, for the committee appointed to prepare an address-in-reply to the speech of His Excellency the Governor-General, presented the proposed address, which was read by the Clerk\\.",
                   "Mrs Sussan Ley, for the committee appointed to prepare an address-in-reply to the speech of His Excellency the Governor-General, presented the proposed address, which was read by the Clerk\\.",
                   "Mr Hale, for the committee elected to prepare an address-in-reply to the speech of His Excellency the Governor-General, presented the proposed address, which was read by the Clerk\\.",
                   "Bill, explanatory memorandum and—by leave—",
                   "Message from His Excellency the Administrator reported informing the House of assent to the following bill",
                   "The following honourable member made and subscribed the oath or affirmation of allegiance",
                   "Mr Rodney Weston Sawford made and subscribed the affirmation of allegiance",
                   "Message[[:lower:]]{0,1} from the Administrator reported informing the House of assent to the bills\\.",
                   "Consideration interrupted\\.",
                   "The petition read as follows—",
                   "Schedule agreed to\\.",
                   "Amendment negatived\\.",
                   "Amendments agreed to\\.",
                   "Amendments negatived\\.",
                   "Question resolved in the negative\\.",
                   "Question resolved in the affirmative\\.",
                   "An incident having occurred in the gallery—",
                   "Bill, as amended, agreed to\\.",
                   "Question put: That this bill be now read a second time\\.",
                   "Question put:[[:space:]]{0,1}That the amendment \\(.{1,35}\\) be agreed to\\.",
                   "Question put:[[:space:]]{0,1}That the amendments \\(.{1,35}\\) be agreed to\\.",
                   "Original question resolved in the affirmative\\.",
                   "Question put:[[:space:]]{0,1}That the words proposed to be omitted stand part of the question\\.",
                   "Question put:[[:space:]]{0,1}That the words proposed to be omitted \\(.{1,50}\\) stand part of the question\\.",
                   "\\nQuestion put:[[:space:]]{0,1}That the amendments be agreed to\\.", 
                   "\\nQuestion put:[[:space:]]{0,1}That the amendments \\(.{1,50}\\) be agreed to\\.",
                   "\\nMain committee adjourned at \\d\\.\\d\\d pm",
                   "\\nQuestion put: That the words proposed to be omitted \\(.{1,50}\\) stand part of the question\\.",
                   "\\n That the Main Committee do now adjourn\\.",
                   "\\n That the House do now adjourn\\.",
                   "\\nDebated adjourned\\.",
                   "\\nQuestion put:[[:space:]]{0,1}That the Speaker’s ruling be dissented from\\.",
                   "\\nQuestion put: That the amendment \\(.{1,50}\\) be agreed to\\.",
                   "\\n\\(Quorum formed\\)",
                   "\\nAt 6\\.45pm, an audio system failure prevented the recording and transcription of the speeches of Ms Parke, MrBruce Scott, Mr Stephen Smith and Mr Simpkins, and of any further proceedings of the Main Committee\\.In accordance with the Speaker’s statement of 1 December 2008, the text of those speeches has been incorporated below\\.",
                   "\\nQuestion put:[[:space:]]{0,1}That Senate amendments .{1,50} be disagreed to\\.",
                   "\\nThe petitions read as follows—",
                   "Petitions received\\.",
                   "\\nI seek leave to continue my remarks later\\.",
                   "\\nDebate \\(on motion by\\nMr Debus\\) adjourned\\.",
                   "\\n[[:upper:]][[:lower:]]{5,9}, \\d{1,2} [[:upper:]][[:lower:]]{3,10} 20\\d{2}")
  
  
  # split on stage notes
  # sometimes they're preceeded by a timestamp, and we want to capture those so in the next step we can use them for the time stamp
  # other times they're not though, so split those out afterward with a negative lookbehind to avoid double—splitting on those with a timestamp
  main <- separate_rows(main, body, sep=paste0("(?=\\d\\d\\:\\d\\d\\:\\d\\d", stage_notes, ")", collapse = "|")) %>% 
    separate_rows(., body, sep=paste0("(?<!\\d\\d\\:\\d\\d\\:\\d\\d)(?=", stage_notes, ")", collapse = "|")) %>% filter(body!="") %>% filter(body!=" ")
  
  # if a stage note starts with a time stamp, remove it
  main <- main %>% mutate(body = ifelse(str_detect(body, paste0("(?=\\d\\d\\:\\d\\d\\:\\d\\d", stage_notes, ")", collapse = "|")),
                                        str_remove(body, "\\d\\d\\:\\d\\d\\:\\d\\d"),
                                        body))
  
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
  
  # specific to 2010-05-24 - there was an extra 's' before a pattern and it's left as it's own row - drop that
  if (filename=="2010-05-24.xml"){
    main <- main %>% filter(body!="s")
  }
  
  # specific to 2006 02 07 - pattern issue
  if (filename=="2006-02-07.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "^41414141—"),
                                          str_remove(body, "^41414141—"),
                                          body))
  }
  
  # pattern issue
  if (filename=="2001-03-05.xml" | filename=="2001-03-07.xml"){
    main <- main %>% filter(body!="[WORKPLACE RELATIONS AMENDMENT (TALLIES) BILL 2000] ")
  }
  
  # pattern issue same across these 3 dates
  if (filename=="2007-02-26.xml" | filename=="2007-02-12.xml" | filename=="2006-10-09.xml"){
    main <- main %>% filter(!str_detect(body, "^3\\dReport13\\d[[:space:]]{0,2}$"))
  }
  
  # 2009—11—19 noticed some rows where text is just page number — this is the result of separating on patterns where likely an extra page number was parsed but not captured in the pattern
  # filter those out
  main <- main %>% filter(!str_detect(body, paste0("^", page.no, "$")))
  
  # we need to split on statements like "Mr Causley interjecting" that aren't transcribed explicitly as interjections in the XML
  # extract those with a lookbehind and lookahead
  interject_specific <- str_extract_all(main$body, "(?<=\\.|\\. )(Mrs|Ms|Mr|Dr) .{1,35} interjecting$|(?<=\\.|\\. )(Mrs|Ms|Mr|Dr) .{1,35} interjecting—") %>% unlist() %>% na.omit() %>% unique()
  
  # now separate rows on those
  if (length(interject_specific)>0){
    main <- separate_rows(main, body, sep=paste0("(?=", interject_specific, ")", collapse = "|"))
    
    # remove details for split statements, so we can fill in with the correct ones
    main <- main %>% mutate(name = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, name),
                            name_short = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, name_short),
                            name.id = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, name.id),
                            role = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, role),
                            in.gov = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, in.gov),
                            first.speech = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, first.speech),
                            electorate = ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, electorate),
                            party = as.factor(ifelse(grepl(paste0("^", interject_specific, collapse = "|"), body), NA, as.character(party))))
    
    # paste the name into name_short
    main <- main %>% mutate(name_short = ifelse(is.na(name_short) & str_detect(body, paste0("^", interject_specific, collapse = "|")),
                                                str_extract(body, "^.{1,35}(?= interjecting)"),
                                                name_short))
  }
  
  # we need to split on statements that start with something like "Mr CHARLES (La Trobe) (10.03 a.m.)—" that wasn't picked up in the patterns data
  # for some reason, Fran Bailey is usually not given a prefix. this is the only person this is an issue for that i've seen, so let's just grab those in their own step
  # grab matches for these
  talk_start <- str_extract_all(main$body, "(Mrs|Ms|Mr|Dr|The|Miss) .{1,25}(?!=—) \\(.{1,250}\\)[[:space:]]{0,2}\\(\\d{1,2}\\.\\d\\d .{2,4}\\)—") %>% 
    unlist() %>% 
    na.omit() %>% 
    unique() %>% 
    as_tibble() %>% 
    bind_rows(., str_extract_all(main$body, "FRAN BAILEY \\(.{1,250}\\)[[:space:]]{0,2}\\(\\d{1,2}\\.\\d\\d .{2,4}\\)—") %>% 
                unlist() %>% 
                na.omit() %>% 
                unique() %>% 
                as_tibble()) %>% 
    mutate(value=str_replace_all(value, "\\.", "\\\\."),
           value=str_replace_all(value, "\\(", "\\\\("),
           value=str_replace_all(value, "\\)", "\\\\)")) %>% 
    pull(value)
    
  
  if (length(talk_start)>0){
    main <- separate_rows(main, body, sep=paste0("(?=", talk_start, ")", collapse = "|")) %>% filter(body!="")
    
    # remove details for split statements, so we can fill in with the correct ones
    main <- main %>% mutate(name = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, name),
                            name_short = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, name_short),
                            name.id = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, name.id),
                            role = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, role),
                            in.gov = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, in.gov),
                            first.speech = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, first.speech),
                            electorate = ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, electorate),
                            party = as.factor(ifelse(grepl(paste0("^", talk_start, collapse = "|"), body), NA, as.character(party))))
    
    # paste the name into name_short,time into time stamp, convert time to 24hr, remove pattern from body
    main <- main %>% mutate(name_short = ifelse(is.na(name_short) & str_detect(body, paste0("^", talk_start, collapse = "|")),
                                                str_extract(body, "^.{1,30}(?= \\(.{1,250}\\)[[:space:]]{1,2}\\(\\d{1,2}\\.\\d\\d .{2,4}\\)—)"),
                                                name_short),
                            time.stamp = ifelse(str_detect(body, paste0("^", talk_start, collapse = "|")),
                                                str_extract(body, "(?<=^.{1,30} \\(.{1,250}\\)[[:space:]]{1,2}\\()\\d{1,2}\\.\\d\\d .{2,4}(?=\\)—)"),
                                                time.stamp),
                            body = ifelse(str_detect(body, paste0("^", talk_start, collapse = "|")),
                                          str_remove(body, paste0("^", talk_start, collapse = "|")),
                                          body))
    
  }
  
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
                                        str_extract(main$body, paste0("^", str_remove(interject_general, " interjecting—$|—$"), collapse = "|")), 
                                        name))
  
  # if general interjection (but doesn't include interjecting—), remove name from body
  main <-  main %>% mutate(body = ifelse(str_detect(body, paste0("^", interject_general, collapse = "|")) & str_detect(body, paste0("^", name, "—")),
                                str_remove(body, paste0("^", name, "—")),
                                body))
  
  # if body starts with "The Clerk—", extract that and paste "The Clerk" in as the name
  main <- main %>% mutate(name = ifelse(is.na(name) & str_detect(body,"^The Clerk—"),
                                        "The Clerk", name),
                          body = ifelse(str_detect(body, "^The Clerk—"),
                                        str_remove(body, "^The Clerk—"),
                                        body))
  
  if (filename=="2005-12-08.xml"){
    main <- main %>% mutate(name = ifelse(str_detect(body, "The ACTING SPEAKERMr Causley\\) then took the chair, and read prayers\\."),
                                          "business start", name),
                            body = ifelse(str_detect(body, "^The ACTING SPEAKERMr Causley\\) then took the chair, and read prayers\\.$"),
                                          "The ACTING SPEAKER (Mr Causley) then took the chair, and read prayers.",
                                          body))
  }

  # remove rows where it's just a page number (result of pattern separation and pattern not entirely being captured)
  main <- main %>% filter(!str_detect(body, "^\\d+$"))
  
  # now that we've split all the rows, let's add an order column so we can keep track of exact order of things
  # need original speech_no column though to keep track of which interjections belong to which speech (will be useful to flag interjections later)
  main <- main %>% rowid_to_column("order") %>% select(-itemindex) %>% select(order, everything())
  
  # specific case - random \ in name ID, causing issues with pattern b/c we removed those from the pattern_text earlier to avoid regex issues, so need to manually fix that in patterns_data before we merge to fill things in
  if (filename=="2001-02-26.xml"){
    patterns_data <- patterns_data %>% mutate(name.id = ifelse(first_pattern=="\\8I4Martin, Stephen, MP", "8I4", name.id), 
                                              first_pattern = ifelse(first_pattern=="\\8I4Martin, Stephen, MP", "8I4Martin, Stephen, MP", first_pattern))
    
    main <- main %>% mutate(body = ifelse(body=="Opposition members interjecting—\\", "Opposition members interjecting—", body))
  }
  
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
           body = ifelse(str_detect(body, "^—|^—"),
                         str_remove(body, "^—|^—"),
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
    main <- main %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0("(?<!the |our |so—called )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|")),
                                                             str_match(body, paste0("(?<!the |our |so—called )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|")),
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
             body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0(sub1_pattern, "(?!, | |\\?|\\. |\\.Th)")),
                           str_remove(body, paste0(sub1_pattern, "(?!, | |\\?|\\. |\\.Th)")),
                           body)) %>% 
      mutate(sub1_flag = ifelse(!is.na(sub1_pattern) & is.na(sub2_pattern), 1, 0),
             sub2_flag = ifelse(!is.na(sub2_pattern), 1, 0)) %>% 
      select(-sub1_pattern, -sub2_pattern)
  } else if (length(sub1_patterns_info)>0 & length(sub2_patterns_info)==0) {
    main <- main %>% mutate(sub1_pattern = ifelse(str_detect(body, paste0("(?<!the |our |so—called )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|")),
                                                             str_match_all(body, paste0("(?<!the |our |so—called )", sub1_patterns_info, "(?!, | |\\?|\\. |\\.Th)", collapse = "|")),
                                                             NA),
                                       body = ifelse(!is.na(sub1_pattern) & str_detect(body, paste0(sub1_pattern, "(?!, | |\\?|\\. |\\.Th)")),
                                                     str_remove(body, paste0(sub1_pattern)),
                                                     body)) %>% 
      mutate(sub1_flag = ifelse(!is.na(sub1_pattern), 1, 0),
             sub2_flag = 0) %>% 
      select(-sub1_pattern)
  }
  
  # specific issues i need to deal with found from test3
  if (filename=="2000-05-29.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Aboriginals: Reconciliation16436 "),
                                          str_remove(body, "(?<=\\(Time expired\\)) Aboriginals: Reconciliation16436 "),
                                          body))
  }
  
  if (filename=="2000-06-22.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Goods and Services Tax: Caravan ParksRobertson Electorate: Gosford Under 17 Netball Team AESOP Business Volunteers Ltd18120Robertson Electorate: Gosford Under 17 Netball Team AESOP Business Volunteers Ltd"),
                                          str_remove(body, "(?<=\\(Time expired\\)) Goods and Services Tax: Caravan ParksRobertson Electorate: Gosford Under 17 Netball Team AESOP Business Volunteers Ltd18120Robertson Electorate: Gosford Under 17 Netball Team AESOP Business Volunteers Ltd"),
                                          body))
  }
  
  if (filename=="2000-10-04.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Eden-Monaro Electorate: Lavender Industry20806"),
                                          str_remove(body, "(?<=\\(Time expired\\)) Eden-Monaro Electorate: Lavender Industry20806"),
                                          body))
  }
  
  if (filename=="2000-10-09.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Groom Electorate: Toowoomba Range Crossing21131"),
                                          str_remove(body, "(?<=\\(Time expired\\)) Groom Electorate: Toowoomba Range Crossing21131"),
                                          body))
  }
  
  if (filename=="2003-09-08.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Family Court19374"),
                                          str_remove(body, "(?<=\\(Time expired\\)) Family Court19374"),
                                          body))
  }
  
  if (filename=="2003-10-15.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) People Living With HIV/AIDS: 15th Anniversary21553"),
                                          str_remove(body, "(?<=\\(Time expired\\)) People Living With HIV/AIDS: 15th Anniversary21553"),
                                          body))
  }
  
  if (filename=="2005-05-25.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Pontian Genocide"),
                                          str_remove(body, "(?<=\\(Time expired\\)) Pontian Genocide"),
                                          body))
  }
  
  if (filename=="2008-09-04.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\) Cook Electorate: Shire 2020\\+ Youth Summit7263"),
                                          str_remove(body, "(?<=\\(Time expired\\)) Cook Electorate: Shire 2020\\+ Youth Summit7263"),
                                          body))
  }
  
  if (filename=="2008-12-01.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\)\\n Universal Declaration of Human Rights"),
                                          str_remove(body, "(?<=\\(Time expired\\))\\n Universal Declaration of Human Rights"),
                                          body))
  }
  
  if (filename=="2010-10-28.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\)\\n Griffith Electorate: Daniel Morcombe Foundation"),
                                          str_remove(body, "(?<=\\(Time expired\\))\\n Griffith Electorate: Daniel Morcombe Foundation"),
                                          body))
  }
  
  if(filename=="2007-05-09.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\)\\n Flinders Electorate: Bitumen Plant"),
                                          str_remove(body, "(?<=\\(Time expired\\))\\n Flinders Electorate: Bitumen Plant"),
                                          body))

  }
  
  if(filename=="2010-03-10.xml"){
    main <- main %>% mutate(body = ifelse(str_detect(body, "\\(Time expired\\)\\n Werriwa Electorate: Hospitals"),
                                          str_remove(body, "(?<=\\(Time expired\\))\\n Werriwa Electorate: Hospitals"),
                                          body))
    
  }
  
  
  
  ######### BELOW IS COMMENTED OUT B/C CAUSING ISSUES — this is something to deal with later, look at 2002—09—26 row 17—18 for example
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
  # # if a line begins with a phrase we split on, extract the name and paste it in — do this for name short as well since they're the same
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
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
    }
    
    # sub—debate info
    a_to_q_sub_info <- item_df(xml_df,"//answers.to.questions/debate/subdebate.1/subdebateinfo") %>% 
      unnest(everything())
      
      #tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/subdebateinfo"), collectNames = F, homogeneous = T), .name_repair = "unique")
    
    # sometimes page number is written twice so multiple of the same column, just grab the first one
    if ("page.no...2" %in% names(a_to_q_sub_info)) {
      a_to_q_sub_info <- a_to_q_sub_info %>% rename(page.no = page.no...2) %>% 
        select(c(title, page.no, id.no)) %>% 
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
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
             answer = 0) %>% 
      mutate(question = ifelse(str_detect(body, "^.{1,20}The answer to the .{1,30} question is as follows\\:|^.{1,20}has provided the following answer to the honourable member's question\\:"), 0, 1),
             answer = ifelse(str_detect(body, "^.{1,20}The answer to the .{1,30} question is as follows\\:|^.{1,20}has provided the following answer to the honourable member's question\\:"), 1, 0)) 
    
    # add any wrongly nested answers (in writing) into answer dataframe
    a_to_q_speech_as <- a_to_q_speech_qs %>% filter(answer==1)
    
    # remove any wrongly nested answers from question in writing dataframe
    a_to_q_speech_qs <- a_to_q_speech_qs %>% filter(answer!=1)
    
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
             answer = 1) %>% 
      bind_rows(., a_to_q_speech_as) %>% 
      arrange(page.no)
    
    # dealing with specific issue on this sitting day where answer is embedded with question text, need to separate and re-categorize manually
    if (filename == "2007-03-21.xml") {
      a_to_q_speech_as <- a_to_q_speech_qs %>% 
        as_tibble() %>% 
        slice(n()) %>% 
        separate_rows(body, sep="(?=McGAURAN, Hon. Peter John, Gippsland238McGauran, Peter, MPXH4GippslandNATSMinister for Agriculture, Fisheries and Forestry1Mr McGauran—The answer to the honourable member’s question is as follows\\:)") %>% 
        slice(2) %>% 
        mutate(question = 0, 
               answer = 1,
               name = "McGauran, Peter, MP",
               name.id = "XH4",
               electorate = "Gippsland",
               party = "NATS",
               in.gov = "1",
               first.speech = NA,
               body = str_remove(body, "^McGAURAN, Hon. Peter John, Gippsland238McGauran, Peter, MPXH4GippslandNATSMinister for Agriculture, Fisheries and Forestry1")) %>% 
        bind_rows(a_to_q_speech_as, .)
      
      a_to_q_speech_qs <- a_to_q_speech_qs %>% 
        as_tibble() %>% 
        separate_rows(body, sep="(?=Mr McGauran—The answer to the honourable member’s question is as follows\\:)") %>% 
        filter(!str_detect(body, "^Mr McGauran—The answer to the honourable member’s question is as follows\\:"))
    }
    
    # combine them in the right order, add flags
    a_to_q_speech <- lst(a_to_q_speech_qs, a_to_q_speech_as) %>% 
      map(rowid_to_column) %>% 
      bind_rows() %>% 
      arrange(rowid) %>% 
      select(-rowid, -role) %>% 
      mutate(fedchamb_flag = 0,
             q_in_writing = 1,
             sub1_flag = 1,
             sub2_flag = 0,
             page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
    
  } else {
    a_to_q_deb_info <- tibble()
    a_to_q_sub_info <- tibble()
    a_to_q_speech <- tibble()
  }
  
  # special case where main committee content aren't nested in maincomm.xscript - its all in the chamber parent node
  # we need to manually grab the bus start and flag the fedchamb proceedings
  if (filename=="2006-02-16.xml"){
    main <- main %>% separate_rows(body, sep="(?=The DEPUTY SPEAKER \\(Hon\\. IR Causley\\) took the chair at 9\\.30 am\\.)") %>% 
      mutate(name = ifelse(str_detect(body, "The DEPUTY SPEAKER \\(Hon\\. IR Causley\\) took the chair at 9\\.30 am\\."), "business start", name),
             name.id = ifelse(name=="business start", NA, name.id),
             party = as.factor(ifelse(name=="business start", NA, as.character(party))),
             electorate = ifelse(name=="business start", NA, electorate),
             first.speech = ifelse(name=="business start", NA, first.speech),
             in.gov = ifelse(name=="business start", NA, in.gov),
             fedchamb_flag = ifelse(str_detect(body, "The DEPUTY SPEAKER \\(Hon\\. IR Causley\\) took the chair at 9\\.30 am\\."), 1, fedchamb_flag))
    
    # now flag fedchamb correctly
    main <- main %>% select(-order) %>% rowid_to_column("order") %>% 
      mutate(fedchamb_flag = ifelse(order > order[str_detect(body, "The DEPUTY SPEAKER \\(Hon\\. IR Causley\\) took the chair at 9\\.30 am\\.")], 1, fedchamb_flag))
  }
  
    # same as above for different date
    if (filename=="2006-02-14.xml"){
      main <- main %>% separate_rows(body, sep="(?=The DEPUTY SPEAKER \\(Mr Jenkins\\) took the chair at 4\\.00 pm\\.)") %>% 
        mutate(name = ifelse(str_detect(body, "The DEPUTY SPEAKER \\(Mr Jenkins\\) took the chair at 4\\.00 pm\\."), "business start", name),
               name.id = ifelse(name=="business start", NA, name.id),
               party = as.factor(ifelse(name=="business start", NA, as.character(party))),
               electorate = ifelse(name=="business start", NA, electorate),
               first.speech = ifelse(name=="business start", NA, first.speech),
               in.gov = ifelse(name=="business start", NA, in.gov),
               fedchamb_flag = ifelse(str_detect(body, "The DEPUTY SPEAKER \\(Mr Jenkins\\) took the chair at 4\\.00 pm\\."), 1, fedchamb_flag))
    
    # now flag fedchamb correctly
    main <- main %>% select(-order) %>% rowid_to_column("order") %>% 
      mutate(fedchamb_flag = ifelse(order > order[str_detect(body, "The DEPUTY SPEAKER \\(Mr Jenkins\\) took the chair at 4\\.00 pm\\.")], 1, fedchamb_flag))
  }
  
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
  
  # grab adjournment, add all flags, select things in right order to be added to main — CHAMBER
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment"),collectNames = F, homogeneous = T), .name_repair = "unique"))==1){
    
    # sometimes there is no page number and time.stamp (i.e. adjournment info), so we need to add an extra if—else for that case
    if (nrow(item_df(xml_df, "//chamber.xscript/adjournment/adjournmentinfo"))>0){
      adjournment_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/adjournment/adjournmentinfo"),
                                                 collectNames = F, homogeneous = T), .name_repair = "unique")
      
      # sometimes page number is written twice so multiple of the same column, just grab the first one
      if ("page.no...2" %in% names(adjournment_chamb)) {
        adjournment_chamb <- adjournment_chamb %>% rename(page.no = page.no...1) %>% 
          select(c(page.no, time.stamp)) %>% 
          mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
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
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
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
  
  # grab adjournment, add all flags, select things in right order to be added to main — FEDERATION CHAMBER
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/adjournment/adjournmentinfo"),collectNames = F, homogeneous = T), .name_repair = "unique"))>0){
    adjournment_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/adjournment/adjournmentinfo"),
                                               collectNames = F, homogeneous = T), .name_repair = "unique")
    
    # sometimes page number is written twice so multiple of the same column, just grab the first one
    if ("page.no...2" %in% names(adjournment_fed)) {
      adjournment_fed <- adjournment_fed %>% rename(page.no = page.no...1) %>% 
        select(c(page.no, time.stamp)) %>% 
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL})
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
  
  # add questions in writing to end of main
  main <- main %>% mutate(page.no = as.numeric(page.no)) %>% 
    mutate(question = 0,
           answer = 0,
           q_in_writing = 0) %>% 
    select(-order) %>% 
    bind_rows(., a_to_q_speech)
  
  # issue specific to 2005—09—08 row 287
  main <- main %>% mutate(name = ifelse(str_detect(body, "^DANBY, Mr Michael"), "Danby, Michael, MP", name),
                          time.stamp = ifelse(str_detect(body, "^DANBY, Mr Michael, Melbourne Ports14812\\.31 pmDanby, Michael, MPWF6Melbourne PortsALP00Mr DANBY\\(Melbourne Ports\\)\\(12\\.31 pm\\)—"),
                                              "12:31:00", time.stamp),
                          body = ifelse(str_detect(body, "^DANBY, Mr Michael, Melbourne Ports14812\\.31 pmDanby, Michael, MPWF6Melbourne PortsALP00Mr DANBY\\(Melbourne Ports\\)\\(12\\.31 pm\\)—"),
                                        str_remove(body, "^DANBY, Mr Michael, Melbourne Ports14812\\.31 pmDanby, Michael, MPWF6Melbourne PortsALP00Mr DANBY\\(Melbourne Ports\\)\\(12\\.31 pm\\)—"),
                                        body))
  
  # have to grab names and clean body up manually, from 2002—09—26
  if (filename=="2002—09—26.xml"){
    main <- separate_rows(main, body, sep="(?=Mr WILLIAMS \\(Tangney—Attorney—General\\) \\(10\\.15 a\\.m\\.\\)—)") %>% 
      mutate(name = ifelse(str_detect(body, "^Mr WILLIAMS \\(Tangney—Attorney—General\\) \\(10\\.1\\d a\\.m\\.\\)—"),
                           "Mr WILLIAMS",
                           name),
             body = str_remove(body, "^Mr WILLIAMS \\(Tangney—Attorney—General\\) \\(10\\.1\\d a\\.m\\.\\)—")) %>% 
      mutate(name = ifelse(str_detect(body, "^The SPEAKER  \\(4\\.14 p\\.m\\.\\)—"),
                           "The SPEAKER",
                           name),
             body = ifelse(str_detect(body, "^The SPEAKER  \\(4\\.14 p\\.m\\.\\)—"),
                           str_remove(body, "^The SPEAKER  \\(4\\.14 p\\.m\\.\\)—"),
                           body)) %>% 
      mutate(name = ifelse(str_detect(body, "^Mr ABBOTT \\(Warringah—Leader of the House\\) \\(4\\.1\\d p\\.m\\.\\)—"),
                           "Mr ABBOTT", name),
             body = str_remove(body, "^Mr ABBOTT \\(Warringah—Leader of the House\\) \\(4\\.1\\d p\\.m\\.\\)—")) %>% 
      mutate(name = ifelse(str_detect(body, "Mr SLIPPER \\(Fisher—Parliamentary Secretary to the Minister for Finance and Administration\\) \\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                           "Mr SLIPPER", name),
             body = str_remove(body, "Mr SLIPPER \\(Fisher—Parliamentary Secretary to the Minister for Finance and Administration\\) \\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—")) %>% 
      mutate(name = ifelse(str_detect(body, "Ms JULIE BISHOP \\(Curtin\\) \\(12\\.06 p\\.m\\.\\)—"), "Ms JULIE BISHOP", name),
             body = str_remove(body, "Ms JULIE BISHOP \\(Curtin\\) \\(12\\.06 p\\.m\\.\\)—"))
  }

  
  # group by speech number and only keep timestamp associated with first statement in that speech - the others just follow with splitting rows and so are technically imputed
  main <- main %>% group_by(speech_no) %>% rowid_to_column("order") %>% 
    mutate(time.stamp = ifelse(n()>1 & order!=min(order), NA, time.stamp)) %>% 
    select(-order) %>% 
    ungroup()
  
  # sometimes if a pattern is missing a couple digits then it won't be removed from the body, they'll just be left on their own lines, so filter those out
  main <- main %>% filter(!str_detect(body, "^\\d{1,2}$"))
  
  # if a row's entire body is just a sub—debate title preceded by a single digit (this happened on 2007—05—08), remove the contents b/c we don't want to keep this row
  # filter it out in next step
  main <- main %>% mutate(body = ifelse(str_detect(body, paste0("^\\d", sub2_patterns_info, "$", collapse = "|")),
                                        "", body))
  
  # get rid of any random empty rows
  main <- main %>% filter(body!="") %>% filter(body!=" ") %>% filter(body!="\t")
  
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
  
  # if name is NA and body starts with "The SPEAKER-", extract that and paste it in
  main <- main %>% mutate(name = ifelse(is.na(name) & str_detect(body, "^The SPEAKER—"), "The SPEAKER", name),
                          body = ifelse(name=="The SPEAKER" & str_detect(body, "^The SPEAKER—"), str_remove(body, "^The SPEAKER—"), body))
  
  # if name is NA and body starts with "The DEPUTY SPEAKER (....)-", extract that and paste it in
  main <- main %>% mutate(name = ifelse(is.na(name) & str_detect(body, "^The DEPUTY SPEAKER \\(.{1,35}\\)—"), str_extract(body, "^The DEPUTY SPEAKER \\(.{1,35}\\)(?=—)"), name),
                          body = ifelse(str_detect(body, "^The DEPUTY SPEAKER \\(.{1,35}\\)—"), str_remove(body, "^The DEPUTY SPEAKER \\(.{1,35}\\)—"), body))
  
  # saw this on 2010—03—17 — caused a lot of names to be left NA b/c they weren't picked up in the patterns we extracted earlier
  # have to grab names and clean body up manually
  main <- main %>% mutate(name = ifelse(is.na(name) & str_detect(body, "^Dr .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Mrs .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Mr .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Ms .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—"),
                                str_extract(body, "^Dr .{1,30}(?=\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—)|^Mrs .{1,30}(?=\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—)|^Mr .{1,30}(?=\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—)|^Ms .{1,30}(?=\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—)"),
                                name),
                  body = ifelse(str_detect(body, "^Dr .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Mrs .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Mr .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Ms .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—"),
                                str_remove(body, "^Dr .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Mrs .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Mr .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—|^Ms .{1,30}\\n\\(.{1,200}\\)\\d\\d\\:\\d\\d\\:\\d\\d—"),
                                body))
  
  # grab date of sitting day from filename so we can filter people who are alive in next step
  thisDate <- as.Date(str_remove(filename, "\\.xml$"))
  
  # on 2000—08—31 Zahra Christian was given Dr Nelson's name_short and name.id — we need to manually fix this
  main <- main %>% mutate(name_short = ifelse(name=="Zahra, Christian, MP" & name_short=="Dr NELSON" & name.id=="RW5",
                                        "Mr Zahra", name_short),
                    name.id = ifelse(name=="Zahra, Christian, MP" & name.id=="RW5", "84H", name.id))
  
  # on 2000 08 17 Mr Howard was given Mr Costello's name and name ID - I checked the PDF and this should be attributed to Mr Howard, fix this manually
  if (filename=="2000-08-17.xml"){
    main <- main %>% mutate(name = ifelse(!is.na(name_short) & name_short=="Mr Howard" & name.id=="CT4" & name=="Costello, Peter, MP",
                                          "Howard, John, MP",
                                          name),
                            name.id = ifelse(name_short=="Mr Howard" & name.id=="CT4" & name=="Howard, John, MP",
                                             "ZD4",
                                             name.id),
                            electorate = ifelse(name_short=="Mr Howard" & name.id=="ZD4" & name=="Howard, John, MP",
                                           "Bennelong",
                                           electorate))
  }
  
  # 2000—12—04 same issue for Kelly Jackie
  main <- main %>% mutate(name_short = ifelse(name=="Kelly, Jackie, MP" & name_short=="Mr CREAN" & name.id=="DT4",
                                              "Ms Kelly Jackie", name_short),
                          name.id = ifelse(name=="Kelly, Jackie, MP" & name.id=="DT4", "GK6", name.id))
  
  # 2000—04—13 mr slipper given mr speaker's name as name_short, need to fix
  main <- main %>% mutate(name = ifelse(name=="SPEAKER, Mr" & name_short=="Mr Slipper" & name.id=="0V5",
                                              "Slipper, Peter, MP", name))
  
  # from 2002—09—18
  main <- main %>% mutate(name = ifelse(name=="Mr McMULLAN,MP" & name.id=="5I4",
                                        "McMullan, Bob, MP", name))
  
  main <- main %>% mutate(name= ifelse(name=="Mr CADMAN,MP" & name.id=="SD4",
                                       "Cadman, Alan, MP", name))
  
  main <- main %>% mutate(name= ifelse(name=="Mr McGAURAN,MP" & name.id=="XH4",
                                       "McGauran, Peter, MP", name))
  
  # from 2005—09—07
  main <- main %>% mutate(name = ifelse(name=="McGauran, and the Peter, MP","McGauran, Peter, MP", name))
  
  # from 2003—03—06
  main <- main %>% mutate(name = ifelse(name=="Abbott, Tony, MP" & name_short =="Mr SWAN" & name.id=="2V5", "Swan, Wayne, MP", name))
  
  # from 2004—03—22
  main <- main %>% mutate(name = ifelse(name=="Latham, Mark, MPis the Prime Minister now expecting", "Latham, Mark, MP", name))
  
  # from 1999—02—08
  main <- main %>% mutate(name = ifelse(name=="Hoare, Kelly, MP" & name_short=="Mr SPEAKER" & name.id=="10000",
                                              "Mr SPEAKER", name),
                          party = as.factor(ifelse(name=="Mr SPEAKER" & party=="ALP", NA, as.character(party))),
                          electorate = ifelse(name=="Mr SPEAKER" & electorate=="Charlton", NA, electorate))
  
  main <- main %>% mutate(name = ifelse(name=="Beazley, Kim, MP" & name_short=="Mr SPEAKER" & name.id=="10000",
                                        "Mr SPEAKER", name),
                          party = as.factor(ifelse(name=="Mr SPEAKER" & party=="ALP", NA, as.character(party))),
                          electorate = ifelse(name=="Mr SPEAKER" & electorate=="Brand", NA, electorate))
  
  # 2000—06—20 issue b/c sometimes name short and name ID are unique but other times they're just the general ones, and this will give us extra rows in the lookup table
  # let's just keep it general b/c it's easier to do and then just back out later
  main <- main %>% mutate(name.id = ifelse(name=="SPEAKER, The" & name.id!="10000", "10000", name.id),
                  name_short = ifelse(name=="SPEAKER, The" & name_short!="The SPEAKER", "The SPEAKER", name_short))
  
  # 2008—06—23
  main <- main %>% mutate(name = ifelse(name ==", Bob, MP" & name.id=="8IS", "Debus, Bob, MP", name))
  
  # trying to keep things general for the speaker so there aren't merging issues with the lookup table — ex 1999—02—08
  main <- main %>% mutate(name.id = ifelse(name_short=="Mr SPEAKER", "10000", name.id),
                          electorate = ifelse(name_short=="Mr SPEAKER", NA, electorate),
                          party = as.factor(ifelse(name_short=="Mr SPEAKER", NA, as.character(party))))
  
  # this is specific to 2009—02—04 — i had a lot of issues because the name of a sub—debate (Nation Building and Jobs Plan) was named a bunch in speeches
  # so i tried to make sure everything was split in the right places, but there was one line that was separated that was actually still part of one speech body
  # it was split b/c the sub—debate was named in the middle of the speech — let's put those two back together
  if (filename=="2009—02—04.xml") {
    main <- main %>% mutate(name = ifelse(str_detect(body, "^The Nation Building and Jobs Plan is based on the reality that now is not the time for half measures\\.It is a time to be bold and to get on with it\\.It is weighted towards productive investment\\."),
                                  "Swan, Wayne, MP", 
                                  name))
  }
  
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
           party = as.factor(ifelse(party=="", NA, as.character(party))),
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
  
  # special case — his last name is capitalized and it's causing issues b/c other times it's not in all caps
  # fix for consistency
  lookup <- lookup %>% mutate(name = ifelse(name=="REITH, Peter, MP", "Reith, Peter, MP", name))
  
  # if someones name short is Mr SPEAKER but their name doesn't include SPEAKER, drop those (will cause merging issues)
  lookup <-  lookup %>% filter(!(!is.na(name_short) & str_detect(name_short, "Mr SPEAKER") & !str_detect(name, "SPEAKER")))
  lookup <-  lookup %>% filter(!(!is.na(name_short) & str_detect(name_short, "The SPEAKER") & !str_detect(name, "SPEAKER")))
  
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
           title = ifelse(displayName=="Kelly, De—Anne", "Mrs|Ms", title),
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
  
  # merge lookup with master list by surname, displayname and title — this will get us the gender and uniqueID of everyone
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
  # DEC 13 - added the name id to group by and changed filter from what's commented out below
  # doing this b/c was having issues with deputy speaker version of name being taken and non-deputy speaker version being dropped
  # but we don't want this b/c want both - by grouping by name ID, we know both will remain b/c name id for deputy speaker version is 10000
  lookup <- lookup %>% 
    group_by(last_name, first_name, displayName, name.id) %>% 
    filter(n()>1 & nchar(full_name)==max(nchar(full_name)) | n()==1) %>% 
    ungroup()
  
  # lookup <- lookup %>% 
  #   group_by(last_name, first_name, displayName, name.id) %>% 
  #   filter(n()>1 & !str_detect(full_name, "\\)") & nchar(full_name)==max(nchar(full_name)) | n()==1 | n()>1 & str_detect(full_name, "\\)")) %>% 
  #   ungroup()
  
  # 04—04—2000 Dr Wooldridge attributed two electorates (Chisholm and Casey) — this is wrong it should just be Casey — fix
  if (thisDate=="2000-04-04") {
    lookup <- lookup %>% mutate(electorate = ifelse(uniqueID=="Wooldridge1956", "Casey", electorate)) %>% 
      unique()
  }
  
  # issue on 2000—09—04 where Anderson is Acting Speaker and "Acting" is being transcribed in his party like (NPActing) — let's fix this
  lookup <- lookup %>% mutate(party = as.factor(ifelse(str_detect(party, "Acting$"), str_remove(party, "Acting$"), as.character(party))))
  
  # for now, if someone is a deputy speaker and their name is duplicated b/c the name ID, electorate and party are full in one row but empty in another, let's just keep it general
  # by this i mean make name id 1000 and electorate/party NA — this is a general approach that will remove a lot of issues as we continue to parse, otherwise the code will keep breaking on each
  # individual where this is the case
  # TO DO — come back to this and make things more complete. just doing this now b/c of time crunch
  lookup <- lookup %>% group_by(full_name) %>% mutate(name.id = ifelse(duplicated(full_name) & str_detect(full_name, "DEPUTY SPEAKER") & name.id!="10000", 10000, name.id),
                                            party = as.factor(ifelse(duplicated(full_name) & str_detect(full_name, "DEPUTY SPEAKER") & !is.na(party), NA, as.character(party))),
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
  # store nrow of main pre—merge to check on after merge
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
  
  # fill in gender, uniqueID, name.id, electorate, party
  main <- main %>% group_by(name) %>% 
    fill(c(gender, uniqueID, name.id, electorate, party), .direction = "downup") %>% 
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

  # add space between by leave—I move — for word count purposes
  # two different types of dashes
  main <- main %>% mutate(body = ifelse(str_detect(body, "by leave\\—[[:upper:]]"), str_replace(body, "(?<=by leave)—", "— "), body))
  main <- main %>% mutate(body = ifelse(str_detect(body, "by leave\\—[[:upper:]]"), str_replace(body, "(?<=by leave)—", "— "), body))
  
  # also add space between "on motion byMr Baird" type of pattern
  main <- main %>% mutate(body = ifelse(str_detect(body, "on motion byM"), str_replace(body, "on motion by(?=M)", "on motion by "), body))
  
  # clean up body of debate text (removing name, title in brackets, and time stamp from body)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{1,2}\\.[:digit:]{2}[[:space:]]{0,1}[[:alpha:]]\\.[[:alpha:]]\\.\\)—")
  
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
  
  # 2002—09—26 speaker title put at the beginning of body needed to be extracted, and put into the name variable b/c name was missing
  # also grab time stamp
  main <- main %>% mutate(name = ifelse(is.na(name) & str_detect(body, "^The SPEAKER[[:space:]]{0,2}\\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                str_extract(body, "^The SPEAKER(?=[[:space:]]{0,2}\\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—)"),
                                name),
                          time.stamp = ifelse(str_detect(body, "^The SPEAKER[[:space:]]{0,2}\\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                              str_extract(body, "(?<=^The SPEAKER[[:space:]]{0,2}\\()\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.(?=\\)—)"),
                                              time.stamp),
                  body = ifelse(str_detect(body, "^The SPEAKER[[:space:]]{0,2}\\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                str_remove(body, "^The SPEAKER[[:space:]]{0,2}\\(\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.\\)—"),
                                body))
  
  # let's extract the time stamp from statements the state the current time, like debate adjourned at ___ or sitting suspended from ___
  # let's use these only if the time stamp isn't already there
  main <- main %>% mutate(time.stamp = ifelse(str_detect(body, "adjourned at \\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.$|^Sitting suspended from \\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.(?= to)|^Order\\! It being \\d{1,2}.\\d{2} [[:lower:]]\\.m\\., |took the chair at \\d{1,2}.\\d{2} [[:lower:]]\\.m\\.$|took the chair at \\d{1,2}.\\d{2} [[:lower:]]m\\.$") & is.na(time.stamp),
                                      str_extract(body, "(?<=adjourned at )\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.$|(?<=^Sitting suspended from )\\d{1,2}\\.\\d{2} [[:lower:]]\\.m\\.(?= to)|(?<=^Order\\! It being )\\d{1,2}.\\d{2} [[:lower:]]\\.m\\.(?=, )|(?<=took the chair at )\\d{1,2}.\\d{2} [[:lower:]]\\.m\\.$|(?<=took the chair at )\\d{1,2}.\\d{2} [[:lower:]]m\\.$"),
                                      time.stamp),
                          time.stamp = ifelse(str_detect(body, "adjourned at \\d{1,2}\\.\\d{2} [[:lower:]]m$|^Sitting suspended from \\d{1,2}\\.\\d{2} [[:lower:]]m(?= to)|^Order\\! It being \\d{1,2}.\\d{2} [[:lower:]]m, |took the chair at \\d{1,2}.\\d{2} [[:lower:]]m$") & is.na(time.stamp),
                                              str_extract(body, "(?<=adjourned at )\\d{1,2}\\.\\d{2} [[:lower:]]m$|(?<=^Sitting suspended from )\\d{1,2}\\.\\d{2} [[:lower:]]m(?= to)|(?<=^Order\\! It being )\\d{1,2}.\\d{2} [[:lower:]]m(?=, )|(?<=took the chair at )\\d{1,2}.\\d{2} [[:lower:]]m$"),
                                              time.stamp),
                  time.stamp = str_replace_all(time.stamp, "(?<=\\d)\\.", "\\:"))
  
  
  # need to convert the am/pm time to 24 hour time, for consistency
  main <- main %>% mutate(time.stamp = str_replace_all(time.stamp, "(?<=\\d)\\.", "\\:"),
                       time.stamp = ifelse(str_detect(time.stamp, "^\\d\\d[[:space:]][[:alpha:]]"), paste0(time.stamp), time.stamp)) %>% 
    mutate(time.stamp = ifelse(str_detect(time.stamp, "a\\.m\\."), str_replace(time.stamp, "a.m.", "AM"),time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "p\\.m\\."), str_replace(time.stamp, "p.m.", "PM"),time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "am\\."), str_replace(time.stamp, "am.", "AM"),time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "pm\\."), str_replace(time.stamp, "pm.", "PM"),time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "am"), str_replace(time.stamp, "am", "AM"),time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "pm"), str_replace(time.stamp, "pm", "PM"),time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "(?<=\\d\\d)(?=[[:alpha:]]{2})"), str_replace(time.stamp, "(?<=\\d\\d)(?=[[:alpha:]]{2})", " "), time.stamp)) %>% 
    mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d{1,2}\\:\\d{2} [[:upper:]]M"), format(strptime(time.stamp, "%I:%M %p"), format="%H:%M:%S"), time.stamp),
           time.stamp = ifelse(str_detect(time.stamp, "^\\d{1,2}[[:space:]][[:alpha:]]"), format(strptime(time.stamp, "%I %p"), format="%H:%M:%S"), time.stamp))
  
  # let's finally clean up any names that precede the body of text. for example, "Mr BRENDAN OCONNER-" before he speaks
  # first, grab all unique name forms to search for
  name_forms <- lookup %>% select(name) %>% unique()
  
  # make them all into title case form just to capture extra variation we might have missed
  name_forms <- name_forms %>% mutate(name = str_to_title(name)) %>% unique() %>% rbind(., name_forms) %>% unique() %>% pull()
  
  # sometimes deputy speakers names are in parentheses, add these in the right form
  #### TO DO - PASTE INTO NAME???
  name_forms <- lookup %>% filter(str_detect(name_use, "DEPUTY")) %>% select(name) %>%
    mutate(name = paste0("(", name, ")")) %>% 
    unique() %>% 
    pull() %>% 
    c(., name_forms)
  
  # escape all characters
  name_forms <- name_forms %>% as_tibble() %>% mutate(value = str_replace_all(value, "\\.", "\\\\."),
                                        value = str_replace_all(value, "\\)", "\\\\)"),
                                        value = str_replace_all(value, "\\(", "\\\\(")) %>% 
    pull(value)
  
  # now, str_remove with lookahead for emdash
  main$body <- str_remove(main$body, paste0("^", name_forms, "—", "(?=[[:upper:]])", collapse = "|"))
  
  # if party is "N/A" make it NA
  main <- main %>% mutate(party = as.factor(ifelse(party=="N/A", NA, as.character(party))))
  
  # if in.gov or first.speech is "" or " ", make it NA
  main <- main %>% mutate(in.gov = ifelse(in.gov=="" | in.gov==" ", NA, in.gov),
                          first.speech = ifelse(first.speech=="" | first.speech==" ", NA, first.speech))
  
  # export CSV
  write.csv(main, paste0("/Volumes/Verbatim/output/main-2000-2011/", str_remove(filename, ".xml"), "-main.csv"), row.names = FALSE)
  
}

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/input/")

# grab a couple years
files_get <- files_all %>%
  as_tibble() %>%
  filter(str_detect(value, "^2000-")) %>% 
  filter(value <="2000-04-13.xml") %>% 
  pull(value)

#"^2000-|^2001-|^2002-|^2003-|^2004-|^2005-|^2006-|^2007-|^2008-|^2009-|^2010-|^2011-"


for(i in 1:length(files_get)){
  parse_hansard(files_get[i])
}
 
dates_now <- c("2005-06-23.xml", "2007-05-09.xml", "2007-09-19.xml", "2008-11-24.xml", "2008-12-01.xml", "2009-02-04.xml", "2010-03-10.xml", "2010-10-28.xml", "2011-03-21.xml")
dates_now <- dates_now %>% separate_rows(value, sep=", ") %>% pull()

for(i in 1:length(dates_now)){
  parse_hansard(dates_now[i])
}
