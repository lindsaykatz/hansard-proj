# changing my approach to parsing so we don't have to merge stuff the way i originally had it
# motivation - maintaining correct order
library(here)
library(tidyverse)
library(xml2)
library(XML)
library(hms)

# for parsing question time
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

# define file name to parse
filename<-"2011-05-11.xml"

# grab "all" dataset from AusPol package
all <- AustralianPoliticians::get_auspol('all')

hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))

# need to read the XML in in a different way for this so we can use the item_df function defined in global environment
xml_df <- read_xml(here("/Volumes/Verbatim/input/", filename))

#################### CHAMBER ####################
######### BUSINESS START #########
# store business start in tibble, add flag for federation chamber, extract date, body, and start time
# in rare cases (ex. 2016-08-30, there is no business start, so add if-else in case of this)
if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start"))))>0) {
  bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start")), 
                            fedchamb_flag = 0)
  
  # need if-else if here because before 2011-05-10, the business start was different
  if ("body" %in% names(bus_start_chamb)) {
    bus_start_chamb <- bus_start_chamb %>% 
      mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
             date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
             body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{4}(?=.{1,})"),
             start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]")) %>% 
      mutate(body = ifelse(str_detect(body, "SPEAKER\\("), str_replace(body, "(?<=SPEAKER)\\(", " ("), body))
  } else if ("para" %in% names(bus_start_chamb)) {
    bus_start_chamb <- bus_start_chamb %>% 
      rename(date = day.start,
             body = para) %>% 
      mutate(date = as.Date(date),
             day_of_week = strftime(date, "%A"),
             start_time = str_extract(body, "[:digit:]{1,2}[:space:][:lower:][:lower:]")) %>% 
      select(-separator) %>% 
      mutate(body = ifelse(str_detect(body, "SPEAKER\\("), str_replace(body, "(?<=SPEAKER)\\(", " ("), body))
  }
  
} else {
  bus_start_chamb <- tibble()
}

######### DEBATE INFORMATION #########
# store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and federation chamber
debate_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0,
         sub1_flag = 0,
         sub2_flag = 0)

######### SUB-DEBATE 1 #########
# store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
sub1_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.text"))) %>%
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})

######### SUB-DEBATE 2 #########
# include if-else statements throughout code in case sub-debate 2 does not exist
# store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})

# sometimes sub-debate 2 isn't nested in sub-debate 1 (most recent date where this is the case is 2021-10-21)
# if the number of rows of this is > 0, let's bind that onto what we already have to ensure we aren't missing anything
if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebate.text"))))) > 0) {
  
  sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/subdebate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
    rbind(., sub2_info_chamb)
}

# grab all debate text, correct var classes, extract time stamps
all_text_chamb <- left_join(item_df(xml_df, "chamber.xscript//debate//speech/talk.start/talker") %>% unnest(everything()),
                         item_df(xml_df, "chamber.xscript//debate//speech/talk.text") %>% unnest(everything()),
                         by = "itemindex") %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
         party = {if("party" %in% names(.)) as.factor(party) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
  mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})

############# all the stuff below is me just trying to get back which are from subdebate 1 or 2 nodes, to add those flags in
# but it's not a main priority - ppl likely won't even use those? idk

# now we want to grab the text individually from sub1, sub2, and regular debate nodes, to add our binary flags
# debate_text <- item_df(xml_df, "//chamber.xscript/debate/speech/talk.text") %>% 
#   select(body) %>% 
#   unnest(body) %>% 
#   mutate(sub1_flag = 0, sub2_flag = 0)
# sub1_text <- item_df(xml_df, "//chamber.xscript/debate/subdebate.1/speech/talk.text") %>% 
#   select(body) %>% 
#   unnest(body) %>% 
#   mutate(sub1_flag = 1, sub2_flag = 0)
# sub2_text <- item_df(xml_df, "//chamber.xscript/debate//subdebate.2/speech/talk.text") %>% 
#   select(body) %>% 
#   unnest(body) %>% 
#   mutate(sub1_flag = 0, sub2_flag = 1)
# 
# all_text_matches <- bind_rows(debate_text, sub1_text, sub2_text)
# 
# stopifnot(nrow(all_text_matches) == nrow(all_text))
# 
# all_text <- left_join(all_text, all_text_matches, by = "body") %>% distinct() %>% 
#   mutate(body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
#                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
#                        body),
#          body = ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
#                        str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
#                        body))
# 
# stopifnot(nrow(all_text_matches) == nrow(all_text))

#############

######### SPEECH INTERJECTIONS #########
# store sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
# use if-else statements in case there are no interjections
sub1_interject_chamb <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})

# store sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
# use if-else statements in case there are no interjections and/or there is no sub-debate 2
sub2_interject_chamb <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})

######### QUESTIONS AND ANSWERS #########

# could actually get all debate text with question time text in order too this way
# but might make it tricky to flag qs and as, plus already have the code to do it with exceptions accounted for
# but this maintains order really nicely and avoids us having to arrange with rows that don't have a time tamp available
left_join(item_df(xml_df, "chamber.xscript//debate//speech/talk.start/talker | chamber.xscript//question/talk.start/talker | chamber.xscript//answer/talk.start/talker") %>% unnest(everything()),
          item_df(xml_df, "chamber.xscript//debate//speech/talk.text | chamber.xscript//question/talk.text | chamber.xscript//answer/talk.text") %>% unnest(everything()),
          by = "itemindex")

################### QUESTIONS WITHOUT NOTICE ###################
if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker"))) > 0) {
  
  # grab all q and a content
  q_wo_notice_content <- item_df(xml_df, "chamber.xscript//question | chamber.xscript//answer") %>% unnest(c(talk.text, talk.start)) %>% select(itemindex, talk.text)
  
  # grab all q and a info to merge with content
  q_wo_notice_info <- item_df(xml_df, "chamber.xscript//question/talk.start/talker | chamber.xscript//answer/talk.start/talker") %>% unnest(everything())
  
  # left join by item index - questions without notice
  q_wo_notice <- left_join(q_wo_notice_info, q_wo_notice_content, by="itemindex")
  
  # grab question text to flag for
  q_wo_notice_text <- item_df(xml_df, "chamber.xscript//question") %>% unnest(talk.text) %>% pull(talk.text)
  
  # grab answer text to flag for
  a_wo_notice_text <- item_df(xml_df, "chamber.xscript//answer") %>% unnest(talk.text) %>% pull(talk.text)
  
  # flag for questions and answers using text grabbed above, rename talk.text for consistency, grab time stamps and fix spacing issue in body
  q_wo_notice <- q_wo_notice %>% mutate(question = ifelse(talk.text %in% q_wo_notice_text, 1, 0),
                                        answer = ifelse(talk.text %in% a_wo_notice_text, 1, 0),
                                        q_in_writing = 0) %>% 
    rename(body = talk.text) %>% 
    mutate(page.no = as.numeric(page.no),
           party = as.factor(party),
           time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
    mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp)) %>% 
    mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                    str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                    body) else NULL},
           body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                    str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                    body) else NULL}) %>% 
    # adding feb 9 based on error caught in 2019-10-22
    mutate(question = ifelse(str_detect(body, "My question is addressed to") & question==0, 1, question),
           answer = ifelse(str_detect(body, "My question is addressed to") & answer==1, 0, answer))
  
  ### checkpoint ###
  # make sure there are no lines flagged as both a question and an answer
  stopifnot(nrow(q_wo_notice %>% filter(question==answer))==0)
  
  # specific cases where the first question/answer node found is an answer that is actually just part of a speech and shouldn't be flagged as an answer
  #### TO DO - ASK ROHAN IF I SHOULD BE CHANGING THESE OR JUST ADD A NOTE TO THE DOCUMENTATION THAT SAYS BEWARE Q/A MAY BE FLAGGED WRONG B/C OF TRANSCRIPTION ERRORS
  if (filename=="2020-10-19.xml" | filename=="2014-05-26.xml" | filename=="2012-08-15.xml" | filename=="2013-06-26.xml" | filename=="") {
    
    # manually fix incorrectly flagged answer
    q_wo_notice <- q_wo_notice %>% mutate(answer = ifelse(itemindex==1, 0, answer))
    
    ### checkpoint ###
    # make sure that the second line is a question
    stopifnot(q_wo_notice$question[2]==1)
    
  } else {
    ### checkpoint ###
    # make sure that the first line is a question
    stopifnot(q_wo_notice$question[1]==1)
  }
  
  
  # add final flags
  q_wo_notice <- q_wo_notice %>% mutate(sub1_flag = 1, sub2_flag = 0, fedchamb_flag = 0) %>% select(-itemindex)
  
} else {
  q_wo_notice <- tibble()
}


################### QUESTIONS IN WRITING ###################
if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.start/talker")))>0) {
  
  # grab all q in writing content
  q_in_writing_content <- item_df(xml_df, "answers.to.questions//question | answers.to.questions//answer") %>% unnest(c(talk.text, talk.start)) %>% select(itemindex, talk.text)
  
  # grab all q in writing info to merge with content
  q_in_writing_info <- item_df(xml_df, "answers.to.questions//question/talk.start/talker | answers.to.questions//answer/talk.start/talker") %>% unnest(everything())
  
  # left join by item index - question in writing
  q_in_writing <- left_join(q_in_writing_info, q_in_writing_content, by="itemindex")
  
  # grab question text to flag for
  q_in_writing_text <- item_df(xml_df, "answers.to.questions//question") %>% unnest(talk.text) %>% pull(talk.text)
  
  # grab answer text to flag for
  a_in_writing_text <- item_df(xml_df, "answers.to.questions//answer") %>% unnest(talk.text) %>% pull(talk.text)
  
  # flag for questions and answers using text grabbed above, rename talk.text for consistency, grab time stamps and fix spacing issue in body
  q_in_writing <- q_in_writing %>% mutate(question = ifelse(talk.text %in% q_in_writing_text, 1, 0),
                                          answer = ifelse(talk.text %in% a_in_writing_text, 1, 0),
                                          q_in_writing = 1) %>% 
    rename(body = talk.text) %>% 
    mutate(page.no = as.numeric(page.no),
           party = as.factor(party),
           time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
    mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp)) %>% 
    mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                    str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                    body) else NULL},
           body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                    str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                    body) else NULL}) %>% 
    #### KEEP AN EYE ON THIS FLAGGING METHOD
    mutate(question = ifelse(str_detect(body, "The answer to the .{1,30} question is as follows\\:|has provided the following answer to the honourable member's question\\:") & question==1, 0, question),
           answer = ifelse(str_detect(body, "The answer to the .{1,30} question is as follows\\:|has provided the following answer to the honourable member's question\\:") & answer==0, 1, answer)) %>% 
    mutate(question = ifelse(str_detect(body, "asked the Minister for .{1,300}, in writing,") & question==0, 1, question),
           answer = ifelse(str_detect(body, "asked the Minister for .{1,300}, in writing,") & answer==1, 0, answer))
  
  ### checkpoint ###
  # make sure there are no lines flagged as both a question and an answer and that the first line is a question
  stopifnot(nrow(q_in_writing %>% filter(question==answer))==0,
            q_in_writing$question[1]==1)
  
  # add final flags
  q_in_writing <- q_in_writing %>% mutate(sub1_flag = 1, sub2_flag = 0, fedchamb_flag = 0) %>% select(-itemindex)
  
} else {
  q_in_writing <- tibble()
}

######### QUESTION AND ANSWER INTERJECTIONS #########
if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker"))) > 0) {
  
  # store question interjections in tibble, correct variable class, add flag for whether question/answer
  # use if-else statements in case there are no interjections
  sub1_q_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           question = {if("page.no" %in% names(.)) 1 else NULL},
           answer = {if("page.no" %in% names(.)) 0 else NULL})
  
  # store answer interjections in tibble, correct variable class, add flag for whether question/answer
  # use if-else statements in case there are no interjections
  sub1_a_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           question = {if("page.no" %in% names(.)) 0 else NULL},
           answer = {if("page.no" %in% names(.)) 1 else NULL})
  
  # if there are question and/or answer interjections, merge, arrange and add flags
  if(nrow(sub1_q_interject) > 0 | nrow(sub1_a_interject) > 0) {
    sub1_q_a_interject <- rbind(sub1_q_interject, sub1_a_interject) %>% 
      arrange(page.no) %>% 
      mutate(sub1_flag = 1, 
             sub2_flag = 0,
             fedchamb_flag = 0)
  } else {
    # else just bind, resulting in empty tibble
    sub1_q_a_interject <- rbind(sub1_q_interject, sub1_a_interject)
  }  
} else {
  sub1_q_a_interject <- tibble()
}  

#################### FEDERATION CHAMBER ####################
# use if-else statement to ensure code works for Hansard with and without federation chamber
# check that there is a business start to know if federation chamber exists

if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/business.start")))) > 0) {
  
  ######### BUSINESS START #########
  # store business start in tibble, add flag for federation chamber, extract date and start time
  # in rare cases (ex. 2016-08-30, there is no business start, so add if-else in case of this)
  bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/business.start")), 
                          fedchamb_flag = 1)
  
  if ("body" %in% names(bus_start_fed)){
    bus_start_fed <- bus_start_fed %>% 
      mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
             date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
             body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"),
             start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]")) %>% 
      mutate(body = ifelse(str_detect(body, "SPEAKER\\("), str_replace(body, "(?<=SPEAKER)\\(", " ("), body))
  } else if ("para" %in% names(bus_start_fed)) {
    bus_start_fed <- bus_start_fed %>% 
      rename(date = day.start,
             body = para) %>% 
      mutate(date = as.Date(date),
             day_of_week = strftime(date, "%A"),
             start_time = str_extract(body, "[:digit:]{1,2}[:space:][:lower:][:lower:]")) %>% 
      mutate(body = ifelse(str_detect(body, "SPEAKER\\("), str_replace(body, "(?<=SPEAKER)\\(", " ("), body))
  }
  
  # merge into single business start tibble
  bus_start <- rbind(bus_start_chamb, bus_start_fed)
  
  ######### DEBATE INFORMATION #########
  # store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and federation chamber
  debate_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/debateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/debate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = as.numeric(page.no),
           fedchamb_flag = 1,
           sub1_flag = 0,
           sub2_flag = 0)
  
  # merge chamber & federation chamber tibbles into single debate information tibble
  debate_info <- rbind(debate_info_chamb, debate_info_fed)
  
  ######### SUB-DEBATE 1 #########
  # store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
  sub1_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.text"))) %>%
    as_tibble() %>% 
    mutate(page.no = as.numeric(page.no),
           fedchamb_flag = 1)
  
  # merge chamber and federation tibbles together, flag for which sub-debate, arrange by fedchamb flag, and page
  sub1_info <- rbind(sub1_info_chamb, sub1_info_fed) %>% 
    arrange(fedchamb_flag, page.no) %>% 
    mutate(sub1_flag = 1,
           sub2_flag = 0)
  
  ######### SUB-DEBATE 2 #########
  # if-else statements are included because sub-debate 2 is not always present in federation chamber
  # store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
  sub2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
  
  # same idea as for chamber, nesting changes, want to account for this so we don't miss anything
  if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebate.text"))))) > 0) {
    
    sub2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
      rbind(., sub2_info_fed)
  }
  
  # if sub-debate 2 exists for chamber and/or federation chamber, merge and process
  if (nrow(sub2_info_chamb > 0) | nrow(sub2_info_fed > 0)) {
    # merge chamber and federation tibbles together, flag for which sub-debate, arrange by page
    sub2_info <- rbind(sub2_info_chamb, sub2_info_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 0,
             sub2_flag = 1)
    
  } else {
    # else just bind, resulting in empty tibbles
    sub2_info <- rbind(sub2_info_chamb, sub2_info_fed)
  }
  
  # now grab all federation chamber text
  # grab all debate text, correct var classes, extract time stamps
  all_text_fed <- left_join(item_df(xml_df, "maincomm.xscript//debate//speech/talk.start/talker") %>% unnest(everything()),
                              item_df(xml_df, "maincomm.xscript//debate//speech/talk.text") %>% unnest(everything()),
                              by = "itemindex") %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
    mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
  
  ######### SPEECH INTERJECTIONS #########
  # store sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
  # use if-else statements in case there are no interjections
  sub1_interject_fed <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
  
  # store sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
  # use if-else statements in case there are no interjections, or there is no sub-debate 2
  sub2_interject_fed <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
  
  # if else statement used in case of no interjections and/or sub-debate 2
  # merge chamber and federation chamber tibbles, add flags for question, answer, and each sub-debate, arrange by page number
  if (nrow(sub1_interject_chamb) > 0 | nrow(sub1_interject_fed) > 0) {
    interject_sub1 <- rbind(sub1_interject_chamb, sub1_interject_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(question = 0, answer = 0, sub1_flag = 1, sub2_flag = 0)
  } else {
    interject_sub1 <- rbind(sub1_interject_chamb, sub1_interject_fed)
  }
  
  # same thing for sub-debate 2 interjections
  if (nrow(sub2_interject_chamb) > 0 | nrow(sub2_interject_fed) > 0) {
    interject_sub2 <- rbind(sub2_interject_chamb, sub2_interject_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(question = 0, answer = 0, sub1_flag = 0, sub2_flag = 1)
  } else {
    interject_sub2 <- rbind(sub2_interject_chamb, sub2_interject_fed)
  }
  
} else {
  # modify all code for case when there is no federation chamber
  # re-name chamber business start and debate info
  bus_start <- bus_start_chamb
  debate_info <- debate_info_chamb

  # use if statement in case no sub-debate 1 in chamber (ex: 2014-07-08)
  if (nrow(sub1_info_chamb)>0) {
    # rename sub-debate 1 chamber info, arrange by page number, add flags for which sub-debate
    sub1_info <- sub1_info_chamb %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0)

  } else {
    # else rename and return empty tibbles
    sub1_info <- sub1_info_chamb
  }
  
  # use if statement in case no sub-debate 2 in chamber
  if (nrow(sub2_info_chamb > 0)) {
    # rename sub-debate 2 chamber info, arrange by page number, add flags for which sub-debate
    sub2_info <- sub2_info_chamb %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 0,
             sub2_flag = 1)
    
  } else {
    # else rename and return empty tibbles
    sub2_info <- sub2_info_chamb
  }
  
  # use if statement in case no interjections in sub-debate 1 in chamber
  # rename chamber interjections, add flags for question, answer, and each sub-debate, arrange by page number
  if (nrow(sub1_interject_chamb) > 0) {
    interject_sub1 <- sub1_interject_chamb %>% arrange(fedchamb_flag, page.no) %>% mutate(question = 0, answer = 0, sub1_flag = 1, sub2_flag = 0)
  } else {
    interject_sub1 <- sub1_interject_chamb
  }
  
  # use if statement in case no interjections in sub-debate 2 in chamber
  # rename chamber interjections, add flags for question, answer, and each sub-debate, arrange by page number
  if (nrow(sub2_interject_chamb) > 0) {
    interject_sub2 <- sub2_interject_chamb %>% arrange(fedchamb_flag, page.no) %>% mutate(question = 0, answer = 0, sub1_flag = 0, sub2_flag = 1)
  } else {
    interject_sub2 <- sub2_interject_chamb
  }
}

######### PREPARING BUSINESS START TO BE ADDED TO MAIN DATA FRAME #########
if (nrow(bus_start) > 0) {
  bus_start <- bus_start %>%
    rename(time.stamp = start_time) %>%
    mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d\\:"), paste0("0", time.stamp), time.stamp),
           body = str_replace(body, "\\n[[:space:]]{1,20}", " ")) %>% 
    select(c(body, time.stamp, fedchamb_flag)) %>%
    mutate(name = "business start",
           page.no = NA,
           name.id = NA,
           party = NA,
           electorate = NA,
           in.gov = "",
           first.speech = "",
           sub1_flag = 0,
           sub2_flag = 0,
           question = 0,
           answer = 0,
           speech_no = NA,
           q_in_writing = 0) %>%
    select(c(speech_no, page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag, question, answer, q_in_writing))
}

######### PUTTING EVERYTHING TOGETHER #########
# table of contents
# before doing this, sometimes sub1_info has an extra variable called "id.no" (2012-09-11) and to merge these all need to have it, so want to add if-else in case
if ("id.no" %in% names(sub1_info)) {
  debate_info <- debate_info %>% mutate("id.no" = NA)
  sub2_info <- sub2_info %>% mutate("id.no" = NA)
}

# all content information, add numeric ordering
toc <- debate_info %>% 
  select(-type) %>% 
  rbind(., sub1_info, sub2_info) %>% 
  arrange(fedchamb_flag, page.no) %>% 
  rowid_to_column("order")

# all interjection information
interject <- rbind(interject_sub1, interject_sub2, sub1_q_a_interject)

# in case there are no interjections in this data frame
if (nrow(interject)>0) {
  interject <- interject %>% 
    arrange(fedchamb_flag, page.no) %>% 
    rowid_to_column("order")
}

# drop extra vars, add q in writing flag
main <- bind_rows(all_text_chamb, q_wo_notice, all_text_fed) %>% arrange(itemindex, fedchamb_flag, page.no, time.stamp) %>% 
  select(-itemindex, -span) %>% 
  mutate(q_in_writing = 0)

########################### EXTRA CODE / OTHER ATTEMPTS THAT DIDN'T WORK
# before we put everything together, let's grab the skeleton structure where time stamps and names are available, so we can make sure ordering is correct
# this is specifically to deal with repeated time stamps
# add spacing fixes that we did earlier so we can match correctly
# escape all special characters for matching
# speech_order <- item_df(xml_df, "chamber.xscript//talk.text/body/p[1] | maincomm.xscript//talk.text/body/p[1]") %>% 
#   unnest(span) %>% 
#   mutate(span = ifelse(str_detect(span, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
#                        str_replace_all(span, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
#                        span),
#          span = ifelse(str_detect(span, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
#                        str_replace_all(span, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
#                        span)) %>% 
#   mutate(span = str_replace_all(span, "\\(", "\\\\("),
#          span = str_replace_all(span, "\\)", "\\\\)"),
#          span = str_replace_all(span, "\\.", "\\\\."),
#          span = str_replace_all(span, "\\$", "\\\\$"),
#          span = str_replace_all(span, "\\[", "\\\\["),
#          span = str_replace_all(span, "\\{", "\\\\{"),
#          span = str_replace_all(span, "\\?", "\\\\?"),
#          span = str_replace_all(span, "\\*", "\\\\*"),
#          span = str_replace_all(span, "\\+", "\\\\+"))

# getting more text after to make the matches more specific in case of duplicate beginnings (like Ms GILLARD (Lalor—Prime Minister) (14:01):  I move:)
# speech_order <- item_df(xml_df, "chamber.xscript//talk.text/body | maincomm.xscript//talk.text/body") %>% unnest(p) %>% 
#   group_by(itemindex) %>% slice(1:2) %>% 
#   summarise(p = paste0(p, collapse = "[[:space:]]{0,2}")) %>% 
#   rename(span = p) %>% 
#   mutate(span = ifelse(str_detect(span, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
#                        str_replace_all(span, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
#                        span),
#          span = ifelse(str_detect(span, "[[:lower:]]{2,5} \\d{4}\\.[[:upper:]][[:lower:]]"), # this one is for something like "November 2007.Of course," being spaced right
#                        str_replace_all(span, "(?<=[[:lower:]]{2,5} \\d{4})\\.(?=[[:upper:]][[:lower:]])", ". "),
#                        span),
#          span = ifelse(str_detect(span, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
#                        str_replace_all(span, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
#                        span),
#          span = ifelse(str_detect(span, "[[:lower:]][[:lower:]] \\d{4}-\\d{2}\\:[[:upper:]]"),
#                        str_replace_all(span, "(?<=[[:lower:]][[:lower:]] \\d{4}-\\d{2})\\:(?=[[:upper:]])", ": "),
#                        span)) %>% 
#   mutate(span = str_replace_all(span, "\\(", "\\\\("),
#          span = str_replace_all(span, "\\)", "\\\\)"),
#          span = str_replace_all(span, "\\.", "\\\\."),
#          span = str_replace_all(span, "\\$", "\\\\$"),
#          span = str_replace_all(span, "\\?", "\\\\?"),
#          span = str_replace_all(span, "\\*", "\\\\*"),
#          span = str_replace_all(span, "\\+", "\\\\+"))

# xmlToDataFrame(node=getNodeSet(hansard_xml, "//span[@class='HPS-Time']"))

# make sure the speech order tibble has the same n rows as everything we're about to bind
#stopifnot(nrow(speech_order) == nrow(bind_rows(debate_speech, sub1_speech, sub2_speech, q_wo_notice)))

# all debate text
# arrange was causing issues with q in writing ordering b/c generally no time stamp so added in q/a in writing after
# main <- bind_rows(debate_speech, sub1_speech, sub2_speech, q_wo_notice) %>% 
#   arrange(fedchamb_flag, page.no, time.stamp) %>% 
#   mutate(q_in_writing = 0) %>% 
#   arrange(page.no)

# new way of merging, using fuzzy left join and arranging by itemindex from the speech order df
# this is my new way of dealing with ordering concerns, which arise largely due to repeated time stamps
# main <- bind_rows(debate_speech, sub1_speech, sub2_speech, q_wo_notice) %>% 
#   fuzzyjoin::fuzzy_left_join(., speech_order, by=c("body" = "span"), match_fun = str_detect) %>% 
#   arrange(itemindex) %>% 
#   distinct()

# to do - stuck on duplicated span (Dr MIKE KELLY: from 2011-05-11)