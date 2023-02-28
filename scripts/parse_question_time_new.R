# trying method for pre 2011 on recent stuff, to deal with q/a issues
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

filename<-"2012-03-20.xml"

hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))
xml_df <- read_xml(here("/Volumes/Verbatim/input/", filename))

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
                                                    body) else NULL}) 

  
  ### checkpoint ###
  # make sure there are no lines flagged as both a question and an answer and that the first line is a question
  stopifnot(nrow(q_wo_notice %>% filter(question==answer))==0,
            q_wo_notice$question[1]==1)
  
  # add final flags
  q_wo_notice <- q_wo_notice %>% mutate(sub1_flag = 1, sub2_flag = 0, fedchamb_flag = 0)
  
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
  stopifnot(nrow(q_wo_notice %>% filter(question==answer))==0,
            q_wo_notice$question[1]==1)
  
  # add final flags
  q_in_writing <- q_in_writing %>% mutate(sub1_flag = 1, sub2_flag = 0, fedchamb_flag = 0)
  
} else {
  q_in_writing <- tibble()
}

