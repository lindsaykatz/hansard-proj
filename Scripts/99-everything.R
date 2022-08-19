# script to parse everything
library(XML)
library(here)
library(tidyverse)
library(AustralianPoliticians)

# walk fxn to generalize
# parse XML
# rename scripts folder to lower case

hansard_xml <- xmlParse(here("XML_files", "2021_11_30.xml"))

# define interjection words
interject_words <- c("Order!", "Order.", "interjecting", "Interjecting", "interjected", "Interjected", "interjections", "interjection", "Interjections", 
                     "Interjection", "interject", "Interject", "The time for the discussion has concluded", "I thank the honourable member for", "I thank the member for", 
                     "should withdraw that remark", "In accordance with standing order 193 the time for constituency statements has concluded", 
                     "There being no further grievances, the debate is adjourned",
                     "the time for members' statements has concluded", "The original question was that this bill be now read a second time", "Is the amendment seconded?",
                     "Do you want to seek the call again?", "The question is that the amendment be disagreed to", "The question now is that the bill be agreed to", 
                     "The question is that the amendments be agreed to.", "will resume his seat", "will resume her seat", "will resume their seat", 
                     "The question is that the bill be now read a second time", "Is leave granted to continue the debate?", 
                     "The question is that the bill now be read a second time", "The question is that the amendment moved by the member for ",
                     "—The SPEAKER", "Mr Speaker, a point of order on relevance", "An opposition member:", "—The DEPUTY SPEAKER", "— (Time expired)The SPEAKER", 
                     "— (Time expired)The DEPUTY SPEAKER", "A government member:", "There being no further speakers, the debate has concluded.",
                     ".The DEPUTY SPEAKER") 

######### SESSION INFORMATION #########
# store session info in tibble, correct variable class
session_info <- xmlToDataFrame(node=getNodeSet(hansard_xml, "//session.header")) %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date),
         parliament.no = as.numeric(parliament.no),
         session.no = as.numeric(session.no),
         period.no = as.numeric(period.no),
         page.no = as.numeric(page.no),
         proof = as.numeric(proof))

######### BUSINESS START #########
#### CHAMBER ####
# store business start in tibble, add flag for federation chamber
bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start")), 
                          fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
# Store business start in tibble, add flag for federation chamber
bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")), 
                        fedchamb_flag = 1)

# merge into single business start tibble
bus_start <- rbind(bus_start_chamb, bus_start_fed)

######### DEBATE INFORMATION #########
#### CHAMBER ####
# store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and federation chamber
debate_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0,
         sub1_flag = 0,
         sub2_flag = 0)

#### FEDERATION CHAMBER ####
# store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and federation chamber
debate_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/debate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 1,
         sub1_flag = 0,
         sub2_flag = 0)

# merge chamber & federation chamber tibbles into single debate information tibble
debate_info <- rbind(debate_info_chamb, debate_info_fed)

######### SUB-DEBATE 1 #########
#### CHAMBER ####
# store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
sub1_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.text"))) %>%
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

# store sub-debate 1 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
sub1_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.start/talker")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
         party = as.factor(party),
         fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
# store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
sub1_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebateinfo")),
                       xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.text"))) %>%
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 1)

# store sub-debate 1 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
sub1_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/talk.start/talker")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
         party = as.factor(party),
         fedchamb_flag = 1)

# merge chamber and federation tibbles together, flag for which sub-debate, arrange by page
sub1_info <- rbind(sub1_info_chamb, sub1_info_fed) %>% 
  arrange(page.no) %>% 
  mutate(sub1_flag = 1,
         sub2_flag = 0)

# merge chamber and federation tibbles together, flag for interjections, question and answer, and which sub-debate, arrange by page and time
sub1_speech <- rbind(sub1_speech_chamb, sub1_speech_fed) %>% 
  arrange(page.no, time.stamp) %>% 
  mutate(sub1_flag = 1,
         sub2_flag = 0,
         question = 0,
         answer = 0,
         has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))

######### SUB-DEBATE 2 #########
#### CHAMBER ####
# store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

# store sub-debate 2 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"),
         party = as.factor(party),
         fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
# store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
sub2_info_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                       xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})

# store sub-debate 2 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
sub2_speech_fed <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                         xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
         party = {if("party" %in% names(.)) as.factor(party) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})

# merge chamber and federation tibbles together, flag for which sub-debate, arrange by page
sub2_info <- rbind(sub2_info_chamb, sub2_info_fed) %>% 
  arrange(page.no) %>% 
  mutate(sub1_flag = 0,
         sub2_flag = 1)

# merge chamber and federation tibbles together, flag for interjections, question and answer, and which sub-debate, arrange by page and time
sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_fed) %>% 
  arrange(page.no, time.stamp) %>% 
  mutate(sub1_flag = 0,
         sub2_flag = 1,
         question = 0,
         answer = 0,
         has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))

######### SPEECH INTERJECTIONS #########
#### CHAMBER ####
# store sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_chamb_sub1 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

# store sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_chamb_sub2 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 0)

#### FEDERATION CHAMBER ####
# store sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_fed_sub1 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         fedchamb_flag = 1)

# store sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
interject_fed_sub2 <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
         fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})

# merge chamber and federation chamber tibbles, add flags for question, answer, and each sub-debate, arrange by page number
interject_sub1 <- rbind(interject_chamb_sub1, interject_fed_sub1) %>% arrange(page.no) %>% mutate(question = 0, answer = 0, sub1_flag = 1, sub2_flag = 0)
interject_sub2 <- rbind(interject_chamb_sub2, interject_fed_sub2) %>% arrange(page.no) %>% mutate(question = 0, answer = 0, sub1_flag = 0, sub2_flag = 1)

######### QUESTIONS AND ANSWERS #########
# store questions in tibble, correct variable class, add flag for question/answer, extract time
sub1_q <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker")),
            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         question = 1,
         answer = 0,
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"))

# store answers in tibble, correct variable class, add flag for question/answer, extract time
sub1_a <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.start/talker")),
            xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         question = 0,
         answer = 1,
         time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d"))

# merge questions and answers, add flag for sub-debate 1 and 2, and federation chamber (always 0 b/c only have question time in chamber), flag for interjections
sub1_q_a <- rbind(sub1_q, sub1_a) %>% 
  arrange(time.stamp) %>% 
  mutate(sub1_flag = 1, 
         sub2_flag = 0,
         fedchamb_flag = 0,
         has_interject = ifelse(str_detect(body, paste(interject_words, collapse="|"))==TRUE, 1, 0))

######### QUESTION AND ANSWER INTERJECTIONS #########
# store question interjections in tibble, correct variable class, add flag for whether question/answer
sub1_q_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.start/talker")),
                      xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         question = 1,
         answer = 0)

# store answer interjections in tibble, correct variable class, add flag for whether question/answer
sub1_a_interject <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.start/talker")),
                      xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/interjection/talk.text"))) %>% 
  as_tibble() %>% 
  mutate(page.no = as.numeric(page.no),
         party = as.factor(party),
         question = 0,
         answer = 1)

# merge question and answer interjections, add flag for which sub-debate (sub debate 1) and federation chamber (always 0 b/c only have question time in chamber)
sub1_q_a_interject <- rbind(sub1_q_interject, sub1_a_interject) %>% 
  arrange(page.no) %>% 
  mutate(sub1_flag=1, 
         sub2_flag=0,
         fedchamb_flag = 0)

######### PUTTING EVERYTHING TOGETHER #########
# all info from contents section of PDF
toc <- debate_info %>% 
  select(-type) %>% 
  rbind(., sub1_info, sub2_info) %>% 
  arrange(page.no)

# all debate text
main <- rbind(sub1_speech, sub2_speech, sub1_q_a) %>% 
  arrange(page.no, time.stamp)

# all interjection information
interjections <- rbind(interject_sub1, interject_sub2, sub1_q_a_interject) %>% 
  arrange(page.no)

######### CONSISTENCY CHECKS #########
# check that all parties belong to list of parties recognized by parliament
parties <- AustralianPoliticians::get_auspol("all")

party_names <- c("Australian Greens", "Australian Labor Party", "Centre Alliance", 
                 "Independent", "Katter's Australia Party", "Liberal National Party", 
                 "Liberal Party of Australia", "The Nationals")

party_abbr <- c("AG", "ALP", "CA", "IND", "KAP", "LNP", "LP", "NATS", "UAP")

main %>% filter(!(party %in% party_abbr)) # noticed Craig Kelly is labelled to be in UAP party in XML, but IND in PDF
# also inconsistency in the period number and parliament number in session_info compared to PDF (46th parliament, 8th period)

interjections %>% filter(!(party %in% c(party_abbr, "")))

# check that nothing has been coded as both sub-debate 1 and sub-debate 2
main %>% filter(sub1_flag == 1 & sub2_flag == 1) %>% nrow() == 0
interjections %>% filter(sub1_flag == 1 & sub2_flag == 1) %>% nrow() == 0

# check that nothing has been coded as both a question and an answer
main %>% filter(question == 1 & answer == 1) %>% nrow() == 0
interjections %>% filter(question == 1 & answer == 1) %>% nrow() == 0

# check that there are equal number of questions and answers (speeches)
main %>% filter(question==0 & answer==1) %>% summarise(n()) %>% pull == 
  main %>% filter(question==1 & answer==0) %>% summarise(n()) %>% pull

# check that all federation chamber rows come after chamber rows (based on page no.)
# last pg of chamber should less than or equal to first page of fed. chamber
main %>% filter(fedchamb_flag==0) %>% summarise(max(page.no)) %>% pull <= 
  main %>% filter(fedchamb_flag==1) %>% summarise(min(page.no)) %>% pull

interjections %>% filter(fedchamb_flag==0) %>% summarise(max(page.no)) %>% pull <= 
  interjections %>% filter(fedchamb_flag==1) %>% summarise(min(page.no)) %>% pull
