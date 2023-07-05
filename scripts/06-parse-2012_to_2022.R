# 2012 to 2022 new script, developed march 21, 2023
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

# grab "all" dataset from AusPol package
all <- AustralianPoliticians::get_auspol('all')

split_interjections_fedchamb <- function(main, interject, filename){
  
  #### step 1: add speech_no to make it easier to see where interjections happened 
  main <- rowid_to_column(main, "speech_no")
  
  # keep original/unmodified version of main to grab names/name.id/electorate/party info from later on
  main_orig <- main
  
  #### step 2: we need a list of names or phrases to separate speech based on ("interject_names")
  
  # start by re-formatting those with title of speaker or deputy speaker
  # paste in a few forms to be safe (w/ and w/out first name), add unique() in case of repeats
  if (nrow(interject)>0){
    interject_names <- interject %>%
      select(name) %>%
      filter(str_detect(name, "\\(The")) %>%
      unique() %>%
      mutate(name = str_remove(name, "[[:punct:]][[:space:]]MP|[[:space:]]MP"),
             title = str_extract(name, "The.{0,10}[:space:][:alpha:]{0,10}"),
             last_name = str_extract(name, "^[:alpha:]{0,10}-[:alpha:]{0,35}|^[:alpha:]{0,10}'[:alpha:]{0,35}|^[:alpha:]{0,35}|^[:alpha:]{0,35}[:space:][:alpha:]{0,35}"),
             first_name = str_extract(name, ",[:space:][:alpha:]{0,35}"),
             first_name = str_replace_all(first_name, "[[:punct:]][[:space:]]", "")) %>%
      reframe(form1 = paste0(title, "[[:space:]]\\((Dr|Mr|Mrs|Ms)[[:space:]]", first_name, "[[:space:]]", last_name, "\\)"),
                form2 = paste0(title, "[[:space:]]\\((Dr|Mr|Mrs|Ms)[[:space:]]", last_name, "\\)"),
                form3 = paste0(title, "\n                  ", "\\((Dr|Mr|Mrs|Ms)[[:space:]]", first_name, "[[:space:]]", last_name, "\\)"),
                form4 = paste0(title, "\n                  ", "\\((Dr|Mr|Mrs|Ms)[[:space:]]", last_name, "\\)"),
                form5 = paste0(title, "[[:space:]]\\((Dr|Mr|Mrs|Ms)[[:space:]]", str_sub(first_name, end=1), "[[:space:]]", last_name, "\\)"),
                form6 = paste0(title, "\n                  ", "\\((Dr|Mr|Mrs|Ms)[[:space:]]", str_sub(first_name, end=1), "[[:space:]]", last_name, "\\)")) %>%
      pivot_longer(form1:form6, names_to = "type", values_to = "form") %>%
      select(form) %>% 
      unique() %>% 
      pull()
    
    # sometimes the names of all deputy speakers aren't captured in main or interject, so let's also manually grab these to avoid missing any
    interject_names <- str_extract_all(main$body, paste0("The DEPUTY SPEAKER \\((Dr|Mr|Mrs|Ms|Hon.)[[:space:]].{0,50}\\)(?=\\:)")) %>% 
      na.omit() %>% 
      unlist() %>% 
      unique() %>% 
      as_tibble() %>% 
      mutate(value = ifelse(str_detect(value, "\\("), str_replace(value, "\\(", "\\\\("), value),
             value = ifelse(str_detect(value, "\\)"), str_replace(value, "\\)", "\\\\)"), value)) %>% 
      pull() %>% c(., interject_names)
    
    # next add the ones in form like "Ms Ryan interjecting-"
    interject_names <- interject %>% 
      select(name) %>% 
      unique() %>% 
      filter(str_detect(name, "interject")) %>% 
      pull() %>%
      c(., interject_names)
    
  } else {
    interject_names <- c()
  }
  
  # finally, add some other phrases that aren't generally captured as interjections in the XML (but where we'd like to split)
  # feb 9 - adding emdash after the clerk to fix issue on 2015-09-08 row 3
  interject_names <- c(interject_names, "Opposition members interjecting—", "Government members interjecting—", 
                       "Opposition members", "Honourable members interjecting—", "An opposition member", 
                       "An honourable member", "The SPEAKER", "The DEPUTY SPEAKER", "A government member interjecting—",
                       "The Clerk—", "The Clerk:", "A government member", "Government members", "Honourable members", "Honourable members and senators",
                       "Honourable member and senators", "The ACTING SPEAKER")
  
  #### step 3: we need another list of names that we haven't grabbed yet to separate speech based on ("all_names")
  
  # start list of all names from interjections
  # accounting for people with two or three first names, sometimes only the first is used
  # accounting for people with punctuation in last name (e.g. Chandler-Mather) or two words in last name (e.g. van Mansen)
  # accounting for people with last name like McInthosh (special capitalization)
  if (nrow(interject)>0){
    all_names <- interject %>%
      select(name) %>%
      filter(!str_detect(name, "interject|The")) %>%
      unique() %>% 
      mutate(name = str_remove(name, "[[:punct:]][[:space:]]MP|[[:space:]]MP"),
             first_name = str_extract(name, "[:alpha:]{0,35}$|[:alpha:]{0,35}[:space:][:alpha:]{0,35}$|[:alpha:]{0,35}[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}$"),
             first_name = str_replace_all(first_name, "^[:blank:]", ""),
             last_name = str_extract(name, "^[:alpha:]{0,35},|^[:alpha:]{0,35}[:space:][:alpha:]{0,35},|^[:alpha:]{0,35}[:punct:][:alpha:]{0,35},"),
             last_name = str_replace_all(last_name, "[:punct:]$", ""),
             mult_first = ifelse(str_detect(first_name, "[[:space:]]"), str_extract(first_name, "^[[:alpha:]]{0,35}"), NA)) %>% 
      reframe(form1 = paste0(first_name, "[[:space:]]", last_name),
                form2 = last_name,
                form3 = ifelse(str_detect(last_name, "^[[:upper:]][[:lower:]][[:upper:]]"), 
                               paste0(str_to_upper(first_name), "[[:space:]]", str_extract(last_name, "^.{2}"),
                                      str_to_upper(str_remove(last_name, "^.{2}"))),
                               paste0(str_to_upper(first_name), "[[:space:]]", str_to_upper(last_name))),
                form4 = ifelse(str_detect(last_name, "^[[:upper:]][[:lower:]][[:upper:]]"), 
                               paste0(str_extract(last_name, "^.{2}"), str_to_upper(str_remove(last_name, "^.{2}"))), 
                               paste0(str_to_upper(last_name))),
                form5 = paste0(first_name, "[[:space:]]", str_to_upper(last_name)),
                form6 = ifelse(is.na(mult_first), NA, paste0(mult_first, "[[:space:]]", last_name)),
                form7 = ifelse(is.na(mult_first), NA, paste0(str_to_upper(mult_first), "[[:space:]]", str_to_upper(last_name))),
                form8 = ifelse(is.na(mult_first), NA, paste0(mult_first, "[[:space:]]", str_to_upper(last_name)))) %>% 
      mutate(form3 = as.character(form3),
             form4 = as.character(form4),
             form6 = as.character(form6),
             form7 = as.character(form7),
             form8 = as.character(form8)) %>% 
      pivot_longer(form1:form8, names_to = "type", values_to = "form") %>% 
      na.omit() %>% 
      unique() %>% 
      pull(form)
    
  } else {
    all_names <- c()
  }
  
  
  # now, add to all_names list using names of speakers from main data frame
  all_names <-  main_orig %>% 
    select(name) %>% 
    unique() %>% 
    mutate(name = str_remove(name, "[:space:]MP.{0,35}$"),
           name = ifelse(str_detect(name, ",$"), str_replace_all(name, ",$", ""), name),
           first_name = str_extract(name, ",[:space:][:alpha:]{0,35}$|,[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}$|,[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}$|,[:space:][:alpha:]{0,35}(?= \\()"),
           first_name = str_replace_all(first_name, "^,[:space:]", ""),
           last_name = str_extract(name, "^[:alpha:]{0,35},|^[:alpha:]{0,35}[:space:][:alpha:]{0,35},|^[:alpha:]{0,35}[:punct:][:alpha:]{0,35},"),
           last_name = str_replace_all(last_name, "[:punct:]$", ""),
           mult_first = ifelse(str_detect(first_name, "[[:space:]]"), str_extract(first_name, "^[[:alpha:]]{0,35}"), NA)) %>% 
    reframe(form1 = paste0(first_name, "[[:space:]]", last_name),
              form2 = last_name,
              form3 = ifelse(str_detect(last_name, "^[[:upper:]][[:lower:]][[:upper:]]"), 
                             paste0(str_to_upper(first_name), "[[:space:]]", str_extract(last_name, "^.{2}"), 
                                    str_to_upper(str_remove(last_name, "^.{2}"))), 
                             paste0(str_to_upper(first_name), "[[:space:]]", str_to_upper(last_name))),
              form4 = ifelse(str_detect(last_name, "^[[:upper:]][[:lower:]][[:upper:]]"), 
                             paste0(str_extract(last_name, "^.{2}"), str_to_upper(str_remove(last_name, "^.{2}"))), 
                             paste0(str_to_upper(last_name))),
              form5 = ifelse(str_detect(last_name, "^[[:upper:]][[:lower:]][[:upper:]]"), 
                             paste0(first_name, "[[:space:]]", str_extract(last_name, "^.{2}"), 
                                    str_to_upper(str_remove(last_name, "^.{2}"))), 
                             paste0(first_name, "[[:space:]]", str_to_upper(last_name))),
              form6 = ifelse(is.na(mult_first), NA, paste0(mult_first, "[[:space:]]", last_name)),
              form7 = ifelse(is.na(mult_first), NA, paste0(str_to_upper(mult_first), "[[:space:]]", str_to_upper(last_name))),
              form8 = ifelse(is.na(mult_first), NA, paste0(mult_first, "[[:space:]]", str_to_upper(last_name)))) %>% 
    pivot_longer(form1:form8, names_to = "type", values_to = "form") %>% 
    na.omit() %>% 
    pull(form) %>% 
    c(., all_names) %>% 
    unique()
  
  #### step 4: extract all matches of the names in actual body, paste in possible prefixes, we will use these to separate rows next
  names_use <- str_extract_all(main$body, paste0("(?<!, )(Dr|Mr|Mrs|Ms)[[:space:]]", all_names, "(?=[[:punct:]]|[[:blank:]])", collapse = "|")) %>% 
    na.omit() %>% 
    unlist() %>% 
    unique()
  
  #### step 5: split rows
  
  # need to separate out general interjections (e.g. Opposition members, A government member, etc.) because these sometimes are followed by "on my left" or "having stood in their places"
  # and don't want to split on those so need specific negative lookaheads
  # can't paste all interject_names b/c regex too long error
  interject_general <- as_tibble(interject_names) %>% filter(!str_detect(interject_names, "SPEAKER")) %>% pull
  
  # grab all other interjection names that aren't in general
  interject_specific <- as_tibble(interject_names) %>% filter(!(value %in% interject_general)) %>% pull
  
  # now separate rows
  main <- separate_rows(main, body, sep=paste0("(?=", interject_general, ")(?!", interject_general, " on my)(?!", interject_general, " having)(?!", interject_general, " standing)(?!", interject_general, " will )(?!", interject_general, " should )(?!", interject_general, "'s )(?!", interject_general, ", )(?!", interject_general, " can)(?!", interject_general, " would)(?!", interject_general, " may)(?!", interject_general, " received)",
                                               collapse="|"))
  
  # march 15 - added "took the chair" specification to avoid separation in business start, and added the house divided negative preceded by
  # conditional added to avoid regex too long error
  if (length(interject_specific)<=50) {
    main <- separate_rows(main, body, sep=paste0("(?=", interject_specific, ")(?!", interject_specific, "[[:space:]]\\(.{1,30}\\)\\n[[:space:]]{11,22}\\()(?!", 
                                                 interject_specific, "[[:space:]]\\(.{5,35}\\)[[:space:]]{1,2}took the chair at)(?!", interject_specific, "[[:space:]]{1,2}took the chair at)(?<!The House divided\\. \\[\\d{2}:\\d{2}\\]\\()", 
                                                 collapse="|"))
  } else {
    main <- separate_rows(main, body, sep=paste0("(?=", interject_specific[1:50], ")(?!", interject_specific[1:50], "[[:space:]]\\(.{1,30}\\)\\n[[:space:]]{11,22}\\()(?!", 
                                                 interject_specific[1:50], "[[:space:]]\\(.{5,35}\\)[[:space:]]{1,2}took the chair at)(?!", interject_specific[1:50], "[[:space:]]{1,2}took the chair at)(?<!The House divided\\. \\[\\d{2}:\\d{2}\\]\\()", 
                                                 collapse="|"))
    
    main <- separate_rows(main, body, sep=paste0("(?=", interject_specific[51:length(interject_specific)], ")(?!", interject_specific[51:length(interject_specific)], "[[:space:]]\\(.{1,30}\\)\\n[[:space:]]{11,22}\\()(?!", 
                                                 interject_specific[51:length(interject_specific)], "[[:space:]]\\(.{5,35}\\)[[:space:]]{1,2}took the chair at)(?!", interject_specific[51:length(interject_specific)], "[[:space:]]{1,2}took the chair at)(?<!The House divided\\. \\[\\d{2}:\\d{2}\\]\\()", 
                                                 collapse="|"))
    
  }
  
  # remove any rows that are just "" in the body, this is the result of separating rows and happens sometimes when there's whitespace at the end of the body
  main <- main %>% filter(!str_detect(body, "^$"))
  # separate rows using full names, make sure they aren't preceded by "SPEAKER (" to avoid splitting deputy speaker/speaker titles from name
  # case where there is punctuation right before name (often case with interjections)
  if(length(names_use)>0){
    main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    main <- separate_rows(main, body, sep=paste0("(?<=\\d\\d\\:\\d\\d)(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    main <- separate_rows(main, body, sep=paste0("(?<=\\d\\d\\:\\d\\d[[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    
    # case when full name is followed by interjecting, but this wasn't captured in the "interject" data frame
    if (!("interjecting" %in% interject$name)) {
      main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, " interjecting—)", collapse = "|"))
      main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, " interjecting—)", collapse = "|"))
    }
    
    # same as above but case when preceded by a bunch of spaces (this one is specific will need to see if number of spaces is consistent in other transcripts)
    main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]]{20})(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)",  collapse = "|"))
    
    # same as above but case when preceded by single space (this one is specific will need to see if number of spaces is consistent in other transcripts)
    main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]]|[[:punct:]][[:space:]][[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)",  collapse = "|"))
  }
  
  # if body starts with a colon, remove that
  main <- main %>% mutate(body = ifelse(str_detect(body, "^\\:"), str_remove(body, "^\\:"), body))
  
  if(length(names_use)>0){
    # deal with stuff like "Mr Sukkar interjecting—Ms RISHWORTH" not splitting b/c of em dash (not captured in [:punct:] character class)
    # change em dash to regular dash for easier processing later on
    main <- main %>% mutate(body = str_replace_all(body, "\u2014", "-")) %>% separate_rows(body, sep=paste0("(?<=interjecting[[:punct:]])(?=", names_use,")", collapse = "|"))
    
    # case where there is punctuation and a single space after interjection, followed by name from names_use
    main <- separate_rows(main, body, sep=paste0("(?<=interjecting[[:punct:]][[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    
    # special case where interjecting- is followed by new line and a bunch of spaces
    main <- separate_rows(main, body, sep=paste0("(?<=interjecting[[:punct:]]\\n                  )(?=", names_use,")", collapse = "|"))
    
    # special case where interjecting- is followed by some spaces
    main <- separate_rows(main, body, sep=paste0("(?<=interjecting[[:punct:]]  )(?=", names_use,")", collapse = "|"))
    
    # special case where someone is interupted with a new line and bunch of whitespace after, before name of interjector is written
    main <- separate_rows(main, body, sep=paste0("(?<=-\\n                  )(?=", names_use,")", collapse = "|"))
    
    # another special case similar to above
    main <- separate_rows(main, body, sep=paste0("(?<=.\\n                  )(?=", names_use,")", collapse = "|"))
    
    # some splits need to be made at places like -Mr Dutton: which aren't being captured already (b/c em dash hadn't been changed yet)
    main <- separate_rows(main, body, sep=paste0("(?<=\\-)(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    
    # same as above but with space after dash
    main <- separate_rows(main, body, sep=paste0("(?<=\\-[[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    
    # split for places like -Ms Flint interjecting-
    main <- separate_rows(main, body, sep=paste0("(?<=\\-)(?<!SPEAKER[[:space:]]\\()(?=", names_use, " interjecting-)", collapse = "|"))
    
    # some splits need to be made at places like JacksonMr JOHN COBB: where name is preceeded by statement ending in lower case letter
    main <- separate_rows(main, body, sep=paste0("(?<!\\:|[[:space:]])(?<=[[:lower:]])(?=", names_use, "\\:)", collapse="|"))
    
    # same as above but with space
    main <- separate_rows(main, body, sep=paste0("(?<!\\:)(?<=[[:lower:]][[:space:]]|[[:lower:]][[:space:]][[:space:]])(?=", names_use, "\\:)", collapse="|"))
    
  }
  
  # note for this and one below - not extracting timestamp because this will cause issues later on with ordering since rows below it from same speech_no will have old time which is less than time extracted
  # might be something to fix in future, but for now leaving as-is
  # unique to 2014-07-08 where Prime Minister of Japan spoke
  if (any(str_detect(main$body, "His Excellency Mr SHINZO ABE \\(Prime Minister of Japan\\)"))){
    main <- separate_rows(main, body, sep=paste0("(?=His Excellency Mr SHINZO ABE \\(Prime Minister of Japan\\)[[:space:]]\\(\\d\\d:\\d\\d\\))")) %>% 
      mutate(name = ifelse(str_detect(body, "His Excellency Mr SHINZO ABE \\(Prime Minister of Japan\\)"), str_extract(body, "His Excellency Mr SHINZO ABE \\(Prime Minister of Japan\\)"), name),
             name.id = ifelse(name== "His Excellency Mr SHINZO ABE (Prime Minister of Japan)", NA, name.id),
             party = as.factor(ifelse(name== "His Excellency Mr SHINZO ABE (Prime Minister of Japan)", NA, as.character(party))),
             electorate = ifelse(name== "His Excellency Mr SHINZO ABE (Prime Minister of Japan)", NA, electorate))
  }
  
  # unique to 2011-06-20 where retired Prime Minister of New Zealand spoke
  if (any(str_detect(main$body, "Rt Hon\\. JOHN KEY \\(Prime Minister of New Zealand\\)"))){
    main <- separate_rows(main, body, sep=paste0("(?=Rt Hon\\. JOHN KEY \\(Prime Minister of New Zealand\\)[[:space:]]\\(\\d\\d:\\d\\d\\))")) %>% 
      mutate(name = ifelse(str_detect(body, "Rt Hon\\. JOHN KEY \\(Prime Minister of New Zealand\\)"), str_extract(body, "Rt Hon\\. JOHN KEY \\(Prime Minister of New Zealand\\)"), name),
             name.id = ifelse(name== "Rt Hon. JOHN KEY (Prime Minister of New Zealand)", NA, name.id),
             party = as.factor(ifelse(name== "Rt Hon. JOHN KEY (Prime Minister of New Zealand)", NA, as.character(party))),
             electorate = ifelse(name== "Rt Hon. JOHN KEY (Prime Minister of New Zealand)", NA, electorate))
  }
  
  
  # also need to change the em dash in the interject names list before flagging interjections below
  interject_names <- interject_names %>% 
    as_tibble() %>% 
    mutate(value = str_replace_all(value, "\u2014", "-")) %>% 
    pull()
  
  # last split on other names that didn't get split on but should have - due to the fact that they didn't exist in main orig or interject dfs
  # don't want "Mr Speaker" (likely being referenced in another members statement and don't want to split on that)
  other_names <- str_extract_all(main$body, paste0(c("(?<!, )(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,35}(?=\\:)",
                                                     "(?<!, )(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\'[[:alpha:]]{0,35}(?=\\:)",
                                                     "(?<!, )(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\-[[:alpha:]]{0,35}(?=\\:)",
                                                     "(?<!, )(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}[[:space:]][[:upper:]][[:alpha:]]{1,10}(?=\\:)"), collapse="|")) %>%
    na.omit() %>%
    unlist() %>%
    unique() %>%
    as_tibble() %>%
    filter(!(value %in% names_use)) %>%
    filter(!str_detect(value, paste0(c("Speaker", "SPEAKER"), collapse="|"))) %>%
    pull()
  
  ########### added to this oct 25, keep an eye on it if causes issues
  # add to this list of other names with others who interjected but weren't captured in main orig or interject dfs
  # filtering out "The Speaker" and any names we already have in interject_general or names_use
  # have the extra filter here for interject general here b/c some names are followed by "interjecting" in that list, and we can check those here
  # since using lookahead of "interjecting", and don't want to double split (cause issues)
  other_names2 <- str_extract_all(main$body, paste0(c("(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,35}(?= interjecting)",
                                                      "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\'[[:alpha:]]{0,35}(?= interjecting)",
                                                      "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}[[:space:]][[:alpha:]]{1,10}\\'[[:alpha:]]{0,35}(?= interjecting)",
                                                      "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\-[[:alpha:]]{0,35}(?= interjecting)",
                                                      "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}[[:space:]][[:alpha:]]{1,10}\\-[[:alpha:]]{0,35}(?= interjecting)",
                                                      "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}[[:space:]][[:upper:]][[:alpha:]]{1,10}(?= interjecting)"), collapse="|")) %>%
    na.omit() %>%
    unlist() %>%
    unique() %>%
    as_tibble() %>%
    filter(!(value %in% names_use)) %>%
    filter(!str_detect(value, paste0(c("Speaker", "SPEAKER"), collapse="|")))
  
  # only filter if the length of other names 2 is greater than 0, then add that filtered list onto original other names list
  if (nrow(other_names2)>0) {
    other_names <- other_names2 %>% 
      filter(!str_detect(paste0(interject_general, collapse="|"), paste0(value, collapse = "|"))) %>% 
      pull() %>% c(., other_names)
  }
  
  # special case issue - 2012-09-10 - two names that are accidentally split on because an external dialogue is being relayed by an MP
  # cannot specify generalizable lookaround to filter this out, so need to remove manually
  if (filename=="2012-09-10.xml"){
    other_names <- NULL
  }
  
  # split on these other names
  if (length(other_names)>0){
    main <- separate_rows(main, body, sep=paste0("(?<!\\:|[[:alpha:]][[:space:]])(?=", other_names, "\\:|", other_names, " interjecting-)", collapse="|"))
  }
  
  #### step 6: clean up names & split on adjournment statements
  if (length(other_names)>0){
    # if body starts with interjection name, clear name, name.id, electorate, and party (else leave as is)
    # adding q_in_writing==0 because those generally start with the name of the person but are also fully filled in so we don't want to change those
    main <- main %>% mutate(name = ifelse(q_in_writing==0 & name!="business start" & !str_detect(body, paste0("^", c(interject_names, names_use, other_names), " would", collapse = "|")) & grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, name),
                            name.id = ifelse(q_in_writing==0 & name!="business start" & !str_detect(body, paste0("^", c(interject_names, names_use, other_names), " would", collapse = "|")) & grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, name.id),
                            electorate = ifelse(q_in_writing==0 & name!="business start" & !str_detect(body, paste0("^", c(interject_names, names_use, other_names), " would", collapse = "|")) & grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, electorate),
                            party = as.factor(ifelse(q_in_writing==0 & name!="business start" & !str_detect(body, paste0("^", c(interject_names, names_use, other_names), " would", collapse = "|")) & grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, as.character(party))))
    
    # extract who is interjecting and paste that into name column
    # clean up body by removing name of person interjecting from it as well as whitespace/punctuation at beginning (only keeping those where body is "___ interjecting-")
    main <- main %>% mutate(name = ifelse(is.na(name) & !str_detect(body, "took the chair\\.|took the chair at"), 
                                          str_extract(main$body, paste0("^", c(names_use, other_names, interject_names), collapse = "|")), 
                                          name),
                            body = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", names_use, collapse = "|")), 
                                          str_remove(body, paste0("^", names_use, "[[:punct:]]", collapse = "|")), 
                                          body),
                            body = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), "(?![[:space:]]having)", collapse = "|"))
                                          & !str_detect(body, "took the chair\\."), 
                                          str_remove(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), "(?![[:space:]]interjecting)", collapse = "|")),  
                                          body),
                            body = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", other_names, collapse = "|")), 
                                          str_remove(body, paste0("^", other_names, "\\:", collapse = "|")), 
                                          body),
                            body = str_replace_all(body, "^[[:blank:]]{0,5}", ""),
                            body = str_replace_all(body, "^[[:punct:]][[:blank:]]{1,5}", ""))
  } else {
    
    # if body starts with interjection name, clear name, name.id, electorate, and party (else leave as is)
    main <- main %>% mutate(name = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, name),
                            name.id = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, name.id),
                            electorate = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, electorate),
                            party = as.factor(ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, as.character(party))))
    
    # extract who is interjecting and paste that into name column
    # clean up body by removing name of person interjecting from it as well as whitespace/punctuation at beginning (only keeping those where body is "___ interjecting-")
    main <- main %>% mutate(name = ifelse(is.na(name), str_extract(main$body, paste0("^", c(interject_names, names_use), collapse = "|")), name),
                            body = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", names_use, "(?![[:space:]]{1,2}asked the|[[:space:]]interjecting-)", collapse = "|")), 
                                          str_remove(body, paste0("^", names_use, collapse = "|")), 
                                          body),
                            body = ifelse(q_in_writing==0 & name!="business start" & str_detect(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), collapse = "|")), 
                                          str_remove(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), "(?![[:space:]]interjecting)", collapse = "|")),  
                                          body),
                            body = str_replace_all(body, "^[[:blank:]]{0,5}", ""),
                            body = str_replace_all(body, "^[[:punct:]][[:blank:]]{1,5}", ""))
  }
  
  # now, to clean these up a bit
  # if row is separated at "The SPEAKER", paste the full name of the speaker in name column
  # there is only one speaker so this should always work since using na.omit and unique
  main <- main %>% mutate(name = ifelse(str_detect(main$name, "^The SPEAKER$") & str_detect(main$name, ".{0,60}\\(The SPEAKER\\)$"), unique(na.omit(str_extract(main$name, ".{0,60}\\(The SPEAKER\\)$"))), name))
  
  # if name and title have a bunch of spaces and/or a newline between them, fix it up
  # ex: The DEPUTY SPEAKER                   (Mr Llew O'Brien):
  main <- main %>% mutate(name = ifelse(str_detect(main$name, "(?<=SPEAKER)\n                  "), str_replace(name, "\n                  ", " "), name))
  
  # need to split adjournment statements onto own lines
  # some have just a punctuation before
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]]|[[:punct:]][[:space:]])(?=Debate adjourned)"))
  main <- separate_rows(main, body, sep = "(?<=\\n                    )(?=Debate adjourned)")
  
  # some have space and punctuation before
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=Debate adjourned)"))
  
  # split house adjournment statement
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=House adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=House adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:digit:]][[:digit:]]\\:[[:digit:]][[:digit:]])(?=House adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=House adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=House adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=House adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=House adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:digit:]][[:digit:]]\\:[[:digit:]][[:digit:]])(?=House adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=House adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=House adjourned \\d\\d\\:\\d\\d)"))
  
  # split fed. chamb adjournment statement
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:digit:]][[:digit:]]\\:[[:digit:]][[:digit:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=Federation Chamber adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:digit:]][[:digit:]]\\:[[:digit:]][[:digit:]])(?=Federation Chamber adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Federation Chamber adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=Federation adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=Federation adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:digit:]][[:digit:]]\\:[[:digit:]][[:digit:]])(?=Federation adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=Federation adjourned \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=Federation adjourned \\d\\d\\:\\d\\d)"))
  
  # split stage notes
  main <- separate_rows(main, body, sep=paste0("(?<=\\-)(?=Debate interrupted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Debate interrupted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\. )(?=Bill, as amended, agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\d\\d\\:\\d\\d)(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Question unresolved\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Question put\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=The House divided\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Question unresolved\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Message from the .{0,100}[[:space:]]announced\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Ordered that this bill be reported to the House without amendment\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Message from the .{0,100}[[:space:]]announced\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\?|\\?[[:space:]])(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\?|\\?[[:space:]])(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\?[[:space:]])(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\?|\\?[[:space:]])(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]]|\\-|\\-[[:space:]])(?=A division having been called in the House of Representatives\\-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\n                  )(?=A division having been called in the House of Representatives\\-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Proposed expenditure agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=The member for [[:alpha:]]{0,50} then left the chamber\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=The member for [[:alpha:]]{1,25}[[:space:]][[:alpha:]]{1,25} then left the chamber\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Honourable members having stood in their places-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]]|\\.[[:space:]][[:space:]])(?=Honourable members standing in their places-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]])(?=\\n                  Honourable\\n                  members\\n                  having\\n                  stood\\n                  in\\n                  their\\n                  places-)")) %>% 
    mutate(body=str_replace(body, "\\n                  Honourable\\n                  members\\n                  having\\n                  stood\\n                  in\\n                  their\\n                  places-",
                            "Honourable members having stood in their places-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]])(?=\\n                  Honourable members having stood in their places-)")) %>% 
    mutate(body=str_replace(body,"\\n                  Honourable members having stood in their places-", "Honourable members having stood in their places-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]])(?=\\n                    Honourable members having stood in their places-)")) %>% 
    mutate(body=str_replace(body,"\\n                    Honourable members having stood in their places-", "Honourable members having stood in their places-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]])(?=\\n                  The Speaker having seated himself in the chair-)")) %>% 
    mutate(body=str_replace(body,"\\n                  The Speaker having seated himself in the chair-", "The Speaker having seated himself in the chair-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]])(?=\\n                  The bells having been rung and a ballot having been taken-)")) %>% 
    mutate(body=str_replace(body,"\\n                  The bells having been rung and a ballot having been taken-", "The bells having been rung and a ballot having been taken-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\-|\\.|\\.[[:space]])(?=\\n                    A division having been called and the bells having been rung-)")) %>% 
    mutate(body=str_replace(body,"\\n                    A division having been called and the bells having been rung-", "A division having been called and the bells having been rung-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]]|\\-)(?=\\n                  Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d)")) %>% 
    mutate(body=str_remove(body,"^\\n                  "))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space]])(?=\\n                  The member for [[:alpha:]]{0,50} then left the chamber\\.)")) %>% 
    mutate(body=str_remove(body,"\\n                  "))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=More than the number of members required by the standing orders having risen in their places-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=The Speaker having seated himself in the chair-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=The Speaker having seated herself in the chair-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Sitting suspended \\d\\d:\\d\\d to \\d\\d:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]]|[[:punct:]] )(?=Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]]|[[:punct:]] )(?=Sitting suspended \\d\\d:\\d\\d to \\d\\d:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\!|\\.)(?=Members and senators rising and applauding,.{1,50}left the chamber\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\! |\\. )(?=Members and senators rising and applauding,.{1,50}left the chamber\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Remainder of bill-by leave-taken as a whole and agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.|\\.[[:space:]])(?=Ordered that this bill be reported to the House without amendment\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\d\\d\\:\\d\\d)(?=\\(Quorum formed\\))"))
  
  main <- separate_rows(main, body, sep=paste0("(?=The House having been counted and a quorum being present-)"))
  
  # separate more stage notes that were found in validation test 3 (After time expired)
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Proposed expenditure agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Expenditure agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=\\(Quorum formed\\))"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\n                  )(?=A quorum having been called in the House of Representatives\\-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Report made a parliamentary paper in accordance with standing order \\d{1,2}\\([[:alpha:]]\\))"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=A division having been called in the House of Representatives-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=A division having been called in the House-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Bill, as amended, agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=An incident having occurred in the gallery-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=In accordance with standing order \\d{1,2}\\([[:alpha:]]\\) the report was made a parliamentary paper\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\)\\.  |\\n                  )(?=\\(Quorum formed\\))"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Debate interrupted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\) )(?=Debate interrupted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Bill read a first time\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\(Time expired\\))(?=Proceedings suspended from \\d\\d\\:\\d\\d to \\d\\d\\:\\d\\d)"))
  
  # add space after "I move:" statements for tidiness - often no space between colon and statement
  main <- main %>% mutate(body = str_replace_all(body, "I move\\:(?=[[:alpha:]])", "I move\\: "))
  
  # same for "I propose the motion:" statements
  main <- main %>% mutate(body = str_replace_all(body, "I propose the motion\\:(?=[[:alpha:]])", "I propose the motion\\: "))
  
  # if body starts with a colon, remove that
  main <- main %>% mutate(body = ifelse(str_detect(body, "^\\:"), str_remove(body, "^\\:"), body))
  
  # if  body starts with whitespace, remove that
  main <- main %>% mutate(body = str_trim(body, side="both"))
  
  stage_notes <- c("Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.",
                   "Debate interrupted\\.",
                   "Message from the .{0,100}[[:space:]]announced\\.", 
                   "Question agreed to\\.",
                   "Question unresolved\\.",
                   "Debate adjourned",
                   "The House divided\\.",
                   "The House having been counted and a quorum being present-",
                   "Question put\\.",
                   "House adjourned at \\d\\d\\:\\d\\d",
                   "Federation Chamber adjourned at \\d\\d\\:\\d\\d",
                   "House adjourned \\d\\d\\:\\d\\d",
                   "Federation Chamber adjourned \\d\\d\\:\\d\\d",
                   "Leave not granted\\.",
                   "Leave granted\\.",
                   "A division having been called in the House of Representatives\\-",
                   "Honourable members having stood in their places-",
                   "Honourable members standing in their places-",
                   "The Speaker having seated himself in the chair-",
                   "The bells having been rung and a ballot having been taken-",
                   "A division having been called and the bells having been rung-",
                   "Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d",
                   "Sitting suspended \\d\\d:\\d\\d to \\d\\d:\\d\\d",
                   "Proccedings suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d",
                   "The member for [[:alpha:]]{0,50} then left the chamber.",
                   "More than the number of members required by the standing orders having risen in their places-",
                   "Members and senators rising and applauding,.{1,50}left the chamber\\.",
                   "Proposed expenditure agreed to\\.",
                   "Expenditure agreed to\\.",
                   "\\(Quorum formed\\)",
                   "A quorum having been called in the House of Representatives\\-",
                   "Ordered that this bill be reported to the House without amendment\\.",
                   "Report made a parliamentary paper in accordance with standing order \\d{1,2}\\([[:alpha:]]\\)",
                   "A division having been called in the House of Representatives-",
                   "A division having been called in the House-",
                   "Bill, as amended, agreed to\\.",
                   "An incident having occurred in the gallery-",
                   "In accordance with standing order \\d{1,2}\\([[:alpha:]]\\) the report was made a parliamentary paper\\.")
  
  # remove name and other info from rows with stage notes / stage directions
  main <- main %>% mutate(name = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                        "stage direction", name),
                          name.id = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                           NA, name.id),
                          electorate = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                              NA, electorate),
                          party = as.factor(ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                                   NA, as.character(party))))
  
  # if row contains "took the chair..." need to paste name back into body and change name to business start
  # need if-else here because in cases where we have the business start and the business start is also already included in the body
  # we don't have to have it twice, so only extract these if we don't have the business start already (about to bind it on in next bit of code)
  if (nrow(main %>% filter(name=="business start"))==0){
    main <- main %>% mutate(body = str_remove(body, "^[[:space:]]{0,30}"),
                            body = ifelse(str_detect(body, "took the chair at \\d\\d:\\d\\d|took the chair at \\d:\\d\\d"), paste0(name, " ", body), body),
                            name = ifelse(str_detect(body, "took the chair at \\d\\d:\\d\\d|took the chair at \\d:\\d\\d"), "business start", name))
  } else {
    # remove rows where the name is not business start and the body contains contents of a business start - this is to avoid duplicates where something was wrongly not-nested as a business start but there is already an actual business start
    main <- main %>% filter(!(name!="business start" & str_detect(body, "^The .{1,35} took the chair at \\d\\d:\\d\\d|^The .{1,35} took the chair at \\d:\\d\\d|^took the chair at \\d\\d:\\d\\d\\.$|^\\(.{1,35}\\)took the chair at \\d\\d:\\d\\d\\.$")))
  }
  
  # if name is "The Clerk:", remove the colon
  main <- main %>% mutate(name = ifelse(name=="The Clerk:", "The Clerk", name))
  
  # if name is "The ACTING SPEAKER" and body starts with time stamp, remove it from the body (if the time stamp is present)
  main <- main %>% mutate(body = ifelse(!is.na(time.stamp) & name=="The ACTING SPEAKER" & str_detect(body, "^\\(\\d\\d:\\d\\d\\):  "),
                                        str_remove(body, "^\\(\\d\\d:\\d\\d\\):  "),
                                        body))

  ######## no longer need this b/c added business start in other function to maintain correct ordering  
  ########## add business start to main
  # NOTE: if there is no business start like 2016-08-30, you'll get a warning msg "Unknown or uninitialised column: `time.stamp`" but just ignore it. (I am for now)
  # sometimes time stamp is missing and if we try to arrange with it and it's NA, it'll cause problems
  # if (sum(is.na(bus_start$time.stamp))==0){
  #   main <- rbind(bus_start, main) %>% 
  #     group_by(fedchamb_flag) %>% 
  #     mutate(page.no = ifelse(is.na(page.no), min(page.no, na.rm=TRUE), page.no)) %>% 
  #     ungroup() %>% 
  #     arrange(fedchamb_flag)
  # } else {
  #   main <- rbind(bus_start, main) %>% 
  #     group_by(fedchamb_flag) %>% 
  #     mutate(page.no = ifelse(is.na(page.no), min(page.no, na.rm=TRUE), page.no)) %>% 
  #     ungroup() %>% 
  #     arrange(fedchamb_flag)
  # }
  
  #### step 7: add order column
  
  # now that we've split all the rows, let's add an order column so we can keep track of exact order of things
  # need original speech_no column though to keep track of which interjections belong to which speech (will be useful to flag interjections later)
  # let's also remove imputed time stamps that follow with splitting rows - adding this to scripts Dec. 7 2022
  # group by speech number and only keep timestamp associated with first statement in that speech - the others just follow with splitting rows and so are technically imputed
  main <- main %>% group_by(speech_no) %>% rowid_to_column("order") %>% 
    mutate(time.stamp = ifelse(n()>1 & order!=min(order), NA, time.stamp)) %>% 
    ungroup()
  
  #### step 8: create a look-up table with everyone's name, name ID, electorate and party
  # we will use this to fill in the information for interjection rows
  # this needs to be done in a few steps, to account for various forms of names
  
  # form: "___, ___ MP"
  name_forms <- main %>% 
    select(name) %>% 
    unique() %>% 
    filter(str_detect(name, "MP$")) %>% 
    mutate(main_form = name,
           name = str_remove(main_form, "[:space:]MP"),
           first_name = str_extract(name, "(?<=\\,).{0,50}$"),
           first_name = str_replace_all(first_name, "^[:blank:]", ""),
           first_name = ifelse(str_detect(first_name, "[[:punct:]]"), str_remove(first_name, "[[:punct:]]"), first_name),
           last_name = str_extract(name, "^[:alpha:]{0,35},|^[:alpha:]{0,35}[:space:][:alpha:]{0,35},|^[:alpha:]{0,35}[:punct:][:alpha:]{0,35},"),
           last_name = str_replace_all(last_name, "[:punct:]$", "")) %>% select(-name)
  
  # form: "Ms/Mr/Dr/Mrs ___ interjecting-"
  name_forms <- main %>% 
    select(name) %>% 
    unique() %>% 
    filter(!str_detect(name, "member") & str_detect(name, "interjecting-")) %>% 
    mutate(main_form = name,
           name = str_remove(main_form, "^Dr[[:blank:]]|^Ms[[:blank:]]|^Mr[[:blank:]]|^Mrs[[:blank:]]"),
           name = str_remove(name, "interjecting[[:punct:]]$"),
           name = str_replace_all(name, "[[:blank:]]$", ""),
           first_name = ifelse(str_detect(name, "[[:lower:]][[:blank:]][[:upper:]]"), str_extract(name, "^[[:alpha:]]{0,15}"), NA),
           last_name = ifelse(is.na(first_name), name, str_remove(name, paste0(first_name, " "))),
           name = ifelse(is.na(first_name), name, paste0(last_name, ", ", first_name))) %>% 
    select(-name) %>% 
    rbind(., name_forms)
  
  # form: ___, ___ MP (The SPEAKER/DEPUTY SPEAKER)
  name_forms <- main %>% 
    select(name) %>% 
    unique() %>% 
    filter(str_detect(name, "The")) %>% 
    filter(!str_detect(name, "^The DEPUTY SPEAKER$|^The SPEAKER$")) %>%
    mutate(main_form = name,
           name = str_remove(main_form, "The SPEAKER.|The DEPUTY SPEAKER."),
           name = str_replace_all(name, "\\(|\\)", ""),
           name = str_remove(name, "[[:space:]]MP|[[:blank:]]MP|^Mr[[:blank:]]|^Mrs[[:blank:]]|^Ms[[:blank:]]|^Dr[[:blank:]]"),
           name = str_remove(name, ", $| $"),
           first_name = ifelse(str_detect(name, "\\,"), 
                               str_extract(name, "\\,[[:blank:]].{0,35}"), # feb 9 - changed from [[:alpha:]]{0,35} to .{0,35} to capture ppl with two first names
                               str_extract(name, "^[[:alpha:]]{0,35}(?=[[:space:]])")),
           first_name = str_replace_all(first_name, "[[:punct:]][[:blank:]]",  ""),
           last_name = ifelse(is.na(first_name), 
                              str_extract(name, ".{0,40}"),
                              ifelse(str_detect(name, "\\,"),
                                     str_extract(name, "^[[:alpha:]]{0,35}(?=\\,)|^[[:alpha:]]{0,35}[[:punct:]][[:alpha:]]{0,35}(?=\\,)"), 
                                     str_extract(name, "(?<=[[:space:]]).{0,40}")))) %>% 
    mutate(first_name = ifelse(str_detect(name, "^The DEPUTY SPEAKER$|^The SPEAKER$"), NA, first_name),
           last_name = ifelse(str_detect(name, "^The DEPUTY SPEAKER$|^The SPEAKER$"), name, last_name)) %>% 
    select(-name) %>% 
    filter(!is.na(last_name)) %>% 
    rbind(., name_forms)
  
  # form: Mr/Ms/Dr/Mrs __ __
  name_forms <- main %>% 
    select(name) %>% 
    unique() %>% 
    filter(str_detect(name, "^Mr[[:blank:]]|^Mrs[[:blank:]]|^Ms[[:blank:]]|^Dr[[:blank:]]")) %>% 
    filter(!str_detect(name, "interjecting-")) %>% 
    mutate(main_form = name,
           name = str_remove(name, "^Mr[[:blank:]]|^Mrs[[:blank:]]|^Ms[[:blank:]]|^Dr[[:blank:]]"),
           first_name = ifelse(name!="VAN MANEN"&name!="van Manen", str_extract(name, "[[:alpha:]]{0,35}[[:blank:]]"), NA),                 # note that this only works for one first name which I think is fine b/c Catherine Fiona King is titled Ms Catherine King, will keep an eye on this
           first_name = str_replace_all(first_name, "[[:blank:]]",  ""),
           last_name = ifelse(is.na(first_name), str_extract(name, ".{0,40}"), str_extract(name, paste0("(?<=", first_name, "[[:space:]])", ".{0,40}")))) %>% 
    select(-name) %>% 
    filter(!is.na(last_name)) %>% 
    mutate(last_name = ifelse(grepl("^[[:upper:]]+$", last_name), str_to_title(last_name), last_name),
           last_name = ifelse(grepl("^[[:alpha:]]{0,20}[[:punct:]][[:alpha:]]{0,20}$", last_name), 
                              paste0(str_to_title(str_extract(last_name, "^[[:alpha:]]{0,20}(?=[[:punct:]])")),
                                     str_extract(last_name, "[[:punct:]]"),
                                     str_to_title(str_extract(last_name, "(?<=[[:punct:]])[[:alpha:]]{0,20}$"))), 
                              last_name),
           last_name = ifelse(grepl("^[[:upper:]][[:lower:]][[:upper:]]+$", last_name), 
                              paste0(str_sub(last_name, end=3L), str_to_lower(str_sub(last_name, start=4L))), last_name),
           last_name = ifelse(grepl("^[[:upper:]][[:punct:]][[:upper:]][[:upper:]]+$", last_name),
                              paste0(str_sub(last_name, end=3L), str_to_lower(str_sub(last_name, start=4L))), last_name),
           first_name = ifelse(grepl("[[:upper:]]$", first_name), str_to_title(first_name), first_name)) %>% 
    rbind(., name_forms)
  
  # if someones last name is two words like van Manen, and all capitalized, let's fix the capitalization so I can group and fill things correctly
  name_forms <- name_forms %>% 
    mutate(last_name = ifelse(str_detect(last_name, "^[[:upper:]]*[[:space:]][[:upper:]]*$"), 
                              paste0(str_to_lower(str_extract(last_name, "^[[:upper:]]*(?=[[:space:]])")), 
                                     " ", 
                                     str_to_title(str_extract(last_name, "(?<=[[:space:]])[[:upper:]]*$"))),
                              last_name))
  
  # if someones first initial is all that's in the first name cell and their name appears more than once, remove it so I can fill correctly
  name_forms <- name_forms %>% group_by(last_name) %>% 
    mutate(first_name = ifelse(str_detect(first_name, "^[[:upper:]]$") & n()>1, NA, first_name)) %>% 
    ungroup()
  
  ######## start of NEW STUFF ########
  name_forms <- name_forms %>% 
    mutate(title = ifelse(str_detect(main_form, "^Mr(?=[[:space:]])"), "Mr", NA),
           title = ifelse(str_detect(main_form, "^Mrs(?=[[:space:]])"), "Mrs", title),
           title = ifelse(str_detect(main_form, "^Ms(?=[[:space:]])"), "Ms", title),
           title = ifelse(str_detect(main_form, "^Dr(?=[[:space:]])"), "Dr", title),
           title = ifelse(str_detect(main_form, "^The.{1,20}(?=\\(Mr|Mrs|Ms|Dr)"), str_extract(main_form, "Mrs|Mr|Ms|Dr"), title)) %>% 
    mutate(title = as.character(title))
  
  
  # fill in missing title based on matching first last name
  name_forms <- name_forms %>% group_by(first_name, last_name) %>% 
    fill(title, .direction = "downup") %>% 
    ungroup()
  
  # if main form has Dr title in it, remove that from first name so it's not added to displayname
  name_forms <- name_forms %>% mutate(first_name = ifelse(str_detect(first_name, "^Dr .{1,15}"),
                                                          str_remove(first_name, "^Dr "),
                                                          first_name))
  # add display names to match those of AusPol list
  # removing titles to match format of AusPol
  # if we have the first and last name, paste it in format for displayName
  name_forms <- name_forms %>% 
    mutate(displayName = case_when(!str_detect(first_name, "[[:space:]]") & !str_detect(main_form, "\\,[[:space:]]Dr[[:space:]]") & str_detect(main_form, "\\,[[:space:]]MP$|[[:space:]]MP$|\\,[[:space:]]MP[[:space:]]\\(The|[[:space:]]\\(The DEPUTY SPEAKER\\)$") ~ 
                                     str_extract(main_form, ".{1,50}(?=\\,[[:space:]]MP|[[:space:]]MP|[[:space:]]\\(The DEPUTY SPEAKER\\))"),
                                   str_detect(first_name, "[[:space:]]") & str_detect(main_form, "\\,[[:space:]]MP$|[[:space:]]MP$|\\,[[:space:]]MP[[:space:]]\\(The|[[:space:]]\\(The DEPUTY SPEAKER\\)$") ~
                                     str_extract(main_form, ".{1,30}(?=[[:space:]].{1,20}\\, MP|[[:space:]].{1,20}[[:space:]]MP|[[:space:]].{1,20}[[:space:]]\\(The DEPUTY SPEAKER\\))"),
                                   !is.na(first_name) & !is.na(last_name) ~ paste0(last_name, ", ", first_name)),
           displayName = ifelse(str_detect(displayName, "\\,$"), str_remove(displayName, "\\,$"), displayName))
  
  
  ######## beginning of NEW STUFF ########
  # keep nrow of name forms at this point so we can do a check after we've merged stuff from the AusPol database that the number of rows hasn't changed
  nrow_name_forms <- nrow(name_forms)
  
  # this will be the master list of names for Hansard 2011-2022. filter out anyone that has died before 2011
  master_list <- all %>% filter(deathDate > "2010-12-31" | is.na(deathDate)) %>% 
    select(c(uniqueID, surname, allOtherNames, firstName, commonName, displayName, title, gender)) %>% 
    mutate(title = ifelse(gender=="male" & is.na(title), "Mr", title),
           title = ifelse(gender=="female" & is.na(title), "Ms", title)) %>% 
    #select(-gender) %>% 
    rename(last_name = surname)
  
  # fixing up women with title "Mrs" based on aph.gov.au website using gender search tool (these have prefixes)
  # keeping original prefix based on "all" b/c these may have changed at some point in time
  master_list <- master_list %>% 
    mutate(title = ifelse(displayName=="Archer, Bridget", "Ms|Mrs", title),
           title = ifelse(displayName=="McIntosh, Melissa", "Ms|Mrs", title),
           title = ifelse(displayName=="Phillips, Fiona", "Ms|Mrs", title),
           title = ifelse(displayName=="Haines, Helen", "Ms|Dr", title),
           title = ifelse(displayName=="Chalmers, Jim", "Mr|Dr", title),
           title = ifelse(displayName=="Freelander, Mike", "Mr|Dr", title),
           title = ifelse(displayName=="Gillespie, David", "Mr|Dr", title),
           title = ifelse(displayName=="Leigh, Andrew", "Mr|Dr", title),
           title = ifelse(displayName=="Wicks, Lucy", "Mrs|Ms", title),
           title = ifelse(displayName=="Southcott, Andrew", "Mr|Dr", title)) %>% 
    separate_rows(title, sep="\\|") %>% 
    mutate(displayName = ifelse(displayName=="Katter, Bob (Jr)", "Katter, Bob", displayName),
           displayName = ifelse(displayName=="O'Neill, Deborah", "O'Neill, Deb", displayName),
           displayName = ifelse(displayName=="Oakeshott, Rob", "Oakeshott, Robert", displayName),
           displayName = ifelse(displayName=="Somlyay, Alex", "Somlyay, Alexander", displayName))
  
  # grab list of names where surname and prefix/title exist more than once
  # we need to leave these out when we merge because if multiple people share a last name and title, multiple rows will be added for each person
  # and we will have extra rows and not know what the right name is
  # best to leave these missing and use name ID later to figure out the full name
  repeated_surnames <- master_list %>% group_by(title, last_name) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    ungroup() %>% 
    select(-n) %>% 
    add_row(title = "Ms", last_name = "Lawrence")
  
  # people who we don't need to fill
  name_forms_1 <- name_forms %>% filter(!is.na(displayName)) %>% left_join(., master_list, by=c("last_name", "displayName", "title")) %>% 
    select(c(main_form, first_name, last_name, title, displayName, gender, uniqueID)) %>% 
    mutate(gender = ifelse(is.na(gender) & title=="Mr", "male", gender),
           gender = ifelse(is.na(gender) & title=="Mrs", "female", gender),
           gender = ifelse(is.na(gender) & title=="Ms", "female", gender))
  
  # people who we don't want to fill due to repeated surname and title issue
  name_forms_2 <- name_forms %>% filter(is.na(first_name) & str_detect(last_name, paste0(repeated_surnames$last_name, collapse = "|"))) %>% 
    mutate(gender = ifelse(title=="Mr", "male", NA),
           gender = ifelse(is.na(gender) & title=="Ms", "female", gender),
           gender = ifelse(is.na(gender) & title=="Mrs", "female", gender),
           gender = ifelse(is.na(gender) & main_form=="The DEPUTY SPEAKER (Hon. BC Scott)", "male", gender),
           last_name = ifelse(last_name == "Hon. BC Scott", "Scott", last_name),
           title = ifelse(is.na(title) & main_form=="The DEPUTY SPEAKER (Hon. BC Scott)", "Mr", title),
           uniqueID = NA)
  
  # people we want to fill using master list
  name_forms_3 <- name_forms %>% filter(is.na(displayName) & !str_detect(last_name, paste0(repeated_surnames$last_name, collapse = "|"))) %>% 
    select(-displayName) %>% 
    left_join(., master_list, by=c("title", "last_name")) %>% 
    select(c(main_form, first_name, last_name, title, displayName, gender, uniqueID)) %>% 
    mutate(gender = ifelse(title=="Mr", "male", NA),
           gender = ifelse(is.na(gender) & title=="Ms", "female", gender),
           gender = ifelse(is.na(gender) & title=="Mrs", "female", gender))
  
  # bind everything back together and fill in missing titles using groupby displayName
  name_forms_final <- rbind(name_forms_1, name_forms_2, name_forms_3) %>% 
    mutate(first_name = ifelse(is.na(first_name) & !is.na(displayName), str_extract(displayName, "(?<=\\,[[:space:]]).{1,30}"), first_name)) %>% 
    group_by(displayName) %>% 
    fill(c(title, uniqueID), .direction = "downup") %>% 
    ungroup() %>% select(-c(title, displayName))
  
  # check that nrow hasn't changed
  stopifnot(nrow_name_forms == nrow(name_forms_final))
  ######## end of NEW STUFF ########
  
  ########## also changed below from "name_forms" to "name_forms_final"
  # combine main forms into one cell
  # need to ensure those referred to both with and without deputy speaker title are kept separate, as title may not be correct to have for all of their speeches
  name_forms_final <- name_forms_final %>%  
    mutate(deputy_flag = str_detect(main_form, "The DEPUTY SPEAKER")) %>% 
    group_by(last_name, first_name, deputy_flag) %>%
    mutate(main_form = paste0(main_form, collapse = "|")) %>% 
    ungroup() %>% 
    select(-deputy_flag) %>% 
    unique()
  
  # filling in gender and unique ID
  name_forms_final <- name_forms_final %>% 
    group_by(first_name, last_name) %>% 
    fill(c(gender, uniqueID), .direction = "downup") %>% 
    ungroup()
  
  # now creating name info tibble
  if (nrow(interject)>0) {
    name_info <-interject %>%
      select(c(name, name.id, electorate, party)) %>%
      filter(str_detect(name, "The")) %>%
      unique() %>%
      mutate(full_name = name,
             name = str_remove(full_name, "[:space:]MP"),
             name = str_remove(name, " \\(The.{0,10}[:space:][:alpha:]{0,10}\\)"),
             last_name = str_extract(name, ".{0,50}(?=\\,)"),
             first_name = str_extract(name, "(?<=\\, ).{1,50}")) %>% 
      select(-name) 
  } else {
    name_info <- c()
  }
  
  if ("SPEAKER" %in% name_info$last_name) {
    name_info <- main %>% filter(str_detect(name, "The SPEAKER") & !is.na(party)) %>% 
      select(c(name.id, electorate, party, name)) %>% unique() %>% 
      mutate(full_name = name,
             name = str_remove(full_name, "[:space:]MP|[:punct:][:space:]MP"),
             name = str_remove(name, " \\(The.{0,10}[:space:][:alpha:]{0,10}\\)"),
             last_name = str_extract(name, ".{0,50}(?=\\,)"),
             first_name = str_extract(name, "(?<=\\, ).{1,50}")) %>% 
      select(-name) %>% 
      rbind(., name_info) %>% 
      filter(last_name!="SPEAKER")
  }
  
  if (nrow(interject) > 0) {
    # get names of interjecting MPs (not the speaker/deputy speaker)
    name_info <- interject %>%
      select(c(name, name.id, electorate, party)) %>%
      filter(!str_detect(name, "interject|The")) %>%
      unique() %>%
      mutate(full_name = name,
             name = str_remove(full_name, "[:space:]MP"),
             name = str_remove(name, ",$"),
             first_name = str_extract(name, "[:alpha:]{0,35}$|[:alpha:]{0,35}[:space:][:alpha:]{0,35}$|[:alpha:]{0,35}[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}$"),
             first_name = str_replace_all(first_name, "^[:blank:]", ""),
             first_name = ifelse(str_detect(first_name, "[[:punct:]]"), str_remove(first_name, "[[:punct:]]"), first_name),
             last_name = str_extract(name, "^[:alpha:]{0,35},|^[:alpha:]{0,35}[:space:][:alpha:]{0,35},|^[:alpha:]{0,35}[:punct:][:alpha:]{0,35},"),
             last_name = str_replace_all(last_name, "[:punct:]$", "")) %>% select(-name) %>% 
      rbind(., name_info)
    
    # get names of people who interject that are just referred to as "Mr/Ms/Mrs/Dr ___ interjecting-" in file
    # for those with first and last name included, re-format into last_name, first_name format
    name_info <- interject %>% 
      select(c(name, name.id, electorate, party)) %>%
      filter(str_detect(name, "interjecting")) %>%
      unique() %>% 
      mutate(name.id = ifelse(name.id=="", NA, name.id),
             electorate = ifelse(electorate == "", NA, electorate),
             party = ifelse(party=="", NA, party)) %>% 
      mutate(full_name = name,
             name = str_remove(name, "^Dr[[:blank:]]|^Ms[[:blank:]]|^Mr[[:blank:]]|^Mrs[[:blank:]]"),
             name = str_remove(name, "interjecting[[:punct:]]$"),
             name = str_replace_all(name, "[[:blank:]]$", ""),
             first_name = ifelse(str_detect(name, "[[:lower:]][[:blank:]][[:upper:]]"), str_extract(name, "^[[:alpha:]]{0,15}"), NA),
             first_name = ifelse(str_detect(first_name, "[[:punct:]]"), str_remove(first_name, "[[:punct:]]"), first_name),
             last_name = ifelse(is.na(first_name), name, str_remove(name, paste0(first_name, " "))),
             name = ifelse(is.na(first_name), name, paste0(last_name, ", ", first_name))) %>% 
      select(-name) %>% 
      rbind(., name_info)
  }
  
  # get names of all speakers from main data frame (MPs)
  name_info <- main_orig %>% 
    select(c(name, name.id, electorate, party)) %>% 
    unique() %>% filter(!str_detect(name, "\\)$")) %>% 
    mutate(full_name = name,
           name = str_remove(full_name, "[:space:]MP.{0,35}$"),
           first_name = str_extract(name, ",[:space:].{0,35}"),
           first_name = str_replace_all(first_name, "^,[:space:]", ""),
           first_name = ifelse(str_detect(first_name, "[[:punct:]]"), str_remove(first_name, "[[:punct:]]"), first_name),
           last_name = str_extract(name, "^[:alpha:]{0,35},|^[:alpha:]{0,35}[:space:][:alpha:]{0,35},|^[:alpha:]{0,35}[:punct:][:alpha:]{0,35},"),
           last_name = str_replace_all(last_name, "[:punct:]$", "")) %>% 
    select(-name) %>% 
    rbind(., name_info) %>% 
    unique()
  
  # finally, need to add first first-name-only form for those with multiple first names
  # name_info <- name_info %>% 
  #   filter(str_detect(first_name, "[[:space:]]")) %>% 
  #   mutate(first_name = str_extract(first_name, "^[[:alpha:]]{0,35}")) %>% 
  #   rbind(., name_info) %>% 
  #   unique()
  
  # treat generic name ID as missing to be filled next
  name_info <- name_info %>% mutate(name.id = ifelse(name.id=="10000", NA, name.id))
  
  # also, if the electorate and/or party are blank, make them NA
  name_info <- name_info %>% mutate(electorate = ifelse(electorate=="", NA, electorate),
                                    party = as.factor(ifelse(party=="", NA, paste0(party))))
  
  # if someones first initial is all that's in the first name cell and their name appears more than once, remove it so I can fill correctly
  name_info <- name_info %>% group_by(last_name) %>% 
    mutate(first_name = ifelse(str_detect(first_name, "^[[:upper:]]$") & n()>1, NA, first_name)) %>% 
    ungroup()
  
  # fill in missing first names using group by last name
  # then fill in electorate, party, and name.id info based on groups of first and last names
  name_info <- name_info %>% group_by(last_name) %>% 
    fill(first_name, .direction = "downup") %>% 
    ungroup() %>% 
    group_by(first_name, last_name) %>% 
    fill(c(name.id, electorate, party), .direction = "downup") %>% 
    ungroup()
  
  # fix dashes
  name_info <- name_info %>% 
    mutate(full_name = str_replace_all(full_name, "\u2014", "-")) %>% 
    filter(!str_detect(last_name, "SPEAKER"))
  
  # merge name info with name info from main, to create lookup data set which we can use to fill main
  name_lookup <- full_join(name_forms_final, name_info, by = intersect(names(name_forms_final), names(name_info)))
  
  # when we full join, people who are the deputy speaker at some times but not others get the wrong form and full name combo
  # fix this b/c deputy speaker can change during proceedings
  name_lookup <- name_lookup %>% filter(str_detect(main_form, "\\(The DEPUTY SPEAKER|The DEPUTY SPEAKER \\(") & str_detect(full_name, "\\(The DEPUTY SPEAKER\\)")|
                                          !str_detect(main_form, "\\(The DEPUTY SPEAKER|The DEPUTY SPEAKER \\(") & !str_detect(full_name, "\\(The DEPUTY SPEAKER\\)")|
                                          str_detect(first_name, "The"))
  
  # remove any "clerk" or "acting speaker" rows, leave those as it
  name_lookup <- name_lookup %>% filter(last_name!="acting Speaker" & last_name!="Clerk")
  
  # clean things up a bit (multiple rows for same person w/ some missing, fill in gaps)
  name_lookup <- name_lookup %>% 
    group_by(uniqueID) %>% 
    fill(names(name_lookup), .direction = "downup") %>% 
    distinct() %>% 
    ungroup()
  
  # for those where we don't have their electorate/name id/party info and their name has "interjecting-", lets clean that up
  # note that sometimes we don't have their first name, so need a case where first name is NA
  name_lookup <- name_lookup %>% 
    mutate(full_name = ifelse(is.na(first_name), 
                              ifelse(str_detect(full_name, "interjecting"), 
                                     paste0(last_name), 
                                     full_name), 
                              ifelse(str_detect(full_name, "interjecting"), 
                                     paste0(last_name, ", ", first_name), 
                                     full_name)))
  
  # sometimes the name is simply "The DEPUTY SPEAKER" (once their full name is referred to, it is usually shortened to just the title in subsequent interjections)
  # going to clean this up in name lookup (so full name isn't DEPUTY SPEAKER, The, but rather The DEPUTY SPEAKER)
  name_lookup <- name_lookup %>% 
    mutate(full_name = ifelse(str_detect(last_name, "SPEAKER"), 
                              paste0("The ", str_remove(last_name, "The ")), 
                              full_name)) %>% 
    mutate(main_form = ifelse(is.na(main_form), full_name, main_form))
  
  # sometimes there would be duplicated rows for people with two first names, because name_info had both and name_forms_final only had first first name
  # I'm just removing the second first name for consistency, so when we merge with main there aren't extra duplicated rows due to the two variations of name
  # here's the line of code i used to detect these:
  # name_lookup %>% filter((duplicated(name.id) & !duplicated(first_name)))
  name_lookup <- name_lookup %>% mutate(first_name = ifelse(str_detect(first_name, "^[[:alpha:]]{1,10}[[:space:]][[:alpha:]]{1,10}$"),
                                                            str_remove(first_name, "[[:space:]][[:alpha:]]{1,10}$"),
                                                            first_name))
  
  #### ADDED
  # if there happens to be a shorter and longer form of the name (ex. Coulton, Mark and Coulton, Mark MP), take the longer one
  name_lookup <- name_lookup %>% 
    group_by(last_name, first_name, main_form) %>% 
    filter(n()>1 & nchar(full_name)==max(nchar(full_name)) | n()==1) %>% 
    ungroup()
  
  
  # clean up / modify lookup table for merge with main
  name_lookup <- name_lookup %>% 
    select(-c(first_name, last_name)) %>% 
    rename(name = main_form,
           name_use = full_name,
           electorate_use = electorate,
           name.id_use = name.id,
           party_use = party) %>% 
    separate_rows(name, sep="\\|") %>% 
    filter(name!=name_use | str_detect(name, "SPEAKER$|^The")) %>% 
    distinct()
  
  
  # fill stuff in 
  name_lookup<- name_lookup %>% 
    group_by(name_use) %>% 
    fill(c(uniqueID, gender, name.id_use, electorate_use, party_use), .direction = "updown") %>% 
    ungroup() %>% unique()
  
  #### step 9: merge main with lookup table, replace names with correct name (as needed) and fill in missing info (name ID/party/electorate)
  
  # store nrow of main pre-merge to check on after merge
  nrow_main_before <- nrow(main)
  
  # perform merge
  main <- merge(main, name_lookup, by="name", all.x = T) %>% 
    mutate(name = ifelse(is.na(name_use), name, name_use),
           electorate = ifelse(is.na(electorate_use), electorate, electorate_use),
           name.id = ifelse(is.na(name.id_use), name.id, name.id_use),
           party = as.factor(ifelse(is.na(party_use), as.character(party), as.character(party_use)))) %>% 
    select(-c(name_use, party_use, electorate_use, name.id_use)) %>% arrange(order)
  
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
  
  # fill electorate, name.id and party info
  main <- main %>% group_by(name) %>% 
    fill(c(name.id, electorate, party), .direction = "downup") %>% 
    ungroup() %>% distinct()
  
  # fix up "___ members interjecting-" names for simplicity
  main <- main %>% mutate(name = ifelse(str_detect(name, "interjecting-"), str_remove(name, "[[:space:]]interjecting-"), name))
  
  #### step 10: flag for interjections
  # group by speech_no, and if the name is not equal to the first name w/ that speech_no, or the speaker, it is an interjection
  # had to add "is.na(speech_no)" condition b/c q in writing stuff doesn't have speech numbers and b/c we group by speech no and they're all NA, they're all getting flagged 
  main_final <- main %>% group_by(speech_no) %>% arrange(order) %>% 
    mutate(interject = case_when(order == min(order) ~ 0,
                                 str_detect(name, "The SPEAKER|The DEPUTY SPEAKER|stage direction|business start") ~ 0,
                                 is.na(speech_no) ~ 0)) %>% 
    ungroup() %>% 
    group_by(name, speech_no) %>%
    fill(interject, .direction = "down") %>% 
    ungroup() %>% 
    mutate(interject = ifelse(is.na(interject), 1, interject))
  
  return(main_final)
}

##### temporary filename
filename <- "2017-12-04.xml"

parse_hansard_new <- function(filename) {
  
  # parse file
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
  
  
  
  ######### DEBATE TEXT #########
  # grab all debate text, correct var classes, extract time stamps
  # could actually get all debate text with question time text in order too this way
  # but might make it tricky to flag qs and as, plus already have the code to do it with exceptions accounted for
  # but this maintains order really nicely and avoids us having to arrange with rows that don't have a time tamp available
  all_text_chamb <- left_join(item_df(xml_df, "chamber.xscript//debate//speech/talk.start/talker | chamber.xscript//question/talk.start/talker | chamber.xscript//answer/talk.start/talker") %>% unnest(everything()),
                              item_df(xml_df, "chamber.xscript//debate//speech/talk.text | chamber.xscript//question/talk.text | chamber.xscript//answer/talk.text") %>% unnest(everything()),
                              by = "itemindex") %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
    mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL}) %>% 
    mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                    str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                    body) else NULL},
           body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                    str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                    body) else NULL})
  
  ################### QUESTIONS WITHOUT NOTICE ###################
  # get it and tidy up the flagging this way so we can then left join on the body of text with the "all_text_chamb" tibble above, to get the correct flags
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
             answer = ifelse(str_detect(body, "My question is addressed to") & answer==1, 0, answer)) %>% 
      mutate(question = ifelse(str_detect(body, "I thank the member for .{1,35} for .{1,35} question\\.") & question==1, 0, question),
             answer = ifelse(str_detect(body, "I thank the member for .{1,35} for .{1,35} question\\.") & answer==0, 1, answer))
    
    ### checkpoint ###
    # make sure there are no lines flagged as both a question and an answer
    stopifnot(nrow(q_wo_notice %>% filter(question==answer))==0)
    
    # specific cases where the first question/answer node found is an answer that is actually just part of a speech and shouldn't be flagged as an answer
    if (filename %in% c("2020-10-19.xml", "2014-05-26.xml", "2012-08-15.xml", "2013-06-26.xml", "2013-11-13.xml" , "2016-09-13.xml", "2018-03-26.xml")) {
      
      # manually fix incorrectly flagged answer
      q_wo_notice <- q_wo_notice %>% mutate(answer = ifelse(itemindex==1, 0, answer))
      
      ### checkpoint ###
      # make sure that the second line is a question
      stopifnot(q_wo_notice$question[2]==1)
      
    } else if (filename=="2017-09-13.xml"){
      # this date has the first two as wrongly flagged, they are statements my the deputy speakers and shouldn't be flagged as answers
      # manually fix incorrectly flagged answers
      q_wo_notice <- q_wo_notice %>% mutate(answer = ifelse(itemindex==1 | itemindex==2, 0, answer)) %>% 
        mutate(body = ifelse(itemindex==1 | itemindex==2, str_replace(body, "\\n                    ", " "), body))
      
      ### checkpoint ###
      # make sure that the third line is a question
      stopifnot(q_wo_notice$question[3]==1)
      
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
  
  # now left join so we can flag the questions without notice correctly
  # grab row number before merging
  nrow_before <- nrow(all_text_chamb)
  
  # left join, if no q without notice, just add the columns that otherwise would be missing
  if (nrow(q_wo_notice)>0){
    all_text_chamb <- q_wo_notice %>% select(body:sub2_flag) %>% left_join(all_text_chamb, ., by="body")
  } else {
    all_text_chamb <- all_text_chamb %>% mutate(sub1_flag = NA, sub2_flag = NA, fedchamb_flag = 0)
  }
  
  # make sure row number hasn't changed
  stopifnot(nrow_before == nrow(all_text_chamb))
  
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
    # conditional b/c 2017 03 23 only one question and the answer was nested as a question too so code broke
    if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions//answer")))>0) {
      a_in_writing_text <- item_df(xml_df, "answers.to.questions//answer") %>% unnest(talk.text) %>% pull(talk.text)
    } else {
      a_in_writing_text <- tibble()
    }
    
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
      mutate(question = ifelse(str_detect(body, "asked the Minister .{1,300}, in writing,") & question==0, 1, question),
             answer = ifelse(str_detect(body, "asked the Minister .{1,300}, in writing,") & answer==1, 0, answer))
    
    ### checkpoint ###
    # make sure there are no lines flagged as both a question and an answer and that the first line is a question
    stopifnot(nrow(q_in_writing %>% filter(question==answer))==0,
              q_in_writing$question[1]==1)
    
    # add final flags
    q_in_writing <- q_in_writing %>% mutate(sub1_flag = 1, sub2_flag = 0, fedchamb_flag = 0) %>% select(-itemindex)
    
  } else {
    q_in_writing <- tibble()
  }
  
  if (filename == "2017-12-04.xml") {
    q_in_writing <- q_in_writing %>% separate_rows(body, sep="(?=Ms Rebekha Sharkie: asked the Minister for Foreign Affairs)") %>% 
      separate_rows(body, sep="(?=Ms Bishop:   the answer to the member's question is as follows)") %>% 
      mutate(name = ifelse(name=="Wyatt, Ken, MP" & str_detect(body, "^Ms Rebekha Sharkie:"), "Sharkie, Rebekha, MP", name),
             name = ifelse(name=="Wyatt, Ken, MP" & str_detect(body, "^Ms Bishop:"), "Ms Bishop", name),
             name.id = ifelse(name=="Sharkie, Rebekha, MP", "265980", name.id),
             name.id = ifelse(name=="Ms Bishop", NA, name.id),
             electorate = ifelse(name=="Sharkie, Rebekha, MP", "Mayo", electorate),
             electorate = ifelse(name=="Ms Bishop", NA, electorate),
             party = as.factor(as.character(ifelse(name=="Ms Bishop", NA, as.character(party)))),
             party = as.factor(as.character(ifelse(name=="Sharkie, Rebekha, MP", "NXT", as.character(party)))),
             question = ifelse(str_detect(body, "^Ms Rebekha Sharkie: asked the Minister"), 1, question),
             answer = ifelse(str_detect(body, "^Ms Rebekha Sharkie: asked the Minister"), 0, answer))
  }
  
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
  
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")))) > 0) {
    
    ######### BUSINESS START #########
    # store business start in tibble, add flag for federation chamber, extract date and start time
    # in rare cases (ex. 2016-08-30, there is no business start, so add if-else in case of this)
    bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")), 
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
    # bus_start <- rbind(bus_start_chamb, bus_start_fed)
    
    # now grab all federation chamber text
    # grab all debate text, correct var classes, extract time stamps
    all_text_fed <- left_join(item_df(xml_df, "fedchamb.xscript//debate//speech/talk.start/talker") %>% unnest(everything()),
                              item_df(xml_df, "fedchamb.xscript//debate//speech/talk.text") %>% unnest(everything()),
                              by = "itemindex") %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
             party = {if("party" %in% names(.)) as.factor(party) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
      mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL}) %>% 
      mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                      body) else NULL},
             body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                      body) else NULL})
    
    ######### SPEECH INTERJECTIONS #########
    # store sub-debate 1 speech interjections in tibble, correct variable class, add flag for federation chamber
    # use if-else statements in case there are no interjections
    sub1_interject_fed <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
    
    # store sub-debate 2 speech interjections in tibble, correct variable class, add flag for federation chamber
    # use if-else statements in case there are no interjections, or there is no sub-debate 2
    sub2_interject_fed <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
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
    # store business start as empty tibble
    bus_start_fed <- tibble()
    
    # store all text fed as empty tibble
    all_text_fed <- tibble()
    
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
  
  ############ GETTING SUBDEBATE 1 AND 2 FLAGS BACK
  ######## CHAMBER
  # last thing we want to do is get back the correct subdebate 1 or 2 flag
  # approach - grab all subdebate 2 and debate text, flag by detecting matches, and the rest are subdebate 1
  # doing it this way with process of elimination b/c usually subdebate 1 has the most content
  # first, debate text
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/speech/talk.text"))) > 0) {
    debate_text_chamb <- item_df(xml_df, "//chamber.xscript/debate/speech/talk.text") %>%
      select(body) %>%
      unnest(body) %>%
      mutate(sub1_flag_use = 0, sub2_flag_use = 0) %>% 
      mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                      body) else NULL},
             body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                      body) else NULL})
  } else {
    debate_text_chamb <- tibble()
  }
  
  
  # next, subdebate 2 text, if-else in case it doesn't exist
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate//subdebate.2/speech/talk.text")))) > 0) {
    sub2_text_chamb <- item_df(xml_df, "//chamber.xscript/debate//subdebate.2/speech/talk.text") %>%
      select(body) %>%
      unnest(body) %>%
      mutate(sub1_flag_use = 0, sub2_flag_use = 1) %>% 
      mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                      body) else NULL},
             body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                      body) else NULL})
  } else {
    sub2_text_chamb <- tibble()
  }
  
  
  # merge the two together
  # we use unique because sometimes, on a rare occasion, a statement might appear more than once, and we don't want extra rows because of that when we left join
  # case i saw is something super general like "I move that the bill be now read a second time"
  match_text_chamb <- bind_rows(debate_text_chamb, sub2_text_chamb) %>% unique()
  
  # now left join so we can flag the questions without notice correctly
  # grab row number before merging
  nrow_before <- nrow(all_text_chamb)
  
  # left join, use correct sub debate flags, and by process of elimination fill in the remaining missing ones
  if (nrow(match_text_chamb)>0){
    all_text_chamb <- left_join(all_text_chamb, match_text_chamb, by="body") %>% 
      mutate(sub1_flag = ifelse(is.na(sub1_flag) & !is.na(sub1_flag_use), sub1_flag_use, sub1_flag),
             sub2_flag = ifelse(is.na(sub2_flag) & !is.na(sub2_flag_use), sub2_flag_use, sub2_flag)) %>% 
      select(-sub1_flag_use, -sub2_flag_use) %>% 
      mutate(sub1_flag = ifelse(is.na(sub1_flag), 1, sub1_flag),
             sub2_flag = ifelse(is.na(sub2_flag), 0, sub2_flag))
    
    # make sure row number hasn't changed
    stopifnot(nrow_before == nrow(all_text_chamb))
    
  } else {
    all_text_chamb <- all_text_chamb %>% mutate(sub1_flag = 1, sub2_flag = 0)
  }
  
  
  ######## FEDERATION CHAMBER
  # last thing we want to do is get back the correct subdebate 1 or 2 flag
  # approach - grab all subdebate 2 and debate text, flag by detecting matches, and the rest are subdebate 1
  # doing it this way with process of elimination b/c usually subdebate 1 has the most content
  # first, debate text
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate/speech/talk.text")))) > 0) {
    debate_text_fed <- item_df(xml_df, "//fedchamb.xscript/debate/speech/talk.text") %>%
      select(body) %>%
      unnest(body) %>%
      mutate(sub1_flag_use = 0, sub2_flag_use = 0) %>% 
      mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                      body) else NULL},
             body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                      body) else NULL})
  } else {
    debate_text_fed <- tibble()
  }
  
  # next, subdebate 2 text
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/debate//subdebate.2/speech/talk.text")))) > 0) {
    sub2_text_fed <- item_df(xml_df, "//fedchamb.xscript/debate//subdebate.2/speech/talk.text") %>%
      select(body) %>%
      unnest(body) %>%
      mutate(sub1_flag_use = 0, sub2_flag_use = 1) %>% 
      mutate(body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\.[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\.(?=[[:upper:]])", ". "),
                                                      body) else NULL},
             body = {if("body" %in% names(.))  ifelse(str_detect(body, "[[:lower:]][[:lower:]]\\:[[:upper:]]"),
                                                      str_replace_all(body, "(?<=[[:lower:]][[:lower:]])\\:(?=[[:upper:]])", ": "),
                                                      body) else NULL})
  } else {
    sub2_text_fed <- tibble()
  }
  
  # merge the two together, conditional here in case there is no fed chamb that day
  if (nrow(all_text_fed)>0) {
    # we use unique because sometimes, on a rare occasion, a statement might appear more than once, and we don't want extra rows because of that when we left join
    # case i saw is something super general like "I move that the bill be now read a second time"
    match_text_fed <- bind_rows(debate_text_fed, sub2_text_fed) %>% unique()
    
    # now left join so we can flag the questions without notice correctly
    # grab row number before merging
    nrow_before <- nrow(all_text_fed)
    
    # left join, use correct sub debate flags, and by process of elimination fill in the remaining missing ones
    # conditional in case there is only subdebate 1 text that day
    if (nrow(match_text_fed)>0) {
      all_text_fed <- left_join(all_text_fed, match_text_fed, by="body") %>% 
        mutate(sub1_flag = sub1_flag_use,
               sub2_flag = sub2_flag_use) %>% 
        select(-sub1_flag_use, -sub2_flag_use) %>% 
        mutate(sub1_flag = ifelse(is.na(sub1_flag), 1, sub1_flag),
               sub2_flag = ifelse(is.na(sub2_flag), 0, sub2_flag))
      
      # make sure row number hasn't changed
      stopifnot(nrow_before == nrow(all_text_fed))
    } else {
      all_text_fed <- all_text_fed %>% mutate(sub1_flag = 1, sub2_flag = 0)
    }
    
  }
  
  # issue on this day - all these subdebates were nested as part of the business start. so i need to split them out indvidiually and add back into sub1 debate content
  # so they can be split properly
  if (filename == "2013-03-12.xml") {
    
    # split all subdebates, fill in correct information
    # page numbers taken from page it starts on in official release b/c not given in xml
    extra_sub1 <- bus_start_fed %>% select(body) %>% 
      mutate(body = str_remove(body, "^The DEPUTY SPEAKER \\(Dr Leigh\\) took the chair at 15\\:59\\.")) %>% 
      separate_rows(body, sep="(?=Cunningham Electorate\\: headspace Wollongong)") %>% 
      separate_rows(body, sep="(?=Flinders Electorate\\: Voices of the Outer Suburbs Campaign)") %>% 
      separate_rows(body, sep="(?=Chifley Electorate\\: International Women's Day)") %>% 
      separate_rows(body, sep="(?=Health\\: Strokes)") %>% 
      separate_rows(body, sep="(?=McEwen Electorate\\: Mount Ridley College)") %>% 
      separate_rows(body, sep="(?=Canning Electorate\\: NBN in Boddington)") %>% 
      separate_rows(body, sep="(?=Moreton Electorate\\: Bus Services)") %>% 
      separate_rows(body, sep="(?=Baillieu, Mr Ted)") %>% 
      separate_rows(body, sep="(?=Canberra Centenary)") %>% 
      mutate(body = str_remove(body, "^Parkes Electorate\\: Clontarf Foundation|^Cunningham Electorate\\: headspace Wollongong|^Flinders Electorate\\: Voices of the Outer Suburbs Campaign"),
             body = str_remove(body, "^Chifley Electorate\\: International Women's Day|^Health\\: Strokes|^McEwen Electorate\\: Mount Ridley College|^Canning Electorate\\: NBN in Boddington"),
             body = str_remove(body, "^Moreton Electorate\\: Bus Services|^Baillieu, Mr Ted|^Canberra Centenary")) %>% 
      mutate(page.no = c(1734, 1734, 1735, 1736, 1737, 1738, 1738, 1739, 1740, 1741),
             fedchamb_flag = 1,
             in.gov = "",
             first.speech = "",
             time.stamp = str_extract(body, "\\d\\d\\:\\d\\d"),
             electorate = str_extract(body, "(?<=\\()[[:alpha:]]{1,10}"),
             name = c("Coulton, Mark, MP",
                      "Bird, Sharon, MP",
                      "Hunt, Greg, MP",
                      "Husic, Ed, MP",
                      "Frydenberg, Josh, MP",
                      "Mitchell, Rob, MP",
                      "Randall, Don, MP",
                      "Perrett, Graham, MP",
                      "Broadbent, Russell, MP",
                      "Brodtmann, Gai, MP"),
             name.id = c("HWN", "DZP", "00AMV", "91219", "FKL", "M3E", "PK6", "HVP", "MT4", "30540"),
             party = c("Nats", "ALP", "LP", "ALP", "LP", "ALP", "LP", "ALP", "LP", "ALP"),
             party = as.factor(party),
             sub1_flag = 1,
             sub2_flag = 0) %>% 
      select(page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag)
    
    # bind back into all text fed tibble to be split
    all_text_fed <- bind_rows(all_text_fed, extra_sub1) %>% arrange(fedchamb_flag, time.stamp)
    
    # clean up bus_start
    bus_start_fed <- bus_start_fed %>% separate_rows(body, sep="(?=Parkes Electorate\\: Clontarf FoundationMr COULTON)") %>% 
      slice(1)
    
  }
  
  # issue on this day - all these subdebates were nested as part of the business start. so i need to split them out indvidiually and add back into sub1 debate content
  # so they can be split properly
  if (filename == "2014-11-26.xml") {
    
    # split all subdebates, fill in correct information
    # page numbers taken from page it starts on in official release b/c not given in xml
    extra_sub1 <- bus_start_chamb %>% select(body) %>% 
      mutate(body = str_remove(body, "^The SPEAKER \\(Hon\\. Bronwyn Bishop\\) took the chair at 09:00, made an acknowledgement of country and read prayers\\.")) %>% 
      mutate(body = str_remove(body, "^Mr Burke:  ")) %>% 
      mutate(page.no = NA,
             fedchamb_flag = 0,
             in.gov = "",
             first.speech = "",
             time.stamp = "09:00",
             name = c("Burke, Tony, MP"),
             name.id = "DYW",
             party = "ALP",
             party = as.factor(party),
             electorate = "Watson",
             sub1_flag = 1,
             sub2_flag = 0) %>% 
      select(page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag)
    
    # bind back into all text fed tibble to be split
    all_text_chamb <- bind_rows(all_text_chamb, extra_sub1) %>% arrange(fedchamb_flag, time.stamp)
    
    # clean up bus_start
    bus_start_chamb <- bus_start_chamb %>% separate_rows(body, sep="(?=Mr Burke:)") %>% 
      slice(1)
    
  }
  
  # same thing on 2021-10-18
  if (filename == "2021-10-18.xml") {
    
    # split all subdebates, fill in correct information
    # page numbers taken from page it starts on in official release b/c not given in xml
    extra_sub1 <- bus_start_fed %>% select(body) %>% 
      mutate(body = str_remove(body, "^The DEPUTY SPEAKER \\(Mr Rob Mitchell\\) took the chair at 10:30\\.Dunkley Electorate: COVID-19")) %>%
      separate_rows(body, sep="(?=Banks ElectorateMr COLEMAN)") %>% 
      separate_rows(body, sep="(?=Fraser Electorate: COVID-19 Vaccination)") %>% 
      separate_rows(body, sep="(?=Respiratory Disease)") %>% 
      separate_rows(body, sep="(?=Eden-Monaro Electorate: Schools)") %>% 
      separate_rows(body, sep="(?=Cole, Ms Ellie Victoria OAM)") %>% 
      separate_rows(body, sep="(?=Dobell Electorate: COVID-19)") %>%
      separate_rows(body, sep="(?=Sturt Electorate: Sporting Infrastructure)") %>%
      separate_rows(body, sep="(?=COVID-19: Morrison Government)") %>%
      separate_rows(body, sep="(?=Lindsay Electorate: Roads)") %>%
      mutate(body = str_remove(body, "^Banks Electorate|^Fraser Electorate: COVID-19 VaccinationMelbourne: Infrastructure|^Respiratory Disease|^Eden-Monaro Electorate: Schools|^Cole, Ms Ellie Victoria OAMLittle, Ms Rosemary|^Dobell Electorate: COVID-19COVID-19: Vaccination|^Sturt Electorate: Sporting Infrastructure|^COVID-19: Morrison Government|^Lindsay Electorate: Roads")) %>% 
      mutate(page.no = NA,
             fedchamb_flag = 1,
             in.gov = "",
             first.speech = "",
             time.stamp = str_extract(body, "\\d\\d\\:\\d\\d"),
             name = c("Murphy, Peta MP", "Coleman, David MP", "Mulino, Daniel MP", "Hammond, Celia MP", "McBain, Kristy MP", "Leeser, Julian MP", "McBride, Emma MP", "Stevens, James MP", "Chesters, Lisa MP", "McIntosh, Melissa MP"),
             name.id = c("133646", "241067", "132880", "80072", "281988", "109556", "248353", "176304", "249710", "281513"),
             party = c("ALP", "LP", "ALP", "LP", "ALP", "LP", "ALP", "LP", "ALP", "LP"),
             party = as.factor(party),
             electorate = c("Dunkley", "Banks", "Fraser", "Curtin", "Eden-Monaro", "Berowra", "Dobell", "Sturt", "Bendigo", "Lindsay"),
             sub1_flag = 1,
             sub2_flag = 0) %>% 
      select(page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag)
    
    # bind back into all text fed tibble to be split
    all_text_fed <- bind_rows(all_text_fed, extra_sub1) %>% arrange(fedchamb_flag, time.stamp)
    
    # clean up bus_start
    bus_start_fed <- bus_start_fed %>% separate_rows(body, sep="(?=Dunkley Electorate: COVID-19Ms MURPHY)") %>% 
      slice(1)
    
  }

  ######### PREPARING BUSINESS START TO BE ADDED TO MAIN DATA FRAME #########
  # chamber
  if (nrow(bus_start_chamb) > 0) {
    bus_start_chamb <- bus_start_chamb %>%
      rename(time.stamp = start_time) %>%
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d\\:"), paste0("0", time.stamp), time.stamp),
             body = str_replace_all(body, "\\n[[:space:]]{1,20}", " "),
             body = str_replace(body, "\\)\\)(?= took the chair)", "\\)")) %>% 
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
             q_in_writing = 0) %>%
      select(c(page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag, question, answer, q_in_writing))
  }
  
  # federation chamber
  if (nrow(bus_start_fed) > 0) {
    bus_start_fed <- bus_start_fed %>%
      rename(time.stamp = start_time) %>%
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d\\:"), paste0("0", time.stamp), time.stamp),
             body = str_replace_all(body, "\\n[[:space:]]{1,20}", " "),
             body = str_replace(body, "\\)\\)(?= took the chair)", "\\)")) %>% 
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
             q_in_writing = 0) %>%
      select(c(page.no, time.stamp, name, name.id, electorate, party, in.gov, first.speech, body, fedchamb_flag, sub1_flag, sub2_flag, question, answer, q_in_writing))
  }
  
  
  ######### PUTTING EVERYTHING TOGETHER #########
  # all interjection information
  interject <- rbind(interject_sub1, interject_sub2, sub1_q_a_interject)
  
  # in case there are no interjections in this data frame
  if (nrow(interject)>0) {
    interject <- interject %>% 
      arrange(fedchamb_flag, page.no) %>% 
      rowid_to_column("order")
  }
  
  # drop extra vars, add q in writing flag
  # there was one day where there were no questions so the original code was breaking
  if (filename=="2016-08-30.xml"){
    main <- bind_rows(bus_start_chamb, all_text_chamb, bus_start_fed, all_text_fed, q_in_writing) %>% 
      mutate(question = 0,
             answer = 0,
             q_in_writing = 0) %>% 
      select(-itemindex)
    
  } else {
    main <- bind_rows(bus_start_chamb, all_text_chamb, bus_start_fed, all_text_fed, q_in_writing) %>% 
      mutate(question = ifelse(is.na(question), 0, question),
             answer = ifelse(is.na(answer), 0, answer),
             q_in_writing = ifelse(is.na(q_in_writing), 0, q_in_writing)) %>% 
      select(-itemindex)
  }

  # if the name preceding the debate text is "The SPEAKER" and the name doesn't contain "The SPEAKER", paste it in, in brackets
  # this is because sometimes the title of the speaker isn't put directly into the name, and this causes issues later on when trying to fill the ID/party etc for the speaker
  main <- main %>% mutate(name = ifelse(str_detect(body, "^The SPEAKER(?= \\()") & !str_detect(name, "\\(The SPEAKER\\)$|business start"), 
                                        paste0(name, " (", str_extract(body, "^The SPEAKER(?= \\()"), ")"), name))
  
  # another case of above, sometimes The SPEAKER isn't followed by name in brackets
  main <- main %>% mutate(name = ifelse(str_detect(body, "^The SPEAKER") & !str_detect(name, "\\(The SPEAKER\\)|business start"), 
                                        paste0(name, " (", str_extract(body, "^The SPEAKER"), ")"), name))
  
  # if the name is complete and has (The SPEAKER) in it, and the body starts with The SPEAKER:, remove that from the body
  # this is to avoid splitting on "The SPEAKER" later and messing things up
  main <- main %>% mutate(body = ifelse(str_detect(name, "(?<=.{1,20})\\(The SPEAKER\\)$") & str_detect(body, "^The SPEAKER\\:"),
                                        str_remove(body, "^The SPEAKER\\:"), body))
  
  # if the name preceding the debate text is "The DEPUTY SPEAKER" and the name doesn't contain "The DEPUTY SPEAKER", paste it in, in brackets
  main <- main %>% mutate(name = ifelse(str_detect(body, "^The DEPUTY SPEAKER[[:space:]]\\(") & !str_detect(name, "\\(The DEPUTY SPEAKER\\)|business start"),
                                        paste0(name, " (", str_extract(body, "^The DEPUTY SPEAKER"), ")"), name))
  
  # if the name if complete and has (The DEPUTY SPEAKER) in it, and the body starts with The DEPUTY SPEAKER (....):, remove that from the body
  # this is to avoid splitting on "The DEPUTY SPEAKER" later and messing things up
  main <- main %>% mutate(body = ifelse(str_detect(name, "(?<=.{1,20})\\(The DEPUTY SPEAKER\\)$") & str_detect(body, "^The DEPUTY SPEAKER[[:space:]]\\("),
                                        str_remove(body, "^The DEPUTY SPEAKER[[:space:]]\\(.{1,35}\\)\\:"), body))
  
  # another case, where name has deputy speaker title and body starts with title: - to avoid splitting on "The DEPUTY SPEAKER" later and messing things up
  main <- main %>% mutate(body = ifelse(str_detect(name, "(?<=.{1,20})\\(The DEPUTY SPEAKER\\)$") & str_detect(body, "^The DEPUTY SPEAKER\\:[[:space:]]"),
                                        str_remove(body, "^The DEPUTY SPEAKER\\:"), body))
  
  # add space between by leave-I move - for word count purposes
  # two different types of dashes
  main <- main %>% mutate(body = ifelse(str_detect(body, "by leave\\—[[:upper:]]"), str_replace(body, "(?<=by leave)—", "- "), body))
  main <- main %>% mutate(body = ifelse(str_detect(body, "by leave\\-[[:upper:]]"), str_replace(body, "(?<=by leave)-", "- "), body))
  
  # clean up body of debate text (removing name, title in brackets, and time stamp from body)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
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
  
  # case when name and title has time but no punctuation after
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)  (?=by leave)")
  
  ######### SPLITTING INTERJECTIONS #########
  main <- split_interjections_fedchamb(main, interject, filename)
  
  # add flag for divisions
  main <- main %>% mutate(div_flag = ifelse(str_detect(body, "The House divided\\."), 1, 0))
  
  # fix interjection and comments within question and answers that are flagged as questions and answers - these shouldn't be flagged as such
  # this is to ensure we have an equal number of questions and answers
  main <- main %>% group_by(speech_no) %>% 
    mutate(question = case_when(order == min(order) & question==1 ~ 1,
                                order != min(order) & question==1 ~ 0,
                                question==0 ~ 0),
           answer = case_when(order == min(order) & answer==1 ~ 1,
                              order != min(order) & answer==1 ~ 0,
                              answer==0 ~ 0)) %>% 
    ungroup()
  
  # trim any whitespace on either end of the body
  main <- main %>% mutate(body = str_trim(body, side="both"))
  
  # extract times from stage directions like adjournment, where time stamp is missing
  main <- main %>% mutate(time.stamp = ifelse(str_detect(body, "adjourned at \\d{1,2}:\\d\\d$|adjourned at \\d{1,2}\\.\\d\\d$|^Sitting suspended from \\d\\d:\\d\\d") & is.na(time.stamp),
                                              str_extract(body, "(?<=adjourned at )\\d{1,2}:\\d\\d$|(?<=adjourned at )\\d{1,2}\\.\\d\\d$|(?<=Sitting suspended from )\\d\\d:\\d\\d"),
                                              time.stamp)) %>% 
    mutate(time.stamp = ifelse(str_detect(time.stamp, "(?<=\\d)\\.(?=\\d)"),
                               str_replace(time.stamp, "(?<=\\d)\\.", ":"),
                               time.stamp))
  
  # same idea as above, extract times from business start, where time stamp is missing
  main <- main %>% mutate(time.stamp = ifelse(is.na(time.stamp) & name=="business start", 
                                              str_extract(body, "\\d{1,2}\\:\\d\\d [[:lower:]]\\.m\\.|\\d{1,2}\\:\\d\\d [[:lower:]]m|\\d{1,2} [[:lower:]]\\.m\\.|\\d{1,2} [[:lower:]]m|\\d\\d\\:\\d\\d"),
                                              time.stamp)) %>% 
    mutate(time.stamp = str_replace_all(time.stamp, "(?<=\\d)\\.", "\\:"),
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
  
  ######### EXPORT FINAL OUTPUT #########
  # export data-sets to CSV files
  write.csv(main, paste0("/Volumes/Verbatim/output/main-2011-2022-v2/", str_remove(filename, ".xml"), "-main.csv"), row.names = FALSE)
  
  return(main)
  
}

# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/input/")

# grab a couple years
files_get_fedchamb <- files_all %>%
  as_tibble() %>%
  filter(value > "2012-06-28.xml") %>% 
  pull(value)

for(i in 1:length(files_get_fedchamb)){
  parse_hansard_new(files_get_fedchamb[i])
}
