# main committee
# script to parse everything from Hansard
#### good for everything up to 2012-06-28 (last day before main committee was renamed to federation chamber)

# read in necessary packages
library(XML)
library(here)
library(tidyverse)

# define function for splitting interjections, which will be referenced in parse_hansard function

split_interjections_maincomm <- function(main, interject, bus_start){
  
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
      summarise(form1 = paste0(title, "[[:space:]]\\((Dr|Mr|Mrs|Ms)[[:space:]]", first_name, "[[:space:]]", last_name, "\\)"),
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
  interject_names <- c(interject_names, "Opposition members interjecting—", "Government members interjecting—", 
                       "Opposition members", "Honourable members interjecting—", "An opposition member", 
                       "An honourable member", "The SPEAKER", "The DEPUTY SPEAKER", "A government member interjecting—",
                       "The Clerk", "A government member", "Government members", "Honourable members", "Honourable members and senators",
                       "Honourable member and senators")
  
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
      summarise(form1 = paste0(first_name, "[[:space:]]", last_name),
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
    summarise(form1 = paste0(first_name, "[[:space:]]", last_name),
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
  names_use <- str_extract_all(main$body, paste0("(Dr|Mr|Mrs|Ms)[[:space:]]", all_names, "(?=[[:punct:]]|[[:blank:]])", collapse = "|")) %>% 
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
  main <- separate_rows(main, body, sep=paste0("(?=", interject_general, ")(?!", interject_general, "[[:space:]]on[[:space:]]my)(?!", interject_general, "[[:space:]]having)", collapse="|"))
  main <- separate_rows(main, body, sep=paste0("(?=", interject_specific, ")", collapse="|"))

  # separate rows using full names, make sure they aren't preceded by "SPEAKER (" to avoid splitting deputy speaker/speaker titles from name
  # case where there is punctuation right before name (often case with interjections)
  if(length(names_use)>0){
    main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
    
    # case when full name is followed by interjecting, but this wasn't captured in the "interject" data frame
    if (!("interjecting" %in% interject$name)) {
      main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, " interjecting—)", collapse = "|"))
      main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, " interjecting—)", collapse = "|"))
    }
    
    # same as above but case when preceded by a bunch of spaces (this one is specific will need to see if number of spaces is consistent in other transcripts)
    main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]]{20})(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)",  collapse = "|"))
    
    # same as above but case when preceded by single space (this one is specific will need to see if number of spaces is consistent in other transcripts)
    main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)",  collapse = "|"))
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
    
    # split for places like -Ms Flint interjecting-
    main <- separate_rows(main, body, sep=paste0("(?<=\\-)(?<!SPEAKER[[:space:]]\\()(?=", names_use, " interjecting-)", collapse = "|"))
    
    # some splits need to be made at places like JacksonMr JOHN COBB: where name is preceeded by statement ending in lower case letter
    main <- separate_rows(main, body, sep=paste0("(?<!\\:|[[:space:]])(?<=[[:lower:]])(?=", names_use, "\\:)", collapse="|"))
    
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
  other_names <- str_extract_all(main$body, paste0(c("(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,35}(?=\\:)",
                                                     "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\'[[:alpha:]]{0,35}(?=\\:)",
                                                     "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\-[[:alpha:]]{0,35}(?=\\:)",
                                                     "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}[[:space:]][[:upper:]][[:alpha:]]{1,10}(?=\\:)"), collapse="|")) %>%
    na.omit() %>%
    unlist() %>%
    unique() %>%
    as_tibble() %>%
    filter(!(value %in% names_use)) %>%
    filter(!str_detect(value, paste0(c("Speaker", "SPEAKER"), collapse="|"))) %>%
    pull()
  
  # add to this list of other names with others who interjected but weren't captured in main orig or interject dfs
  # filtering out "The Speaker" and any names we already have in interject_general or names_use
  # have the extra filter here for interject general here b/c some names are followed by "interjecting" in that list, and we can check those here
  # since using lookahead of "interjecting", and don't want to double split (cause issues)
  other_names <- str_extract_all(main$body, paste0(c("(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,35}(?= interjecting)",
                                                     "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\'[[:alpha:]]{0,35}(?= interjecting)",
                                                     "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}\\-[[:alpha:]]{0,35}(?= interjecting)",
                                                     "(Dr|Mr|Mrs|Ms)[[:space:]][[:alpha:]]{1,10}[[:space:]][[:upper:]][[:alpha:]]{1,10}(?= interjecting)"), collapse="|")) %>%
    na.omit() %>%
    unlist() %>%
    unique() %>%
    as_tibble() %>%
    filter(!(value %in% names_use)) %>%
    filter(!str_detect(value, paste0(c("Speaker", "SPEAKER"), collapse="|"))) %>%
    filter(!str_detect(paste0(interject_general, collapse="|"), paste0(value, collapse = "|"))) %>% 
    pull() %>% c(., other_names)
  
  # split on these other names
  if (length(other_names)>0){
    main <- separate_rows(main, body, sep=paste0("(?<!\\:|[[:space:]])(?=", other_names, "\\:|", other_names, " interjecting-)", collapse="|"))
  }
  
  #### step 6: clean up names & split on adjournment statements
  if (length(other_names)>0){
    # if body starts with interjection name, clear name, name.id, electorate, and party (else leave as is)
    main <- main %>% mutate(name = ifelse(grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, name),
                            name.id = ifelse(grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, name.id),
                            electorate = ifelse(grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, electorate),
                            party = as.factor(ifelse(grepl(paste0("^", c(interject_names, names_use, other_names), collapse = "|"), body), NA, as.character(party))))
    
    # extract who is interjecting and paste that into name column
    # clean up body by removing name of person interjecting from it as well as whitespace/punctuation at beginning (only keeping those where body is "___ interjecting-")
    main <- main %>% mutate(name = ifelse(is.na(name) & !str_detect(body, "took the chair"), 
                                          str_extract(main$body, paste0("^", c(names_use, other_names, interject_names), collapse = "|")), 
                                          name),
                            body = ifelse(str_detect(body, paste0("^", names_use, collapse = "|")), 
                                          str_remove(body, paste0("^", names_use, "[[:punct:]]", collapse = "|")), 
                                          body),
                            body = ifelse(str_detect(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), "(?![[:space:]]having)", collapse = "|"))
                                          & !str_detect(body, "took the chair"), 
                                          str_remove(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), "(?![[:space:]]interjecting)", collapse = "|")),  
                                          body),
                            body = ifelse(str_detect(body, paste0("^", other_names, collapse = "|")), 
                                          str_remove(body, paste0("^", other_names, "\\:", collapse = "|")), 
                                          body),
                            body = str_replace_all(body, "^[[:blank:]]{0,5}", ""),
                            body = str_replace_all(body, "^[[:punct:]][[:blank:]]{1,5}", ""))
  } else {
    
    # if body starts with interjection name, clear name, name.id, electorate, and party (else leave as is)
    main <- main %>% mutate(name = ifelse(str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, name),
                            name.id = ifelse(str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, name.id),
                            electorate = ifelse(str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, electorate),
                            party = as.factor(ifelse(str_detect(body, paste0("^", c(interject_names, names_use), "(?![[:space:]]{1,2}asked the)", collapse = "|")), NA, as.character(party))))
    
    # extract who is interjecting and paste that into name column
    # clean up body by removing name of person interjecting from it as well as whitespace/punctuation at beginning (only keeping those where body is "___ interjecting-")
    main <- main %>% mutate(name = ifelse(is.na(name), str_extract(main$body, paste0("^", c(interject_names, names_use), collapse = "|")), name),
                            body = ifelse(str_detect(body, paste0("^", names_use, "(?![[:space:]]{1,2}asked the|[[:space:]]interjecting-)", collapse = "|")), 
                                          str_remove(body, paste0("^", names_use, collapse = "|")), 
                                          body),
                            body = ifelse(str_detect(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), collapse = "|")), 
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
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=Debate adjourned)"))
  main <- separate_rows(main, body, sep = "(?<=\n                    )(?=Debate adjourned)")
  
  # some have space and punctuation before
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?=Debate adjourned)"))
  
  # split house adjournment statement
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=House adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=House adjourned at \\d\\d\\:\\d\\d)"))
  
  # split fed. chamb adjournment statement
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=Federation Chamber adjourned at \\d\\d\\:\\d\\d)"))
  
  # split main committee adjournment statement
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]])(?=Main Committee adjourned at \\d\\d\\:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?=Main Committee adjourned at \\d\\d\\:\\d\\d)"))
  
  # split stage notes
  main <- separate_rows(main, body, sep=paste0("(?<=\\-)(?=Debate interrupted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]])(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Question unresolved\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]])(?=Question unresolved\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.)")) 
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Message from the .{0,100}[[:space:]]announced\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\?)(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\?)(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=The member for [[:alpha:]]{0,50} then left the chamber\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Honourable members having stood in their places-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]])(?=Honourable members having stood in their places-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                  Honourable\n                  members\n                  having\n                  stood\n                  in\n                  their\n                  places-)")) %>% 
    mutate(body=str_replace(body, "\n                  Honourable\n                  members\n                  having\n                  stood\n                  in\n                  their\n                  places-",
                            "Honourable members having stood in their places-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                  Honourable members having stood in their places-)")) %>% 
    mutate(body=str_replace(body,"\n                  Honourable members having stood in their places-", "Honourable members having stood in their places-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                    Honourable members having stood in their places-)")) %>% 
    mutate(body=str_replace(body,"\n                    Honourable members having stood in their places-", "Honourable members having stood in their places-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                  The Speaker having seated himself in the chair-)")) %>% 
    mutate(body=str_replace(body,"\n                  The Speaker having seated himself in the chair-", "The Speaker having seated himself in the chair-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                  The bells having been rung and a ballot having been taken-)")) %>% 
    mutate(body=str_replace(body,"\n                  The bells having been rung and a ballot having been taken-", "The bells having been rung and a ballot having been taken-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                    A division having been called and the bells having been rung-)")) %>% 
    mutate(body=str_replace(body,"\n                    A division having been called and the bells having been rung-", "A division having been called and the bells having been rung-"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                  Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d)")) %>% 
    mutate(body=str_remove(body,"^\n                  ")) 
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=\n                  The member for [[:alpha:]]{0,50} then left the chamber.)")) %>% 
    mutate(body=str_remove(body,"\n                  "))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=More than the number of members required by the standing orders having risen in their places-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=The Speaker having seated himself in the chair-)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\!|\\.)(?=Members and senators rising and applauding,.{1,50}left the chamber\\.)"))
  
  # add space after "I move:" statements for tidiness - often no space between colon and statement
  main <- main %>% mutate(body = str_replace_all(body, "I move\\:(?=[[:alpha:]])", "I move\\: "))
  
  # same for "I propose the motion:" statements
  main <- main %>% mutate(body = str_replace_all(body, "I propose the motion\\:(?=[[:alpha:]])", "I propose the motion\\: "))
  
  # if body starts with a colon, remove that
  main <- main %>% mutate(body = ifelse(str_detect(body, "^\\:"), str_remove(body, "^\\:"), body))
  
  stage_notes <- c("Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.", 
                   "Message from the .{0,100}[[:space:]]announced\\.",
                   "Debate interrupted\\.",
                   "Question agreed to\\.",
                   "Question unresolved\\.",
                   "Debate adjourned",
                   "House adjourned at \\d\\d\\:\\d\\d",
                   "Federation Chamber adjourned at \\d\\d\\:\\d\\d",
                   "Leave not granted\\.",
                   "Leave granted\\.",
                   "Honourable members having stood in their places-",
                   "The Speaker having seated himself in the chair-",
                   "The bells having been rung and a ballot having been taken-",
                   "A division having been called and the bells having been rung-",
                   "Sitting suspended from \\d\\d:\\d\\d to \\d\\d:\\d\\d",
                   "The member for [[:alpha:]]{0,50} then left the chamber.",
                   "More than the number of members required by the standing orders having risen in their places-",
                   "Members and senators rising and applauding,.{1,50}left the chamber\\.")
  
  # remove name and other info from rows with stage notes / stage directions
  main <- main %>% mutate(name = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                        "stage direction", name),
                          name.id = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                           NA, name.id),
                          electorate = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                              NA, electorate),
                          party = as.factor(ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                                   NA, as.character(party))))
  
  # commented out b/c causing issues
  # if there's something more than the stage note itself in the body, remove that (ex. House adjourned at 19:59Federation Chamber --> House adjourned at 19:59)
  # main <- main %>% mutate(body = ifelse(name=="stage direction", str_extract(body, paste0(stage_notes, collapse = "|")), body))
  
  # if row contains "took the chair..." need to paste name back into body and change name to business start
  # need if-else here because in cases where we have the business start and the business start is also already included in the body
  # we don't have to have it twice, so only extract these if we don't have the business start already (about to bind it on in next bit of code)
  if (nrow(bus_start)==0){
    main <- main %>% mutate(body = str_remove(body, "^[[:space:]]{0,30}"),
                            body = ifelse(str_detect(body, "took the chair at \\d\\d:\\d\\d"), paste0(name, " ", body), body),
                            name = ifelse(str_detect(body, "took the chair at \\d\\d:\\d\\d"), "business start", name))
  } else {
    main <- main %>% filter(!str_detect(body, "took the chair at \\d\\d:\\d\\d"))
  }
  
  ########## add business start to main
  # NOTE: if there is no business start like 2016-08-30, you'll get a warning msg "Unknown or uninitialised column: `time.stamp`" but just ignore it. (I am for now)
  # sometimes time stamp is missing and if we try to arrange with it and it's NA, it'll cause problems
  if (sum(is.na(bus_start$time.stamp))==0){
    main <- rbind(bus_start, main) %>% 
      group_by(fedchamb_flag) %>% 
      mutate(page.no = ifelse(is.na(page.no), min(page.no, na.rm=TRUE), page.no)) %>% 
      ungroup() %>% 
      arrange(fedchamb_flag)
  } else {
    main <- rbind(bus_start, main) %>% 
      group_by(fedchamb_flag) %>% 
      mutate(page.no = ifelse(is.na(page.no), min(page.no, na.rm=TRUE), page.no)) %>% 
      ungroup() %>% 
      arrange(fedchamb_flag)
  }
  
  #### step 7: add order column
  
  # now that we've split all the rows, let's add an order column so we can keep track of exact order of things
  # need original speech_no column though to keep track of which interjections belong to which speech (will be useful to flag interjections later)
  main <- rowid_to_column(main, "order")
  
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
                               str_extract(name, "\\,[[:blank:]][[:alpha:]]{0,35}"), 
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
  
  # add display names to match those of AusPol list
  # removing titles to match format of AusPol
  # if we have the first and last name, paste it in format for displayName
  name_forms <- name_forms %>% 
    mutate(displayName = case_when(!str_detect(first_name, "[[:space:]]") & str_detect(main_form, "\\,[[:space:]]MP$|[[:space:]]MP$|\\,[[:space:]]MP[[:space:]]\\(The|[[:space:]]\\(The DEPUTY SPEAKER\\)$") ~ 
                                     str_extract(main_form, ".{1,50}(?=\\,[[:space:]]MP|[[:space:]]MP|[[:space:]]\\(The DEPUTY SPEAKER\\))"),
                                   str_detect(first_name, "[[:space:]]") & str_detect(main_form, "\\,[[:space:]]MP$|[[:space:]]MP$|\\,[[:space:]]MP[[:space:]]\\(The|[[:space:]]\\(The DEPUTY SPEAKER\\)$") ~
                                     str_extract(main_form, ".{1,30}(?=[[:space:]].{1,20}\\, MP|[[:space:]].{1,20}[[:space:]]MP|[[:space:]].{1,20}[[:space:]]\\(The DEPUTY SPEAKER\\))"),
                                   !is.na(first_name) & !is.na(last_name) ~ paste0(last_name, ", ", first_name)),
           displayName = ifelse(str_detect(displayName, "\\,$"), str_remove(displayName, "\\,$"), displayName))
  
  
  ######## beginning of NEW STUFF ########
  # keep nrow of name forms at this point so we can do a check after we've merged stuff from the AusPol database that the number of rows hasn't changed
  nrow_name_forms <- nrow(name_forms)
  
  # grab "all" dataset from AusPol package
  all <- AustralianPoliticians::get_auspol('all')
  
  # this will be the master list of names for Hansard 2011-2022. filter out anyone that has died before 2011
  master_list <- all %>% filter(deathDate > "2010-12-31" | is.na(deathDate)) %>% 
    select(c(surname, allOtherNames, firstName, commonName, displayName, title, gender)) %>% 
    mutate(title = ifelse(gender=="male" & is.na(title), "Mr", title),
           title = ifelse(gender=="female" & is.na(title), "Ms", title)) %>% 
    select(-gender) %>% 
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
           title = ifelse(displayName=="Wicks, Lucy", "Mrs|Ms", title)) %>% 
    separate_rows(title, sep="\\|")
  
  # grab list of names where surname and prefix/title exist more than once
  # we need to leave these out when we merge because if multiple people share a last name and title, multiple rows will be added for each person
  # and we will have extra rows and not know what the right name is
  # best to leave these missing and use name ID later to figure out the full name
  repeated_surnames <- master_list %>% group_by(title, last_name) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    ungroup() %>% 
    select(-n)
  
  # people who we don't need to fill
  name_forms_1 <- name_forms %>% filter(!is.na(displayName))
  
  # people who we don't want to fill due to repeated surname and title issue
  name_forms_2 <- name_forms %>% filter(is.na(first_name) & str_detect(last_name, paste0(repeated_surnames$last_name, collapse = "|")))
  
  # people we want to fill using master list
  name_forms_3 <- name_forms %>% filter(is.na(displayName) & !str_detect(last_name, paste0(repeated_surnames$last_name, collapse = "|"))) %>% 
    select(-displayName) %>% 
    left_join(., master_list, by=c("title", "last_name")) %>% 
    select(c(main_form, first_name, last_name, title, displayName))
  
  # bind everything back together and fill in missing titles using groupby displayName
  name_forms_final <- rbind(name_forms_1, name_forms_2, name_forms_3) %>% 
    mutate(first_name = ifelse(is.na(first_name) & !is.na(displayName), str_extract(displayName, "(?<=\\,[[:space:]]).{1,30}"), first_name)) %>% 
    group_by(displayName) %>% 
    fill(title, .direction = "downup") %>% 
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
    mutate(full_name = str_replace_all(full_name, "\u2014", "-"))
  
  # merge name info with name info from main, to create lookup data set which we can use to fill main
  name_lookup <- full_join(name_forms_final, name_info, by = intersect(names(name_forms_final), names(name_info)))
  
  # when we full join, people who are the deputy speaker at some times but not others get the wrong form and full name combo
  # fix this b/c deputy speaker can change during proceedings
  name_lookup <- name_lookup %>% filter(str_detect(main_form, "\\(The DEPUTY SPEAKER|The DEPUTY SPEAKER \\(") & str_detect(full_name, "\\(The DEPUTY SPEAKER\\)")|
                                          !str_detect(main_form, "\\(The DEPUTY SPEAKER|The DEPUTY SPEAKER \\(") & !str_detect(full_name, "\\(The DEPUTY SPEAKER\\)")|
                                          str_detect(first_name, "The"))
  
  # clean things up a bit (multiple rows for same person w/ some missing, fill in gaps)
  name_lookup <- name_lookup %>% 
    group_by(last_name) %>% 
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
  
  # check number of rows didn't change from merge
  stopifnot(nrow_main_before == nrow(main))  
  
  ##### taking this out bc might cause issues not sure
  # also sometimes there isn't a full version of the deputy speaker's name in the speech, so I'm going to fill those with the previous available full name of the deputy speaker
  # added a flag to make filling easier, dropped it after
  # main <- main %>% mutate(deputy_flag = ifelse(str_detect(name, "The DEPUTY SPEAKER|The Deputy Speaker"), 1, 0)) %>%
  #   group_by(speech_no) %>%
  #   mutate(name = ifelse(str_detect(name, "^The DEPUTY SPEAKER$|^The Deputy Speaker$"), NA, name)) %>%
  #   ungroup() %>%
  #   group_by(deputy_flag) %>%
  #   fill(name, .direction = "down") %>%
  #   ungroup() %>%
  #   select(-deputy_flag)
  
  # fill electorate, name.id and party info
  main <- main %>% group_by(name) %>% 
    fill(c(name.id, electorate, party), .direction = "downup") %>% 
    ungroup() %>% distinct()
  
  # fix up "___ members interjecting-" names for simplicity
  main <- main %>% mutate(name = ifelse(str_detect(name, "interjecting-"), str_remove(name, "[[:space:]]interjecting-"), name))
  
  #### step 10: flag for interjections
  # group by speech_no, and if the name is not equal to the first name w/ that speech_no, or the speaker, it is an interjection
  main_final <- main %>% group_by(speech_no) %>% arrange(order) %>% 
    mutate(interject = case_when(order == min(order) ~ 0,
                                 str_detect(name, "The SPEAKER|The DEPUTY SPEAKER|stage direction") ~ 0)) %>% 
    ungroup() %>% 
    group_by(name, speech_no) %>%
    fill(interject, .direction = "down") %>% 
    ungroup() %>% 
    mutate(interject = ifelse(is.na(interject), 1, interject)) %>% 
    select(-c(in.gov, first.speech))
  
  return(main_final)
}

# define script as function with filename argument
parse_hansard_maincomm <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("/Volumes/Verbatim/input/", filename))
  
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
               start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"))
    } else if ("para" %in% names(bus_start_chamb)) {
      bus_start_chamb <- bus_start_chamb %>% 
        rename(date = day.start,
               body = para) %>% 
        mutate(date = as.Date(date),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}[:space:][:lower:][:lower:]")) %>% 
        select(-separator)
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
  
  ######### DEBATE SPEECH #########
  debate_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/speech/talk.start/talker")),
                               xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
    mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
  
  ######### SUB-DEBATE 1 #########
  # store sub-debate 1 information & text in tibble, correct variable class, add flag for federation chamber
  sub1_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.text"))) %>%
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # store sub-debate 1 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
  sub1_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
    mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
  
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
  
  # store sub-debate 2 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
  sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
    mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
  
  # same idea as for info, nesting changes, want to account for this so we don't miss anything
  if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.start/talker")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.text"))))) > 0) {
    
    sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.start/talker")),
                               xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.2/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
             party = {if ("party" %in% names(.)) as.factor(party) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL}) %>% 
      mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL}) %>% 
      rbind(., sub2_speech_chamb)
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
  
  ######### QUESTIONS AND ANSWERS #########
  if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker"))) > 0) {
    # store questions in tibble, correct variable class, add flag for question/answer, extract time
    sub1_q <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.start/talker")),
                xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/question/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             party = as.factor(party),
             question = 1,
             answer = 0,
             time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp))
    
    # store answers in tibble, correct variable class, add flag for question/answer, extract time
    sub1_a <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.start/talker")),
                xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/answer/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             party = as.factor(party),
             question = 0,
             answer = 1,
             time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp))
    
    # noticed presence of this node ending in 2014
    # questions in writing
    if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.start/talker")))>0){
      sub1_q_writing <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.text"))) %>% 
        as_tibble() %>% 
        mutate(page.no = as.numeric(page.no),
               party = as.factor(party),
               time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
        mutate(question = ifelse(str_detect(body, "The answer to the .{1,30} question is as follows\\:|has provided the following answer to the honourable member's question\\:"), 0, 1),
               answer = ifelse(str_detect(body, "The answer to the .{1,30} question is as follows\\:|has provided the following answer to the honourable member's question\\:"), 1, 0)) %>% 
        mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp)) 
      # arrange(page.no, time.stamp) 
      
      # add any wrongly nested answers (in writing) into answer dataframe
      sub1_a_writing <- sub1_q_writing %>% filter(answer==1)
      
      # remove any wrongly nested answers from question in writing dataframe
      sub1_q_writing <- sub1_q_writing %>% filter(answer!=1)
      
      # need separate if-else for answers because sometimes the answers are actually embedded in the question node above, so nrow may be 0
      if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.start/talker")))>0){
        sub1_a_writing <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.start/talker")),
                            xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.text"))) %>% 
          as_tibble() %>% 
          mutate(page.no = as.numeric(page.no),
                 party = as.factor(party),
                 question = 0,
                 answer = 1,
                 time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
          mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp)) %>% 
          rbind(., sub1_a_writing) %>% 
          arrange(page.no)
      } 
    } else {
      sub1_q_writing <- tibble()
      sub1_a_writing <- tibble()
    }
    
    # list and map row ID and bind rows to maintain correct ordering of q and a
    # merge questions and answers, add flag for sub-debate 1 and 2, and federation chamber (always 0 b/c only have question time in chamber)
    sub1_q_a_writing <- lst(sub1_q_writing, sub1_a_writing) %>% 
      map(rowid_to_column) %>% 
      bind_rows() %>% 
      arrange(rowid) %>% 
      select(-rowid) %>% 
      mutate(sub1_flag = 1, 
             sub2_flag = 0,
             fedchamb_flag = 0,
             q_in_writing = 1) 
    
    # not adding q in writing flag here b/c will add later (for rbind purposes)
    sub1_q_a <- lst(sub1_q, sub1_a) %>% 
      map(rowid_to_column) %>% 
      bind_rows() %>% 
      arrange(rowid) %>% 
      select(-rowid) %>% 
      mutate(sub1_flag = 1, 
             sub2_flag = 0,
             fedchamb_flag = 0)
    
    ######### QUESTION AND ANSWER INTERJECTIONS #########
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
  } else if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.start/talker")))>0) {
    
    sub1_q_writing <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.start/talker")),
                        xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/question/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             party = as.factor(party),
             time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
      mutate(question = ifelse(str_detect(body, "The answer to the .{1,30} question is as follows\\:|has provided the following answer to the honourable member's question\\:"), 0, 1),
             answer = ifelse(str_detect(body, "The answer to the .{1,30} question is as follows\\:|has provided the following answer to the honourable member's question\\:"), 1, 0)) %>% 
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp)) 
    #arrange(page.no, time.stamp)
    
    # add any wrongly nested answers into answer dataframe
    sub1_a_writing <- sub1_q_writing %>% 
      filter(answer==1) 
    #arrange(time.stamp, page.no)
    
    # remove any wrongly nested answers from question dataframe
    sub1_q_writing <- sub1_q_writing %>% filter(answer!=1)
    
    # need separate if-else for answers because sometimes the answers are actually embedded in the question node above, so nrow may be 0
    if (nrow(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.start/talker")))>0){
      sub1_a_writing <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.start/talker")),
                          xmlToDataFrame(node=getNodeSet(hansard_xml, "//answers.to.questions/debate/subdebate.1/answer/talk.text"))) %>% 
        as_tibble() %>% 
        mutate(page.no = as.numeric(page.no),
               party = as.factor(party),
               question = 0,
               answer = 1,
               time.stamp = str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d")) %>% 
        mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp)) %>% 
        rbind(., sub1_a_writing) %>% 
        arrange(page.no)
    } else {
      sub1_a_writing <- tibble()
    }
    
    # list and map row ID and bind rows to maintain correct ordering of q and a
    # merge questions and answers, add flag for sub-debate 1 and 2, and federation chamber (always 0 b/c only have question time in chamber)
    sub1_q_a_writing <- lst(sub1_q_writing, sub1_a_writing) %>% 
      map(rowid_to_column) %>% 
      bind_rows() %>% 
      arrange(rowid) %>% 
      select(-rowid) %>% 
      mutate(sub1_flag = 1, 
             sub2_flag = 0,
             fedchamb_flag = 0,
             q_in_writing = 1)  
    
    sub1_q_a <- tibble()
    
    ######### QUESTION AND ANSWER INTERJECTIONS #########
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
    sub1_q_a <- tibble()
    sub1_q_a_interject <- tibble()
    sub1_q_a_writing <- tibble()
  }  
  
  #################### MAIN COMMITTEE ####################
  # use if-else statement to ensure code works for Hansard with and without main committee
  # check that there is a business start to know if main committee exists
  
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/business.start")))) > 0) {
    
    ######### BUSINESS START #########
    # store business start in tibble, add flag for main committee, extract date and start time
    # in rare cases there is no business start, so add if-else in case of this
    bus_start_maincomm <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/business.start")), 
                                 fedchamb_flag = 1)
    
    if ("body" %in% names(bus_start_maincomm)){
      bus_start_maincomm <- bus_start_maincomm %>% 
        mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
               date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
               body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"),
               start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"))
    } else if ("para" %in% names(bus_start_maincomm)) {
      bus_start_maincomm <- bus_start_maincomm %>% 
        rename(date = day.start,
               body = para) %>% 
        mutate(date = as.Date(date),
               day_of_week = strftime(date, "%A"),
               start_time = str_extract(body, "[:digit:]{1,2}[:space:][:lower:][:lower:]"))
    }
    
    # merge into single business start tibble
    bus_start <- rbind(bus_start_chamb, bus_start_maincomm)
    
    ######### DEBATE INFORMATION #########
    # store debate information in tibble, correct variable class, add flags for sub-debate 1 and 2, and main committee
    debate_info_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/debateinfo")),
                                  xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/debate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             fedchamb_flag = 1,
             sub1_flag = 0,
             sub2_flag = 0)
    
    # merge chamber & main committee tibbles into single debate information tibble
    debate_info <- rbind(debate_info_chamb, debate_info_maincomm)
    
    ######### DEBATE SPEECH #########
    # store debate speech that aren't part of sub-debates
    debate_speech_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/speech/talk.start/talker")),
                                    xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
             party = {if("party" %in% names(.)) as.factor(party) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
      mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
    
    # merge chamber and main committee tibbles together, flag for question and answer, and which sub-debate, arrange by main committee flag, page and time
    debate_speech <- rbind(debate_speech_chamb, debate_speech_maincomm)
    
    # need if statement in case debate_speech is an empty tibble
    if (nrow(debate_speech)>0) {
      debate_speech <- debate_speech %>% 
        arrange(fedchamb_flag, time.stamp, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 0,
               question = 0,
               answer = 0)
    }
    
    ######### SUB-DEBATE 1 #########
    # store sub-debate 1 information & text in tibble, correct variable class, add flag for main committee
    sub1_info_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebateinfo")),
                                xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.text"))) %>%
      as_tibble() %>% 
      mutate(page.no = as.numeric(page.no),
             fedchamb_flag = 1)
    
    # store sub-debate 1 talker info & speech in tibble, correct variable class, add flag for main committee, extract time
    sub1_speech_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/speech/talk.start/talker")),
                                  xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             time.stamp = {if("time.stamp" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
             party = {if("party" %in% names(.)) as.factor(party) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
      mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
    
    # merge chamber and main committee tibbles together, flag for which sub-debate, arrange by main committee flag, and page
    sub1_info <- rbind(sub1_info_chamb, sub1_info_maincomm) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0)
    
    # merge chamber and main committee tibbles together, flag for question and answer, and which sub-debate, arrange by main committee flag, page and time
    sub1_speech <- rbind(sub1_speech_chamb, sub1_speech_maincomm) %>% 
      arrange(fedchamb_flag, time.stamp, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0,
             question = 0,
             answer = 0)
    
    ######### SUB-DEBATE 2 #########
    # if-else statements are included because sub-debate 2 is not neccessarily always present in main committee
    # store sub-debate 2 information & text in tibble, correct variable class, add flag for main committee
    sub2_info_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                                xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
    
    # same idea as for chamber, nesting changes, want to account for this so we don't miss anything
    if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebateinfo")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebate.text"))))) > 0) {
      
      sub2_info_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebateinfo")),
                                  xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/subdebate.text"))) %>% 
        as_tibble() %>% 
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
               fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
        rbind(., sub2_info_maincomm)
    }
    
    # store sub-debate 2 talker info & speech in tibble, correct variable class, add flag for main committee, extract time
    sub2_speech_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                                  xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
             party = {if("party" %in% names(.)) as.factor(party) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>% 
      mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL})
    
    # same idea as for chamber, nesting changes, want to account for this so we don't miss anything
    if (nrow(as_tibble(cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/speech/talk.text"))))) > 0) {
      
      sub2_speech_maincomm <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/speech/talk.start/talker")),
                                    xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.2/speech/talk.text"))) %>% 
        as_tibble() %>% 
        mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
               time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
               party = {if ("party" %in% names(.)) as.factor(party) else NULL},
               fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL}) %>%
        mutate(time.stamp = {if("time.stamp" %in% names(.)) ifelse(str_detect(time.stamp, "^\\d:\\d\\d"), paste0("0", time.stamp), time.stamp) else NULL}) %>% 
        rbind(., sub2_speech_maincomm)
    }
    
    # if sub-debate 2 exists for chamber and/or main committee, merge and process
    if (nrow(sub2_info_chamb > 0) | nrow(sub2_info_maincomm > 0)) {
      # merge chamber and main committee tibbles together, flag for which sub-debate, arrange by page
      sub2_info <- rbind(sub2_info_chamb, sub2_info_maincomm) %>% 
        arrange(fedchamb_flag, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 1)
      
      # merge chamber and main committee tibbles together, flag for question and answer, and which sub-debate, arrange by page and time
      sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_maincomm) %>% 
        arrange(fedchamb_flag, time.stamp, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 1,
               question = 0,
               answer = 0)
    } else {
      # else just bind, resulting in empty tibbles
      sub2_info <- rbind(sub2_info_chamb, sub2_info_maincomm)
      sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_maincomm)
    }
    
    
    ######### SPEECH INTERJECTIONS #########
    # store sub-debate 1 speech interjections in tibble, correct variable class, add flag for main committee
    # use if-else statements in case there are no interjections
    sub1_interject_maincomm <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/speech/interjection/talk.start/talker")),
                                 xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/speech/interjection/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
    
    # store sub-debate 2 speech interjections in tibble, correct variable class, add flag for main committee
    # use if-else statements in case there are no interjections, or there is no sub-debate 2
    sub2_interject_maincomm <- c(xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.start/talker")),
                                 xmlToDataFrame(node=getNodeSet(hansard_xml, "//maincomm.xscript/debate/subdebate.1/subdebate.2/speech/interjection/talk.text"))) %>% 
      as_tibble() %>% 
      mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
             fedchamb_flag = {if("page.no" %in% names(.)) 1 else NULL})
    
    # if else statement used in case of no interjections and/or sub-debate 2
    # merge chamber and main committee tibbles, add flags for question, answer, and each sub-debate, arrange by page number
    if (nrow(sub1_interject_chamb) > 0 | nrow(sub1_interject_maincomm) > 0) {
      interject_sub1 <- rbind(sub1_interject_chamb, sub1_interject_maincomm) %>% 
        arrange(fedchamb_flag, page.no) %>% 
        mutate(question = 0, 
               answer = 0, 
               sub1_flag = 1, 
               sub2_flag = 0)
    } else {
      interject_sub1 <- rbind(sub1_interject_chamb, sub1_interject_maincomm)
    }
    
    # same thing for sub-debate 2 interjections
    if (nrow(sub2_interject_chamb) > 0 | nrow(sub2_interject_maincomm) > 0) {
      interject_sub2 <- rbind(sub2_interject_chamb, sub2_interject_maincomm) %>% 
        arrange(fedchamb_flag, page.no) %>% 
        mutate(question = 0, 
               answer = 0, 
               sub1_flag = 0, 
               sub2_flag = 1)
    } else {
      interject_sub2 <- rbind(sub2_interject_chamb, sub2_interject_maincomm)
    }
    
  } else {
    # modify all code for case when there is no main committee
    # re-name chamber business start and debate info
    bus_start <- bus_start_chamb
    debate_info <- debate_info_chamb
    
    # need if-else here in case debate speech has no rows, will get error when trying to arrange
    if (nrow(debate_speech_chamb) > 0) {
      debate_speech <- debate_speech_chamb %>% 
        arrange(fedchamb_flag, time.stamp, page.no) %>% 
        mutate(sub1_flag = 1,
               sub2_flag = 0,
               question = 0,
               answer = 0)
    } else {
      debate_speech <- debate_speech_chamb
    }
    
    
    # use if statement in case no sub-debate 1 in chamber (ex: 2014-07-08)
    if (nrow(sub1_info_chamb)>0) {
      # rename sub-debate 1 chamber info, arrange by page number, add flags for which sub-debate
      sub1_info <- sub1_info_chamb %>% 
        arrange(fedchamb_flag, page.no) %>% 
        mutate(sub1_flag = 1,
               sub2_flag = 0)
      
      # rename sub-debate 1 chamber speech, flag for question and answer, and which sub-debate, arrange by page, main committee flag and time
      sub1_speech <- sub1_speech_chamb %>% 
        arrange(fedchamb_flag, time.stamp, page.no) %>% 
        mutate(sub1_flag = 1,
               sub2_flag = 0,
               question = 0,
               answer = 0) 
    } else {
      # else rename and return empty tibbles
      sub1_info <- sub1_info_chamb
      sub1_speech <- sub1_speech_chamb
    }
    
    # use if statement in case no sub-debate 2 in chamber
    if (nrow(sub2_info_chamb > 0)) {
      # rename sub-debate 2 chamber info, arrange by page number, add flags for which sub-debate
      sub2_info <- sub2_info_chamb %>% 
        arrange(fedchamb_flag, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 1)
      
      # rename sub-debate 2 chamber speech, flag for question and answer, and which sub-debate, arrange by page and time
      sub2_speech <- sub2_speech_chamb %>% 
        arrange(fedchamb_flag, time.stamp, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 1,
               question = 0,
               answer = 0)
    } else {
      # else rename and return empty tibbles
      sub2_info <- sub2_info_chamb
      sub2_speech <- sub2_speech_chamb
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
      mutate(time.stamp = ifelse(str_detect(time.stamp, "^\\d\\:"), paste0("0", time.stamp), time.stamp)) %>% 
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
  # before doing this, sometimes sub1_info has an extra variable called "id.no" (2012-09-11), so want to add if-else in case
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
  
  # all debate text
  # arrange was causing issues with q in writing ordering b/c generally no time stamp so added in q/a in writing after
  main <- rbind(debate_speech, sub1_speech, sub2_speech, sub1_q_a) %>% 
    arrange(fedchamb_flag, page.no, time.stamp) %>% 
    mutate(q_in_writing = 0) %>% 
    rbind(., sub1_q_a_writing) %>% 
    arrange(page.no)
  
  # if the name preceding the debate text is "The SPEAKER" and the name doesn't contain "The SPEAKER", paste it in, in brackets
  # this is because sometimes the title of the speaker isn't put directly into the name, and this causes issues later on when trying to fill the ID/party etc for the speaker
  main <- main %>% mutate(name = ifelse(str_detect(body, "^The SPEAKER(?= \\()") & !str_detect(name, "\\(The SPEAKER\\)$"), 
                                        paste0(name, " (", str_extract(body, "^The SPEAKER(?= \\()"), ")"), name))
  
  # another case of above, sometimes The SPEAKER isn't followed by name in brackets
  main <- main %>% mutate(name = ifelse(str_detect(body, "^The SPEAKER") & !str_detect(name, "\\(The SPEAKER\\)"), 
                                        paste0(name, " (", str_extract(body, "^The SPEAKER"), ")"), name))
  
  # if the name is complete and has (The SPEAKER) in it, and the body starts with The SPEAKER:, remove that from the body
  # this is to avoid splitting on "The SPEAKER" later and messing things up
  main <- main %>% mutate(body = ifelse(str_detect(name, "(?<=.{1,20})\\(The SPEAKER\\)$") & str_detect(body, "^The SPEAKER\\:"),
                                        str_remove(body, "^The SPEAKER\\:"), body))
  
  # if the name preceding the debate text is "The DEPUTY SPEAKER" and the name doesn't contain "The DEPUTY SPEAKER", paste it in, in brackets
  main <- main %>% mutate(name = ifelse(str_detect(body, "^The DEPUTY SPEAKER[[:space:]]\\(") & !str_detect(name, "\\(The DEPUTY SPEAKER\\)"),
                                        paste0(name, " (", str_extract(body, "^The DEPUTY SPEAKER"), ")"), name))
  
  # if the name if complete and has (The DEPUTY SPEAKER) in it, and the body starts with The DEPUTY SPEAKER (....):, remove that from the body
  # this is to avoid splitting on "The DEPUTY SPEAKER" later and messing things up
  main <- main %>% mutate(body = ifelse(str_detect(name, "(?<=.{1,20})\\(The DEPUTY SPEAKER\\)$") & str_detect(body, "^The DEPUTY SPEAKER[[:space:]]\\("),
                                        str_remove(body, "^The DEPUTY SPEAKER[[:space:]]\\(.{1,35}\\)\\:"), body))
  
  # clean up body of debate text (removing name, title in brackets, and time stamp from body)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # same as above but in case time stamp has single digit to begin (e.g. 9:01 instead of 09:01)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{1}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when there is no title, just name and time stamp to be removed from body
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when there is no title, just name and time stamp to be removed from body, with spaces before time stamp
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                  \\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when name is followed by new line and 18 spaces
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                  \\(.{0,250}\\)\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when name and title is followed by new line and 20 spaces
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\\(.{0,250}\\)\n                    \\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when name is followed by newline and 20 spaces, then title and timestamp
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}\n                    \\(.{0,250}\\)[[:space:]]\\([:digit:]{2}:[:digit:]{2}\\)\\:[[:space:]]{0,5}")
  
  ######### SPLITTING INTERJECTIONS #########
  main <- split_interjections_maincomm(main, interject, bus_start)
  
  ######### EXPORT FINAL OUTPUT #########
  # export data-sets to CSV files
  write.csv(toc, paste0("/Volumes/Verbatim/output/toc/", str_remove(filename, ".xml"), "-toc.csv"), row.names = FALSE)
  write.csv(main, paste0("/Volumes/Verbatim/output/main/", str_remove(filename, ".xml"), "-main.csv"), row.names = FALSE)
}


# grab list of all file names
files_all <- list.files("/Volumes/Verbatim/input/")

# grab a couple years
files_get_maincomm <- files_all %>%
  as_tibble() %>%
  filter(str_detect(value, "^2011-|^2012-")) %>%
  filter(value >= "2011-05-10.xml" & value <= "2012-06-28.xml") %>% 
  pull(value)

for(i in 1:length(files_get_maincomm)){
  parse_hansard_maincomm(files_get_maincomm[i])
}
