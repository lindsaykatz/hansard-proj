# script to parse everything from Hansard

# read in necessary packages
library(XML)
library(here)
library(tidyverse)

# define function for splitting interjections, which will be referenced in parse_hansard function

split_interjections <- function(main, interject){
  
  #### step 1: add index to make it easier to see where interjections happened 
  main <- rowid_to_column(main, "index")
  
  # keep original/unmodified version of main to grab names/name.id/electorate/party info from later on
  main_orig <- main
  
  #### step 2: we need a list of names or phrases to separate speech based on ("interject_names")
  
  # start by re-formatting those with title of speaker or deputy speaker
  # paste in a few forms to be safe (w/ and w/out first name), add unique() in case of repeats
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
              form4 = paste0(title, "\n                  ", "\\((Dr|Mr|Mrs|Ms)[[:space:]]", last_name, "\\)")) %>%
    pivot_longer(form1:form4, names_to = "type", values_to = "form") %>%
    select(form) %>% 
    unique() %>% 
    pull()
  
  # next add the ones in form like "Ms Ryan interjecting-"
  interject_names <- interject %>% 
    select(name) %>% 
    unique() %>% 
    filter(str_detect(name, "interject")) %>% 
    pull() %>%
    c(., interject_names)
  
  # finally, add some other phrases that aren't generally captured as interjections in the XML (but where we'd like to split)
  interject_names <- c(interject_names, "Opposition members interjecting—", "Government members interjecting—", 
                       "Opposition members", "Honourable members interjecting—", "An opposition member", 
                       "An honourable member", "The SPEAKER", "The DEPUTY SPEAKER", "A government member interjecting—",
                       "The Clerk", "A government member")
  
  #### step 3: we need another list of names that we haven't grabbed yet to separate speech based on ("all_names")
  
  # start list of all names from interjections
  # accounting for people with two or three first names, sometimes only the first is used
  # accounting for people with punctuation in last name (e.g. Chandler-Mather) or two words in last name (e.g. van Mansen)
  # accounting for people with last name like McInthosh (special capitalization)
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
    pivot_longer(form1:form8, names_to = "type", values_to = "form") %>% 
    na.omit() %>% 
    unique() %>% 
    pull(form)
  
  # now, add to all_names list using names of speakers from main data frame
  all_names <-  main_orig %>% 
    select(name) %>% 
    unique() %>% 
    mutate(name = str_remove(name, "[:space:]MP.{0,35}$"),
           name = ifelse(str_detect(name, ",$"), str_replace_all(name, ",$", ""), name),
           first_name = str_extract(name, ",[:space:][:alpha:]{0,35}$|,[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}$|,[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}$"),
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
  names_use <- str_extract_all(main$body, paste0("(Dr|Mr|Mrs|Ms)[[:space:]]", all_names, collapse = "|")) %>% 
    na.omit() %>% 
    unlist() %>% 
    unique()
  
  #### step 5: split rows
  
  # use interject_names list to separate rows
  main <- separate_rows(main, body, sep=paste0(paste0("(?=", interject_names, ")", collapse="|")))
  
  # separate rows using full names, make sure they aren't preceded by "SPEAKER (" to avoid splitting deputy speaker/speaker titles from name
  # case where there is punctuation right before name (often case with interjections)
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
  
  # same as above but case when preceded by a bunch of spaces (this one is specific will need to see if number of spaces is consistent in other transcripts)
  main <- separate_rows(main, body, sep=paste0("(?<=[[:space:]]{20})(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)",  collapse = "|"))
  
  # same as above but case when preceded by single space (this one is specific will need to see if number of spaces is consistent in other transcripts)
  main <- separate_rows(main, body, sep=paste0("(?<=[[:punct:]][[:space:]])(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)",  collapse = "|"))
  
  # if body starts with a colon, remove that
  main <- main %>% mutate(body = ifelse(str_detect(body, "^\\:"), str_remove(body, "^\\:"), body))
  
  # deal with stuff like "Mr Sukkar interjecting—Ms RISHWORTH" not splitting b/c of em dash (not captured in [:punct:] character class)
  # change em dash to regular dash for easier processing later on
  main <- main %>% mutate(body = str_replace_all(body, "\u2014", "-")) %>% separate_rows(body, sep=paste0("(?<=interjecting[[:punct:]])(?=", names_use,")", collapse = "|"))
  
  # special case where interjecting- is followed by new line and a bunch of spaces
  main <- separate_rows(main, body, sep=paste0("(?<=interjecting[[:punct:]]\\n                  )(?=", names_use,")", collapse = "|"))
  
  # some splits need to be made at places like -Mr Dutton: which aren't being captured already (b/c em dash hadn't been changed yet)
  main <- separate_rows(main, body, sep=paste0("(?<=\\-)(?<!SPEAKER[[:space:]]\\()(?=", names_use, "\\:)", collapse = "|"))
  
  # also need to change the em dash in the interject names list before flagging interjections below
  interject_names <- interject_names %>% 
    as_tibble() %>% 
    mutate(value = str_replace_all(value, "\u2014", "-")) %>% 
    pull()
  
  #### step 6: clean up names & split on adjournment statements

  # if body starts with interjection name, clear name, name.id, electorate, and party (else leave as is)
  main <- main %>% mutate(name = ifelse(grepl(paste0("^", c(interject_names, names_use), collapse = "|"), body), NA, name),
                          name.id = ifelse(grepl(paste0("^", c(interject_names, names_use), collapse = "|"), body), NA, name.id),
                          electorate = ifelse(grepl(paste0("^", c(interject_names, names_use), collapse = "|"), body), NA, electorate),
                          party = as.factor(ifelse(grepl(paste0("^", c(interject_names, names_use), collapse = "|"), body), NA, as.character(party))))
  
  # extract who is interjecting and paste that into name column
  # clean up body by removing name of person interjecting from it as well as whitespace/punctuation at beginning (only keeping those where body is "___ interjecting-")
  main <- main %>% mutate(name = ifelse(is.na(name), str_extract(main$body, paste0("^", c(interject_names, names_use), collapse = "|")), name),
                          body = ifelse(str_detect(body, paste0("^", names_use, collapse = "|")), 
                                        str_remove(body, paste0("^", names_use, "[[:punct:]]", collapse = "|")), 
                                        body),
                          body = ifelse(str_detect(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), collapse = "|")), 
                                        str_remove(body, paste0("^", str_subset(interject_names, pattern="interjecting", negate = TRUE), "(?![[:space:]]interjecting)", collapse = "|")),  
                                        body),
                          body = str_replace_all(body, "^[[:blank:]]{0,5}", ""),
                          body = str_replace_all(body, "^[[:punct:]][[:blank:]]{1,5}", ""))
  
  # now, to clean these up a bit
  # if row is separated at "The SPEAKER", paste the full name of the speaker in name column
  # there is only one speaker so this should always work since using na.omit and unique
  main <- main %>% mutate(name = ifelse(str_detect(main$name, "^The SPEAKER$"), unique(na.omit(str_extract(main$name, ".{0,60}\\(The SPEAKER\\)$"))), name))
  
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
  
  # split stage notes
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.[[:space:]])(?=Question agreed to\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.)")) 
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Message from the .{0,100}[[:space:]]announced\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Leave not granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Leave granted\\.)"))
  main <- separate_rows(main, body, sep=paste0("(?<=\\.)(?=Honourable members having stood in their places-)"))
  
  # add space after "I move:" statements for tidiness - often no space between colon and statement
  main <- main %>% mutate(body = str_replace_all(body, "I move\\:(?=[[:alpha:]])", "I move\\: "))
  
  # same for "I propose the motion:" statements
  main <- main %>% mutate(body = str_replace_all(body, "I propose the motion\\:(?=[[:alpha:]])", "I propose the motion\\: "))
  
  # if body starts with a colon, remove that
  main <- main %>% mutate(body = ifelse(str_detect(body, "^\\:"), str_remove(body, "^\\:"), body))
  
  stage_notes <- c("Bill read a [[:alpha:]]{0,10}[[:space:]]time\\.", 
                   "Message from the .{0,100}[[:space:]]announced\\.", 
                   "Question agreed to\\.",
                   "Debate adjourned",
                   "House adjourned at \\d\\d\\:\\d\\d",
                   "Federation Chamber adjourned at \\d\\d\\:\\d\\d",
                   "Leave not granted\\.",
                   "Leave granted\\.",
                   "Honourable members having stood in their places-")
  
  # remove name and other info from rows with stage notes / stage directions
  main <- main %>% mutate(name = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                        "stage direction", name),
                          name.id = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                           NA, name.id),
                          electorate = ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                              NA, electorate),
                          party = as.factor(ifelse(str_detect(body, paste0("^", stage_notes, collapse = "|")), 
                                                   NA, as.character(party))))
  
  # if there's something more than the stage note itself in the body, remove that (ex. House adjourned at 19:59Federation Chamber --> House adjourned at 19:59)
  main <- main %>% mutate(body = ifelse(name=="stage direction", str_extract(body, paste0(stage_notes, collapse = "|")), body))
  
  # if row contains "took the chair..." need to paste name back into body and change name to stage direction
  main <- main %>% mutate(body = str_remove(body, "^[[:space:]]{0,30}"),
                          body = ifelse(str_detect(body, "took the chair at \\d\\d:\\d\\d"), paste0(name, " ", body), body),
                          name = ifelse(str_detect(body, "took the chair at \\d\\d:\\d\\d"), "stage direction", name))
  
  #### step 7: add order column
  
  # now that we've split all the rows, let's add an order column so we can keep track of exact order of things
  # need original index column though to keep track of which interjections belong to which speech (will be useful to flag interjections later)
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
           last_name = ifelse(is.na(first_name), name, str_extract(name, "(?<=[[:blank:]])[[:alpha:]]{0,35}")),
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
           first_name = str_extract(name, "[[:alpha:]]{0,35}[[:blank:]]"),                 # note that this only works for one first name which I think is fine b/c Catherine Fiona King is titled Ms Catherine King, will keep an eye on this
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
  
  # fill in missing first names based on matching last name
  name_forms <- name_forms %>% group_by(last_name) %>% 
    fill(first_name, .direction = "downup") %>% 
    ungroup()
  
  # combine main forms into one cell
  name_forms <- name_forms %>% group_by(last_name, first_name) %>% 
    mutate(main_form = paste0(main_form, collapse="|")) %>% 
    ungroup() %>% 
    unique()
  
  # now, want to grab correct name id, electorate and party for interjectors (for which we have this info)
  # obviously wont have it for "A government member" etc.
  
  # get names for people titled The Speaker / The Deputy Speaker
  name_info <- interject %>%
    select(c(name, name.id, electorate, party)) %>%
    filter(str_detect(name, "The")) %>%
    unique() %>%
    mutate(full_name = name,
           name = str_remove(full_name, "[:space:]MP"),
           title = str_extract(name, "The.{0,10}[:space:][:alpha:]{0,10}"),
           last_name = str_extract(name, "^[:alpha:]{0,35}[:punct:][:alpha:]{0,35}|^[:alpha:]{0,35}[:space:][:alpha:]{0,35}|^[:alpha:]{0,35}"),
           last_name = str_replace_all(last_name, "\\,", ""),
           first_name = str_extract(name, ",[:space:][:alpha:]{0,35}|,[:space:][:alpha:]{0,35}[:space:][:alpha:]{0,35}"),
           first_name = str_replace_all(first_name, "[:punct:][:space:]|[:space:]$", "")) %>% 
    select(-c(title, name))
  
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
           last_name = ifelse(is.na(first_name), name, str_extract(name, "(?<=[[:blank:]])[[:alpha:]]{0,35}")),
           name = ifelse(is.na(first_name), name, paste0(last_name, ", ", first_name))) %>% 
    select(-name) %>% 
    rbind(., name_info)
  
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
  name_info <- name_info %>% 
    filter(str_detect(first_name, "[[:space:]]")) %>% 
    mutate(first_name = str_extract(first_name, "^[[:alpha:]]{0,35}")) %>% 
    rbind(., name_info) %>% 
    unique()
  
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
  
  # for those people with more than 2 unique forms in main, lets just select one to go with for consistency
  # I'm selecting the one in it's fullest form 
  # i.e. we prefer Ware, Jenny MP over Ms Ware interjecting-, and we prefer Buchholz, Scott MP (The DEPUTY SPEAKER) over Buchholz, Scott MP
  name_info <- name_info %>% group_by(last_name, first_name) %>% 
    filter(n()>1 & !str_detect(full_name, "Mr|Ms|Dr|Mrs") | n()==1) %>% 
    filter(n()>1 & str_detect(full_name, "SPEAKER") | n()==1) %>% 
    ungroup()
  
  # merge name info with name info from main, to create lookup data set which we can use to fill main
  name_lookup <- full_join(name_forms, name_info, by = intersect(names(name_forms), names(name_info)))
  
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
                              full_name))
  
  # lets just grab the ones where the electorate, party and name id info is missing in main
  # aka those where main form does not equal the full correctly formatted name
  # name_lookup <- name_lookup %>% filter(main_form != full_name | is.na(main_form))
  
  # clean up / modify lookup table for merge with main
  name_lookup <- name_lookup %>% 
    select(-c(first_name, last_name)) %>% 
    rename(name = main_form,
           name_use = full_name,
           electorate_use = electorate,
           name.id_use = name.id,
           party_use = party) %>% 
    separate_rows(name, sep="\\|") %>% 
    filter(name!=name_use)
  
  #### step 9: merge main with lookup table, replace names with correct name (as needed) and fill in missing info (name ID/party/electorate)
  main <- merge(main, name_lookup, by="name", all.x = T) %>% 
    mutate(name = ifelse(is.na(name_use), name, name_use),
           electorate = ifelse(is.na(electorate_use), electorate, electorate_use),
           name.id = ifelse(is.na(name.id_use), name.id, name.id_use),
           party = as.factor(ifelse(is.na(party_use), as.character(party), as.character(party_use)))) %>% 
    select(-c(name_use, party_use, electorate_use, name.id_use)) %>% arrange(order)
  
  # within same speech, deputy speaker doesn't change, so use same name for each (notice before this, repeats of deputy speaker won't include full name)
  # also sometimes there isn't a full version of the deputy speaker's name in the speech, so I'm going to fill those with the previous available full name of the deputy speaker
  # added a flag to make filling easier, dropped it after
  main <- main %>% mutate(deputy_flag = ifelse(str_detect(name, "The DEPUTY SPEAKER|The Deputy Speaker"), 1, 0)) %>% 
    group_by(index) %>% 
    mutate(name = ifelse(str_detect(name, "^The DEPUTY SPEAKER$|^The Deputy Speaker$"), str_subset(name, ".{0,50}\\(The DEPUTY SPEAKER\\)"), name)) %>% 
    ungroup() %>% 
    group_by(deputy_flag) %>% 
    fill(name, .direction = "down") %>% 
    ungroup() %>% 
    select(-deputy_flag)
  
  # fill electorate, name.id and party info
  main <- main %>% group_by(name) %>% 
    fill(c(name.id, electorate, party), .direction = "downup") %>% 
    ungroup() %>% distinct()
  
  # fix up "___ members interjecting-" names for simplicity
  main <- main %>% mutate(name = ifelse(str_detect(name, "interjecting-"), str_remove(name, "[[:space:]]interjecting-"), name))
  
  #### step 10: flag for interjections
  # group by index, and if the name is not equal to the first name w/ that index, or the speaker, it is an interjection
  main_final <- main %>% group_by(index) %>% arrange(order) %>% 
    mutate(has_interject = case_when(order == min(order) ~ 0,
                                     str_detect(name, "The SPEAKER|The DEPUTY SPEAKER|stage direction") ~ 0)) %>% 
    ungroup() %>% 
    group_by(name, index) %>%
    fill(has_interject, .direction = "down") %>% 
    ungroup() %>% 
    mutate(has_interject = ifelse(is.na(has_interject), 1, has_interject))
  
  return(main_final)
}

# define script as function with filename argument
parse_hansard <- function(filename){
  
  # parse XML
  hansard_xml <- xmlParse(here("input", filename))
  
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
  bus_start_chamb <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/business.start")), 
                            fedchamb_flag = 0) %>% 
    mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
           date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
           body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"),
           start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"))
  
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
  
  ######### SUB-DEBATE 2 #########
  # include if-else statements throughout code in case sub-debate 2 does not exist
  # store sub-debate 2 information & text in tibble, correct variable class, add flag for federation chamber
  sub2_info_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebateinfo")),
                           xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/subdebate.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
  # store sub-debate 2 talker info & speech in tibble, correct variable class, add flag for federation chamber, extract time
  sub2_speech_chamb <- cbind(xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.start/talker")),
                             xmlToDataFrame(node=getNodeSet(hansard_xml, "//chamber.xscript/debate/subdebate.1/subdebate.2/speech/talk.text"))) %>% 
    as_tibble() %>% 
    mutate(page.no = {if("page.no" %in% names(.)) as.numeric(page.no) else NULL},
           time.stamp = {if ("body" %in% names(.)) str_extract(body, "\\d\\d:\\d\\d|\\d:\\d\\d") else NULL},
           party = {if ("party" %in% names(.)) as.factor(party) else NULL},
           fedchamb_flag = {if("page.no" %in% names(.)) 0 else NULL})
  
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
  
  ################# NEED TO FIGURE OUT CORRECT ORDERING OF Q AND A WHEN TIME STAMP IS THE SAME, MAYBE SHOULD JUST HVE Q-A-Q-A PATTERN IF TIME STAMP IS THE SAME ("No." in wrong place)
  # I fixed this with the lead/lag stuff, need to keep an eye if it causes any issues with other dates
  # merge questions and answers, add flag for sub-debate 1 and 2, and federation chamber (always 0 b/c only have question time in chamber)
  sub1_q_a <- rbind(sub1_q, sub1_a) %>% 
    arrange(time.stamp) %>% 
    rowid_to_column("order") %>% 
    mutate(order = ifelse(lead(time.stamp)==time.stamp & lag(question)==question & order!=1, order+1, order)) %>% 
    mutate(order = ifelse(lead(order)==order, order+1, order)) %>% arrange(order) %>% 
    select(-order) %>% 
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
  
  #################### FEDERATION CHAMBER ####################
  # use if-else statement to ensure code works for Hansard with and without federation chamber
  # check that there is a business start to know if federation chamber exists
  
  if (nrow(tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")))) > 0) {
    
    ######### BUSINESS START #########
    # store business start in tibble, add flag for federation chamber, extract date and start time
    bus_start_fed <- tibble(xmlToDataFrame(node=getNodeSet(hansard_xml, "//fedchamb.xscript/business.start")), 
                            fedchamb_flag = 1) %>% 
      mutate(day_of_week = str_extract(body, "^[:alpha:]{0,6}day"),
             date = as.Date(str_extract(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"), "%A, %d %B %Y"),
             body = str_remove(body, "^[:alpha:]{0,6}day,[:space:][:digit:]{0,2}[:space:][:alpha:]{0,9}[:space:][:digit:]{0,4}"),
             start_time = str_extract(body, "[:digit:]{0,2}[:punct:][:digit:][:digit:]"))
    
    # merge into single business start tibble
    bus_start <- rbind(bus_start_chamb, bus_start_fed)
    
    ######### DEBATE INFORMATION #########
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
    
    # merge chamber and federation tibbles together, flag for which sub-debate, arrange by fedchamb flag, and page
    sub1_info <- rbind(sub1_info_chamb, sub1_info_fed) %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0)
    
    # merge chamber and federation tibbles together, flag for question and answer, and which sub-debate, arrange by fedchamb flag, page and time
    sub1_speech <- rbind(sub1_speech_chamb, sub1_speech_fed) %>% 
      arrange(fedchamb_flag, time.stamp, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0,
             question = 0,
             answer = 0)
    
    ######### SUB-DEBATE 2 #########
    # if-else statements are included because sub-debate 2 is not always present in federation chamber
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
    
    # if sub-debate 2 exists for chamber and/or federation chamber, merge and process
    if (nrow(sub2_info_chamb > 0) | nrow(sub2_info_fed > 0)) {
      # merge chamber and federation tibbles together, flag for which sub-debate, arrange by page
      sub2_info <- rbind(sub2_info_chamb, sub2_info_fed) %>% 
        arrange(fedchamb_flag, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 1)
      
      # merge chamber and federation tibbles together, flag for question and answer, and which sub-debate, arrange by page and time
      sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_fed) %>% 
        arrange(fedchamb_flag, time.stamp, page.no) %>% 
        mutate(sub1_flag = 0,
               sub2_flag = 1,
               question = 0,
               answer = 0)
    } else {
      # else just bind, resulting in empty tibbles
      sub2_info <- rbind(sub2_info_chamb, sub2_info_fed)
      sub2_speech <- rbind(sub2_speech_chamb, sub2_speech_fed)
    }

    
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
    # re-name chamber business start and debate info
    bus_start <- bus_start_chamb
    debate_info <- debate_info_chamb
    
    # rename sub-debate 1 chamber info, arrange by page number, add flags for which sub-debate
    sub1_info <- sub1_info_chamb %>% 
      arrange(fedchamb_flag, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0)
    
    # rename sub-debate 1 chamber speech, flag for question and answer, and which sub-debate, arrange by page, fedchamb flag and time
    sub1_speech <- sub1_speech_chamb %>% 
      arrange(fedchamb_flag, time.stamp, page.no) %>% 
      mutate(sub1_flag = 1,
             sub2_flag = 0,
             question = 0,
             answer = 0)
    
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
  
  ######### PUTTING EVERYTHING TOGETHER #########
  # all content information, add numeric ordering
  toc <- debate_info %>% 
    select(-type) %>% 
    rbind(., sub1_info, sub2_info) %>% 
    arrange(fedchamb_flag, page.no) %>% 
    rowid_to_column("order")
  
  # all interjection information
  interject <- rbind(interject_sub1, interject_sub2, sub1_q_a_interject) %>% 
    arrange(fedchamb_flag, page.no) %>% 
    rowid_to_column("order")
  
  # all debate text
  main <- rbind(sub1_speech, sub2_speech, sub1_q_a) %>% 
    arrange(fedchamb_flag, time.stamp, page.no)
  
  # clean up body of debate text (removing name, title in brackets, and time stamp from body)
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\(.{0,250}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  # case when there is no title, just name and time stamp to be removed from body
  main$body <- str_remove(main$body, "^.{0,6}[:space:].{0,35}[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:[:space:]{0,5}")
  
  ######### SPLITTING INTERJECTIONS #########
  main <- split_interjections(main, interject)
  
  ######### EXPORT FINAL OUTPUT #########
  # export data-sets to CSV files
  # write.csv(toc, paste0("output/", str_remove(filename, ".xml"), "-toc.csv"), row.names = FALSE)
  # write.csv(main, paste0("output/", str_remove(filename, ".xml"), "-main.csv"), row.names = FALSE)
  # write.csv(interject, paste0("output/", str_remove(filename, ".xml"), "-interject.csv"), row.names = FALSE)
  return(main)
}

# View(parse_hansard("2021_11_25.xml"))
# View(parse_hansard("2022_08_02.xml"))
# View(parse_hansard("2022_08_04.xml"))
# View(parse_hansard("2021_10_21.xml"))
