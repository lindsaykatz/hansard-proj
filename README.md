# hansard-proj

## urls folder
- this folder has CSV files which contain the links for each sitting day's XML file, and the name it should be assigned when downloaded
- these files cover all sitting days from 2000 to 2022

## scripts folder
- 00-scrape_files.R
    - download and store all XML files using `HeapsOfPaper` package and the CSV files from the urls folder

- 01-session_info.R
    - obtain date, parliament number, session number, period number, chamber, page number, and proof
    - create a single tibble containing above information (`session_info`)
    
- 02-debate_info.R
    - obtain debate information (business start, title, page number, type, body)
    - create a single tibble with business start (`bus_start`), and single tibble with remaining specified debate information (`debate_info`)
    - final tibbles contain flag to specify whether information is from chamber (`fedchamb_flag = 0`) or federation chamber (`fedchamb_flag = 1`)
    - example - where to find in `2021-11-03` [Hansard PDF](https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25175/toc_pdf/House%20of%20Representatives_2021_11_30_Official.pdf;fileType=application%2Fpdf):
        - `debate_info` - all capitalized entries in contents section starting on page 15 of PDF
        - `bus_start` - first line of page 1105 (chamber) & first line of page 11192 (federation chamber)

- 03-subdebate_1.R
    - store sub-debate 1 information, talker information, and speech
    - create a tibble for sub-debate 1 general info (title, page no., body) (`sub1_info`), and a tibble for sub-debate 1 talker info and speech (page.no, name, ID, electorate, party, speech body) (`sub1_speech`)
    - `fedchamb_flag` included in both
    - example - where to find in `2021-11-03` [Hansard PDF](https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25175/toc_pdf/House%20of%20Representatives_2021_11_30_Official.pdf;fileType=application%2Fpdf): 
        - `sub1_info` - all entries in contents section below the capitalized ones (**not indented**) starting on page 15 of PDF
        - `sub1_speech` - page 11147 of PDF, Mr. Goodenough speaking and the deputy speaker interjects because time expired

- 04-subdebate_2.R
    - store sub-debate 2 information, talker information, and speech
    - create tibbles with same variables as in `03-subdebate_1.R`
    - example - where to find in `2021-11-03` [Hansard PDF](https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25175/toc_pdf/House%20of%20Representatives_2021_11_30_Official.pdf;fileType=application%2Fpdf): 
        - `sub2_info` - all entries in contents section below the capitalized ones (**indented**) starting on page 15 of PDF
        - `sub2_speech` - page 11109 Mr. Hunt begins, text pulled includes multiple interjections from Rob Mitchell (the deputy speaker) such as that at the bottom of page 11110 in PDF and top of page 11111 in PDF

- 05-debate_interjections.R
    - some interjections are structured under their own sub-child node in the XML files, with information on the individual interjecting
        - not all interjections are categorized this way, and these sub-child nodes do not contain the actual text of the interjection
    - parse and store all these uniquely specified interjections, including the information they provide which is the page no., name, name ID, electorate and party of the individual making the interjection
    - these are parsed and stored separately for sub-debate 1 and 2 (`interject_sub1` and `interject_sub2`, respectively) with the `fedchamb_flag` included in both
    - take the examples provided for `sub1_speech` and `sub2_speech` above – the details corresponding to the interjections embedded in those speeches are obtained and stored here as tibbles

- 06-q_and_a.R
    - similar to interjections, some questions and answers are structured under their own child nodes
    - store all questions and answers, as well as interjections within questions and answers
    - question time only occurs in sub-debate 1 of the chamber
    - first the information and text for all questions and answers are stored in one tibble (`sub1_q_a`) which contains the page number, time stamp, name, name ID, electorate, party, the body/text, `fedchamb_flag`, and a flag for whether it is a question (`question`)
        - similar to `sub1_speech` and `sub2_speech`, interjections are embedded within the body/text
        - tibble is arranged by time stamp
    - next, the details corresponding to all interjections that occur within questions and answers are stored in a tibble called `sub1_q_a_interject`
        - includes `fedchamb_flag` and `question`

- 99-everything.R
    - this script incorporates code from all the above scripts, to parse and process all the data at once
    - this script also combines many of the individual tibbles together with additional flags for simplicity
        - `sub_info` - combnes `debate_info`, `sub1_info`, and `sub2_info` to essentially recreate the table of contents
            - contains `fedchamb_flag`, `sub1_flag` and `sub2_flag` to mark whether title comes from a sub-debate or the main debate
        - `sub_speech` - combines `sub1_speech`, `sub2_speech`, and `sub1_q_a` to capture all body/text
            - contains `fedchamb_flag`, `sub1_flag`, `sub2_flag`, `has_interject` and `question` and `answer` flags to capture whether question, answer, or speech
        - `sub_interject` - combines `interject_sub1`, `interject_sub2`, and `sub1_q_a_interject` to capture all interjections
            - contains `fedchamb_flag`, `sub1_flag`, `sub2_flag`, and `question` and `answer`

## paper folder