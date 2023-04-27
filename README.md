# Hansard project

## urls folder

-   this folder has CSV files which contain the links for each sitting day's XML file, and the name it should be assigned when downloaded
-   these files cover all sitting days from 1998 to 2022

## scripts folder

-   00-scrape_files.R
    -   download and store all XML files using `HeapsOfPapers` package and the CSV files from the urls folder
-   01-session_info.R
    -   obtain date, parliament number, session number, period number, chamber, page number, and proof
    -   create a single tibble containing above information (`session_info`)
-   02-debate_info.R
    -   obtain debate information (business start, title, page number, type, body)
    -   create a single tibble with business start (`bus_start`), and single tibble with remaining specified debate information (`debate_info`)
    -   final tibbles contain flag to specify whether information is from chamber (`fedchamb_flag = 0`) or federation chamber (`fedchamb_flag = 1`)
    -   example - where to find in `2021-11-03` [Hansard PDF](https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25175/toc_pdf/House%20of%20Representatives_2021_11_30_Official.pdf;fileType=application%2Fpdf):
        -   `debate_info` - all capitalized entries in contents section starting on page 15 of PDF
        -   `bus_start` - first line of page 1105 (chamber) & first line of page 11192 (federation chamber)
-   03-subdebate_1.R
    -   store sub-debate 1 information, talker information, and speech
    -   create a tibble for sub-debate 1 general info (title, page no., body) (`sub1_info`), and a tibble for sub-debate 1 talker info and speech (page.no, name, ID, electorate, party, speech body) (`sub1_speech`)
    -   `fedchamb_flag` included in both
    -   example - where to find in `2021-11-03` [Hansard PDF](https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25175/toc_pdf/House%20of%20Representatives_2021_11_30_Official.pdf;fileType=application%2Fpdf):
        -   `sub1_info` - all entries in contents section below the capitalized ones (**not indented**) starting on page 15 of PDF
        -   `sub1_speech` - page 11147 of PDF, Mr. Goodenough speaking and the deputy speaker interjects because time expired
-   04-subdebate_2.R
    -   store sub-debate 2 information, talker information, and speech
    -   create tibbles with same variables as in `03-subdebate_1.R`
    -   example - where to find in `2021-11-03` [Hansard PDF](https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/25175/toc_pdf/House%20of%20Representatives_2021_11_30_Official.pdf;fileType=application%2Fpdf):
        -   `sub2_info` - all entries in contents section below the capitalized ones (**indented**) starting on page 15 of PDF
        -   `sub2_speech` - page 11109 Mr. Hunt begins, text pulled includes multiple interjections from Rob Mitchell (the deputy speaker) such as that at the bottom of page 11110 in PDF and top of page 11111 in PDF
-   05-debate_interjections.R
    -   some interjections are structured under their own sub-child node in the XML files, with information on the individual interjecting
        -   not all interjections are categorized this way, and these sub-child nodes do not contain the actual text of the interjection
    -   parse and store all these uniquely specified interjections, including the information they provide which is the page no., name, name ID, electorate and party of the individual making the interjection
    -   these are parsed and stored separately for sub-debate 1 and 2 (`interject_sub1` and `interject_sub2`, respectively) with the `fedchamb_flag` included in both
    -   take the examples provided for `sub1_speech` and `sub2_speech` above -- the details corresponding to the interjections embedded in those speeches are obtained and stored here as tibbles
-   06-q_and_a.R
    -   similar to interjections, some questions and answers are structured under their own child nodes
    -   store all questions and answers, as well as interjections within questions and answers
    -   question time only occurs in sub-debate 1 of the chamber
    -   first the information and text for all questions and answers are stored in one tibble (`sub1_q_a`) which contains the page number, time stamp, name, name ID, electorate, party, the body/text, `fedchamb_flag`, and a flag for whether it is a question (`question`)
        -   similar to `sub1_speech` and `sub2_speech`, interjections are embedded within the body/text
        -   tibble is arranged by time stamp
    -   next, the details corresponding to all interjections that occur within questions and answers are stored in a tibble called `sub1_q_a_interject`
        -   includes `fedchamb_flag` and `question`
- 07-divisions_data.R
    -   this script loops over all Hansard XML files from 1998 to 2022, extracts any divisions data present in each file, and produces a single tibble with everything, including the time stamp, date, number of votes, names of voters, and result of each division
- 99-everything-1998_to_1999-FINAL.R
    -   this script parses and cleans all Hansard records from 1998 and 1999
    -   output from this script can be found in version 2 of our database
- 99-everything-2000_to_2011-FINAL.R
    -   this script parses and cleans all Hansard records from 2000 to 24 March 2011
    -   output from this script can be found in version 2 of our database
- 99-everything-2011_to_2012-FINAL.R
    -   this script parses and cleans all Hansard records from 10 May 2011 to 28 June 2012
    -   output from this script can be found in version 2 of our database
- 99-everything-2012_to_2022-FINAL.R
    -   this script parses and cleans all Hansard records from 14 August 2012 to 2022
    -   output from this script can be found in version 2 of our database
- auspol_lookup.R
    -   this script combines data from the [`AustralianPoliticians`](https://github.com/RohanAlexander/AustralianPoliticians) and [`ausPH`](https://github.com/palesl/ausPH) R packages to create lookup tables on Members of Parliament which were later used to fill details in our database
- check_names.R
    - when changing our methodology to develop the second version of our database, we wrote this script to catch rows with missing names so we could go back into our scripts and fix them accordingly so that no remaining rows were missing a name
- fill_details.R
    - this script was written to fill in missing speaker details in each file in our database, such as gender, unique ID or electorate details
    - it identifies short-form names belonging to people with common surnames in each CSV, looks for the full version of that individuals name if available in that same CSV file, and replaces the short-form name with the full name, and fills the rest of the speaker details in accordingly with data from the `AustralianPoliticians` package
    - it also does this for anyone with a unique surname but is missing details
    - each file in our database was filled using this script after being parsed, and before being validated in the `data_validation.R` script
- data_validation.R
    -   this script contains code to run 7 automated validation tests on each file in our database
    -   we validated all filled in CSV files with this script before publishing the second version of our database
- csv_to_parquet.R
    -   this script converts all filled and validated CSV files in our database to Parquet format
- descriptive_stats.R
    - this script is unfinished and was created to produce descriptive statistics on our database
- row-checks.R
    -   when changing our methodology to develop the second version of our database, we wrote this script to compare the number of rows in the newly parsed data to that of the data parsed using the original methodology. This allowed us to catch any issues associated with the new methodology, and modify our new scripts accordingly
- old-versions
    -   this folder contains old versions of our main scripts

## paper folder

-   `first-draft` contains contents from the first draft of our paper
      -   contains all datasets and images used to create the paper in quarto, including the resulting PDF and tex files
      -   `fedchamb_rows.csv` contains data on the number of rows in each Hansard disaggregated by proceedings in the House of Representatives and proceedings in the Federation Chamber
      -   `interject_data.csv` contains data on the number of interjections on each sitting day, and `interject_gender_data.csv` contains this data disaggregated by the gender of those making the interjections, and by House vs. Federation Chamber
      -   `unique_data.csv` contains data on the number of unique names, name IDs, electorates, and parties found in each CSV, disaggregated by House vs. Federation Chamber proceeding
      -   `word_counts.csv` contains data on the word count of the Hansard CSV for each sitting day, also disaggregated by House vs. Federation Chamber proceeding
-   `images` contains images used in our paper
-    All files starting with `paper-arxiv` are associated with the version of our paper available on [arxiv](https://arxiv.org/abs/2304.04561)

## input folder

- contains some Hansard records from 2021 and 2022 in XML form, downloaded from the Australian Parliament [website](https://www.aph.gov.au/Parliamentary_Business/Hansard/Hansreps_2011)

## output folder

- contains old CSV files with some XML content which was parsed at the beginning of this work
- contains an rda file with data on divisions which took place in parliament, including the date, time stamp, number of votes and names of members who voted. This was created using the script `07-divisions_data.R`
