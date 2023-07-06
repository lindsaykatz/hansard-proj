# Hansard project

This repository contains all materials relating to the Digitization of the Australian Parliamentary Debates (1998-2022). The most recent version of our database is available for download on [Zenodo](https://zenodo.org/record/7799678).

## Workflow

To produce the most recently published version of our dataset, we used the following workflow, where all R scripts can be found in our code folder.

-   With `00-scrape_files.R`, we first download and store all Hansard XML files using the `HeapsOfPapers` package, with the CSV files from the urls folder.

-   In the `01-session_info.R` script, we parse and clean the session info from each Hansard XML file. The resulting dataframe is used later on in our data validation script.
  
-   Using data from the `AustralianPoliticians` and `ausPH` R packages, in `02-auspol_lookup.R`, create and export various lookup tables which will later be used in the fill details script. These tables contain data on Members of Parliament such as their electorate, party, and unique identification code which corresponds to that in the Australian Parliamentary Handbook.

-   Parse, clean and export each XML file to CSV format using:

    -   `03-everything-1998_to_1999-FINAL.R` for proceedings from 02 March 1998 to 09 December 1999 (inclusive)

    -   `04-everything-2000_to_2011-FINAL.R` for proceedings from 15 February 2000 to 24 March 2011 (inclusive)

    -   `05-everything-2011_to_2012-FINAL.R` for proceedings from 10 May 2011 to 28 June 2012 (inclusive)

    -   `06-everything-2012_to_2022-FINAL.R` for proceedings from 14 August 2012 to 08 September 2022 (inclusive)

-   Fill in member details for each CSV produced in step 1 using the `07-fill_details.R` script. We then exported the filled in datasets as new CSV files.

-   Run filled in datasets from step 2 through a suite of automated tests using the `08-data_validation.R` script, making necessary corrections to the data which did not pass all of these tests. We then re-exported those files which required additional cleaning identified by these tests, and re-ran them through the full suite of tests to ensure every file in our dataset passed every validation test.

-   Run a time stamp validation check on the data with `09-check_time_stamps.R`.

-   Add a PartyFacts ID variable to each validated file in our database with `10-add_party_facts.R`, and export each to both CSV and Parquet forms. In this script we also export the `PartyFacts_map.csv` file, which was used to map the correct PartyFact identification number to it's corresponding party abbreviation.

-   Generate a single corpus with all sitting day's data in both CSV and Parquet forms with `11-ceate_one_corpus.R`.

-   Extract and clean all debate topics from each Hansard XML, and export them as a single CSV file and a single Parquet file using `12-get_debate_topics.R`.

## Example Code

This is an example of how to load one file from our dataset into R, shown for the CSV and Parquet file formats.

``` r
library(tidyverse)
library(arrow)

# csv
hansard_csv <- readr::read_csv("hansard_1998_to_2022-csv/2000-06-05.csv", 
                               col_types = list(name = col_character(),
                                                order = col_double(),
                                                speech_no = col_double(),
                                                page.no = col_double(),
                                                time.stamp = col_character(),
                                                name.id = col_character(),
                                                electorate = col_character(),
                                                party = col_factor(),
                                                in.gov = col_double(),
                                                first.speech = col_double(),
                                                body = col_character(),
                                                fedchamb_flag = col_factor(),
                                                question = col_factor(),
                                                answer = col_factor(),
                                                q_in_writing = col_factor(),
                                                div_flag = col_factor(),
                                                gender = col_factor(),
                                                uniqueID = col_character(),
                                                interject = col_factor(),
                                                partyfacts_id = col_double()))

# parquet
hansard_parquet <- arrow::read_parquet("hansard_1998_to_2022-parquet/2000-06-05.parquet")
```

The following code shows you how to read in the full corpus of data in Parquet format, filter for particular dates of interest (in this case, all available Hansard data from the 1990s which in our database is 1998 and 1999), and then split each sitting day's data into a separate tibble, stored as a list.

``` r
hansard_corpus <- arrow::read_parquet("hansard_1998_to_2022-parquet/hansard_corpus_1998_to_2022.parquet")

hansard_1990s <- hansard_corpus |> 
  filter(str_detect(date, "^199")) |>  
  group_split(date)
```

If you wish to filter out stage directions, you can do so with the following code, which also updates the order variable to reflect the new ordering of the filtered dataframe.

``` r
hansard_csv |> 
  filter(!str_detect(name, "stage direction")) |> 
  select(-order) |> 
  rowid_to_column("order")
```

Below is an example of how to merge debate topics with the Hansard dataframe.

First, read in the all_debate_topics file, and then filter for the sitting day of interest. This corresponds to the date of the Hansard file we already read in, which is from 2000-06-05.

We then group the topics dataframe by page number, and summarise the title variable into a list form. This is done because often multiple debate titles have the same page number, and multiple rows of Hansard proceedings have the same page number, and there is no straightforward way of knowing exactly which row of the Hansard data corresponds to which debate title.

Finally, we ungroup the data and then right join onto the Hansard dataframe by page number. We use the multiple="all" argument because this allows each row in topics to match multiple rows of the Hansard data. In other words, since multiple rows in the Hansard data have the same page number, they will join to the same row in the topics data. This can also be done similarly with the full corpus.

``` r
topics <- arrow::read_parquet("all_debate_topics.parquet")

topics |> filter(date=="2000-06-05") |> 
  group_by(page.no) |> 
  summarise(title = list(title)) |> 
  ungroup() |> 
  right_join(hansard_parquet, by = "page.no", multiple = "all")
```

## URLs folder

-   this folder has CSV files which contain the links for each sitting day's XML file, and the name it should be assigned when downloaded
-   these files cover all sitting days from 1998 to September 2022

## Accessing original data

As at July 2023, the Hansard website navigated to obtain the datasets that we need. Begin by going to: https://www.aph.gov.au/Parliamentary_Business/Hansard. Then "House Hansard" from the menu on the right. Click "back to 1901." Each day's content is grouped within decades, which can be navigated on the menu on the left.
