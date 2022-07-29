# hansard-proj

## Scripts folder
- 01-session_info.R
    - obtain date, parliament number, session number, period number, chamber, page number, and proof
    - create a single tibble with above information
    
- 02-debate_info.R
    - obtain debate information (business start, title, page number, type, body)
    - final tibbles contain flag (`fedchamb_flag`) to specify whether information is from chamber (`fedchamb_flag = FALSE`) or federation chamber (`fedchamb_flag = TRUE`)
    - create a single tibble with business start, and single tibble with remaining specified debate information

- 03-subdebate_1.R
    - store sub-debate 1 information, talker information, and speech
    - create a tibble for sub-debate 1 general info (title, page no., body), and a tibble for sub-debate 1 talker info and speech (page.no, name, ID, electorate, party, speech body)
        - `fedchamb_flag` included in both

- 04-subdebate_2.R
    - store sub-debate 2 information, talker information, and speech
    - create tibbles with same variables as in `03-subdebate_1.R`

- 05-interjections.R
    - store all interjections, including page no., name, name ID, electorate and party
    - stored separately for sub-debate 1 and 2 with `fedchamb_flag` included in both
