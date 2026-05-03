https://willkinnebrew.shinyapps.io/NFLCoachValue/

# NFL Coaching Value
This project studies how NFL coaches add value, and attempts to quantify that value

## Data
Raw data is stored in 'data/raw' (with the exception of play by play data which can be retrieved from nflfastR using the provided code)
Processed data is stored in 'data/processed' using scripts in 'code/data_cleaning'

## Reproducibility
Run scripts in the following order :
1. code/data_cleaning/pbp.Rmd
2. code/data_cleaning/coaches.Rmd
3. code/data_cleaning/season records.Rmd
4. Everything in code/models in any order (run Coach Scoring last)
5. For the Shiny App, everything is located in 'code/Shiny/NFLCoachValue'

## Folder Structure
- data: raw and processed datasets
- code: all scripts
- output: figures and tables
- paper: final report
- slides: poster template and presentation
