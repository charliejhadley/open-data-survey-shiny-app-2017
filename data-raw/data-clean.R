library("tidyverse")
library("readxl")

raw_survey_data <- read_xlsx("data-raw/FINAL Open Data 2017 anon rawdata.xlsx")

# raw_survey_data <- read_csv("data-raw/State of Open Data prelim.csv")
survey_labels <- read_csv("data-raw/survey-question-labels.csv")

raw_survey_data <- raw_survey_data %>%
  slice(-1) %>%
  slice(-1)


plyr::mapvalues(colnames(raw_survey_data),
                from = survey_labels$q.id,
                to = survey_labels$short.colname)



colnames(raw_survey_data) <- survey_labels$short.colname

## Drop NA columns
raw_survey_data <- raw_survey_data[, which(!is.na(colnames(raw_survey_data)))]

## Drop redundant columns:
raw_survey_data <- raw_survey_data %>%
  select(-v10, -source, -q1.1)

## ================= Reformat survey_labels!

survey_labels %>%
  gather(question.type, value, "single.option", "multiple.choice", "long.form", 
         "name.multiple", "other.value") %>%
  filter(!is.na(value)) %>%
  select(-value) %>%
  write_csv("data/survey-labels.csv")

## ================= Ensure multiple choice questions are properly deliminiated

multi_choice_qs <- survey_labels %>%
  filter(multiple.choice == TRUE) %>%
  select(short.colname) %>%
  .[[1]]

## Regex matches "[anything],[not a space]" and replaces , with ;;
raw_survey_data <- raw_survey_data %>%
  mutate_at(vars(one_of(multi_choice_qs)), funs(gsub("(.)(,)([^[:space:]])", '\\1;;\\3', ., perl = TRUE)))

## ================= Export
raw_survey_data %>%
  write_csv("data/survey_data.csv")
