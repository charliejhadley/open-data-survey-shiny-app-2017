# library("tidyverse")
# library("readxl")
# library("forcats")
# library("rfigshare")

## ======== Import final responses

fs_deposit_id <- 5480710
deposit_details <- fs_details(fs_deposit_id)

deposit_details <- unlist(deposit_details$files)
deposit_details <-
  data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)

deposit_details %>%
  filter(name == "FINAL Open Data 2017 anon rawdata.xlsx") %>%
  select(download_url) %>%
  .[[1]] %>%
  download.file(destfile = "data/download_responses.xlsx")


deposited_final_survey_responses <- read_xlsx("data/download_responses.xlsx")
deposited_final_survey_responses <- deposited_final_survey_responses %>%
  slice(-1) %>%
  slice(-1)

deposited_final_survey_responses <- deposited_final_survey_responses %>%
  select(-V1, -V2, -V3, -V4, -V5, -V7, -V8, -V9)

## ======== Import survey question labels

final_survey_labels <- read_csv("data/survey-question-labels.csv")

## ======== Manage multiple choice questions

## Use this to check responses
multichoice_ids <- final_survey_labels %>%
  filter(multiple.choice == TRUE) %>%
  select(q.id) %>%
  .[[1]]

## ======== Creating sensible question ids


id_tibble <- tibble(responses.column.names = colnames(deposited_final_survey_responses)) %>%
  mutate(q.group = gsub("[_].*", "", responses.column.names)) %>%
  group_by(q.group) %>%
  mutate(id = row_number()) %>%
  mutate(
    short.name = plyr::mapvalues(
      q.group,
      from = final_survey_labels$q.id,
      to = final_survey_labels$short.colname,
      warn_missing = FALSE
    )
  )


## These questions are unique: "Q5.1" "Q5.8" "Q6.3" "Q6.4" "Q9.4" "Q9.5" "Q9.6"
special_cases <- id_tibble %>%
  filter(grepl("Q", short.name)) %>%
  select(short.name) %>%
  unique()

## Fix special cases from above
id_tibble <- id_tibble %>%
  mutate(short.name = if_else(
    grepl(paste0(special_cases$q.group, collapse = "|"),
          q.group),
    plyr::mapvalues(
      responses.column.names,
      from = final_survey_labels$q.id,
      to = final_survey_labels$short.colname,
      warn_missing = FALSE
    ),
    short.name
  ))

## Ensure unique colnames:

id_tibble <- id_tibble %>%
  mutate(unique.short.name = if_else(
    grepl("TEXT", responses.column.names),
    paste0(short.name, "_TEXT"),
    paste0(short.name, "_", id)
  ))

## Export for use

id_tibble %>%
  write_csv("data/final_q-id-tibble.csv")

## ======== Export responses with tidied colnames

export_survey_responses <- deposited_final_survey_responses
colnames(export_survey_responses) <- id_tibble$unique.short.name

export_survey_responses %>%
  write_csv("data/final_survey-data.csv")

## ================= Reformat survey_labels!

export_survey_question_groups <- final_survey_labels %>%
  gather(question.type, value, "single.option", "multiple.choice", "long.form", 
         "name.multiple", "other.value", "special.case") %>%
  filter(!is.na(value)) %>%
  select(-value)

export_survey_question_groups %>%
  write_csv("data/final_survey-labels.csv")

## ================= Import the cleaned data!

survey_responses <- read_csv("data/final_survey-data.csv")

survey_labels <- read_csv("data/final_survey-labels.csv")

q_id_tibble <- read_csv("data/final_q-id-tibble.csv")
