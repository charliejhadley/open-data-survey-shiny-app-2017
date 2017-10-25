
## Only 59 respondants provided information about tools, so I'm not visualising that data
# institution_tool_types <- list("Managing active data" = "managing.active.data",
#                    "Data sharing" = "data.sharing",
#                    "Data publishing" = "data.publishing",
#                    "Discovering existing datasets" = "discovering.existing.data",
#                    "Data archive and preservation" = "data.archive.and.preservation")
# 
# qs_institution_tools <- survey_labels %>%
#   filter(combined.q.id == 3) %>%
#   select(short.colname) %>%
#   .[[1]] %>%
#   unique()
# 
# survey_responses[, grepl(qs_institution_tools, colnames(survey_responses))] %>%
#   filter(rowSums(is.na(.)) != ncol(.))

output$institution_open_data_guidance_hc <- renderHighchart({
  survey_responses %>%
    select(contains("how.org.provides.open.data.guidance")) %>%
    multiple_choice_tally("how.org.provides.open.data.guidance", drop.skipped = TRUE) %>%
    arrange(desc(count)) %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_style_survey_percentage() %>%
    hc_title(text = "How does your institution/organisation provide guidance to researchers on open data?")
  
})

survey_responses %>%
  select(contains("does.org.incentvise.open.data")) %>%
  filter(!is.na(.))


  multiple_choice_tally("does.org.advise.on.what.should.be.open", drop.skipped = TRUE) %>%
  arrange(desc(count)) %>%
  hchart(type = "bar",
         hcaes(
           x = response,
           y = count
         )) %>%
  hc_style_survey_percentage() %>%
  hc_title(text = "How does your institution/organisation provide guidance to researchers on open data?")
