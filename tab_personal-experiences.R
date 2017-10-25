## =========== Highlighted Personal Experience Questions ===============
## =====================================================================

output$how_often_open_data_chart <- renderHighchart({
  
  shinyjs::show(id = "loading-content",
                anim = TRUE,
                animType = "fade")
  
  shinyjs::hide(id = "loading-content",
                anim = TRUE,
                animType = "fade")
  
  
  survey_responses %>%
    single_option_tally("how.often.made.open.data", drop.skipped = TRUE) %>%
    single_option_order_heuristic() %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_title(text = "How often have you made your research data openly available?") %>%
    hc_style_survey_percentage()
})

output$effort_to_make_reusable_chart <- renderHighchart({
  survey_responses %>%
    single_option_tally("effort.to.make.data.reusable", drop.skipped = TRUE) %>%
    single_option_order_heuristic() %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_title(text = "How much effort is typically required to make your data re-usable by others?") %>%
    hc_style_survey_percentage()
  
})

output$motivation_to_share_chart <- renderHighchart({
  survey_responses %>%
    multiple_choice_tally(question = "motivation.to.share.data", drop.skipped = TRUE) %>%
    mutate(count = 100 * count / sum(count)) %>%
    arrange(desc(count)) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_title(text = "What circumstances would motivate you to share your data?") %>%
    hc_style_survey_percentage()
  
})

output$most_important_motivator_to_share_chart <- renderHighchart({
  title <- survey_labels %>%
    filter(short.colname == "most.important.motivation.to.share.data") %>%
    select(actual.question) %>%
    .[[1]] %>%
    gsub(" - Selected Choice", "", .)
  
  
  survey_responses %>%
    multiple_choice_tally("most.important.motivation.to.share.data", drop.skipped = TRUE) %>%
    mutate(count = 100 * count / sum(count)) %>%
    arrange(desc(count)) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_title(text = title) %>%
    hc_style_survey_percentage()
  
})

## =========== Personal Experience Question Selector ===================
## =====================================================================

output$personal_exp_qsubgroup_UI <- renderUI({
  selectInput(
    "personal_exp_qsubgroup",
    label = "Questions concerning:",
    choices = list(
      "Respondents own data" = "own.data",
      "Usage of others data, or opinions on how others work with open data" = "others.data"
    ),
    width = "100%"
  )
})

output$personal_experiences_selected_q_UI <- renderUI({
  if (is.null(input$personal_exp_qsubgroup)) {
    return()
  }
  
  personal_exp_qs <- survey_labels %>%
    filter(question.group == "personal.experience") %>%
    filter(question.subgroup == input$personal_exp_qsubgroup) %>%
    filter(!question.type == "other.value") %>%
    mutate(tidied.question = gsub("([?]).*", "\\1", actual.question)) %>%
    mutate(tidied.question = gsub(": - Other [(]please specify[)] - Text", "?", tidied.question)) %>%
    mutate(tidied.question = gsub(": - Selected Choice", "?", tidied.question)) %>%
    select(tidied.question, short.colname)
  
  personal_exp_qs <-
    setNames(personal_exp_qs$short.colname,
             personal_exp_qs$tidied.question)
  
  selectInput(
    "personal_experienced_selected_q",
    label = "Question",
    choices = personal_exp_qs,
    width = "100%"
  )
  
})

## =========== Visualising Selected Personal Experience Question =======
## =====================================================================


output$personal_exp_selected_single_option_hc <- renderHighchart({
  single_option_tally(
    survey_responses,
    input$personal_experienced_selected_q,
    drop.skipped = input$personal_experienced_drop_skipped
  ) %>%
    single_option_order_heuristic(drop.skipped = input$personal_experienced_drop_skipped) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_style_survey_percentage()
  
})

output$personal_exp_selected_multiple_choice_hc <- renderHighchart({
  multiple_choice_tally(
    survey_responses,
    input$personal_experienced_selected_q,
    drop.skipped = input$personal_experienced_drop_skipped
  ) %>%
    single_option_order_heuristic(drop.skipped = input$personal_experienced_drop_skipped) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_style_survey_percentage()
  
})

output$personal_exp_selected_longform_table <- renderDataTable({
  survey_responses %>%
    select(paste0(input$personal_experienced_selected_q, "_1")) %>%
    na.omit() %>%
    unique() %>%
    setNames("Free-form responses:")
  
},
colnames = "",
rownames = FALSE,
options = list(
  pageLength = 10000,
  scrollY = "400px",
  dom = "t"
))


output$personal_exp_selected_chart <- renderUI({
  if (is.null(input$personal_experienced_selected_q)) {
    return()
  }
  
  selected_q_type <- survey_labels %>%
    filter(short.colname == input$personal_experienced_selected_q) %>%
    select(question.type) %>%
    .[[1]]
  
  switch(
    selected_q_type,
    "single.option" = {
      highchartOutput("personal_exp_selected_single_option_hc")
    },
    "multiple.choice" = {
      highchartOutput("personal_exp_selected_multiple_choice_hc")
    },
    "long.form" = {
      dataTableOutput("personal_exp_selected_longform_table")
    },
    "name.multiple" = {
      "multiple type"
    }
  )
})
