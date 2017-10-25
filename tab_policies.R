output$policies_need_more_guidance_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("want.guidance.on.compliance", drop.skipped = TRUE) %>%
    single_option_order_heuristic() %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_title(text = "Would you like more guidance on how to comply with your funder/institution/publisher's policy?") %>%
    hc_style_survey_percentage()
  
})

output$policies_who_gives_guidance_hc <- renderHighchart({
  
  survey_responses %>%
    multiple_choice_tally("who.provides.support.in.open.data", drop.skipped = TRUE) %>%
    arrange(desc(count)) %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_title(text = "Which sources you would look to for support in relation to open data?") %>%
    hc_subtitle(text = "Respondents could select multiple responses to this question") %>%
    hc_style_survey_percentage()
  
})


output$national_mandate_support_level_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("how.supportive.for.open.data.national.mandate", drop.skipped = TRUE) %>%
    single_option_order_heuristic() %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_title(text = "How supportive would you be of a national mandate for making primary research data openly available?") %>%
    hc_subtitle(text = "(By open, we mean free to access, reuse, repurpose, and redistribute)") %>%
    hc_style_survey_percentage()
  
})


output$policies_who_has_policies_hc <- renderHighchart({
  survey_responses %>%
    select(
      funder.requires.most.recent.public.data_1,
      institution.requires.most.recent.public.data_2,
      publisher.requires.most.recent.public.data_3
    ) %>%
    gather(type, response) %>%
    group_by(type, response) %>%
    na.omit() %>%
    summarise(count = n()) %>%
    mutate(count = 100 * count / sum(count)) %>%
    mutate(response = factor(
      response,
      levels = c(
        "Yes, I'm required to make data publicly available on or around date of publication",
        "Yes, I'm required to make data  publicly available following an embargo period",
        "No, I'm not required to make my data publicly available",
        "I don't know",
        "N/A"
      )
    )) %>%
    mutate(order.col = plyr::mapvalues(
      as.character(response),
      from =  c(
        "Yes, I'm required to make data publicly available on or around date of publication",
        "Yes, I'm required to make data  publicly available following an embargo period",
        "No, I'm not required to make my data publicly available",
        "I don't know",
        "N/A"
      ),
      to = 1:5
    )) %>%
    arrange(order.col) %>%
    ungroup() %>%
    mutate(type = plyr::mapvalues(
      type,
      from = c(
        "funder.requires.most.recent.public.data_1",
        "institution.requires.most.recent.public.data_2",
        "publisher.requires.most.recent.public.data_3"
      ),
      to = c(
        "Funder Requirement",
        "Institution Requirement",
        "Publisher Requirement"
      )
    )) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count,
                 group = type)) %>%
    hc_yAxis(
      labels = list(format = "{value}%"),
      title = list(text = "Percentage of Respondants")
    ) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_title(text = "For your most recent piece of research were you subject to any data sharing policies?") %>%
    hc_colors(colors = c("#afca0a", "#c74550", "#86bcb6"))
})

## =============================== All Questions ==========================
## ========================================================================

output$policies_qsubgroup_UI <- renderUI({
  selectInput(
    "policies_qsubgroup",
    label = "Questions concerning:",
    choices = list(
      "Respondents own data" = "own.data",
      "Usage of others data, or opinions on how others work with open data" = "others.data",
      "Insitutional policies or practices" = "institution",
      "Cost of publishing/depositing" = "cost"
    ),
    width = "100%"
  )
})

output$policies_selected_q_UI <- renderUI({
  if (is.null(input$policies_qsubgroup)) {
    return()
  }
  
  policies_qs <- survey_labels %>%
    filter(question.group %in% c("data.access.sharing" , "policy")) %>%
    filter(question.subgroup == input$policies_qsubgroup) %>%
    filter(!question.type == "other.value") %>%
    mutate(tidied.question = gsub("([?]).*", "\\1", actual.question)) %>%
    mutate(tidied.question = gsub(": - Other [(]please specify[)] - Text", "?", tidied.question)) %>%
    mutate(tidied.question = gsub(": - Selected Choice", "?", tidied.question)) %>%
    select(tidied.question, short.colname)
  
  policies_qs <-
    setNames(policies_qs$short.colname,
             policies_qs$tidied.question)
  
  selectInput(
    "policies_selected_q",
    label = "Question",
    choices = policies_qs,
    width = "100%"
  )
  
})

output$policies_selected_single_option_hc <- renderHighchart({
  
  single_option_tally(
    survey_responses,
    input$policies_selected_q,
    drop.skipped = input$policies_drop_skipped
  ) %>%
    single_option_order_heuristic(drop.skipped = input$policies_drop_skipped) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_style_survey_percentage()
  
})

output$policies_selected_multiple_choice_hc <- renderHighchart({
  multiple_choice_tally(
    survey_responses,
    input$policies_selected_q,
    drop.skipped = input$policies_drop_skipped
  ) %>%
    single_option_order_heuristic(drop.skipped = input$policies_drop_skipped) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count)) %>%
    hc_style_survey_percentage()
  
})

output$policies_selected_longform_table <- renderDataTable({
  
  survey_responses %>%
    select(paste0(input$policies_selected_q, "_1")) %>%
    na.omit() %>%
    unique() %>%
    setNames("Free-form responses:")
  
},
colnames = "",
rownames = FALSE,
options = list(pageLength = 10000,
               scrollY = "400px", dom = "t"))

output$policies_selected_who_has_policies_hc <- renderHighchart({
  
  
  survey_responses %>%
    select(
      funder.requires.most.recent.public.data_1,
      institution.requires.most.recent.public.data_2,
      publisher.requires.most.recent.public.data_3
    ) %>%
    gather(type, response) %>%
    group_by(type, response) %>%
    na.omit() %>%
    summarise(count = n()) %>%
    mutate(count = 100 * count / sum(count)) %>%
    mutate(response = factor(
      response,
      levels = c(
        "Yes, I'm required to make data publicly available on or around date of publication",
        "Yes, I'm required to make data  publicly available following an embargo period",
        "No, I'm not required to make my data publicly available",
        "I don't know",
        "N/A"
      )
    )) %>%
    mutate(order.col = plyr::mapvalues(
      as.character(response),
      from =  c(
        "Yes, I'm required to make data publicly available on or around date of publication",
        "Yes, I'm required to make data  publicly available following an embargo period",
        "No, I'm not required to make my data publicly available",
        "I don't know",
        "N/A"
      ),
      to = 1:5
    )) %>%
    arrange(order.col) %>%
    ungroup() %>%
    mutate(type = plyr::mapvalues(
      type,
      from = c(
        "funder.requires.most.recent.public.data_1",
        "institution.requires.most.recent.public.data_2",
        "publisher.requires.most.recent.public.data_3"
      ),
      to = c(
        "Funder Requirement",
        "Institution Requirement",
        "Publisher Requirement"
      )
    )) %>%
    hchart(type = "bar",
           hcaes(x = response,
                 y = count,
                 group = type)) %>%
    hc_style_survey_percentage()
})

output$policies_selected_chart <- renderUI({
  if (is.null(input$policies_selected_q)) {
    return()
  }
  
  selected_q_type <- survey_labels %>%
    filter(short.colname == input$policies_selected_q) %>%
    select(question.type) %>%
    .[[1]]
  
  switch(
    selected_q_type,
    "single.option" = {
      highchartOutput("policies_selected_single_option_hc")
    },
    "multiple.choice" = {
      highchartOutput("policies_selected_multiple_choice_hc")
    },
    "long.form" = {
      dataTableOutput("policies_selected_longform_table")
    },
    "name.multiple" = {
      highchartOutput("policies_selected_who_has_policies_hc")
    }
  )
})

