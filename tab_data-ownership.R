


output$who_owns_data_during_research_one_answer_hc <- renderHighchart({
  owner_before <- survey_responses %>%
    select(contains("data.owner.before.publication")) %>%
    filter(rowSums(is.na(.)) == ncol(.) - 1) %>%
    gather(option, response) %>%
    select(response) %>%
    na.omit() %>%
    group_by(response) %>%
    summarise(count = n()) %>%
    mutate(count = 100 * count / sum(count)) %>%
    mutate(before.or.after = "Owner before publication")
  
  owner_after <- survey_responses %>%
    select(contains("data.owner.after.publication")) %>%
    filter(rowSums(is.na(.)) == ncol(.) - 1) %>%
    gather(option, response) %>%
    select(response) %>%
    na.omit() %>%
    group_by(response) %>%
    summarise(count = n()) %>%
    mutate(count = 100 * count / sum(count)) %>%
    mutate(before.or.after = "Owner after publication")
  
  
  grouped_categories <- list(
    list(
      name = "Individual",
      categories = c("Myself", "Colleagues and collaborators")
    ),
    list(
      name = "Institution",
      categories = c("My publisher", "My institution/organisation",
                     "My funder")
    ),
    list(
      name = "Other",
      categories = I("Unsure")
    )
  )
  
  data_to_chart <- owner_after %>%
    bind_rows(owner_before) %>%
    mutate(order.col = plyr::mapvalues(
      as.character(response),
      from =  c(
        "Myself",
        "Colleagues and collaborators",
        "My publisher",
        "My institution/organisation",
        "My funder",
        "Unsure"
      ),
      to = 1:6
    )) %>%
    arrange(order.col) %>%
    mutate(before.or.after = factor(
      before.or.after,
      levels = c("Owner before publication", "Owner after publication")
    ))
  
  
  highchart() %>%
    hc_xAxis(categories = grouped_categories) %>%
    hc_add_series(
      data = data_to_chart,
      type = "bar",
      hcaes(x = response,
            y = count,
            group = before.or.after),
      showInLegend = TRUE
    ) %>%
    hc_yAxis(
      labels = list(format = "{value}%"),
      title = list(text = "Percentage of Respondants")
    ) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_title(text = "Who owns the data you produce during the course of your research?") %>%
    hc_tooltip(valueDecimals = 1,
               valueSuffix = "%") %>%
    hc_colors(colors = c("#c74550", "#566978"))
})

output$lost_data_have_you_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("have.you.lost.research.data", drop.skipped = TRUE) %>%
    single_option_order_heuristic() %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_title(text = "Have you ever lost any research data?") %>%
    hc_style_survey_percentage()

})

output$lost_data_where_stored_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("where.was.lost.data.stored", drop.skipped = TRUE) %>%
    single_option_order_heuristic() %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           ))  %>%
    hc_title(text = "Where was that data stored that you lost?") %>%
    hc_style_survey_percentage()
  
})

output$lost_data_reasons_DT <- renderDataTable({
  
  survey_responses %>%
    select(contains("cause.of.data.loss")) %>%
    na.omit() %>%
    unique()
},
colnames = "",
rownames = FALSE,
options = list(pageLength = 1000,
               scrollY = "400px", dom = "t"))



