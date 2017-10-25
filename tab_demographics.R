output$demographics_world_map <- renderLeaflet({
  survey_country_groupings <- survey_responses %>%
    select(contains("country")) %>%
    gather(survey.region, country) %>%
    mutate(survey.region = gsub("country.in.", "", survey.region)) %>%
    mutate(survey.region = gsub("_1", "", survey.region)) %>%
    filter(!is.na(country)) %>%
    mutate(survey.region = str_to_title(gsub("[.]", " ", survey.region))) %>%
    group_by(country) %>%
    mutate(count = n()) %>%
    unique() %>%
    ungroup()
  
  ## ============= Manual corrections
  ## ===========================================
  
  manual_corrections <- list(
    "South Korea" = "Korea",
    "Burma" = "Myanmar",
    "Congo, Democratic Republic of" = "Dem. Rep. Congo",
    "Turks and Caicos Islands" = "Turks and Caicos Is.",
    "Czech Republic" = "Czech Rep.",
    "Bosnia and Herzegovina" = "Bosnia and Herz."
  )
  
  ## ============= Manual corrections
  ## ===========================================
  
  survey_country_groupings <- survey_country_groupings %>%
    mutate(country = plyr::mapvalues(
      country,
      from = names(manual_corrections),
      to = as.character(manual_corrections),
      warn_missing = FALSE
    ))
  
  factpal <-
    colorFactor(palette_figshare(5),
                unique(survey_country_groupings$type),
                na.color = "#566978")
  
  total_respondents <- sum(survey_country_groupings$count)
  
  
  country_label <- function(name, count) {
    paste0(name,
           "<br>",
           "Respondents: ",
           ifelse(is.na(count), "0 reported", paste0(
             count,
             " (",
             round(100 * count / total_respondents, 2),
             "% of total)"
           )))
    
  }
  
  
  world_shapefiles %>%
    left_join(survey_country_groupings,
              by = c("name" = "country")) %>%
    filter(name != "Antarctica") %>%
    leaflet(options = leafletOptions(minZoom = 1.3)) %>%
    addPolygons(
      label = ~ name_long,
      popup = ~country_label(name, count),
      fillColor = ~ factpal(survey.region),
      fillOpacity = 0.8,
      color = "#000",
      weight = 1,
      stroke = TRUE
    ) %>%
    setMapWidgetStyle(style = list(background = "#aacbff")) %>%
    addLegend(pal = factpal,
              value = ~ survey.region,
              opacity = 0.8,
              na.label = "No respondents",
              title = "Survey Region")
  
  
})

output$demographics_age_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("age", drop.skipped = TRUE) %>%
    single_option_order_heuristic(drop.skipped = TRUE) %>%
    arrange(desc(count)) %>%
    mutate(order.col = plyr::mapvalues(
      as.character(response),
      from =  c("Under 18", "18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 or over"),
      to = 1:7
    )) %>%
    arrange(order.col) %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_style_survey_percentage() %>%
    hc_title(text = "What is your age?")
  
  
})

output$demographics_job_title_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("closest.job.title.to.you", drop.skipped = TRUE) %>%
    single_option_order_heuristic(drop.skipped = TRUE) %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_style_survey_percentage() %>%
    hc_title(text = "Which of the following job titles best applies to you?")
  
})

output$demographics_org_type_hc <- renderHighchart({
  
  survey_responses %>%
    single_option_tally("type.of.organisation", drop.skipped = TRUE) %>%
    single_option_order_heuristic(drop.skipped = TRUE) %>%
    hchart(type = "bar",
           hcaes(
             x = response,
             y = count
           )) %>%
    hc_style_survey_percentage() %>%
    hc_title(text = "Which type of organisation do you work in?")
  
})

