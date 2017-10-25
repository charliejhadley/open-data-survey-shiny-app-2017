survey_country_groupings <- survey_responses %>%
  select(contains("country")) %>%
  gather(type, country) %>%
  mutate(type = gsub("country.in.", "", type)) %>%
  mutate(type = gsub("_1", "", type)) %>%
  filter(!is.na(country)) %>%
  mutate(type = str_to_title(gsub("[.]", " ", type)))

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

survey_countries <- world_shapefiles %>%
  filter(name %in% survey_country_groupings$country) %>%
  mutate(survey.grouping = plyr::mapvalues(name,
                                           from = survey_country_groupings$country,
                                           to = survey_country_groupings$type,
                                           warn_missing = FALSE))

factpal <- colorFactor(brewer.pal(5, "Set1"), unique(survey_country_groupings$type))

bbox_survey_countries <- survey_countries %>%
  st_bbox() %>%
  as.list()

my_fitBounds <- function(map, bbox){
  fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
}

survey_countries %>%
  leaflet(options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
  addPolygons(label = ~name_long,
              fillColor = ~factpal(survey.grouping),
              fillOpacity = 0.8,
              color = "#000",
              weight = 1,
              stroke = TRUE) %>%
  my_fitBounds(bbox_survey_countries) %>%
  addLegend(pal = factpal, 
            value = ~survey.grouping,
            opacity = 0.8)

