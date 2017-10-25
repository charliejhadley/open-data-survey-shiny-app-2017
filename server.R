library("shiny")
library("tidyverse")
library("scales")
library("rlang")
library("stringr")
library("highcharter")
library("DT")
library("sf")
library("leaflet")
library("stringr")
library("RColorBrewer")
library("leaflet.extras")
library("rfigshare")
library("readxl")
library("shinyjs")

source("data-processing.R")

load("data/world_shapefiles.rdata")

hc_style_survey_percentage <- function(hc){
  hc %>%
    hc_tooltip(valueDecimals = 1,
               valueSuffix = "%") %>%
    hc_yAxis(
      labels = list(format = "{value}%"),
      title = list(text = "Percentage of Respondants")
    ) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_colors(colors = c("#c74550"))
}

palette_figshare <- colorRampPalette(c("#afca0a", "#c74550", "#86bcb6"))

shinyServer(
  function(input, output, session){
    
    source("survey_logic.R", local = TRUE)$value
    
    source("tab_personal-experiences.R", local = TRUE)$value
    
    source("tab_data-ownership.R", local = TRUE)$value
    
    source("tab_policies.R", local = TRUE)$value
    
    source("tab_demographics.R", local = TRUE)$value
    
  }
)