library("highcharter")
library("DT")
library("leaflet")
library("shinyjs")

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}
"


shinyUI(
  navbarPage(
    "State of Open Data 2017",
    tabPanel("Personal experiences",
             fluidPage(
               theme = "animate.min.css",
               useShinyjs(),
               inlineCSS(appCSS),
               div(id = "loading-content", class = "animated infinite pulse",
                   fluidPage(
                     # h2(class = "animated infinite pulse", "Loading data directly from: \n https://doi.org/10.6084/m9.figshare.5480710...")
                     
                     HTML("<h2>Loading survey data directly from: <br> doi.org/10.6084/m9.figshare.5480710...</h2>")
                     
                     
                     # HTML("<img src=images/cruk-logo.png width='50%'></img>")
                   )),
               tabsetPanel(
                 tabPanel(
                   "Highlighted questions",
                   
                   wellPanel(fluidRow(
                     column(highchartOutput("how_often_open_data_chart"),
                            width = 6),
                     column(highchartOutput("effort_to_make_reusable_chart"),
                            width = 6)
                   )),
                   
                   
                   wellPanel(fluidRow(
                     column(
                       highchartOutput("most_important_motivator_to_share_chart"),
                       width = 6
                     ),
                     column(highchartOutput("motivation_to_share_chart"),
                            width = 6)
                   ))
                   
                   
                 ),
                 tabPanel("All Questions",
                          fluidPage(
                            wellPanel(
                              uiOutput("personal_exp_qsubgroup_UI"),
                              uiOutput("personal_experiences_selected_q_UI"),
                              checkboxInput(
                                "personal_experienced_drop_skipped",
                                "Drop skipped responses?",
                                value = TRUE
                              )
                            ),
                            uiOutput("personal_exp_selected_chart")
                          )),
                 type = "pills"
               )
             )),
    tabPanel("Data Ownership",
             fluidPage(tabsetPanel(
               tabPanel("Who owns your research data?",
                        fluidRow(column(
                          wellPanel(
                            p(
                              "Respondents were asked \"Who owns data you produce during the course of your research before/after publication?\", the respondents could select multiple options."
                            ),
                            p(
                              "The chart displays the responses from the 49.6% of participants who selected only ONE option."
                            ),
                            highchartOutput("who_owns_data_during_research_one_answer_hc")
                          ),
                          width = 12
                        ))),
               tabPanel("Lost Data",
                        wellPanel(
                          fluidRow(
                            column(highchartOutput("lost_data_have_you_hc"),
                                   width = 6),
                            column(highchartOutput("lost_data_where_stored_hc"),
                                   width = 6)
                          ),
                          p(
                            "The table below contains the free-text responses to the question, \"What was the cause of the data loss?\""
                          ),
                          dataTableOutput("lost_data_reasons_DT")
                        ))
             ))),
    tabPanel("Policy",
             fluidPage(
               tabsetPanel(
                 tabPanel("National Data Mandate",
                          wellPanel(
                            highchartOutput("national_mandate_support_level_hc")
                          )),
                 tabPanel("Data sharing policies",
                          wellPanel(highchartOutput("policies_who_has_policies_hc"))
                 ),
                 tabPanel("Policy support",
                          wellPanel(
                            
                            fluidRow(
                              column(
                                highchartOutput("policies_need_more_guidance_hc"),
                                width = 6
                              ),
                              column(
                                highchartOutput("policies_who_gives_guidance_hc"),
                                width = 6
                              )
                            ))),
                 tabPanel("Other questions",
                          
                          wellPanel(
                            uiOutput("policies_qsubgroup_UI"),
                            uiOutput("policies_selected_q_UI"),
                            checkboxInput(
                              "policies_drop_skipped",
                              "Drop skipped responses?",
                              value = TRUE
                            )
                          ),
                          uiOutput("policies_selected_chart")
                 )
               )
             )),
    # tabPanel("Institution",
    #          fluidPage()),
    tabPanel("Survey Demographics",
             tabsetPanel(
               tabPanel(
                 "World Map",
                 wellPanel(
                   leafletOutput("demographics_world_map")
                 )
               ),
               tabPanel("Age of respondents",
                        wellPanel(
                          highchartOutput("demographics_age_hc")
                        )),
               tabPanel("Roles & Institution",
                        wellPanel(
                          fluidRow(
                            column(highchartOutput("demographics_job_title_hc"),
                                   width = 6),
                            column(highchartOutput("demographics_org_type_hc"),
                                   width = 6)
                                   
                        )))
             )),
    tabPanel(
      HTML(
        '<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'
      ),
      fluidPage(wellPanel(includeMarkdown(
        knitr::knit("app_description.Rmd")
      )))
    ),
    collapsible = TRUE
  )
)