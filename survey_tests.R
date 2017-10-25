## =========== Single Choice Question Test ===============
## =======================================================

test_single_choice_q <- "how.often.made.open.data"


single_option_tally <- function(survey.data, question, drop.skipped = FALSE){
  q_quo <- rlang::sym(paste0(question,"_1"))
  
  if(drop.skipped == TRUE){
    survey_responses %>%
      count(!!q_quo) %>%
      na.omit() %>%
      setNames(c("response", "count")) %>%
      mutate(count = 100 * count / sum(count))
  } else {
    survey_responses %>%
      count(!!q_quo) %>%
      setNames(c("response", "count")) %>%
      replace_na(list(response = "Question skipped")) %>%
      mutate(count = 100 * count / sum(count))
  }
}

single_option_tally(survey_responses, test_single_choice_q)

single_option_tally(survey_responses, test_single_choice_q, drop.skipped = TRUE) %>%
  single_option_order_heuristic(drop.skipped = FALSE)

single_option_tally(survey_responses, "would.you.reuse.data") %>%
  single_option_order_heuristic(drop.skipped = TRUE)


all_single_options_qs <- survey_labels %>%
  filter(question.type == "single.option") %>%
  select(short.colname) %>%
  .[[1]]

lapply(all_single_options_qs, function(x){
  single_option_tally(survey_responses, x)
})

colnames(survey_responses)[grepl("institution.requires.most.recent.public.data", colnames(survey_responses))]

## =========== Single Choice Question Heurestics Test ===============
## ==================================================================

test_single_choice_q <- "how.often.made.open.data"

single_option_order_heuristic <-
  function(tallied.responses, drop.skipped = FALSE) {
    responses <- tallied.responses %>%
      select(response) %>%
      unique() %>%
      .[[1]]
    
    if (all(c("Yes", "No", "Unsure") %in% responses)) {
      ynu_order <- c("Yes",
                     "No",
                     "Unsure")
      
      if (drop.skipped == TRUE) {
        ynu_order <- c(ynu_order, "Question skipped")
      }
      
      tallied.responses <- tallied.responses %>%
        slice(match(ynu_order,
                    response))
      
      
      
    } else {
      if (all(c("Extremely",
                "Very",
                "Moderately",
                "Slightly",
                "Not at all") %in% responses)) {
        evmsnq_order <- c("Extremely",
                          "Very",
                          "Moderately",
                          "Slightly",
                          "Not at all")
        
        if (drop.skipped == TRUE) {
          evmsnq_order <- c(evmsnq_order, "Question skipped")
        }
        
        tallied.responses <- tallied.responses %>%
          slice(match(evmsnq_order,
                      response))
      } else {
        if (all(c(
          "Very much",
          "Quite a lot",
          "Somewhat",
          "Neutral",
          "Not at all"
        ) %in% responses)) {
          vqsnn_order <- c("Extremely",
                           "Very",
                           "Moderately",
                           "Slightly",
                           "Not at all")
          
          if (drop.skipped == TRUE) {
            vqsnn_order <- c(vqsnn_order, "Question skipped")
          }
          
          tallied.responses <- tallied.responses %>%
            slice(match(vqsnn_order,
                        response))
        } else{
          if (all(
            c(
              "Strongly support",
              "Somewhat support",
              "Neutral",
              "Somewhat oppose",
              "Strongly oppose"
            ) %in% responses
          )) {
            ssnss <- c(
              "Strongly support",
              "Somewhat support",
              "Neutral",
              "Somewhat oppose",
              "Strongly oppose"
            )
            
            if (drop.skipped == TRUE) {
              ssnss <- c(ssnss, "Question skipped")
            }
            
            
            tallied.responses <- tallied.responses %>%
              slice(match(ssnss,
                          response))
          } else {
            if (all(
              c(
                "A lot of effort",
                "Some effort",
                "Little effort",
                "No effort"
              ) %in% responses
            )) {
              asln_order <-
                c("A lot of effort",
                  "Some effort",
                  "Little effort",
                  "No effort")
              
              if (drop.skipped == TRUE) {
                asln_order <- c(asln_order, "Question skipped")
              }
              
              
              tallied.responses <- tallied.responses %>%
                slice(match(asln_order,
                            response))
            } else {
              tallied.responses <- tallied.responses %>%
                arrange(desc(count))
            }
          }
        }
      }
    }
    
    
    tallied.responses
    
  }

## =========== Multiple Choice Question Test ===============
## =========================================================

test_multiple_choice_q <- "motivation.to.share.data"

multiple_choice_tally <-
  function(survey.data, question, drop.skipped = FALSE) {
    
    target_cols <- q_id_tibble %>%
      filter(short.name == question) %>%
      select(unique.short.name) %>% 
      .[[1]]
    print(question)
    choice_tally <- survey_responses %>%
      select(one_of(target_cols[!grepl("TEXT", target_cols)])) %>%
      filter(rowSums(is.na(.)) != ncol(.)) %>%
      gather(discard, response) %>%
      select(response) %>%
      group_by(response) %>%
      count() %>%
      ungroup() %>%
      na.omit() %>%
      setNames(c("response", "count")) %>%
      mutate(count = 100 * count / sum(count))
    
    if (drop.skipped == TRUE) {
      choice_tally
    } else {
      skipped_responses <- survey_responses %>%
        select(one_of(target_cols[!grepl("TEXT", target_cols)])) %>%
        filter(rowSums(is.na(.)) == ncol(.)) %>%
        nrow()
      
      choice_tally %>%
        add_row(response = "Question Skipped", count = skipped_responses)
      
    }
    
  }

survey_responses %>%
  multiple_choice_tally(test_multiple_choice_q, drop.skipped = FALSE)

all_multiple_choice_qs <- survey_labels %>%
  filter(question.type == "multiple.choice") %>%
  select(short.colname) %>%
  .[[1]]

lapply(all_multiple_choice_qs, function(x){
  multiple_choice_tally(survey_responses, x)
})

## =========== Name multiple options =======================
## =========================================================

test_name_multiple_qid <- "Q5.8_1_TEXT"

survey_labels %>%
  filter(question.type == "name.multiple") %>%
  View()

q_id_tibble %>%
  head()







