## ================== multiple_choice_tally

multiple_choice_tally <-
  function(survey.data, question, drop.skipped = FALSE) {
    
    target_cols <- q_id_tibble %>%
      filter(short.name == question) %>%
      select(unique.short.name) %>% 
      .[[1]]

    choice_tally <- survey.data %>%
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
      skipped_responses <- survey.data %>%
        select(one_of(target_cols[!grepl("TEXT", target_cols)])) %>%
        filter(rowSums(is.na(.)) == ncol(.)) %>%
        nrow()
      
      choice_tally %>%
        add_row(response = "Question Skipped", count = skipped_responses)
      
    }
    
  }

## ================== single_option_tally

single_option_tally <- function(survey.data, question, drop.skipped = FALSE){
  q_quo <- rlang::sym(paste0(question,"_1"))
  
  if(drop.skipped == TRUE){
    survey.data %>%
      count(!!q_quo) %>%
      na.omit() %>%
      setNames(c("response", "count")) %>%
      mutate(count = 100 * count / sum(count))
  } else {
    survey.data %>%
      count(!!q_quo) %>%
      setNames(c("response", "count")) %>%
      replace_na(list(response = "Question skipped")) %>%
      mutate(count = 100 * count / sum(count))
  }
}


## ================== Single Choice Orderings


single_option_order_heuristic <- function(tallied.responses, drop.skipped = FALSE){
  
  responses <- tallied.responses %>%
    select(response) %>%
    unique() %>%
    .[[1]]
  
  if (all(c("Yes", "No", "Unsure") %in% responses)) {
    
    ynu_order <- c(
      "Yes",
      "No",
      "Unsure"
    )
    
    if(drop.skipped == FALSE){
      ynu_order <- c(ynu_order, "Question skipped")
    }
    
    tallied.responses <- tallied.responses %>%
      slice(match(ynu_order,
      response))
    
    
    
  } else {
    if (all(
      c(
        "Extremely",
        "Very",
        "Moderately",
        "Slightly",
        "Not at all"
      ) %in% responses
    )) {
      
      evmsnq_order <- c(
        "Extremely",
        "Very",
        "Moderately",
        "Slightly",
        "Not at all"
      )
      
      if(drop.skipped == FALSE){
        evmsnq_order <- c(evmsnq_order, "Question skipped")
      }
      
      tallied.responses <- tallied.responses %>%
        slice(match(
          evmsnq_order,
          response
        ))
    } else {
      if(all(c("Very much", "Quite a lot","Somewhat", "Neutral", "Not at all") %in% responses)){
        
        vqsnn_order <- c(
          "Extremely",
          "Very",
          "Moderately",
          "Slightly",
          "Not at all"
        )
        
        if(drop.skipped == FALSE){
          vqsnn_order <- c(vqsnn_order, "Question skipped")
        }
        
        tallied.responses <- tallied.responses %>%
          slice(match(
            vqsnn_order,
            response
          ))
      } else{
        if(all(c("Strongly support", "Somewhat support", "Neutral", "Somewhat oppose", "Strongly oppose") %in% responses)){
          
          ssnss <- c(
            "Strongly support", "Somewhat support", "Neutral", "Somewhat oppose", "Strongly oppose"
          )
          
          if(drop.skipped == FALSE){
            ssnss <- c(ssnss, "Question skipped")
          }
          
          
          tallied.responses <- tallied.responses %>%
            slice(match(
              ssnss,
              response
            ))
        } else {
          if(all(c("A lot of effort", "Some effort", "Little effort", "No effort") %in% responses)){
            
            asln_order <- c("A lot of effort", "Some effort", "Little effort", "No effort")
            
            if(drop.skipped == FALSE){
              asln_order <- c(asln_order, "Question skipped")
            }
            
            
            tallied.responses <- tallied.responses %>%
              slice(match(
                asln_order,
                response
              ))
          } else {
            
            if(all(c("Very complementary", "Somewhat complementary", "Neutral", "Somewhat contradictory", "Very contradictory", "Unsure") %in% responses)){
              
              complimentary_order <- c("Very complementary", "Somewhat complementary", "Neutral", "Somewhat contradictory", "Very contradictory", "Unsure")
              
              if(drop.skipped == FALSE){
                complimentary_order <- c(complimentary_order, "Question skipped")
              }
              
              
              tallied.responses <- tallied.responses %>%
                slice(match(
                  complimentary_order,
                  response
                ))
            } else {
              
              
              if(all(c("Frequently", "Sometimes", "Rarely", "Never") %in% responses)){
                
                frequently_order <- c("Frequently", "Sometimes", "Rarely", "Never")
                
                if(drop.skipped == FALSE){
                  frequently_order <- c(frequently_order, "Question skipped")
                }
                
                
                tallied.responses <- tallied.responses %>%
                  slice(match(
                    frequently_order,
                    response
                  ))
              } else {
                
                
                
                if(all(c("Under 18", "18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 or over") %in% responses)){
                  
                  age_order <- c("Under 18", "18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 or over")
                  
                  if(drop.skipped == FALSE){
                    age_order <- c(age_order, "Question skipped")
                  }
                  
                  
                  tallied.responses <- tallied.responses %>%
                    slice(match(
                      age_order,
                      response
                    ))
                } else {
                  tallied.responses <- tallied.responses %>%
                    arrange(desc(count))
                }
                
                
                
              }
              
              
            }
            
          }
        }
      }
    }
  }

  
  tallied.responses
  
}



