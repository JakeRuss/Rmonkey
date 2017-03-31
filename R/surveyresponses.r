#' survey_responses
#' 
#' Extracts data from the survey responses data set and formats it as a data frame for analysis
#' 
#' @param survey A sm_survey object, as retrieved by \code{surveylist()}.
#' @param response_format A string indicating the desired data frame response format: 'Table' = one survey response per row and one column per question, or 'Column' = a key/value arrangement with each row holding data for a single question response
#' @return A data frame with survey responses
#' @export


survey_responses <- function(survey,
                            response_format) {
  
  if (missing(response_format)) {response_format = 'table'}
  
  df <- data.frame()
  sr <- get_responses(survey, bulk = TRUE, all_page = TRUE, per_page = 100)
  sq <- survey_questions(survey)
  survey_id <- survey$id
  
  # Iterate through responses
  for (h in sr) {
    response_id <- h$id
    recipient_id <- h$recipient_id
    collector_id <- h$collector_id
    questions <-
      do.call('c', lapply(h$pages, function(x)
        x[['questions']]))
    for (i in questions) {
      question_id <- i$id
      j <- 0
      # use a repeat loop to account for cases where there are no answer rows
      repeat {
        j <- j + 1     # increment counter first for array indexing
        answertext <- NA
        if (is.null(i$answers[[j]]$row_id)) {
          subquestion_id <- NA
        } else {
          subquestion_id <- i$answers[[j]]$row_id
        }
        if (is.null(i$answers[[j]]$choice_id)) {
          if (is.null(i$answers[[j]]$other_id)) {
            answerchoice_id <- NA
            answertext <- i$answers[[j]]$text
          } else {
            answerchoice_id <-i$answers[[j]]$other_id
            answertext <- i$answers[[j]]$text
          }
        } else {
          answerchoice_id <- i$answers[[j]]$choice_id
        }
        newrow <-
          data.frame(
            response_id,
            survey_id,
            recipient_id,
            collector_id,
            question_id,
            subquestion_id,
            answerchoice_id,
            answertext,
            stringsAsFactors = FALSE,
            check.rows = FALSE
          )
        df <- dplyr::bind_rows(df, newrow)
        if (j >= length(i$answers)) {
          break
        }
      }
    }
  }
  
  # join responses to question data
  df <- dplyr::left_join(df, sq, by = c("survey_id", "question_id", "subquestion_id", "answerchoice_id")) %>%
    dplyr::mutate(subquestion_id = dplyr::if_else(question_type == "multiple_choice", # give MC questions unique values for sub_q ID - they need their own columns, matches clean_sm_names()
                                    answerchoice_id,
                                    subquestion_id)) 
  
  # join responses to question IDs
  q_ids <- clean_sm_labels(survey)
  df <- dplyr::left_join(df,
                         q_ids %>% dplyr::select(question_id, subquestion_id, master_id, appearance_order),
                         by = c("question_id", "subquestion_id"))
  
  # Combine the two question headers to make a single one
  df$question_text_full <-
    ifelse (
      df$question_type == 'multiple_choice',
      paste(df$question_text, "-", df$answerchoice_text),
      ifelse(
        !is.na(df$subquestion_text),
        paste(df$question_text, "-", df$subquestion_text),
        paste(df$question_text)
      )
    )
  
  # # Remove rows with NA as question_text (these are the 'other' responses that still need to be managed)
  # df <- df[!is.na(df$question_text_full),]
  
  # for text responses replace the answerchoice field with the text
  df$answerchoice_text[!is.na(df$answertext)] <- df$answertext[!is.na(df$answertext)]
  
  # Select only the columns for the final dataframe
  df <- df %>%
    dplyr::arrange(appearance_order) %>%
      dplyr::mutate(question_text_full = factor(question_text_full, unique(question_text_full))) %>% # relevel after sorting to match apperance order
    dplyr::select(response_id, survey_id, collector_id, recipient_id, question_text_full, answerchoice_text, master_id)

  if (tolower(response_format) == 'column') {return(df)} else {
    
    # remove any duplicate rows (need to change questiontext to quesiton ID to avoid this)
    df <- df[!duplicated(df),] %>%
      dplyr::filter((is.na(question_text_full) + is.na(answerchoice_text)) <2)
    
    var_names_crosswalk <- df %>% dplyr::distinct(question_text_full, master_id)
    # Spread from column to tablular form
    df_table <- tidyr::spread(df %>% select(-question_text_full), master_id, answerchoice_text)
    labels_to_use <- c("response_id",
                       "survey_id",
                       "collector_id",
                       "recipient_id", as.character(var_names_crosswalk$question_text_full))
    
    for(i in seq_along(df_table)){ # set labels
      Hmisc::label(df_table[, i]) <- labels_to_use[i]
    }
    return(df_table)}
  
}
  
#' @title Get sensible variable names for an \code{sm_survey} object.
#'
#' @description
#' Called internally by \code{survey_responses()}, but can also be used interactively.
#'
#'
#' @param survey an \code{sm_survey} object, as retrieved by \code{surveylist()}
#'
#' @return a tibble with question_id and subquestion_id and metadata columns related to main and sub-questions and order of appearance in the survey.
#' @export

clean_sm_labels <- function(survey){
  survey_qs <- survey_questions(survey)
  uniques <- survey_qs %>%
    dplyr::mutate(subquestion_id = dplyr::if_else(question_type == "multiple_choice", # give MC questions unique values for sub_q ID - they need their own columns
                                    answerchoice_id,
                                    subquestion_id)) %>%
    dplyr::distinct(question_id, subquestion_id)
  
  processed <- uniques %>%
    dplyr::mutate(main_q_id = as.numeric(forcats::fct_inorder(question_id))) %>%
    dplyr::group_by(question_id) %>%
    dplyr::mutate(sub_q_id = as.numeric(as.factor(subquestion_id))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(master_id = dplyr::if_else(
      is.na(subquestion_id),
      paste("q", main_q_id, sep = ""),
      paste("q", main_q_id, "_", sub_q_id, sep = "")
    )) %>%
    dplyr::arrange(main_q_id, sub_q_id) %>%
    dplyr::mutate(appearance_order = 1:nrow(.))
  
  processed
  
}

  # Future work
  #
  
  # do.call(rbind, lapply(i$answers, function(x) data.frame(answerchoice_id = x$choice_id, subquestion_id = x$row_id, stringsAsFactors = FALSE)))