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
  
  sr <- get_responses(survey, bulk = TRUE, all_page = TRUE, per_page = 100)
  sq <- survey_questions(survey)
  resp <- parse_respondent_list(sr)
  sc <- survey_choices(survey)
  
  resp_full <- left_join(resp, sc, by = c("survey_id", "choice_id", "question_id"))
  
  join_cols <- c("survey_id", "question_id", "subquestion_id")
  join_cols <- join_cols[join_cols %in% names(resp)] # remove subquestion_id if that variable got dropped for being NA in both data.frames
  
  # join responses to question data
  df <- dplyr::left_join(df, sq, by = join_cols)
  
  #### PICK UP HERE.  NEED TO JOIN ALL THREE SOURCES TOGETHER SO THAT Q TYPE CAN INFORM RESPONSES
  
  if("subquestion_id" %in% join_cols){ # can't run this - and don't need to - if there are no subquestion_ids
    resp_full$q_unique_id <- dplyr::if_else(resp_full$sq$question_type == "multiple_choice", # give MC questions unique values for sub_q ID - they need their own columns, matches clean_sm_names()
                                    sq$answerchoice_id,
                                    sq$subquestion_id)
  }
  
  # join responses to question IDs
  q_ids <- clean_sm_labels(survey)
  df <- suppressMessages( # it will join by question_id and subquestion_id if both present, otherwise just question_id - but I don't want the message to print
    dplyr::left_join(df,
                     q_ids %>% dplyr::select(-main_q_id, -sub_q_id))
  )
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

  if (tolower(response_format) == 'column') {
    return(df)
  } else {
    # remove any duplicate rows (need to change questiontext to quesiton ID to avoid this)
    df <- df[!duplicated(df),] %>%
      dplyr::filter((is.na(question_text_full) + is.na(answerchoice_text)) < 2)
    
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
  if(sum(!is.na(survey_qs$subquestion_id)) == 0){ survey_qs$subquestion_id <- NULL } # remove empty column, if there are no subquestions.  Keep other empty cols.
  uniques <- survey_qs
  
  if("subquestion_id" %in% names(survey_qs)){
    uniques <- uniques %>%
    dplyr::mutate(subquestion_id = dplyr::if_else(question_type == "multiple_choice", # give MC questions unique values for sub_q ID - they need their own columns
                                    answerchoice_id,
                                    subquestion_id)) %>%
      dplyr::distinct(question_id, subquestion_id)
  } else { # if there are no multiple choice Qs and thus no subquestion IDs
    uniques <- uniques %>%
      dplyr::distinct(question_id) %>%
      mutate(subquestion_id = as.character(NA))
  }
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
  
  if(sum(!is.na(processed$subquestion_id)) == 0){
    processed$subquestion_id <- NULL # remove if empty, so it joins correctly later
  }
  processed
  
}
