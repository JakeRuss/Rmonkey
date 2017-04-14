#' @title Fetch a survey by name.
#'
#' @description
#' Supply the name of your survey and a survey_list and get back the \code{sm_survey} object.
#'
#' @param s_title the exact name of the survey to retrieve.
#' @param survey_list the survey list to retrieve from
#'
#' @return the desired sm_survey object.
#' @export
#'
#' @examples
#' # not run:
#' # s <- survey_list(per_page = 200)
#' # my_survey <- find_survey_by_name("survey for testing", s)

find_survey_by_name <- function(s_title, survey_list){
  titles <- unlist(
    lapply(survey_list, function(x) x$title)
  ) %>%
    gsub("[^[:alnum:]]", "", .)
  survey_list[[which(titles == gsub("[^[:alnum:]]", "", s_title))]]
}