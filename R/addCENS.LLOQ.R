#' Add a CENS column according to LLOQ.
#' @param .data Data table formatted for Monolix WITHOUT CENS column.
#' @param LLOQ Lower Limit Of Quantification.
#' @param rm Remove previous CENS column. Default to TRUE.
#' @export addCENS.LLOQ
#' @keywords censoring
#' @examples # addCENS.LLOQ(TAB, LLOQ=.01)
#' @import dplyr

addCENS.LLOQ <- function(.data, LLOQ = .1, rm = TRUE){
  TAB = .data
  Y <- CENS <- NULL
  if(rm==TRUE){
    TAB %>%
    mutate(Y = ifelse(is.na(Y),NA,ifelse(Y <=LLOQ,LLOQ,Y)),
           CENS = ifelse(is.na(Y),NA,ifelse(Y==LLOQ, 1, 0)))
  }else{
    TAB %>%
      mutate(Y = ifelse(is.na(Y),NA,ifelse(Y <=LLOQ,LLOQ,Y)),
             CENS = ifelse(is.na(Y),NA,ifelse(Y==LLOQ, 1, CENS)))
  }
}

