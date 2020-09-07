#' Add a CENS column according to LLOQ.
#' @param TAB Data table formatted for Monolix WITHOUT CES column.
#' @param LLOQ Lower Limit Of Quantification.
#' @export addCENS.LLOQ
#' @keywords censoring
#' @examples # addCENS.LLOQ(TAB, LLOQ=.01)
#' @import dplyr

addCENS.LLOQ <- function(TAB, LLOQ = .1){
  TAB %>%
    mutate(Y = ifelse(is.na(Y),NA,ifelse(Y <=LLOQ,LLOQ,Y)),
           CENS = ifelse(is.na(Y),NA,ifelse(Y==LLOQ, 1, 0)))
}

