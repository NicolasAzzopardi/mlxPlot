#' Add a CENS column according to LLOQ.
#' @param .data Data table formatted for Monolix WITHOUT CENS column.
#' @param LLOQ Value of the lower limit of quantification.
#' @param rm Remove previous CENS column. Default to TRUE.
#' @export addCENS.LLOQ
#' @keywords censoring
#' @examples
#' # addCENS.LLOQ(TAB, LLOQ = 0.01)
#' @import dplyr

addCENS.LLOQ <- function(.data, LLOQ = .1, rm = TRUE) {
  TAB <- .data
  Y <- CENS <- NULL
  if (rm == TRUE) {
    TAB %>%
      mutate(Y = ifelse(is.na(Y), NA, ifelse(Y <= LLOQ, LLOQ, Y))) |>
      mutate(CENS = ifelse(is.na(Y), NA, ifelse(Y == LLOQ, 1, 0)), .after = Y) |>
      mutate(LIMIT = ifelse(CENS == 1, 0, NA), .after = CENS)
  } else {
    TAB %>%
      mutate(Y = ifelse(is.na(Y), NA, ifelse(Y <= LLOQ, LLOQ, Y))) |>
      mutate(CENS = ifelse(is.na(Y), NA, ifelse(Y == LLOQ, 1, CENS)), .after = Y) |>
      mutate(LIMIT = ifelse(CENS == 1, 0, NA), .after = CENS)
  }
}
