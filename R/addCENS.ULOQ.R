#' Add a CENS column according to ULOQ.
#' @param .data Data table formatted for Monolix WITHOUT CENS column.
#' @param ULOQ Value of the upper limit of quantification.
#' @param rm Remove previous CENS column. Default to TRUE.
#' @export addCENS.ULOQ
#' @keywords censoring
#' @examples
#' # addCENS.ULOQ(TAB, ULOQ = 300)
#' @import dplyr

addCENS.ULOQ <- function(.data, ULOQ = 100, rm = TRUE) {
  TAB <- .data
  Y <- CENS <- NULL
  if (rm == TRUE) {
    TAB |>
      mutate(Y = ifelse(is.na(Y), NA, ifelse(Y >= ULOQ, ULOQ, Y))) |>
      mutate(CENS = ifelse(is.na(Y), NA, ifelse(Y == ULOQ, -1, 0)), .after = Y) |>
      mutate(LIMIT = NA, .after = CENS)
  } else {
    TAB |>
      mutate(Y = ifelse(is.na(Y), NA, ifelse(Y >= ULOQ, ULOQ, Y))) |>
      mutate(CENS = ifelse(is.na(Y), NA, ifelse(Y == ULOQ, -1, CENS)), .after = Y) |>
      mutate(LIMIT = ifelse(CENS == -1, NA, LIMIT), .after = CENS)
  }
}
