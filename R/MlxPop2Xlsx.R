#' A function to export Populationparameters.txt file of a Monolix project to xlsx file.
#'
#' This function allows you to have almost a publication ready table of the population parameter of a model project.
#' @param mlxproj Data Table.
#' @param output Logical. output .xlsx file or not. default to TRUE.
#' @keywords monolix
#' @export
#' @examples
#' @import dplyr
#' @import readr
#' @import writexl

MlxPop2Xlsx <- function(mlxproj, output=TRUE){
  scale0 <- function(x, na.rm = FALSE) round(x, 0)
  scale4 <- function(x, na.rm = FALSE) round(x, 4)

POP <- read_delim(
  paste0(mlxproj, "/data/populationParameters.txt"),
  delim = ",") %>%
      mutate_at(vars(starts_with("value")), scale4) %>%
      mutate_at(vars(starts_with("se")), scale4) %>%
      mutate_at(vars(starts_with("rse")), scale0) %>%
      mutate_at(vars(starts_with("pvalue")), scale4) %>%
      mutate(parameter = gsub("_pop", "", parameter),
             parameter = gsub("alpha", "\u03B1", parameter),
             parameter = gsub("beta_", "\u03B2", parameter),
             parameter = gsub("gamma", "\u03B3", parameter),
             parameter = gsub("delta", "\u03B4", parameter),
             parameter = gsub("epsilon", "\u03B5", parameter),
             parameter = gsub("omega_", "\u03C9", parameter),
             parameter = gsub("rho", "\u03C1", parameter),
             parameter = gsub("b", "\u03C3", parameter)

             )
if( output == TRUE ){
  write_xlsx(POP,
                     paste0(mlxproj, "_PopParams.xlsx")
                     )
  }
}
