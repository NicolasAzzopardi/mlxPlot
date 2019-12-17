#' A function to export Populationparameters.txt file of a Monolix project to xlsx file.
#'
#' This function allows you to have almost a publication ready table of the population parameter of a model project.
#' @param mlxproj Name of the monolix project. The project must be in ./monolix/
#' @param estim "sa" or "lin". default to "sa".
#' @param caption Caption of the table.
#' @param greek Convert to greek letters, default to FALSE.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @import readr
#' @import writexl
#' @import knitr
#' @import latex2exp

mlxpop.xlsx <- function(mlxproj,estim = "sa",caption="Pop params",greek=FALSE){

  scale0 <- function(x, na.rm = FALSE) round(x, 0)
  scale4 <- function(x, na.rm = FALSE) round(x, 4)

  mlxproj = gsub(".mlxtran","",mlxproj)

  POP <- read_delim(
    paste0("./monolix/",mlxproj,"/populationParameters.txt"),
    delim = ",") %>%
    mutate_at(vars(starts_with("value")),  scale4) %>%
    mutate_at(vars(starts_with("se")),     scale4) %>%
    mutate_at(vars(starts_with("rse")),    scale0) %>%
    mutate_at(vars(starts_with("pvalue")), scale4) %>%
    mutate_all(~ replace(., is.na(.), "")) %>%
    mutate(parameter = gsub("_pop", "", .data$parameter))

  if(greek==TRUE){
    POP = POP %>%
      mutate(parameter = gsub("alpha"  , "\u03B1"  , .data$parameter),
             parameter = gsub("beta_"  , "\u03B2 " , .data$parameter),
             parameter = gsub("gamma"  , "\u03B3 " , .data$parameter),
             parameter = gsub("delta"  , "\u03B4"  , .data$parameter),
             parameter = gsub("epsilon", "\u03B5"  , .data$parameter),
             parameter = gsub("omega_" , "\u03C9 " , .data$parameter),
             parameter = gsub("rho"    , "\u03C1"  , .data$parameter),
             parameter = gsub("b"      , "\u03C3_b", .data$parameter)
      )
  }
  if(estim=="sa"){
    POP = POP %>% select(.data$parameter,.data$value,`s.e.`= .data$se_sa, `r.s.e.(%)`= .data$rse_sa)
  }
  if(estim=="lin"){
    POP = POP %>% select(.data$parameter,.data$value,`s.e.`= .data$se_lin, `r.s.e.(%)`= .data$rse_lin)
  }

    write_xlsx(POP, paste0(mlxproj, "_PopParams.xlsx"))
}
