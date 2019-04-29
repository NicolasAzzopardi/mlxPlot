#' A function to export Populationparameters.txt file of a Monolix project to xlsx file.
#'
#' This function allows you to have almost a publication ready table of the population parameter of a model project.
#' @param mlxproj Data Table.
#' @param estim "sa" or "lin". default to "sa".
#' @param output Logical. output .xlsx file or not. default to TRUE.
#' @param caption Caption of the table.
#' @keywords monolix
#' @export
#' @examples
#' @import dplyr
#' @import readr
#' @import writexl
#' @import knitr
#' @import latex2exp

mlxpop.print <- function(mlxproj,estim = "sa", output="xlsx",caption="Pop params"){
  scale0 <- function(x, na.rm = FALSE) round(x, 0)
  scale4 <- function(x, na.rm = FALSE) round(x, 4)

POP <- read_delim(
  paste0("./monolix/",mlxproj,"/populationParameters.txt"),
  delim = ",") %>%
      mutate_at(vars(starts_with("value")), scale4) %>%
      mutate_at(vars(starts_with("se")), scale4) %>%
      mutate_at(vars(starts_with("rse")), scale0) %>%
      mutate_at(vars(starts_with("pvalue")), scale4) %>%
      mutate(parameter = gsub("_pop", "", parameter),
             parameter = gsub("alpha", "\u03B1", parameter),
             #parameter = gsub("beta_", "\u03B2", parameter),
             parameter = gsub("beta_", "$\\beta$", parameter),
             parameter = gsub("gamma", "\u03B3", parameter),
             parameter = gsub("delta", "\u03B4", parameter),
             parameter = gsub("epsilon", "\u03B5", parameter),
             parameter = gsub("omega_", "\u03C9", parameter),
             parameter = gsub("rho", "\u03C1", parameter),
             parameter = gsub("b", "\u03C3", parameter)

             )%>%  mutate_all(~ replace(., is.na(.), ""))
if(estim=="sa"){
  POP = POP %>% select(-`se_lin`, -`rse_lin`) %>%
  rename(`s.e.`= `se_sa`, `r.s.e.(%)`= `rse_sa`)
  }
if(estim=="lin"){
  POP = POP %>% select(-`se_sa`, -`rse_sa`) %>%
  rename(`s.e.`= `se_lin`, `r.s.e.(%)`= `rse_lin`)
}
if( output == "xlsx" ){
  write_xlsx(POP,
             paste0(mlxproj, "_PopParams.xlsx")
             )
}
if(output == "kable"){
  kable(POP, booktabs = T, caption = caption) %>%
    kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"), position = "left")
}
}
