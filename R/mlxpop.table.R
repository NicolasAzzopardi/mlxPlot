#' A function to to print Populationparameters.txt file of a Monolix project to Rmarksdown.
#'
#' This function allows you to have almost a publication ready table of the population parameter of a model project.
#' @param project.dir Absolute or relative name of the folder of the monolix project.
#' @param project.name Name of the monolix project file without *.mlxtran* extension.. The project must be in *project.dir*
#' @param estim "sa" or "lin". default to "sa".
#' @param caption Caption of the table.
#' @param greek Convert to greek letters, "latex_engine: xelatex" is needed. default to FALSE.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @importFrom readr read_delim
#' @import writexl
#' @import knitr
#' @import latex2exp
#' @importFrom rlang .data

mlxpop.table <- function(project.dir = "../monolix/",
                         project.name = "", estim = "sa",
                         caption="Population parameters of the PK model.",
                         greek=FALSE){

  scale0 <- function(x, na.rm = FALSE) round(x, 0)
  scale4 <- function(x, na.rm = FALSE) round(x, 2)

  project.name = gsub(".mlxtran","",project.name)

  POP <- read_delim(paste0(project.dir,project.name,"/populationParameters.txt"), delim = ",") %>%
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
             parameter = gsub("b"      , "\u03C3", .data$parameter)
      )
  }
  if(estim=="sa"){
    POP = POP %>% select(.data$parameter,.data$value,`s.e.`= .data$se_sa, `r.s.e.(%)`= .data$rse_sa)
  }
  if(estim=="lin"){
    POP = POP %>% select(.data$parameter,.data$value,`s.e.`= .data$se_lin, `r.s.e.(%)`= .data$rse_lin)
  }

  POP2 <- POP |>
    select(-s.e.) |>
    mutate(`r.s.e.(%)` = paste0("(", `r.s.e.(%)`, ")")) |>
    unite(PAR, c(`value`, `r.s.e.(%)`), remove = TRUE, sep = " ")

  THETAS <-
    POP2 |>
    filter(!grepl("omega_", parameter)) |>
    filter(!grepl("b", parameter)) |>
    filter(!grepl("a1", parameter)) |>
    filter(!grepl("a2", parameter)) |>
    rename("Theta (r.s.e.%)" = PAR)

  OMEGAS <-
    POP2 |>
    filter(grepl("omega_", parameter)) |>
    rename("Omega (r.s.e.%)" = PAR) |>
    mutate(parameter = gsub("omega_", "", parameter))

  TAB <- THETAS |>
    left_join(OMEGAS, by = "parameter") |>
    mutate_all(~ replace(., is.na(.), "-"))

  gt(TAB) |>
    tab_header(title = "",subtitle = caption)
  }
