#' This function plots a publication-ready OBS vs. PRED of a monolix project run.
#'
#' @name mlx.OvsP
#' @param project.dir Absolute or relative name of the folder of the monolix project.
#' @param project.name Name of the monolix project file without *.mlxtran* extension. The project must be in *project.dir*
#' @param drug Text for the legend. Typicaly name of the drug. Can be a **TeX** object from package **latex2exp**
#' @param y Name of the Obs values.
#' @param type Does the Obs vs Pop plot is *pop* or *ind*.
#' @param disp Display in *lin* or *log*. Default to *lin*.
#' @param Color Color of the dots. Default to *black*.
#' @param size Size of the dots. Default to *0.3*.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @importFrom readr read_delim
#' @import knitr
#' @import ggplot2
#' @import latex2exp
#' @importFrom scales trans_breaks
#' @importFrom scales trans_format
#' @importFrom scales label_math
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang .data

utils::globalVariables(c(".x"))

mlx.OvsP <- function(project.dir = "../monolix/",
                     project.name = "",
                     drug = NULL, y = NULL, type = "ind", disp = "lin",
                     Color = "black", size = .3) {

  ## DATA

  OBSvsPRED <- read_delim(paste0(project.dir, project.name, "/ChartsData/ObservationsVsPredictions/", y, "_obsVsPred.txt"), delim = ",",show_col_types = FALSE)

  ## LIMITS

  rangeY <- range(OBSvsPRED %>% select(starts_with(y), popPred, indivPredMode))
  minY <- max(.1, rangeY[1])
  maxY <- rangeY[2]
  orderY <- 10^(round(log10(maxY)) - 1)
  max.Y <- ceiling(maxY / orderY) * orderY

  ## PLOTS
  if(sum(OBSvsPRED %>% select(censored))==0){

    OvsiP <- ggplot(OBSvsPRED) +
      geom_point(aes(
        x = .data[["indivPredMode"]],
        y = .data[[y]]
      ),
      color = Color, size = size, alpha = .8
      ) +
      geom_abline(slope = 1, intercept = 0) +
      theme_classic() +
      theme(legend.position = "none", aspect.ratio = 1)


    OvspP <- ggplot(OBSvsPRED) +
      geom_point(aes(
        x = .data[["popPred"]],
        y = .data[[y]]
      ),
      color = Color, size = size, alpha = .8
      ) +
      geom_abline(slope = 1, intercept = 0) +
      theme_classic() +
      theme(legend.position = "none", aspect.ratio = 1)

  }else{
    OvsiP <- ggplot(OBSvsPRED) +
      geom_point(aes(
        x = .data[["indivPredMode"]],
        y = .data[[paste0(y, "_simBlq_mode")]]
      ),
      color = Color, size = size, alpha = .8
      ) +
      geom_abline(slope = 1, intercept = 0) +
      theme_classic() +
      theme(legend.position = "none", aspect.ratio = 1)


    OvspP <- ggplot(OBSvsPRED) +
      geom_point(aes(
        x = .data[["popPred"]],
        y = .data[[paste0(y, "_simBlq_mode")]]
      ),
      color = Color, size = size, alpha = .8
      ) +
      geom_abline(slope = 1, intercept = 0) +
      theme_classic() +
      theme(legend.position = "none", aspect.ratio = 1)
  }

  ## Conditionals SCALES

  if (disp != "log") {
    OvsiP <- OvsiP + scale_x_continuous(TeX(paste("Ind. pred.", drug)), limits = c(0, max.Y), expand = c(0, 0)) +
      scale_y_continuous(TeX(paste("Obs.", drug)), limits = c(0, max.Y), expand = c(0, 0))

    OvspP <- OvspP + scale_x_continuous(TeX(paste("Pop. pred.", drug)), limits = c(0, max.Y), expand = c(0, 0)) +
      scale_y_continuous(TeX(paste("Obs.", drug)), limits = c(0, max.Y), expand = c(0, 0))
  } else {
    if (minY>1000) {
      OvsiP <- OvsiP + scale_x_log10(TeX(paste("Ind. pred.", drug)), #limits = c(minY, max.Y),
                                     expand = c(0, 0),
                                     breaks = trans_breaks("log10", function(x) 10^x),
                                     labels = trans_format("log10", label_math(10^.x))) +
        scale_y_log10(TeX(paste("Obs.", drug)),# limits = c(minY, max.Y),
                      expand = c(0, 0),
                      breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", label_math(10^.x)))

      OvspP <- OvspP + scale_x_log10(TeX(paste("Pop. pred.", drug)), #limits = c(minY, max.Y),
                                     expand = c(0, 0),
                                     breaks = trans_breaks("log10", function(x) 10^x),
                                     labels = trans_format("log10", label_math(10^.x))) +
        scale_y_log10(TeX(paste("Obs.", drug)), #limits = c(minY, max.Y),
                      expand = c(0, 0),
                      breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", label_math(10^.x)))
    } else{
      OvsiP <- OvsiP + scale_x_log10(TeX(paste("Ind. pred.", drug)), limits = c(minY, max.Y), expand = c(0, 0)) +
        scale_y_log10(TeX(paste("Obs.", drug)), limits = c(minY, max.Y), expand = c(0, 0))

      OvspP <- OvspP + scale_x_log10(TeX(paste("Pop. pred.", drug)), limits = c(minY, max.Y), expand = c(0, 0)) +
        scale_y_log10(TeX(paste("Obs.", drug)), limits = c(minY, max.Y), expand = c(0, 0))
    }

  }

  ## Conditionals OUTPUT

  if (type == "ind") {
    OvsiP
  } else {
    if (type == "pop") {
      OvspP
    } else {
      grid.arrange(OvspP, OvsiP, nrow = 1)
    }
  }
}
