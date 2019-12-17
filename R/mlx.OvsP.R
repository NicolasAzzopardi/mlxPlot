#' A function to export Populationparameters.txt file of a Monolix project to xlsx file.
#'
#' This function allows you to have almost a publication ready table of the population parameter of a model project.
#' @param mlxproj Name of the monolix project. The project must be in ./monolix/
#' @param drug Text for the legend. Typicaly name of the drug.
#' @param y First letters of the file.
#' @param pop Logical. does the Obs vs Pop have to be plotted.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @import readr
#' @import knitr
#' @import ggplot2
#' @import latex2exp
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang .data
#'
mlx.OvsP <- function(mlxproj,drug=NULL,y=NULL,pop=FALSE){
  OBSvsPRED <- read_delim(paste0("./monolix/", mlxproj, "/ChartsData/ObservationsVsPredictions/",y,"_obsVsPred.txt"),
                          delim = ",", col_types = cols(ID = col_character(), time = col_double(), pop = col_double(), popPred = col_double(), indivPredMean = col_double(), indivPredMode = col_double()))

  max.Y = ceiling(max(OBSvsPRED %>% select(.data$popPred,.data$indivPredMode))/100)*100
  OvsiP = ggplot(OBSvsPRED) +
    geom_point(aes(x = .data$indivPredMode, y = .data$y_simBlq_mode,color=as.factor(.data$censored)),alpha=.8) +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous(paste("Individual predicted",drug), limits = c(0, max.Y), expand = c(0, 0)) +
    scale_y_continuous(paste("Observed", drug), limits = c(0, max.Y), expand = c(0, 0)) +
    theme_classic() + theme(legend.position="none",aspect.ratio = 1)

  OvspP = ggplot(OBSvsPRED) +
    geom_point(aes(x = .data$popPred, y = .data$y_simBlq_mode,color=as.factor(.data$censored)),alpha=.8) +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous(paste("Population predicted",drug), limits = c(0, max.Y), expand = c(0, 0)) +
    scale_y_continuous(paste("Observed", drug), limits = c(0, max.Y), expand = c(0, 0)) +
    theme_classic() + theme(legend.position="none",aspect.ratio = 1)

  if(pop==FALSE){
    OvsiP
  } else {
    grid.arrange(OvspP,OvsiP,nrow=1)
  }
}
