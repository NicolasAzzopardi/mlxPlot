#' This function plots a publication-ready IWRES distribution of a monolix project run.
#'
#' @param project.dir Absolute or relative name of the folder of the monolix project.
#' @param project.name Name of the monolix project file without *.mlxtran* extension. The project must be in *project.dir*
#' @param drug Text for the legend. Typicaly name of the drug. Can be a **TeX** object from package **latex2exp**
#' @param y Name of the Obs values.
#' @param type Does the IWRES plot is *"pdf"* or *"cdf"*.
#' @param Color Color of the bars or dots. Default to *black*.
#' @param size Size of the dots (for type = "cdf"). Default to *0.3*.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @importFrom readr read_delim
#' @import knitr
#' @import ggplot2
#' @import latex2exp
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang .data
#'

mlx.IWRES <- function(project.dir = "../monolix/",
                     project.name = "",
                     drug = NULL, y = NULL, type = "pdf",
                     Color = "black", size = .3) {

  ## DATA

  THEO <- read_delim(paste0(project.dir, project.name, "/ChartsData/DistributionOfTheResiduals/theoreticalGuides.txt"), delim = ",",show_col_types = FALSE)
  PDF  <- read_delim(paste0(project.dir, project.name, "/ChartsData/DistributionOfTheResiduals/", y, "_pdf.txt"), delim = ",",show_col_types = FALSE)
  CDF  <- read_delim(paste0(project.dir, project.name, "/ChartsData/DistributionOfTheResiduals/", y, "_cdf.txt"), delim = ",",show_col_types = FALSE)

  ## PLOT

  IWRESpdf <- ggplot() +
    theme_classic() +
    geom_col(data = PDF, aes(x = iwRes_abscissa, y = iwRes_pdf), fill = Color, alpha = .7) +
    geom_line(data = THEO, aes(x = abscissa, y = pdf)) +
    scale_y_continuous(TeX(paste("Individual weigthed residus", drug)), expand = c(0, 0)) +
    coord_cartesian(xlim = c(-4, 4)) +
    scale_x_continuous("", expand = c(0, 0))

  IWREScdf <- ggplot() +
    theme_classic() +
    geom_line(data = THEO, aes(x = abscissa, y = cdf)) +
    geom_line(data = CDF, aes(x = iwRes_abscissa, y = iwRes_cdf), size = 2, color = Color, alpha = .7) +
    scale_y_continuous(TeX(paste("Individual weigthed residus (cdf)", drug)), expand = c(0, 0)) +
    coord_cartesian(xlim = c(-4, 4), ylim = c(0, 1)) +
    scale_x_continuous("", expand = c(0, 0))

  ## Conditionals OUTPUT

  if (type == "pdf") {
    IWRESpdf
  } else {
    if (type == "cdf") {
      IWREScdf
    } else {
      grid.arrange(IWRESpdf, IWREScdf, nrow = 1)
    }
  }
}
