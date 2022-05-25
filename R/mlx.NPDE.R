#' A function to export Populationparameters.txt file of a Monolix project to xlsx file.
#'
#' This function plots a publication-ready NPDE of a model project.
#' @param project.dir Absolute or relative name of the folder of the monolix project.
#' @param project.name Name of the monolix project file without *.mlxtran* extension.. The project must be in *project.dir*
#' @param drug Text for the legend. Typicaly name of the drug. Can be a **TeX** object from package **latex2exp**
#' @param y Name of the Obs values.
#' @param type Does the NPDE plot is *"pdf"* or *"cdf"*.
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

mlx.NPDE <- function(project.dir = "../monolix/",
                     project.name = "",
                     drug = NULL, y = NULL, type = "pdf",
                     Color = "black", size = .3) {

  ## DATA

  Res_theo <- read_delim(paste0(project.dir, project.name, "/ChartsData/DistributionOfTheResiduals/theoreticalGuides.txt"), delim = ",",show_col_types = FALSE)
  NPDE_pdf <- read_delim(paste0(project.dir, project.name, "/ChartsData/DistributionOfTheResiduals/", y, "_pdf.txt"), delim = ",",show_col_types = FALSE)
  NPDE_cdf <- read_delim(paste0(project.dir, project.name, "/ChartsData/DistributionOfTheResiduals/", y, "_cdf.txt"), delim = ",",show_col_types = FALSE)

  ## PLOT

  NPDEpdf <- ggplot() +
    theme_classic() +
    geom_col(data = NPDE_pdf, aes(x = npde_abscissa, y = npde_pdf), fill = Color, alpha = .7) +
    geom_line(data = Res_theo, aes(x = abscissa, y = pdf)) +
    scale_y_continuous(TeX(paste("NPDE", drug)), expand = c(0, 0)) +
    coord_cartesian(xlim = c(-4, 4)) +
    scale_x_continuous("", expand = c(0, 0))

  NPDEcdf <- ggplot() +
    theme_classic() +
    geom_line(data = Res_theo, aes(x = abscissa, y = cdf)) +
    geom_line(data = NPDE_cdf, aes(x = npde_abscissa, y = npde_cdf), size = 2, color = Color, alpha = .7) +
    scale_y_continuous(TeX(paste("NPDE (cdf)", drug)), expand = c(0, 0)) +
    coord_cartesian(xlim = c(-4, 4), ylim = c(0, 1)) +
    scale_x_continuous("", expand = c(0, 0))

  ## Conditionals OUTPUT

  if (type == "pdf") {
    NPDEpdf
  } else {
    if (type == "cdf") {
      NPDEcdf
    } else {
      grid.arrange(NPDEpdf, NPDEcdf, nrow = 1)
    }
  }
}
