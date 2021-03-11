#' A function to plot VPCs from a Monolix project.
#'
#' This function allows you to have almost a publication ready VPC plot of a monolix project.
#' @param project.dir Absolute name of the folder of the monolix project.
#' @param project.name Name of the monolix project. The project must be in *project.dir*
#' @param drug Text for the legend. Typicaly name of the drug. Can be a **TeX** object from package **latex2exp**
#' @param y Name of the Obs values.
#' @param disp Display in *lin* or *log*. Default to *lin*.
#' @param size Size of the dots. Default to *0.5*.
#' @param simulated Use simulated Blq data? Default to *FALSE*.
#' @param corrected Dose the predictions are corrected? Default to *FALSE*.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @import readr
#' @import knitr
#' @import ggplot2
#' @import scales
#' @import latex2exp
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang .data
#'
mlx.VPC <- function(project.dir = "/home/nicolas/Documents/Etudes/Thymo/monolix/",
                    project.name = "190916_190722_THYMO_VALID_retro",
                    drug = NULL, y = NULL, disp = "lin", size=.5, simulated=FALSE, corrected = FALSE) {

  ## DATA

  vpcobsdata <- read_delim(paste0(project.dir, project.name, "/ChartsData/VisualPredictiveCheck/", y, "_observations.txt"), delim = ",")
  vpcpercdata <- read_delim(paste0(project.dir, project.name, "/ChartsData/VisualPredictiveCheck/", y, "_percentiles.txt"), delim = ",")
  vpcbindata <- read_delim(paste0(project.dir, project.name, "/ChartsData/VisualPredictiveCheck/", y, "_bins.txt"), delim = ",")

  if (corrected == TRUE) {    PC <- "_pc"  } else {    PC <- ""  }
  if (simulated == TRUE) {    SIM <- "_simBlq"  } else {    SIM <- ""  }

  ## PLOT

  VPCplot <- ggplot() +
    theme_classic() +
    geom_ribbon(data = vpcpercdata,
                aes_string(x = "bins_middles",
                           ymin = paste0("theoretical_upper_piLower", PC),
                           ymax = paste0("theoretical_upper_piUpper", PC)
                ), fill = "#0080ff", alpha = 0.3) +
    geom_ribbon(data = vpcpercdata,
                aes_string(x = "bins_middles",
                           ymin = paste0("theoretical_median_piLower", PC),
                           ymax = paste0("theoretical_median_piUpper", PC)
                ), fill = "red", alpha = 0.2) +
    geom_ribbon(data = vpcpercdata,
                aes_string(x = "bins_middles",
                           ymin = paste0("theoretical_lower_piLower", PC),
                           ymax = paste0("theoretical_lower_piUpper", PC)
                ), fill = "#0080ff", alpha = 0.3) +
    geom_point(data = vpcobsdata, aes_string(x = "time", paste0(y, SIM, PC)), size=size, color="#4682b4", alpha = .5) +
    geom_line(data = vpcpercdata, aes_string(x = "bins_middles", y = "empirical_upper"), color = "#4682b4") +
    geom_line(data = vpcpercdata, aes_string(x = "bins_middles", y = "empirical_median"), color = "#4682b4") +
    geom_line(data = vpcpercdata, aes_string(x = "bins_middles", y = "empirical_lower"), color = "#4682b4") +
    xlab("Time (day)") +
    ylab(TeX(drug)) +
    scale_x_continuous(expand = c(0, 0))

  ## Conditionals OUTPUT

  if (disp == "log") {
#    VPCplot + scale_y_log10(expand = c(0, 0), oob = oob_squish())
    VPCplot + scale_y_log10(expand = c(0, 0), oob = scales::squish())
  } else {
    VPCplot
  }
}
