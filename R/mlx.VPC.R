#' A function to plot VPCs from a Monolix project.
#'
#' This function allows you to have almost a publication ready VPC plot of a monolix run.
#' @param project.dir Absolute or relative name of the folder of the monolix project.
#' @param project.name Name of the monolix project file without *.mlxtran* extension.. The project must be in *project.dir*
#' @param drug Text for the legend. Typicaly name of the drug. Can be a **TeX** object from package **latex2exp**
#' @param y Name of the Obs values.
#' @param disp Display in *lin* or *log*. Default to *lin*.
#' @param obs Plot observed data?. Default to *TRUE*.
#' @param size Size of the observed data dots. Default to *0.5*. Useless if *obs = FALSE*.
#' @param time.unit Time unit for the x legend. Default to *"day"*.
#' @param simulated Use simulated Blq data? Default to *FALSE*.
#' @param corrected Dose the predictions are corrected? Default to *FALSE*.
#' @param colors Color palette : order: c(lines and dots, central ribbon, lower and uper ribbon). Default to *c("#4682B4", "#FF0000", "#0080FF")*.
#' @keywords monolix
#' @export
#' @examples # getwd()
#' @import dplyr
#' @importFrom readr read_delim
#' @import knitr
#' @import ggplot2
#' @importFrom scales squish
#' @import latex2exp
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang .data
#'
mlx.VPC <- function(project.dir = "../monolix/",
                    project.name = "",
                    drug = NULL, y = NULL, disp = "lin", obs=TRUE, size=.5, time.unit="day", simulated=FALSE, corrected = FALSE,
                    colors = c("#4682B4", "#FF0000", "#0080FF")) {

  ## DATA

  vpcobsdata <- read_delim(paste0(project.dir, project.name, "/ChartsData/VisualPredictiveCheck/", y, "_observations.txt"), delim = ",", show_col_types = FALSE)
  vpcpercdata <- read_delim(paste0(project.dir, project.name, "/ChartsData/VisualPredictiveCheck/", y, "_percentiles.txt"), delim = ",", show_col_types = FALSE)
  vpcbindata <- read_delim(paste0(project.dir, project.name, "/ChartsData/VisualPredictiveCheck/", y, "_bins.txt"), delim = ",", show_col_types = FALSE)

  if (corrected == TRUE) {    PC <- "_pc"  } else {    PC <- ""  }
  if (simulated == TRUE) {    SIM <- "_simBlq"  } else {    SIM <- ""  }

  ## PLOT

  VPCplot <- ggplot() +
    theme_classic() +
    geom_ribbon(data = vpcpercdata,
                aes(x = .data[["bins_middles"]],
                           ymin = .data[[paste0("theoretical_upper_piLower", PC)]],
                           ymax = .data[[paste0("theoretical_upper_piUpper", PC)]]
                ), fill = colors[3], alpha = 0.3) +
    geom_ribbon(data = vpcpercdata,
                aes(x = .data[["bins_middles"]],
                           ymin = .data[[paste0("theoretical_median_piLower", PC)]],
                           ymax = .data[[paste0("theoretical_median_piUpper", PC)]]
                ), fill = colors[2], alpha = 0.2) +
    geom_ribbon(data = vpcpercdata,
                aes(x = .data[["bins_middles"]],
                           ymin = .data[[paste0("theoretical_lower_piLower", PC)]],
                           ymax = .data[[paste0("theoretical_lower_piUpper", PC)]]
                ), fill = colors[3], alpha = 0.3) +
    geom_line(data = vpcpercdata, aes(x = .data[["bins_middles"]], y = .data[["empirical_upper"]]), color = colors[1]) +
    geom_line(data = vpcpercdata, aes(x = .data[["bins_middles"]], y = .data[["empirical_median"]]), color = colors[1]) +
    geom_line(data = vpcpercdata, aes(x = .data[["bins_middles"]], y = .data[["empirical_lower"]]), color = colors[1]) +
    xlab(paste0("Time (", time.unit, ")")) +
    ylab(TeX(drug)) +
    scale_x_continuous(expand = c(0, 0))

  ## Conditionals OUTPUT

  if (obs == TRUE) {
    VPCplot + geom_point(data = vpcobsdata, aes(x = .data[["time"]], .data[[paste0(y, SIM, PC)]]), size=size, color=colors[1], alpha = .5)
  } else {
    VPCplot
  }

  if (disp == "log") {
#    VPCplot + scale_y_log10(expand = c(0, 0), oob = oob_squish())
    VPCplot + scale_y_log10(expand = c(0, 0), oob = scales::squish)
  } else {
    VPCplot
  }
}
