#' A function forest plot the Time spent vs. survival.
#'
#' This function allows you to plot...
#' @param project.file Monolix (2018) project file.
#' @param c.name Name of the concentration in the mlxtran project.
#' @param prop.name Name of the sigma parameter. "b" is default.
#' @param survtab Table of survival: 3 columns ID, TIME, PROG.
#' @param simtimes Time of evaluation
#' @param c.range Concentrations
#' @param hr.range HR range to plot (powers of 2). default: c(-3,3) gives [.125 - 8].
#' @keywords monolix survival
#' @export forest.timeC
#' @examples
#' @import ggplot2
#' @import purrr
#' @import mlxR
#' @import survival
#' @import survivalAnalysis

forest.timeC <- function(project.file="./monolix/181011_2cCLk0.mlxtran"
                         , c.name="C1"
                         , prop.name="b"
                         , survtab=PFS
                         , simtimes=7
                         , hr.range = c(-3,3)
                         , c.range = c(5, 10, 20, 30, 40, 50, 75, 100, 150, 200)
){

  sim.param <- list("mode")

  if(prop.name=="b"){
    sim.param <- list("mode",c(b=0))
  }
  if(prop.name=="b1"){
    sim.param <- list("mode",c(b1=0))
  }
  if(prop.name=="b2"){
    sim.param <- list("mode",c(b2=0))
  }


  add.timec <- list(
    section = "[LONGITUDINAL]", block = "EQUATION:",
    formula = c(
      paste0("expo", c.range, " = 0"),
      paste0("if(", c.name, ">", c.range, ") expo", c.range, "=1 end"),
      paste0("ddt_timeC", c.range, " = expo", c.range)
    )
  )
  timec.names <- paste0("timeC", c.range)
  out.timec <- list(name = timec.names, time = simtimes)

  sim.res1 <- simulx(
    project = project.file,
    addlines = add.timec,
    output = out.timec,
    parameter = sim.param
  )

  timecs <- list()
  for(n in 1:length(timec.names)){
    timecs[[n]] <- eval(parse(text = paste0("sim.res1$", timec.names[n])))
  }
  timecs <-
    left_join(timecs %>% reduce(left_join, by = c("id", "time")) %>%
                mutate(`id` = as.character(`id`)), sim.res1$originalId %>%
                mutate(`id` = as.character(`newId`)), by = "id") %>%
    select(-`id`, -`newId`) %>%
    select(`id` = `oriId`, everything()) %>%
    rename(`ID` = `id`) %>% mutate_at(vars(`timec.names`), function( x ) {
      ifelse(x > stats::median(x), "> median", "<= median")
    }) %>% mutate(ID=as.numeric(as.character(ID)))

  survtab <- survtab %>% inner_join(timecs, by = "ID")

  map(timec.names, function(by) {
    analyse_multivariate(survtab, vars(TIME, PROG), covariates = list(by))
  }
  ) %>%
    forest_plot(
      endpoint_labeller = c(time = "PFS"), relative_widths = c(.7, 1.5, .8),
      HR_x_limits = 2 ^ hr.range,
      HR_x_breaks = 2 ^ seq(hr.range[1],hr.range[2],by=1),
      # orderer = ~order(HR),
      labels_displayed = c("factor"),
      values_displayed = c("HR", "CI", "p"), # , "subgroup_n"),
      ggtheme = theme_bw(base_size = 10)
    )+theme(axis.title.x = element_text(angle = 45))
}



