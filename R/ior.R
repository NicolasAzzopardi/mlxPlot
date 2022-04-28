#' Install or require a list of packages.
#' @param pack List of packages names to require or if not installed, to install and require.
#' @keywords package install
#' @export ior
#' @keywords package
#' @examples # ior("ggplot2")
#' @import utils

ior <- function(pack, mirror = "https://cloud.r-project.org/"){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE, repos=mirror)
  sapply(pack, require, character.only = TRUE)
}



impute_all <- function(.tbl, .na, ...) {
  for (i in 1:length(.tbl)) {
    .tbl[[i]] <- na.tools::na.replace(.tbl[[i]], .na, ...)
  }
  .tbl
}


impute_at <- function(.tbl, .na, .vars, ...) {
  #.vars <- dplyr::select_vars(names(.tbl), .vars)
  .vars <- tidyselect::vars_select(names(.tbl), .vars)
  for (i in .vars) {
    .tbl[[i]] <- na.tools::na.replace(x = .tbl[[i]], .na = .na, ...)
  }
  .tbl
}
