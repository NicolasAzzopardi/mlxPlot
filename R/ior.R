#' Install or require a list of packages.
#' @param pack List of packages names to require or if not installed, to install and require.
#' @keywords package install
#' @export ior
#' @examples # ior("ggplot2")

ior <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}
