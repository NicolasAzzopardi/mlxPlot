#' Get individual params (EBEs) from a Monolix run.
#' @param project.dir Absolute or relative name of the folder of the monolix project.
#' @param project.name Name of the monolix project file without *.mlxtran* extension.. The project must be in *project.dir*.
#' @param covs Optional covariates to include in the extraxtion (vector of strings).
#' @keywords monolix
#' @export

mlxind.params <- function(project.dir = "../monolix/",
                      project.name = "",
                      covs = NA) {
  NoMode <- function(x) {
    gsub("_mode", "", x)
  }

  read_csv(paste0(project.dir, project.name, "/IndividualParameters/estimatedIndividualParameters.txt"), show_col_types = FALSE) |>
    select(ID = id, ends_with("_mode"), matches(covs)) |>
    select(-starts_with("logt")) |>
    rename_with(.fn = NoMode, .cols = ends_with("mode"))
}



