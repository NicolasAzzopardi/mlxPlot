% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlx.OvsP.R
\name{mlx.OvsP}
\alias{mlx.OvsP}
\title{A function to export Populationparameters.txt file of a Monolix project to xlsx file.}
\usage{
mlx.OvsP(
  project.dir = "../monolix/",
  project.name = "",
  drug = NULL,
  y = NULL,
  type = "ind",
  disp = "lin",
  Color = "black",
  size = 0.3
)
}
\arguments{
\item{project.dir}{Absolute or relative name of the folder of the monolix project.}

\item{project.name}{Name of the monolix project file without *.mlxtran* extension.. The project must be in *project.dir*}

\item{drug}{Text for the legend. Typicaly name of the drug. Can be a **TeX** object from package **latex2exp**}

\item{y}{Name of the Obs values.}

\item{type}{Does the Obs vs Pop plot is *pop* or *ind*.}

\item{disp}{Display in *lin* or *log*. Default to *lin*.}

\item{Color}{Color of the dots. Default to *black*.}

\item{size}{Size of the dots. Default to *0.3*.}
}
\description{
This function allows you to have almost a publication ready table of the population parameter of a model project.
}
\examples{
# getwd()
}
\keyword{monolix}
