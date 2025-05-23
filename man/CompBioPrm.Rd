% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CompareBiologicalPrms.R
\name{CompBioPrm}
\alias{CompBioPrm}
\title{Compares biological parameters from different Atlantis models}
\usage{
CompBioPrm(file1, file2, file1group, file2group, diffmodel)
}
\arguments{
\item{x}{Description of parameter x}
}
\value{
Description of the returned object
}
\description{
Script to compare single biological parameters from two different prm files from the same or different models.
Does not currently support parameters supplied by an array.
In theory could be used for harvest files but those are mostly arrays.
Jacob Kasper, MFRI.
}
\examples{
##CompBioPrm("E:/Atlantis/input/atlantis6702/Biological_6702_2025_02_07_02.prm",
"E:/Atlantis/NoBa_params/nordic_biology.prm",
"WMW", "MWH", 1)
my_function(10)
}
