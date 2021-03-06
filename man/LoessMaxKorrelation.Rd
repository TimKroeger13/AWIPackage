% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LoessMaxKorrelation.R
\name{LoessMaxKorrelation}
\alias{LoessMaxKorrelation}
\title{LoessMaxKorrelation}
\usage{
LoessMaxKorrelation(main, folder, thick, offset, IgnorePercent = 0.1)
}
\arguments{
\item{main}{Vector from FolderCreater.}

\item{folder}{Folder structur to calculate all values from.}

\item{thick}{Thickness of age intervals to interpolate to.}

\item{offset}{Size of the area in which all correlations are calculated. must be dividable by the thickness!}

\item{IgnorePercent}{Change this values carefully. Specifies a minimum percentage of the read data that should not be NA.}
}
\value{
Returns a Dataframe with all Min and Max value for each element of the FOlder structure.
}
\description{
Calculates all Max and Min values for an Folder structure with the Loess function.
}
\note{
This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.
}
\author{
Tim Kröger
}
