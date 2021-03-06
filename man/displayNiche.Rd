% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/displayNiche.R
\name{displayNiche}
\alias{displayNiche}
\title{Plot the climatic niche of a species}
\usage{
displayNiche(
  data,
  lon = "x",
  lat = "y",
  name = "worldclim",
  res = 10,
  var = c("tmin", "tmax", "prec"),
  tres = "year",
  path = "",
  extent = NA,
  hist = FALSE,
  map = FALSE
)
}
\arguments{
\item{data}{raster layer or stack of the presence of one or multiple species, 
also takes a list}

\item{lon}{Name of longitude column in data file, default x}

\item{lat}{Name of latitude column in data file, default y}

\item{name}{specifying which environmental data to use, currently only worldclim is supported.}

\item{res}{specifying the desired spatial resolution. Valid resolutions are 0.5, 2.5, 5, 10 (minutes of a degree).}

\item{var}{specifying which variable to get, can be tmin, tmax and/or prec or bio.}

\item{tres}{specifying the desired temporal resolution. One of year, month, quarter.}

\item{path}{Character. Path name indicating where to store the data. Default is the current working directory.}

\item{extent}{specify extent of area considered}

\item{hist}{logical. specify if a histogram should be added to the plot.}

\item{map}{logical. specify if a map should be added to the plot.}
}
\value{
plot of climatic niche of species
}
\description{
This function extracts worldclim data for a given spatial object 
and displays the worldclim data for the spatial points.
}
\examples{
\dontrun{
# Load library
library(climateNiche)

# Load House sparrow data
data(Passer_domesticus)

# Create plot of climatic niche
displayNiche(data=Passer_domesticus)

# Load Great sparrow data
data(Passer_motitensis)

# Create list of the two species
Passer_spp <- list(Passer_domesticus, Passer_motitensis)

# Create plot of climatic niche
displayNiche(data=Passer_spp)
}
}
