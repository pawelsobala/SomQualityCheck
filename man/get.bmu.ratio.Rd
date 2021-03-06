% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.bmu.ratio.R
\name{get.bmu.ratio}
\alias{get.bmu.ratio}
\title{The ratio of neurons that are best matching units for a data point}
\usage{
get.bmu.ratio(dist.cross)
}
\arguments{
\item{dist.cross}{the distance matrix between the data points and the map neurons}
}
\value{
List containing a value and a vector:
ratio - the ratio of neurons that are a BMU for a
data point .
neurons - number of data points mapped to each neuron
}
\description{
The ratio of neurons that are best matching units for a data point
}
