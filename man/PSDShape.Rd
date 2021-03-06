% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PSDShape.R
\name{PSDShape}
\alias{PSDShape}
\title{Pooled Standard Deviations of Shape}
\usage{
PSDShape(Coords, Sex)
}
\arguments{
\item{Coords}{Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)}

\item{Sex}{A character or factor vector recording sex for each individual as 'm' or 'f'}
}
\description{
The square root of the total variance of the coordinate's covariance matrix after overlaying male and female coordinates on top of each other.
}
