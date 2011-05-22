\name{get_units}
\alias{get_units}
\alias{get_resolution}
\alias{get_resolution.numeric}
\alias{get_resolution.Spectra}

\title{Utility functions}

\usage{
get_units(object, ...)
get_resolution(object, ...)
}

\description{Retrieves the wavelengths units and the spectral resolution from \code{Spectra*} objects.}

\value{A vector.}

\arguments{
  \item{object}{an object inheriting from class \code{Spectra}, or a \code{"numeric"} vector for \code{get_resolution}}
  \item{...}{ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}