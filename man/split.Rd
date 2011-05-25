\name{split}

\alias{split}
\alias{split.SpectraDataFrame}

\title{Utility functions}

\usage{
split(x, f, drop = FALSE, ...)
}

\description{Divide a SpectraDataFrame object into Groups}

\details{
This is an adaptation of the \code{split} function in the base package.
}

\value{A list of SpectraDataFrame objects.}

\arguments{
  \item{x}{vector or data frame containing values to be divided into groups.}
  \item{f}{a \code{"factor"} in the sense that \code{as.factor(f)} defines the grouping, or a list of such factors in which case their interaction is used for the grouping.}
  \item{drop}{logical indicating if levels that do not occur should be dropped (if \code{f} is a \code{factor} or a list).}
  \item{value}{a list of vectors or data frames compatible with a splitting of \code{x}. Recycling applies if the lengths do not match.}
  \item{...}{further potential arguments passed to methods.}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}