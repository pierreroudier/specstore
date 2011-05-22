\name{mutate}

\alias{mutate}
\alias{mutate.Spectra}
\alias{mutate.SpectraDataFrame}

\title{plyr package to the Spectra objects, which it wraps.}

\usage{mutate.Spectra(.data, ...)}

\description{plyr package to the Spectra objects, which it wraps.}

\details{This function is very similar to \code{\link{transform}} but it executes
the transformations iteratively so that later transformations can use the
columns created by earlier transformations. Like transform, unnamed
components are silently dropped.

Mutate seems to be considerably faster than transform for large data
frames.}

\seealso{\code{\link{mutate}}}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}, from code from Hadley Wickham}

\arguments{
  \item{.data}{an object inheriting from the \code{Spectra} class}
  \item{...}{named parameters giving definitions of new columns}
}
