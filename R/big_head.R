## Function that extends the head function
## to big data.frame objects.
##
## Not only big.head() gives a limited set of rows,
## but it laso gives a limited number of columns.
##

#' Return the First or Last Part of an Object
#' 
#' Returns the first or last rows of a data frame like head() and tail(),
#' but also only returns the first and last columns. This has been
#' implemented to check big data frames.
#'
#' @param x a data.frame
#' @param n a single, positive integer, number of rows for the object to return 
#' @param s a single, positive integer
#' @param f a single, positive integer
#' @examples 
#' big.head(mtcars)
#' big.tail(mtcars)
#' big.tail(mtcars, 10)
#' big.head(mtcars, 10, 2, 4)
#' big.head(mtcars, , , 1)
#'
big.head <- function(x, n=5, s=5, f=5){
  
  stopifnot(length(s) == 1L)
  stopifnot(length(f) == 1L)
  stopifnot(length(n) == 1L)
  
  x1 <- x[seq_len(n), seq_len(s), drop=FALSE]
  xdots <- rep('...', length.out = n)
  x2 <- x[seq_len(n), seq(ncol(x) - f + 1, ncol(x)), drop=FALSE]
  res <- data.frame(x1, xdots, x2)
  names(res)[s + 1] <- "..."
  res
}

big.tail <- function(x, n=5, s=5, f=5){
  
  stopifnot(length(s) == 1L)
  stopifnot(length(f) == 1L)
  stopifnot(length(n) == 1L)
  
  x1 <- x[seq.int(to = nrow(x), length.out = n), seq_len(s), drop=FALSE]
  xdots <- rep('...', length.out = n)
  x2 <- x[seq.int(to=nrow(x), length.out = n), seq(ncol(x) - f + 1, ncol(x)), drop=FALSE]
  res <- data.frame(x1, xdots, x2)
  names(res)[s + 1] <- "..."
  res
}
