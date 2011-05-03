## Function that extends the head function
## to big data.frame objects.
##
## Not only big.head() gives a limited set of rows,
## but it laso gives a limited number of columns.
##
big.head <- function(x, n=5, s=5, f=5){
  
  stopifnot(length(s) == 1L)
  stopifnot(length(f) == 1L)
  stopifnot(length(n) == 1L)
  
  x1 <- x[seq_len(n), seq_len(s), drop=FALSE]
  xdots <- rep('...', length.out=r)
  x2 <- x[seq_len(n), seq(ncol(x) - f, ncol(x)), drop=FALSE]
  res <- data.frame(x1, xdots, x2)
  names(res)[s+1] <- "..."
  res
}

big.tail <- function(x, n=5, s=5, f=5){
  
  stopifnot(length(s) == 1L)
  stopifnot(length(f) == 1L)
  stopifnot(length(n) == 1L)
  
  x1 <- x[seq.int(to=nrow(x), length.out = n), seq_len(s), drop=FALSE]
  xdots <- rep('...', length.out = n)
  x2 <- x[seq.int(to=nrow(x), length.out = n), seq(ncol(x) - f, ncol(x)), drop=FALSE]
  res <- data.frame(x1, xdots, x2)
  names(res)[s+1] <- "..."
  res
}
