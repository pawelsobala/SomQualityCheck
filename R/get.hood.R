#' Get the indices of the neighboring nodes
#'
#' @param x TODO
#' @param y TODO
#' @param xdim TODO
#' @param ydim TODO
#' @return TODO
#' @export

get.hood <-
  function(x, y, xdim, ydim) {
    hood <- list()
    # gettheneighbors
    t <- c(x, y + 1)
    r <- c(x + 1, y)
    b <- c(x, y - 1)
    l <- c(x - 1, y)
    # checkiftheyarevalid
    if (t[2] <= ydim) {
      hood[["t"]] <- t
    }
    if (r[1] <= xdim) {
      hood[["r"]] <- r
    }
    if (b[2] > 0) {
      hood[["b"]] <- b
    }
    if (l[1] > 0) {
      hood[["l"]] <- l
    }
    hood
  }
