#' Gets the x, y position in the map
#' 
#' @param i TODO
#' @param xdim TODO
#' @param ydim TODO
#' @return TODO
#' @export

get.map.coord <-
function(i, xdim, ydim) {
  # getthepositioninthemap
  x <- i %% xdim
  if (x == 0) x <- xdim
  y <- ceiling(i / xdim)
  list(x = x, y = y)
}
