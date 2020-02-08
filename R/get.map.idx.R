#' Get the index in the map
#' 
#' @param x TODO
#' @param y TODO
#' @param xdim TODO
#' @return TODO
#' @export

get.map.idx <-
function(x, y, xdim) {
  # gettheindexinthemap
  ((y - 1) * xdim) + x
}
