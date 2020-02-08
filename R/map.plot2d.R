#' Plot the map in 2 - dimensions with data point and neurons colored by label
#' 
#' @param map TODO
#' @param x.idx TODO
#' @param y.idx TODO
#' @param show.dead TODO
#' @param highlight TODO
#' @param use.rgl TODO
#' @return TODO
#' @export

map.plot2d <-
function(map, x.idx = 1, y.idx = 2,
                       show.dead = F, highlight = NULL,
                       use.rgl = F) {
  n <- dim(map$data)[1]
  # needtocheckifthedatahaslabels
  if (is.null(map$labels)) {
    # getthenodelabels
    node.labels <- rep(1, dim(map$neurons)[1])
    pal <- "white"
    # plotdatapoints
    if (use.rgl) {
      plot3d(map$data[, x.idx], map$data[, y.idx],
        rep(0, n),
        col = c("grey"), pch = rep(20, n),
        cex = rep(0.5, n)
      )
    } else {
      plot(map$data[, x.idx], map$data[, y.idx],
        col = c("grey"), pch = 20, cex = 0.5
      )
    }
  } else {
    # getthenodelabels
    node.labels <- as.numeric(as.factor(
      map.label.accuracy(map)$neurons
    ))
    if (max(node.labels, na.rm = T) < 10) {
      pal <- c(brewer.pal(
        max(node.labels, na.rm = T),
        "Set1"
      ), "white")
    } else {
      pal <- c(
        rainbow(max(node.labels, na.rm = T)),
        "white"
      )
    }
    na.lab <- max(node.labels, na.rm = T) + 1
    node.labels[which(is.na(node.labels))] <- na.lab
    # plotdatapoints
    if (use.rgl) {
      plot3d(map$data[, x.idx], map$data[, y.idx],
        rep(0, n),
        col = pal[as.factor(
          as.data.frame(map$labels)[, 1]
        )],
        pch = rep(20, n), cex = rep(0.5, n)
      )
    } else {
      plot(map$data[, x.idx], map$data[, y.idx],
        col = pal[as.factor(
          as.data.frame(map$labels)[, 1]
        )],
        pch = 20, cex = 0.5
      )
    }
  } # endif
  # plotedgesbeforenodesowecanseethecolors
  for (i in 1:dim(map$neurons)[1]) {
    # getthepositioninthemap
    coord <- get.map.coord(i, map$xdim, map$ydim)
    # gettheneighboringnodes
    hood <- get.hood(coord$x, coord$y, map$xdim, map$ydim)
    # addsegmentsbetweenthenodes
    for (j in 1:length(hood)) {
      # convertbacktoindex
      point <- hood[[j]]
      l <- get.map.idx(point[1], point[2], map$xdim)
      if (use.rgl) {
        segments3d(
          c(
            map$neurons[i, x.idx],
            map$neurons[l, x.idx]
          ),
          c(
            map$neurons[i, y.idx],
            map$neurons[l, y.idx]
          ), c(0, 0)
        )
      } else {
        segments(
          map$neurons[i, x.idx],
          map$neurons[i, y.idx],
          map$neurons[l, x.idx],
          map$neurons[l, y.idx]
        )
      }
    } # endfor(j)
  } # endfor(i)
  # checkifweshouldhighlightdeadnodes
  if (is.null(highlight) && show.dead) {
    highlight <- which(!(1:(map$xdim * map$ydim)
    %in% unique(map$visual)))
  }
  # plotmapnodes
  if (is.null(highlight)) {
    if (use.rgl) {
      points3d(map$neurons[, x.idx], map$neurons[, y.idx],
        rep(0, n),
        col = "black",
        bg = pal[node.labels],
        pch = rep(21, n), cex = rep(20, n)
      )
    } else {
      points(map$neurons[, x.idx], map$neurons[, y.idx],
        col = "black", bg = pal[node.labels], pch = 21
      )
    }
  } else {
    all.n <- 1:dim(map$neurons)[1]
    h <- all.n %in% highlight
    not.h <- !h
    if (use.rgl) {
      points3d(map$neurons[not.h, x.idx],
        map$neurons[not.h, y.idx],
        rep(0, n),
        col = "black",
        bg = pal[node.labels[not.h]],
        pch = rep(21, n)
      )
      points3d(map$neurons[h, x.idx],
        map$neurons[h, y.idx], rep(0, n),
        col = "darkgoldenrod1",
        bg = pal[node.labels[h]], pch = rep(23, n)
      )
    } else {
      points(map$neurons[not.h, x.idx],
        map$neurons[not.h, y.idx],
        col = "black",
        bg = pal[node.labels[not.h]],
        pch = rep(21, n)
      )
      points(map$neurons[h, x.idx],
        map$neurons[h, y.idx],
        col = "darkgoldenrod1",
        bg = pal[node.labels[h]], pch = rep(23, n)
      )
    } # endif
  } # endif
}
