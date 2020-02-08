#' Plot the map in 3 - dimensions with data point and neurons colored by label
#' 
#' @param map TODO
#' @param x.idx TODO
#' @param y.idx TODO
#' @param z.idx TODO
#' @param show.dead TODO
#' @param highlight TODO
#' @param use.rgl TODO
#' @return TODO
#' @export

map.plot3d <-
function(map, x.idx = 1, y.idx = 2,
                       z.idx = 3, show.data = T,
                       show.map = T, show.dead = F,
                       highlight = NULL, use.rgl = F) {
  # resetthedevicebeforedrawing
  if (use.rgl) {
    clear3d()
  }
  # needtocheckifthedatahaslabels
  if (is.null(map$labels)) {
    # getthenodelabels
    node.labels <- rep(1, dim(map$neurons)[1])
    pal <- "white"
    # checkifweshoulddrawthedatapoints
    if (show.data) {
      # plotdatapoints
      if (use.rgl) {
        plot3d(map$data[, x.idx], map$data[, y.idx],
          map$data[, z.idx],
          col = c("grey")
        )
      } else {
        s <- scatterplot3d(map$data[, x.idx],
          map$data[, y.idx],
          map$data[, z.idx],
          color = "grey",
          pch = 20, cex.symbols = 0.5
        )
      }
    } # endif
  } else {
    # getthenodelabels
    node.labels <- as.numeric(
      as.factor(map.label.accuracy(map)$neurons)
    )
    if (max(node.labels, na.rm = T) < 10) {
      pal <- c(brewer.pal(
        max(node.labels, na.rm = T),
        "Set1"
      ), "white")
    } # getmaxwithNAs
    else {
      pal <- c(
        rainbow(max(node.labels, na.rm = T)),
        "white"
      )
    }
    node.labels[which(is.na(node.labels))] <-
      max(node.labels, na.rm = T) + 1
    # checkifweshoulddrawthedatapoints
    if (show.data) {
      # plotdatapoints
      if (use.rgl) {
        plot3d(map$data[, x.idx], map$data[, y.idx],
          map$data[, z.idx],
          col = pal[as.factor(
            as.data.frame(map$labels)[, 1]
          )]
        )
      } else {
        s <- scatterplot3d(map$data[, x.idx],
          map$data[, y.idx],
          map$data[, z.idx],
          color = pal[as.factor(
            as.data.frame(map$labels)[, 1]
          )],
          pch = 20, cex.symbols = 0.5
        )
      }
    } # endif
  } # endif
  # checkifweshoulddrawthemap
  if (show.map) {
    # usegreyinsteadofwhiteforunlabeledwithrgl
    if (use.rgl) pal[length(pal)] <- "grey"
    # plotedgesbeforenodesowecanseethecolors
    for (i in 1:dim(map$neurons)[1]) {
      # getthepositioninthemap
      coord <- get.map.coord(i, map$xdim, map$ydim)
      # gettheneighboringnodes
      hood <- get.hood(
        coord$x, coord$y, map$xdim,
        map$ydim
      )
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
            ),
            c(
              map$neurons[i, z.idx],
              map$neurons[l, z.idx]
            )
          )
        } else {
          p1 <- s$xyz.convert(
            map$neurons[i, x.idx],
            map$neurons[i, y.idx],
            map$neurons[i, z.idx]
          )
          p2 <- s$xyz.convert(
            map$neurons[l, x.idx],
            map$neurons[l, y.idx],
            map$neurons[l, z.idx]
          )
          segments(p1$x, p1$y, p2$x, p2$y)
        } # endif
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
        points3d(map$neurons[, x.idx],
          map$neurons[, y.idx],
          map$neurons[, z.idx],
          col = pal[node.labels], size = 10
        )
      } else {
        nodes.2d <- s$xyz.convert(
          map$neurons[, x.idx],
          map$neurons[, y.idx],
          map$neurons[, z.idx]
        )
        points(nodes.2d$x, nodes.2d$y,
          col = "black",
          bg = pal[node.labels], pch = 21
        )
      }
    } else {
      all.n <- 1:dim(map$neurons)[1]
      h <- all.n %in% highlight
      not.h <- !h
      if (use.rgl) {
        points3d(map$neurons[not.h, x.idx],
          map$neurons[not.h, y.idx],
          map$neurons[not.h, z.idx],
          col = pal[node.labels[not.h]], size = 2
        )
        points3d(map$neurons[h, x.idx],
          map$neurons[h, y.idx],
          map$neurons[not.h, z.idx],
          col = "darkgoldenrod1", size = 2
        )
      } else {
        nodes.2d <- s$xyz.convert(
          map$neurons[not.h, x.idx],
          map$neurons[not.h, y.idx],
          map$neurons[not.h, z.idx]
        )
        nodes.h.2d <- s$xyz.convert(
          map$neurons[h, x.idx],
          map$neurons[h, y.idx],
          map$neurons[h, z.idx]
        )
        points(nodes.2d$x, nodes.2d$y,
          col = "black",
          bg = pal[node.labels[not.h]], pch = 21
        )
        points(nodes.h.2d$x, nodes.h.2d$y,
          col = "darkgoldenrod1",
          bg = pal[node.labels[h]], pch = 23
        )
      } # endif
    } # endif
  } # endif
}
