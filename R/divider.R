
aes_lookup <- function(aes_str){

  #browser()

  aes_str_check = aes_checker(aes_str)
  if (is.na(aes_str_check)==FALSE) {

  # TODO: parse out {color, colour, alpha, fill}
  switch (aes_str_check,
    x.height = return(hbar),
    x.cond = return(hspine),
    y.cond = return(vspine),
    cond = return(hbar), #ACHTUNG
    x.width = return(bonshakalaka), # ACHTUNG
    y.width = return(vbar),
    y.height = return(shakabonbon), # ACHTUNG
    height = return(vspine),
    width = return(hspine),
    fill.width = return(hspine),
    fill.height = return(vspine)
  )
  } else {
    next
  }
}

aes_checker <- function(aes_str){
  #browser()

  aes_splitted = as.vector(unlist(strsplit(aes_str, "[.]")))
  for( i in seq_along(aes_splitted)){
    if (aes_splitted[i] %in% c("alpha","color","colour","fill")){
      aes_splitted = aes_splitted[-i]
    }
  }
  if(length(aes_splitted) != 0){
    aes_result = paste(aes_splitted, collapse = ".")
    return(aes_result)
  } else {
    return(NA)
  }


}



# ====== from ggmosaic


#' Internal helper function
#'
#' Squeeze pieces to lie within specified bounds; directly copied from package productplots
#' @param pieces rectangle specified via l(eft), r(ight), b(ottom), t(op)
#' @param bounds rectangle specified via l(eft), r(ight), b(ottom), t(op)
#' @return re-scaled values for piece according to boundaries given by bounds
#' @author Hadley Wickham
squeeze <- function(pieces, bounds = bound()) {
  scale_x <- function(x) x * (bounds$r - bounds$l) + bounds$l
  scale_y <- function(y) y * (bounds$t - bounds$b) + bounds$b

  pieces$l <- scale_x(pieces$l)
  pieces$r <- scale_x(pieces$r)
  pieces$b <- scale_y(pieces$b)
  pieces$t <- scale_y(pieces$t)
  pieces
}


rotate <- function(data) {
   l <- b <- t <- r <- NULL # visible binding
   dplyr::rename(data, b=l, t=r, l=b, r=t)
 }

#' Spine partition: divide longest dimension.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
spine <- function(data, bounds, offset = offset, max = NULL) {
  w <- bounds$r - bounds$l
  h <- bounds$t - bounds$b

  if (w > h) {
    hspine(data, bounds, offset, max)
  } else {
    vspine(data, bounds, offset, max)
  }
}


#' Horizontal spine partition: height constant, width varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
hspine <- function(data, bounds, offset = offset, max = NULL) {
  #browser()
  n <- length(data)
  # n + 1 offsets

  if (ncol(bounds)>4)  offsets <- ((c(0, rep(1, n - 1), 0) * offset))/sqrt((bounds$level+.1))
  else offsets <- (c(0, rep(1, n - 1), 0) * offset)

  data <- data * (1 - sum(offsets))

  widths <- as.vector(t(cbind(data, offsets[-1])))
  widths[is.na(widths)] <- 0

  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = 1
  )
  squeeze(locations, bounds)
}

#' Vertical spine partition: width constant, height varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
vspine <- function(data, bounds, offset = offset, max = NULL) {
  rotate(hspine(data, rotate(bounds), offset, max = max))
}

#' Horizontal bar partition: width constant, height varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
hbar <- function(data, bounds, offset = 0.02, max = NULL) {
  if (is.null(max)) max <- 1

  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset

  width <- (1 - sum(offsets)) / n
  heights <- data / max

  widths <- as.vector(t(cbind(width, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = heights
  )
  squeeze(locations, bounds)
}

#' Vertical bar partition: height constant, width varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
vbar <- function(data, bounds, offset = 0.02, max = NULL) {
 rotate(hbar(data, rotate(bounds), offset, max = max))
}

bonshakalaka <- function(data, bounds, offset = 0.02, max = NULL){
  if (is.null(max)) max <- 1

  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset

  width <- (1 - sum(offsets)) / n
  width_weighted <- data / sum(data)

  widths <- as.vector(t(cbind(width, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(1, 2 * n - 1, by = 2)] +
      (pos[seq(2, 2 * n, by = 2)] - pos[seq(1, 2 * n - 1, by = 2)]) * width_weighted,
    b = 0,
    t = 1
  )
  squeeze(locations, bounds)
}

shakabonbon <- function(data, bounds, offset = 0.02, max = NULL) {
  rotate(bonshakalaka(data, rotate(bounds), offset, max = max))
}
.directions <- c("vertical", "horizontal")

#' Template for a mosaic plot.
#' A mosaic plot is composed of spines in alternating directions.
#'
#' @param direction direction of first split
#' @export
mosaic <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) rev(rep(splits, length = n))
}


#' Template for a double decker plot.
#' A double decker plot is composed of a sequence of spines in the same
#' direction, with the final spine in the opposite direction.
#'
#' @param direction direction of first split
#' @export
ddecker <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) c(splits[2], rep(splits[1], length = n - 1))
}
