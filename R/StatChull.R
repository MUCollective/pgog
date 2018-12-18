
#' @importFrom ggplot2 ggproto
#' @export
StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       ret <- data[chull(data$x, data$y), , drop = FALSE]
                       print(ret)
                       ret
                     },

                     required_aes = c("x", "y")
)


stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
