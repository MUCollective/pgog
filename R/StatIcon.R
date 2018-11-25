StatIcon <- ggproto("StatIcon", Stat,
                     compute_group = function(data, scales) {
                       browser()
                       data
                     },

                     required_aes = c("x")
)


stat_icon <- function(mapping = NULL, data = NULL, geom = "point", # TODO
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  browser()
  layer(
    stat = StatIcon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


