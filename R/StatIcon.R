StatIcon <- ggproto(
  "StatIcon", ggplot2::Stat,

  setup_params = function(data, params){
    N <- nrow(data)
    w_h <- factor_w_h(N)
    params$width <- w_h$w
    params$height <- w_h$h
    params
  },

  compute_panel = function(data, scales, params) {
    #browser()
    res <- data.frame(
      x = seq(1, params$width),
      y = seq(1, params$height)
    )
    res
  },

  required_aes = c("x")
)


stat_icon <- function(mapping = NULL, data = NULL, geom = "point", # TODO
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, width = NULL, height = NULL, ...) {
  # browser()
  ggplot2::layer(
    stat = StatIcon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, width = width, height = height, ...)
  )
}


