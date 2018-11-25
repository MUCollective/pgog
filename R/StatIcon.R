StatIcon <- ggproto(
  "StatIcon", ggplot2::Stat,

  setup_params = function(data, params){
    N <- nrow(data)
    w_h <- factor_w_h(N)
    params$icon_array_w <- w_h$w
    params$icon_array_h <- w_h$h
    params
    browser()
  },

  compute_panel = function(data, scales, params) {
    browser()
    data
  },

  required_aes = c("x")
)


stat_icon <- function(mapping = NULL, data = NULL, geom = "point", # TODO
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  # browser()
  layer(
    stat = StatIcon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


