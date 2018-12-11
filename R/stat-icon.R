StatIcon <- ggproto(
  "StatIcon", ggplot2::Stat,

  setup_params = function(data, params){
    N <- nrow(data)
    w_h <- factor_w_h(N)
    params$width <- w_h$w
    params$height <- w_h$h
    message("Picking width and height to be ", w_h$w, ", ", w_h$h)

    params
  },

  setup_data = function(data, params) {
    # browser()
    data$colour <- data$x # TODO: assuming P(A|B), color = A
    data
  },


  compute_panel = function(data, scales, width, height) {
    # res <- data.frame(
    #   x = rep(1:height, each = width),
    #   y = rep(seq(1:width), height),
    #   colour = data$colour
    # )
    # res
    # browser()
    data$x <- rep(1:height, each = width)
    data$y <- rep(seq(1:width), height)
    data
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


