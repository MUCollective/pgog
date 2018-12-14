#' From geom_point
geom_icon <- function(mapping = NULL, data = NULL,
                      stat = "icon",
                      ...,
                      na.rm = FALSE,
                      position = "identity",
                      show.legend = NA,
                      inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIcon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomIcon <- ggproto("GeomIcon", Geom,
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "shape", "colour"),
                    default_aes = aes(
                      shape = 15, colour = "gray", size = 10, fill = NA,
                      alpha = NA, stroke = 0.5
                    ),




                    draw_group = function(data, panel_params, coord, na.rm = FALSE) {
                      print("geom_icon: draw_group")

                      # browser()

                      # Below is the geom_point source
                      if (is.character(data$shape)) {
                        data$shape <- translate_shape_string(data$shape)
                      }

                      coords <- coord$transform(data, panel_params)
                      ggname("geom_icon",
                             pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               gp = gpar(
                                 col = alpha(coords$colour, coords$alpha),
                                 fill = alpha(coords$fill, coords$alpha),
                                 # Stroke is added around the outside of the point
                                 fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                 lwd = coords$stroke * .stroke / 2
                               )
                             )
                      )
                    },

                    draw_key = draw_key_point
)
