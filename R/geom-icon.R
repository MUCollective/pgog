GeomIcon<- ggplot2::ggproto(
  "GeomIcon", ggplot2::GeomPoint,
  required_aes = c("x", "y"),

  # from geom-point.R
  default_aes = aes(
    shape = 15, colour = "gray", size = 10, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  # TODO: this doesn't do anything
  setup_data = function(data, params){
    # print("setup_data from geom_icon")
    # browser()
    data$width <- NULL
    data
  },

  # TODO: is it because
  # draw_group = function(data, panel_params, coord) {
  #   print("draw_group")
  #   browser()
  # }

  # stole from geom-point.R
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    browser()

    # secretly changing stuff
    # TODO: DIW; read the doc: https://github.com/tidyverse/ggplot2/blob/master/R/geom-.r
    # data[data$group == 2, ]$x <-data[data$group == 2, ]$x + 10


    ggname <- function(prefix, grob) {
      grob$name <- grid::grobName(grob, prefix)
      grob
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
  }

)


geom_icon <- function(mapping = NULL, data = NULL, stat = "icon", # StatIcon!
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = FALSE,  ...)
{

  # from geom_point source
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIcon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    # ... looks important
    params = list(
      na.rm = na.rm,
      # TODO: probably something here to specify layout:
      # arrange = vertial, horizontal, density/random
      ...
    )
  )
}
