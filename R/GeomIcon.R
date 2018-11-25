GeomIcon<- ggplot2::ggproto(
  "GeomIcon", ggplot2::GeomPoint,
  required_aes = c("x", "y")

  # from geom-mosaic.R
  # default_aes = ggplot2::aes(
  #   fontsize=5,
  #   shape = 15, colour = NA,
  #   size = 10, fill = "grey30", alpha = 1, stroke = 0,
  #   linewidth=.1, weight = 1)
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
