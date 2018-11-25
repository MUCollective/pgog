GeomIcon<- ggproto("GeomIcon", GeomPoint)


geom_icon <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = FALSE,  ...)
{
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
