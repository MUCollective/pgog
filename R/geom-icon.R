#' From geom_point
geom_icon <- function(mapping = NULL, data = NULL,
                      stat = "icon",
                      ...,
                      na.rm = FALSE,
                      position = "identity",
                      show.legend = NA,
                      inherit.aes = TRUE) {


  # Logic check: stacking does not make sense without x, y aes
  aes_names <- names(enexpr(mapping))
  has_coord_aes <- "x" %in% aes_names | "y" %in% aes_names

  if ((!has_coord_aes) & position == "stack"){
    message("Warning: Can't stack those aesthetics")
  }

  # Assign the appropriate position adjustment




  # browser()

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


#' @export
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
GeomIcon <- ggproto("GeomIcon", GeomPoint,
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "shape", "colour"),
                    default_aes = aes(
                      shape = 15, colour = "gray", size = 10, fill = NA,
                      alpha = NA, stroke = 0.5
                    ),

                    setup_data = function(data, params) {
                      #browser()
                      data
                    }


)
