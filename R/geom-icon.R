
#' @export
geom_icon <- function(mapping = NULL,
                      data = NULL,
                      stat = "icon",
                      position = "identity",
                      ...,
                      binwidth = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      offset = 0.01,
                      prob.struct = NULL){

  # same as in geom_bloc
  # parse prob structure
  parsed_mapping <- parse_aes(mapping)
  pprint(parsed_mapping)


  # hack the aes mapping so that ggplot selects the column in data
  rv_syms <- get_all_rv(parsed_mapping)
  rv_names <- paste0("p.", as.character(rv_syms))

  # TODO: may need to deal with fill/alpha aesthetics here?

  # TODO: put 'em in
  for (i in seq_along(rv_names)){
    mapping[[rv_names[i]]] <- rv_syms[[i]]
  }

  # TODO: put 1's in there
  mapping$x <- structure(1L, class = "pgog")
  mapping$y <- structure(1L, class = "pgog")
  if (!is.null(mapping$height)){
    mapping$height <- NULL
  }
  if (! is.null(mapping$width)){
    mapping$width <- NULL
  }
}


# ===== old geom_icon

#' From geom_point
#' geom_icon <- function(mapping = NULL, data = NULL,
#'                       stat = "icon",
#'                       ...,
#'                       na.rm = FALSE,
#'                       position = "identity",
#'                       show.legend = NA,
#'                       inherit.aes = TRUE) {
#'
#'
#'   # Logic check: stacking does not make sense without x, y aes
#'   # ACHTUNG: this is no longer needed
#'   aes_names <- names(enexpr(mapping))
#'   has_coord_aes <- "x" %in% aes_names | "y" %in% aes_names
#'
#'   if ((!has_coord_aes) & position == "stack"){
#'     message("Warning: Can't stack those aesthetics")
#'   }
#'
#'   # Assign the appropriate position adjustment
#'   if (position == "array"){
#'     position <- mod_position(aes_names)
#'   }
#'
#'
#'   # browser()
#'
#'   ggplot2::layer(
#'     data = data,
#'     mapping = mapping,
#'     stat = stat,
#'     geom = GeomIcon,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = list(
#'       na.rm = na.rm,
#'       ...
#'     )
#'   )
#' }
#'
#'
#' #' @export
#' #' @importFrom grid pointsGrob
#' #' @importFrom grid gpar
#' GeomIcon <- ggproto("GeomIcon", GeomPoint,
#'                     required_aes = c("x", "y"),
#'                     non_missing_aes = c("size", "shape", "colour"),
#'                     default_aes = aes(
#'                       shape = 15, colour = "gray", size = 10, fill = NA,
#'                       alpha = NA, stroke = 0.5
#'                     ),
#'
#'                     setup_data = function(data, params) {
#'                       #browser()
#'                       data
#'                     }
#'
#'
#' )
