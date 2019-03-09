#' @export
#' @importFrom ggmosaic geom_mosaic
#' @importFrom ggmosaic product
#' @import rlang
#' @importFrom ggridges geom_density_ridges
#'
geom_bloc <- function(mapping = NULL, data = NULL,
                      stat = "bloc",
                      position = "identity",
                      ...,
                      binwidth = NULL,
                      bins = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      offset = 0.01,
                      prob.struct = NULL) {

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


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBloc,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      offset = offset,
      prob.struct = parsed_mapping,
      ...
    )
  )

}


#' @importFrom grid grobTree
#' @references GeomMosaic
GeomBloc <- ggplot2::ggproto(
  "GeomBloc",
  ggplot2::Geom,

  setup_data = function(data, params){
    data
  },

  # required_aes = c("xmin", "xmax", "ymin", "ymax"),
  required_aes = c(),

  # from ggmosaic
  # default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
  #                            shape = 19, colour = NA,
  #                            size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
  #                            linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),

  default_aes = aes(
    colour = NA, fill = "grey20", size = 0.5,
    linetype = 1, alpha = 1
  ),

  # draw_panel = function(data, panel_scales, coord) {
  draw_group = function(data, panel_params, coord) {

    if ("density" %in% names(data)){
      # from  https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
      n <- nrow(data)
      if (n <= 2) return(grid::nullGrob())

      # ACHTUNG
      data$y <- data$density

      coords <- coord$transform(data, panel_params)
      # A polygon can only have a single colour, fill, etc, so take from first row
      first_row <- coords[1, , drop = FALSE]

      grid::polygonGrob(
        coords$x, coords$y,
        default.units = "native",
        gp = grid::gpar(
          col = first_row$colour,
          fill = scales::alpha(first_row$fill, first_row$alpha),
          lwd = first_row$size * .pt,
          lty = first_row$linetype
        )
      )
    } else {
      stop("not implemented yet")
          # GeomRect$draw_panel(subset(data, level==max(data$level)), panel_params, coord)

    }


    },

  #   # stuff from ggmosaic; for colors
  #   # if (all(is.na(data$colour)))
  #     # data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination
  #
  #   # TODO: check if it's a density plot or not?
  #   if ("xmin" %in% names(data)){
  #     browser()
  #     GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord)
  #   } else {
  #     # stop("density plots not implemented")
  #     browser()
  #     GeomArea$draw_panel(data, panel_params = panel_params, coord = coord)
  #   }
  # },
  # draw_key = ggplot2::draw_key_rect
  draw_key = ggplot2::draw_key_polygon

)
