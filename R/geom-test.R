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
  browser()

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


  # TODO: calculate weights, etc
  # TODO: look up var in dataframe... but `data` is not available until later???

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
  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  # from ggmosaic
  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = NA,
                             size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
                             linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),

  draw_panel = function(data, panel_scales, coord) {
    browser()
    # if (all(is.na(data$colour)))
      # data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination

    GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord)
  },
  draw_key = ggplot2::draw_key_rect

)
