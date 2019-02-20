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
                      inherit.aes = TRUE) {

  # parsed_mapping <- parse_aes(mapping)
  # pprint(parsed_mapping)

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
      # offset = offset,
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
  required_aes = c("xmin", "xmax", "ymin", "ymax")
)
