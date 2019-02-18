#' @export
#' @importFrom ggmosaic geom_mosaic
#' @importFrom ggmosaic product
#' @import rlang
#' @importFrom ggridges geom_density_ridges
#'
geom_test <- function(mapping = NULL, data = NULL,
                      stat = NULL,
                      position = NULL,
                      ...,
                      binwidth = NULL,
                      bins = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {

  # browser()
  # extract expressions from quosures?

  parsed_mapping <- parse_aes(mapping)

  # TODO: calculate weights, etc
  # TODO: look up var in dataframe... but `data` is not available until later???

  # draw()
  NULL
}
