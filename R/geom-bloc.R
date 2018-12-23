#' @export
#' @importFrom ggmosaic geom_mosaic
#' @importFrom ggmosaic product
#' @importFrom rlang eval_tidy
geom_bloc <- function(mapping = NULL, data = NULL,
                     stat = "bin",
                     position = "stack",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  # pick layer based on aesthetics combo
  aes_names <- names(enexpr(mapping))

  # if ("x" %in% aes_names &
  #     !("y" %in% aes_names)){
  #   # geom_histogram
  #   ggplot2::layer(
  #     data = data,
  #     mapping = mapping,
  #     stat = stat,
  #     geom = GeomBar,
  #     position = position,
  #     show.legend = show.legend,
  #     inherit.aes = inherit.aes,
  #     params = list(
  #       binwidth = binwidth,
  #       bins = bins,
  #       na.rm = na.rm,
  #       pad = FALSE,
  #       ...
  #     ))
  # } else if (!("x" %in% aes_names ) & !("y" %in% aes_names))  {
    # mosaic plot
    # TODO: swap out the width to x
      # browser()
      geom_mosaic(mapping=mapping)
  # }

}
