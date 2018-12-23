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
  aes_names <- names(mapping)

  if ("x" %in% aes_names &
      !("y" %in% aes_names)){
    # geom_histogram
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomBar,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        binwidth = binwidth,
        bins = bins,
        na.rm = na.rm,
        pad = FALSE,
        ...
      ))
  } else if (!("x" %in% aes_names ) & !("y" %in% aes_names))  {
    # mosaic plot
      # P(A) -> product(A)
      width_quosure <- mapping[["width"]]
      PA_expr <- quo_get_expr(width_quosure)
      PA_expr[[1]] <- expr(product)
      mapping$width <- quo_set_expr(mapping[["width"]], PA_expr)

      # Replace width with x
      replaced <- replace(names(mapping), match("width", aes_names), "x")
      names(mapping) <- replaced
      geom_mosaic(mapping=mapping)
  }
}
