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

  if ("x" %in% aes_names & "height" %in% aes_names){

    # check for the discrete flag
    # browser()

    if (grepl("x = ~discrete", quo_text(mapping))){
      # geom_bar
      geom_bar(mapping = mapping,
               stat = stat,
               position = position,
               ...,
               na.rm = na.rm,
               show.legend = show.legend,
               inherit.aes = inherit.aes)

    } else {
      print("geom_histogram")
      # geom_histogram
      # browser()
      # height -> y
      # P(.) -> stat(width*density)
      height_quosure <- mapping[["height"]]
      mapping[["height"]] <- quo_set_expr(mapping[["height"]], expr(stat(width*density)))

      replaced <- replace(names(mapping), match("height", aes_names), "y")
      names(mapping) <- replaced


      geom_histogram(mapping = mapping,
                     stat = stat,
                     position = position,
                     ...,
                     binwidth = binwidth,
                     na.rm = na.rm,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes)
    }

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

    # yay
    geom_mosaic(mapping=mapping)
  }
}
