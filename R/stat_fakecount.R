#' A geom that doesn't work
#'
#' @return a geom
stat_fakecount <- function(mapping = NULL, data = NULL,
                   geom = "bar", position = "stack",
                   ...,
                   width = NULL,
                   na.rm = FALSE,
                   show.legend = NA,
                   inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    width = width,
    ...
  )
  if (!is.null(params$y)) {
    stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatConditionalProb,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}
