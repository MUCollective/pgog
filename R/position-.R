#' @references https://github.com/tidyverse/ggplot2/blob/master/R/position-.r

Position <- ggproto("Position",
                    required_aes = character(),

                    setup_params = function(self, data) {
                      list()
                    },

                    setup_data = function(self, data, params) {
                      check_required_aesthetics(self$required_aes, names(data), snake_class(self))
                      data
                    },

                    compute_layer = function(self, data, params, layout) {
                      dapply(data, "PANEL", function(data) {
                        if (empty(data)) return(new_data_frame())

                        scales <- layout$get_scales(data$PANEL[1])
                        self$compute_panel(data = data, params = params, scales = scales)
                      })
                    },

                    compute_panel = function(self, data, params, scales) {
                      abort("Not implemented")
                    }
)

#' Convenience function to transform all position variables.
#'
#' @param trans_x,trans_y Transformation functions for x and y aesthetics.
#'   (will transform x, xmin, xmax, xend etc)
#' @param ... Additional arguments passed to `trans_x` and `trans_y`.
#' @keywords internal
#' @export
transform_position <- function(df, trans_x = NULL, trans_y = NULL, ...) {
  # Treat df as list during transformation for faster set/get
  oldclass <- class(df)
  df <- unclass(df)
  scales <- aes_to_scale(names(df))

  if (!is.null(trans_x)) {
    df[scales == "x"] <- lapply(df[scales == "x"], trans_x, ...)
  }
  if (!is.null(trans_y)) {
    df[scales == "y"] <- lapply(df[scales == "y"], trans_y, ...)
  }

  class(df) <- oldclass

  df
}
