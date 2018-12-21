# https://github.com/eclarke/ggbeeswarm/blob/master/R/position-quasirandom.R
# https://github.com/eclarke/ggbeeswarm
# https://github.com/tidyverse/ggplot2/blob/c84d9a075280d374892e5a3e0e25dd0ba246caad/R/position-.r

#' @import dplyr
PositionArrayZ <- ggproto("PositionArrayZ",
                        ggplot2:::Position,
                        required_aes = c("x", "y"),

                        # compute_layer = function(self, data, params, layout) {
                        #   dapply(data, "PANEL", function(data) {
                        #     if (empty(data)) return(new_data_frame())
                        #
                        #     scales <- layout$get_scales(data$PANEL[1])
                        #     self$compute_panel(data = data, params = params, scales = scales)
                        #   })
                        # },

                        compute_panel = function(self, data, params, scales) {
                          print("position_icon: compute_panel")

                          data$i <- rownames(data) # add indices
                          N <- nrow(data)
                          n_columns <- length(unique(data$x))
                          internal_width <- as.integer(sqrt(N) / n_columns)


                          # browser()
                          data %<>%
                            group_by(x) %>%
                            mutate(coord = x + adjust(0.7, row_number(), width = internal_width)) %>%
                            ungroup()

                          data %<>%
                            mutate(x = coord)

                          data %<>%
                            group_by(y) %>%
                            mutate(coord = y + adjust(0.7, row_number(), width = internal_width)) %>%
                            ungroup()

                          data %<>%
                            mutate(y = coord)


                          # browser()

                          # flip y axis
                          max_y <- tail(data$y, n = 1)
                          data$y <- max_y - data$y


                          data
                          # df <- transform_position(data, ident, trans_x)
                          # df$y <- sample(df$y)

                          # df

                        }
)


position_array_z <- function (
  width = NULL,
  varwidth = FALSE,
  bandwidth=.5,
  nbins=NULL,
  method='quasirandom',
  groupOnX=NULL,
  dodge.width=0){
  ggplot2::ggproto(
    NULL,
    PositionArrayZ,
    width = width,
    varwidth = varwidth,
    bandwidth=bandwidth,
    nbins=nbins,
    method=method,
    groupOnX=groupOnX,
    dodge.width=dodge.width)
}
