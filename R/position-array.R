# https://github.com/eclarke/ggbeeswarm/blob/master/R/position-quasirandom.R
# https://github.com/eclarke/ggbeeswarm
# https://github.com/tidyverse/ggplot2/blob/c84d9a075280d374892e5a3e0e25dd0ba246caad/R/position-.r

#' @import dplyr
#' @import magrittr
#' @import ggplot2
PositionArray <- ggplot2::ggproto("PositionArray",
                        ggplot2:::Position,
                        required_aes = c("x", "y"),

                        setup_params=function(self,data){
                          list(aes_names=self$aes_names)
                        },

                        compute_panel = function(self, data, params, scales) {
                          print("position_icon: compute_panel")

                          # data$i <- rownames(data) # add indices
                          # N <- nrow(data)
                          # n_columns <- length(unique(data$x))
                          # internal_width <- as.integer(sqrt(N) / n_columns)

                          counts <- data %>%
                            group_by(x, y) %>%
                            count()
                          max_n <- max(counts$n)
                          internal_width <- as.integer(sqrt(max_n * 0.618))

                          if ("height" %in% params$aes_names){
                            # browser()
                            x_spacing <- 0.7
                            data %<>%
                              group_by(x, y) %>%
                              # rowwise() %>%
                              mutate(coord = x + adjust(x_spacing, row_number() , width = internal_width)) %>%
                              ungroup()

                            data %<>%
                              mutate(x = coord)

                            # y coords are just row numbers for each group of unique x's
                            y_spacing <- x_spacing / (internal_width - 1) * 0.618
                            data %<>%
                              group_by(x, y) %>%
                              mutate(coord = y + (row_number()) * y_spacing) %>%
                              ungroup()

                            data %<>%
                              mutate(y = coord)
                            # flip horizontally
                            max_y <- tail(data$y, n = 1)
                            data$y <- max_y - data$y
                          }


                          data

                        }
)


position_array <- function (
  width = NULL,
  aes_names = NULL,
  varwidth = FALSE,
  bandwidth=.5,
  nbins=NULL,
  method='quasirandom',
  groupOnX=NULL,
  dodge.width=0){
  ggplot2::ggproto(
    NULL,
    PositionArray,
    width = width,
    aes_names = aes_names,
    varwidth = varwidth,
    bandwidth=bandwidth,
    nbins=nbins,
    method=method,
    groupOnX=groupOnX,
    dodge.width=dodge.width)
}
