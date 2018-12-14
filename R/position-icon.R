PositionIcon <- ggproto("PositionIcon",
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
                          browser()

                          # calculate some offsets
                          trans_x <- function(xx) {
                            new_x <- vipor::offsetX(
                              data[, 'x'],
                              xx,
                              method = "quasirandom"
                            )
                            return(new_x + xx)
                          }
                          df <- transform_position(data, trans_x, trans_x)
                          df$y <- sample(df$y)
                          df

                        }
)


position_icon <- function (
  width = NULL,
  varwidth = FALSE,
  bandwidth=.5,
  nbins=NULL,
  method='icon',
  groupOnX=NULL,
  dodge.width=0){
  ggplot2::ggproto(
    NULL,
    PositionIcon,
    width = width,
    varwidth = varwidth,
    bandwidth=bandwidth,
    nbins=nbins,
    method=method,
    groupOnX=groupOnX,
    dodge.width=dodge.width)
}
