
#' calculates the x, y coordiantes for the icon array
#' @importFrom ggplot2 Stat
#' @export
StatIcon <- ggproto(
  "StatIcon", ggplot2::Stat,

  setup_params = function(data, params){
    N <- nrow(data)
    w_h <- factor_w_h(N)
    params$width <- w_h$w
    params$height <- w_h$h
    message("Picking width and height to be ", w_h$w, ", ", w_h$h)

    params
  },

  setup_data = function(data, params) {
    print("stat_icon: setup_data")
    # browser()

    data %<>%
      group_by(group) %>%
      mutate(colour = sort(colour)) %>%
      ungroup()

    # browser()

    #ACHTUNG: because there's a bug in util.R ...
    if ("width" %in% names(data)){
      browser()
      if ("is_conditional" %in% names(attributes(data$width))){
        if (attributes(data$width)$is_conditional) {
          # browser()
          # print("magic")
          data$colour <- as.integer(data$width) # TODO: assuming P(A|B), color = A
        }
      }
    } else if ("height" %in% names(data)) {
      if ("is_conditional" %in% names(attributes(data$height))){
        if (attributes(data$height)$is_conditional){
          data$colour <- as.integer(data$height)
        }
      }
    }

    data
  },


  compute_layer = function(self, data, params, layout) {
    print("stat_icon: compute_layer")



    if ("x" %in% names(data)){
      data$y <-rep(seq(1:params$width), params$height)

    } else {
      if ("width" %in% names(data)){
        data$x <- rev(rep(1:params$height, each = params$width))
        data$y <- rep(seq(1:params$width), params$height)

      } else if ("height" %in% names(data)){
        data$y <- rep(1:params$height, each = params$width)
        data$x <- rev(rep(seq(1:params$width), params$height))
      }
    }


    data
  },

  # compute_panel = function(data, scales, width, height) {
  #   print("compute_panel")
  #   # browser()
  #   data$x <- rep(1:height, each = width)
  #   data$y <- rep(seq(1:width), height)
  #   data
  # },

  # required_aes = c("x")
  # hack so that can either use height or width as aes
  # Warning messages don't count as bugs
  required_aes = c("height"), #c("height"),
  non_missing_aes = c("height", "width")

)


#' @import ggplot2
stat_icon <- function(mapping = NULL, data = NULL, geom = "point", # TODO
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, width = NULL, height = NULL, ...) {
  ggplot2::layer(
    stat = StatIcon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, width = width, height = height, ...)
  )
}


