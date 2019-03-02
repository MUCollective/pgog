
stat_icon <- function(
  mapping = NULL,
  data = NULL,
  geom = "icon",
  position = "identity",
  na.rm = FALSE,
  shwo.legend = NA,
  inherit.aes = TRUE, ...){

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatIcon,
    geom = geom,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      prob.struct = prob.struct,
      offset = offset,
      ...
    )
  )
}


StatIcon <- ggplot2::ggproto(
  "StatIcon",
  ggplot2::Stat,
  non_missing_aes = "weight",
  compute_panel = function(self, data, scales, na.rm = FALSE,
                           prob.struct, offset = 0.01){

    marg_var <- sapply(get_margs(prob.struct), as.character)
    if (!(is.list(marg_var) & length(marg_var) == 0)) {
      marg_var <- paste0("p.", marg_var)
    } else {
      marg_var <- c()
    }

    cond_var <- sapply(get_conds(prob.struct), as.character)
    if (!(is.list(cond_var) & length(cond_var) == 0)){
      cond_var <- paste0("p.", cond_var)
    } else {
      cond_var <- c()
    }

    # margin <- getFromNamespace("margin", "productplots")
    # browser()
    wt <- margin(data, marg_var, cond_var)

    base_layout <- icon_divide(data = wt, prob.struct = prob.struct, offset = offset)
    full_data <- bloc_divide(data = wt, prob.struct = prob.struct, offset = offset)
    res <- pack_icons(data = full_data, bounds = base_layout, prob.struct = prob.struct, offset = offset)

    res$PANEL <- data$PANEL
    res$group <- data$group

    # res <- base_layout

    # base_layer <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
    res
  }
)







# ======== old stat-icon.R =========

#' #' calculates the x, y aesthetics so that GeomPoint will be happy
#' #' @importFrom ggplot2 Stat
#' #' @export
#' StatIcon <- ggproto(
#'   "StatIcon", ggplot2::Stat,
#'
#'   setup_params = function(data, params){
#'     N <- nrow(data)
#'     w_h <- factor_w_h(N)
#'     params$width <- w_h$w
#'     params$height <- w_h$h
#'     message("Picking width and height to be ", w_h$w, ", ", w_h$h)
#'
#'     params
#'   },
#'
#'   setup_data = function(data, params) {
#'     print("stat_icon: setup_data")
#'
#'     data %<>%
#'       group_by(group) %>%
#'       mutate(colour = sort(colour)) %>%
#'       ungroup()
#'
#'     #ACHTUNG: hack to get the colors right
#'     # Assume: P(A|B), color = A immer
#'     if ("width" %in% names(data)){
#'       # browser()
#'       if ("is_conditional" %in% names(attributes(data$width))){
#'         if (attributes(data$width)$is_conditional) {
#'           # browser()
#'           # print("magic")
#'           data$colour <- as.integer(data$width) # TODO: assuming P(A|B), color = A
#'         }
#'       }
#'     } else if ("height" %in% names(data)) {
#'       if ("is_conditional" %in% names(attributes(data$height))){
#'         if (attributes(data$height)$is_conditional){
#'           data$colour <- as.integer(data$height)
#'         }
#'       }
#'     }
#'
#'
#'     # ACHTUNG: hack to preserve original aes mapping
#'     attributes(data)$original_aes <- names(data)
#'
#'     data
#'   },
#'
#'
#'   compute_layer = function(self, data, params, layout) {
#'     print("stat_icon: compute_layer")
#'     if ("x" %in% names(data) & "y" %in% names(data)){
#'       return(data)
#'     } else if ("x" %in% names(data)){
#'       # data$y <-rep(seq(1:params$width), params$height)
#'       data$y <- rep(c(1), each = nrow(data))
#'     } else {
#'       if ("width" %in% names(data)){
#'         data$x <- rev(rep(1:params$height, each = params$width))
#'         # data$x <- rep(c(1), each = nrow(data))
#'         data$y <- rep(seq(1:params$width), params$height)
#'
#'       } else if ("height" %in% names(data)){
#'         data$y <- rep(1:params$height, each = params$width)
#'         data$x <- rev(rep(seq(1:params$width), params$height))
#'       }
#'     }
#'
#'
#'     data
#'   },
#'
#'   # compute_panel = function(data, scales, width, height) {
#'   #   print("compute_panel")
#'   #   # browser()
#'   #   data$x <- rep(1:height, each = width)
#'   #   data$y <- rep(seq(1:width), height)
#'   #   data
#'   # },
#'
#'   # required_aes = c("x")
#'   # hack so that can either use height or width as aes
#'   # Warning messages don't count as bugs
#'   required_aes = c("height"), #c("height"),
#'   non_missing_aes = c("height", "width")
#'
#' )
#'
#'
#' #' @import ggplot2
#' stat_icon <- function(mapping = NULL, data = NULL, geom = "point", # TODO
#'                        position = "identity", na.rm = FALSE, show.legend = NA,
#'                        inherit.aes = TRUE, width = NULL, height = NULL, ...) {
#'   ggplot2::layer(
#'     stat = StatIcon, data = data, mapping = mapping, geom = geom,
#'     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, width = width, height = height, ...)
#'   )
#' }
#'
#'
