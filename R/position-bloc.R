
position_bloc <- function(vjust = 1, reverse = FALSE, mapping = "") {
  ggproto(NULL, PositionBloc, vjust = vjust, reverse = reverse, mapping = mapping)
}


#' @format NULL
#' @usage NULL
#' @export
PositionBloc <-
  ggproto(
    "PositionBloc",
    Position,
    type = NULL,
    vjust = 1,
    fill = FALSE,
    reverse = FALSE,
    mapping = "",

    setup_params = function(self, data) {
      flipped_aes <- has_flipped_aes(data)
      data <- flip_data(data, flipped_aes)

      # TODO: parse mapping here to figure out which position to use
      # stack
      # fill (stack + fill = true)
      # everything else: identity



      list(
        var = self$var %||% stack_var(data),
        fill = self$fill,
        vjust = self$vjust,
        reverse = self$reverse,
        mapping = self$mapping,
        flipped_aes = flipped_aes
      )
    },

    setup_data = function(self, data, params) {
      data <- flip_data(data, params$flipped_aes)
      if (is.null(params$var)) {
        return(data)
      }

      # P(A|B)
      return(setup_data_stack(data, params))
      # P(B|A)


    },

    compute_panel = function(data, params, scales) {
      data <- flip_data(data, params$flipped_aes)
      if (is.null(params$var)) {
        return(data)
      }

      compute_panel_stack(data, params, scales)

    }
  )


setup_data_stack <- function(data, params) {

  data$ymax <- switch(params$var,
                      y = data$y,
                      ymax = ifelse(data$ymax == 0, data$ymin, data$ymax)
  )

  data <- remove_missing(
    data,
    vars = c("x", "xmin", "xmax", "y"),
    name = "position_stack"
  )
  flip_data(data, params$flipped_aes)

}


compute_panel_stack <- function(data, params, scales){
  negative <- data$ymax < 0
  negative[is.na(negative)] <- FALSE

  neg <- data[negative, , drop = FALSE]
  pos <- data[!negative, , drop = FALSE]

  if (any(negative)) {
    neg <- collide(neg, NULL, "position_stack", pos_stack,
                   vjust = params$vjust,
                   fill = params$fill,
                   reverse = params$reverse
    )
  }
  if (any(!negative)) {
    pos <- collide(pos, NULL, "position_stack", pos_stack,
                   vjust = params$vjust,
                   fill = params$fill,
                   reverse = params$reverse
    )
  }

  data <- rbind(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))),]


  flip_data(data, params$flipped_aes)
}

pos_stack <- function(df, width, vjust = 1, fill = FALSE) {
  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  heights <- c(0, cumsum(y))

  if (fill) {
    heights <- heights / abs(heights[length(heights)])
  }

  df$ymin <- pmin(heights[-n], heights[-1])
  df$ymax <- pmax(heights[-n], heights[-1])
  df$y <- (1 - vjust) * df$ymin + vjust * df$ymax
  df
}

stack_var <- function(data) {
  if (!is.null(data$ymax)) {
    if (any(data$ymin != 0 & data$ymax != 0, na.rm = TRUE)) {
      warning("Stacking not well defined when not anchored on the axis", call. = FALSE)
    }
    "ymax"
  } else if (!is.null(data$y)) {
    "y"
  } else {
    warning(
      "Stacking requires either ymin & ymin or y aesthetics.\n",
      "Maybe you want position = 'identity'?",
      call. = FALSE
    )
    NULL
  }
}



#' @references https://github.com/tidyverse/ggplot2/blob/master/R/position-collide.r
collide <- function(data, width = NULL, name, strategy,
                    ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$xmin, data$group), ]
  } else {
    data <- data[order(data$xmin, -data$group), ]
  }

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$ymax)) {
    dapply(data, "xmin", strategy, ..., width = width)
  } else if (!is.null(data$y)) {
    data$ymax <- data$y
    data <- dapply(data, "xmin", strategy, ..., width = width)
    data$y <- data$ymax
    data
  } else {
    stop("Neither y nor ymax defined")
  }
}

collide_setup <- function(data, width = NULL, name, strategy,
                          check.width = TRUE, reverse = FALSE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x
      data$xmax <- data$x
    }

    # Width determined from data, must be floating point constant
    widths <- unique(data$xmax - data$xmin)
    widths <- widths[!is.na(widths)]

    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(widths))) {
    #       warn(name, " requires constant width: output may be incorrect")
    #     }
    width <- widths[1]
  }

  list(data = data, width = width)
}

