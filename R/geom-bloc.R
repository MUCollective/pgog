#' @export
#' @importFrom ggmosaic geom_mosaic
#' @importFrom ggmosaic product
#' @import rlang
#' @importFrom ggridges geom_density_ridges
#'
geom_bloc <- function(mapping = NULL, data = NULL,
                     stat = NULL,
                     position = NULL,
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  # pick layer based on aesthetics combo
  aes_names <- names(mapping)

  # violin plot
  if (reduce(c("x", "y", "width") %in% aes_names, `&&`) &&
      grepl("width = ~density", quo_text(mapping))){
    mapping$width <- NULL

    # geom_violin defaults
    if (is.null(stat)){
      stat = "ydensity"
    }

    if (is.null(position)){
      position = "dodge"
    }

  return(geom_violin(mapping = mapping, data = data, stat = stat, position = position, ... ))
  }

  # ggridges
  if (reduce(c("x", "y", "height") %in% aes_names, `&&`) &&
      grepl("height = ~density", quo_text(mapping))){
    mapping$height <- NULL

    # geom_violin defaults
    if (is.null(stat)){
      stat = "density_ridges"
    }

    if (is.null(position)){
      position = "points_sina"
    }

    return(geom_density_ridges(mapping = mapping, data = data, stat = stat, position = position, ... ))
  }

  if ("x" %in% aes_names & "height" %in% aes_names){
    if (grepl("height = ~density", quo_text(mapping))){
      # geom_density
      # * `x`      -> `mpg`
      # * `height` -> `density(cyl | .)`
      mapping_env <- quo_get_env(mapping[[1]])
      y_expr <- expr(stat(density * n))
      mapping[["y"]] <- new_quosure(y_expr, env = mapping_env)
      browser()

      # only specify fill when needed (conditional)?
      if (is_conditional(quo_get_expr(mapping$height)) ||
          is_joint(quo_get_expr(mapping$height))){

        if (is_conditional(quo_get_expr(mapping$height))){
          fill_expr_dot <- get_conditional(quo_get_expr(mapping$height))
          fill_expr <- expr(factor(!!fill_expr_dot))

          mapping[["fill"]] <- new_quosure(fill_expr, env = mapping_env)
          mapping$height <- NULL
          geom_density(mapping = mapping,
                       data = data,
                       stat = "density",
                       position = "fill",... ,
                       na.rm = na.rm,
                       show.legend = show.legend,
                       inherit.aes = inherit.aes)
        } else {
          fill_expr_dot <- get_joint(quo_get_expr(mapping$height))
          fill_expr <- expr(factor(!!fill_expr_dot))

          mapping[["fill"]] <- new_quosure(fill_expr, env = mapping_env)
          mapping$height <- NULL

          mapping$height <- NULL
          geom_density(mapping = mapping,
                       data = data,
                       stat = "density",
                       position = "stack",... ,
                       na.rm = na.rm,
                       show.legend = show.legend,
                       inherit.aes = inherit.aes)
        }

      } else {
        mapping$height <- NULL
        geom_density(mapping = mapping,
                     data = data,
                     stat = "density",
                     position = "stack",... ,
                     na.rm = na.rm,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes)

      }
      # TODO: position is either stack or fill

    } else if (grepl("x = ~discrete", quo_text(mapping)) &
        grepl("height", quo_text(mapping))){
      # geom_bar
      # generate fill based on conditional P(A|B)
      if (is.null(mapping$fill)){
        # extract what P() is conditioned on
        # browser()
        cond <- get_conditional(quo_get_expr(mapping$height))
        fill_expr <- expr(!!cond)

        # fill_expr <- expr(happy)
        fill_env <- quo_get_env(mapping[[1]])
        mapping$fill <- new_quosure(fill_expr, env = fill_env)
      }

      if (!identical(quo_get_expr(mapping$height)[[2]], quo_get_expr(mapping$fill))){
        # x = A, height = P(B|.)
        height_quosure <- mapping[["height"]]
        mapping[["height"]] <- quo_set_expr(mapping[["height"]], expr((..count..)/sum(..count..)))

        replaced <- replace(names(mapping), match("height", aes_names), "y")
        names(mapping) <- replaced

        if (is.null(stat)){
          stat <- "count"
        }

        geom_bar(mapping = mapping,
                 stat = stat,
                 position = position,
                 ...,
                 na.rm = na.rm,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes)
      } else {
        # ggmosaic
        # given  x = discrete(happy), height = P(B)
        # -> aes(x = product(happy), fill=happy, conds=product(sex))

        # make product(A) expr
        x_fill_aes <- get_conditional(quo_get_expr(mapping$x))
        prod_expr <- expr(product(!!x_fill_aes))
        mapping$x <- quo_set_expr(mapping$x, prod_expr)
        mapping$fill <- quo_set_expr(mapping$fill, expr(!!x_fill_aes))

        # build cond mapping
        conds_aes <- get_conditional(quo_get_expr(mapping$height))
        mapping$conds <- new_quosure(expr = expr(product(!!conds_aes)), quo_get_env(mapping$x))

        # delete extra mapping
        mapping$height <- NULL

        browser()


        geom_mosaic(mapping = mapping)
      }

    } else {
      # geom_histogram

      # height -> y
      # P(.) -> stat(width*density)
      height_quosure <- mapping[["height"]]
      mapping[["height"]] <- quo_set_expr(mapping[["height"]], expr(stat(width*density)))

      replaced <- replace(names(mapping), match("height", aes_names), "y")
      names(mapping) <- replaced

      if (is.null(binwidth)){
        binwidth = 1
      }

      if (is.null(stat)){
        stat = "bin"
      }

      geom_histogram(mapping = mapping,
                     stat = stat,
                     position = position,
                     ...,
                     binwidth = binwidth,
                     na.rm = na.rm,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes)
    }
  } else if (!("x" %in% aes_names ) && !("y" %in% aes_names)) {
    # mosaic plot, either has width or height aes, or both?
    # P(A) -> product(A)
    if ("width" %in% aes_names){

      # generate fill based on conditional P(A|B)
      if (is.null(mapping$fill)){
        # extract what P() is conditioned on
        cond <- get_conditional(quo_get_expr(mapping$height))
        fill_expr <- expr(!!cond)

        # fill_expr <- expr(happy)
        fill_env <- quo_get_env(mapping[[1]])
        mapping$fill <- new_quosure(fill_expr, env = fill_env)
      }

      # remove height argument since ggmosaic doesn't understand
      mapping$height <- NULL

      # change width -> x
      width_quosure <- mapping[["width"]]
      PA_expr <- quo_get_expr(width_quosure)
      PA_expr[[1]] <- expr(product)
      mapping$width <- quo_set_expr(mapping[["width"]], PA_expr)

      # Replace width with x
      replaced <- replace(names(mapping), match("width", aes_names), "x")
      names(mapping) <- replaced
    } else if ("height" %in% aes_names){
     stop("height/y spec not supported by ggmosaic")
    } else {
      stop(paste("unsupported aesthetics mapping:", as.character(mapping)))
    }

    geom_mosaic(mapping=mapping)
  }
}
