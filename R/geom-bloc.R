#' @export
#' @importFrom ggmosaic geom_mosaic
#' @importFrom ggmosaic product
#' @importFrom grid polygonGrob
#' @import rlang
#' @importFrom ggridges geom_density_ridges
#'
geom_bloc <- function(mapping = NULL, data = NULL,
                      stat = "bloc",
                      position = "identity",
                      side = "up",
                      ...,
                      binwidth = NULL,
                      bins = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      offset = 0.01,
                      prob.struct = NULL,
                      extra_var = NULL
                      ) {

  # TODO: only check probability related aesthetics
  # aes_p <- c("x", "y", "width", "height")
  # is_aes_p <- function(i) i %in% aes_p
  # mapping_p <- mapping[is_aes_p(names(mapping))]

  #browser()
  # parse prob structure
  parsed_mapping <- parse_aes(mapping)
  print(parsed_mapping)


  # print(parsed_mapping)


  # hack the aes mapping so that ggplot selects the column in data
  rv_syms <- get_all_rv(parsed_mapping)
  rv_names <- paste0("p.", as.character(rv_syms))

  # TODO: may need to deal with fill/alpha aesthetics here?

  # TODO: put 'em in
  for (i in seq_along(rv_names)){
    mapping[[rv_names[i]]] <- rv_syms[[i]]
  }

  # TODO: put 1's in there
  mapping$x <- structure(1L, class = "pgog")
  mapping$y <- structure(1L, class = "pgog")
  if (!is.null(mapping$height)){
    mapping$height <- NULL
  }
  if (! is.null(mapping$width)){
    mapping$width <- NULL
  }


  print(mapping)

  #get fill var if  exits
  extra_var = c()
  extra_var["fill"] = as_label(mapping$fill)
  extra_var["alpha"] = as_label(mapping$alpha)
  #TODO:can not both exit!
  extra_var["color"] = as_label(mapping$color)
  extra_var["colour"] = as_label(mapping$colour)
  name = names(extra_var)
  #trim the factor() out
  extra_var = str_extract(extra_var,"\\(.*\\)") %>% str_replace("\\(","") %>% str_replace("\\)","")
  #browser()
  names(extra_var) = name
  # hack to get position arg right
  # ACHTUNG: but geom doesn't have data values yet

  # position = "fill"
  # position = "stack"

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBloc,
    position = position, # use customized position, pass mapping as argument
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      offset = offset,
      prob.struct = parsed_mapping,
      extra_var = extra_var,
      side = side,
      ...
    )
  )

}


#' @importFrom grid grobTree
#' @references GeomMosaic
GeomBloc <- ggplot2::ggproto(
  "GeomBloc",
  ggplot2::Geom,

  setup_data = function(data, params){
    browser()
    data
  },

  # required_aes = c("xmin", "xmax", "ymin", "ymax"),
  required_aes = c(),

  # from ggmosaic
  # default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
  #                            shape = 19, colour = NA,
  #                            size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
  #                            linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),

  default_aes = ggplot2::aes(
    colour = NA, fill = "#4eb3d3", size = 0.5,stroke = 0.1, linewidth = .1, weight = 1,
    linetype = 1, alpha = 1, rel_min_height = 0, scale = 1.8
  ),

  # from ggplot, geom-ribbon.r
  setup_data = function(self, data, params){
    browser()
    if ("density" %in% names(data)) {
      browser()
      # all density plots

      order = data$x
      if(all(data$flip == TRUE)){
        order = data$y
        data <- transform(data[order(data$PANEL, data$group, order), ], xmin =min(x), xmax = x)
      } else {
        data <- transform(data[order(data$PANEL, data$group, order), ], ymin =0, ymax = y)
      }

      #data <- transform(data[order(data$PANEL, data$group, data$y), ], xmin =min(x), xmax = x)

      if ("height" %in% names(data)){
        # ridge plots
        # code from ggridges/R/geoms.R
        if (is.null(params$panel_scaling)) {
          params$panel_scaling <- TRUE
        }

        # calculate internal scale
        tmp = data$y
        if(all(data$flip == TRUE)){
          tmp = data$x
        }
        range = max(tmp) - min(tmp)
        n = length(unique(tmp))

        if (n<2) {
          hmax <- max(data$height, na.rm = TRUE)
          iscale <- 1
        } else {
          #scale per panel or globally?
          if (params$panel_scaling) {
            heights <- split(data$height, data$PANEL)
            max_heights <- vapply(heights, max, numeric(1), na.rm = TRUE)
            hmax <- max_heights[data$PANEL]
            iscale <- range/((n-1)*hmax)
          } else {
            hmax <- max(data$height, na.rm = TRUE)
            iscale <- range/((n-1)*hmax)
          }

        }
        data <- cbind(data, iscale)

        if (!"scale" %in% names(data)) {
          if (!"scale" %in% names(params))
            data <- cbind(data, scale = self$default_aes$scale)
          else
            data <- cbind(data, scale = params$scale)
        }

        if (!"rel_min_height" %in% names(data)){
          if (!"rel_min_height" %in% names(params))
            data <- cbind(data, rel_min_height = self$default_aes$rel_min_height)
          else
            data <- cbind(data, rel_min_height = params$rel_min_height)
        }

         browser()
        if (all(data$flip == TRUE)){
          data <- transform(data,
                            xmin = x,
                            xmax = x + iscale*scale*height,
                            min_height = hmax*rel_min_height)

        } else {
          data <- transform(data,
                  ymin = y,
                  ymax = y + iscale*scale*height,
                  min_height = hmax*rel_min_height)
        }

        browser()
        if ("down" %in% names(data)){
          data$ymax  = -(data$ymax) + 2 * data$y

        }
        else if("both" %in% names(data)){
          temp = data
          temp$ymax  = -(temp$ymax) + 2 * temp$y
          data = rbind(data,temp)
        }
        return(data)

      }
      browser()
      if ("down" %in% names(data)){
        data$ymax  = -(data$ymax) + 2 * data$ymin

      }
      else if("both" %in% names(data)){
        temp = data
        temp$ymax  = -(temp$ymax) + 2 * temp$ymin
        data = rbind(data,temp)
      }
      data

    } else {
      # not density plots

      # product plots, etc. only want top level rectangles

      browser()
      subset(data, level==max(data$level))

    }
  },

  # draw_panel = function(data, panel_scales, coord) {
  draw_group = function(data, panel_params, coord, na.rm = FALSE) {
    # Check that aesthetics are constant
    browser()
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      stop("Aesthetics can not vary along a ridgeline")
    }
    aes <- as.list(aes)
    browser()
    if ("density" %in% names(data)){
      if ("height" %in% names(data)){
        # ridge plot
        # remove all points that fall below the minimum height
        if (all(data$flip == TRUE)){
          data$xmax[data$height < data$min_height] <- NA
        }else{
          data$ymax[data$height < data$min_height] <- NA
        }
      }


      # from ggplot, geom-ribbon.r

      if (all(data$flip == TRUE)){
        if (na.rm) data <- data[stats::complete.cases(data[c("y", "xmin", "xmax")]), ]
      } else {
        if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
      }

      data <- data[order(data$group), ]


      if (all(data$flip == TRUE)){
        missing_pos <- !stats::complete.cases(data[c("y", "xmin", "xmax")])
      } else {
        missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
      }

      ids <- cumsum(missing_pos) + 1
      ids[missing_pos] <- NA

      browser()
      data <- unclass(data) #for faster indexing

      if (all(data$flip == TRUE)){
        positions <- new_data_frame(list(
          x = c(data$xmax, rev(data$xmin)),
          y = c(data$y, rev(data$y)),
          id = c(ids, rev(ids))
        ))
      } else {
        positions <- new_data_frame(list(
          x = c(data$x, rev(data$x)),
          y = c(data$ymax, rev(data$ymin)),
          id = c(ids, rev(ids))
        ))
      }

      #munched <- coord_munch(coord, positions, panel_params)


      browser()
      if("both" %in% names(data)){
        positions = positions %>% filter(y != data$ymin[1])
      }
      #data$y  = -(data$y) + 2 * data$ymin[1]
      # munched <- tibble(x=c(1,2,3,3,2,1),
      #              y=c(1,2,1,-1,-2,-1),
      #              id=c(1))
      # munched <- coord_munch(coord, positions, panel_params)
      # if(mean(positions$y) == data$ymin[1]){
      if (all(data$flip == TRUE)){

      if(mean(positions$y) == data$xmin[1]){
        position_positive = positions %>% filter(y > data$xmin[1]) %>% arrange(y)
        position_negative = positions %>% filter(y < data$xmin[1]) %>% arrange(desc(y))
        munched = rbind(position_positive, position_negative)
        munched <- coord_munch(coord, munched, panel_params)
      }
      else{
        munched <- coord_munch(coord, positions, panel_params)
      }
      } else {
        if(mean(positions$y) == data$ymin[1]){
          position_positive = positions %>% filter(y > data$ymin[1]) %>% arrange(x)
          position_negative = positions %>% filter(y < data$ymin[1]) %>% arrange(desc(x))
          munched = rbind(position_positive, position_negative)
          munched <- coord_munch(coord, munched, panel_params)
        }
        else{
          munched <- coord_munch(coord, positions, panel_params)
        }
      }
      #munched <- coord_munch(coord, positions, panel_params)
      #browser()

      ggname("geom_bloc", grid::polygonGrob(
        munched$x, munched$y, id = munched$id,
        default.units = "native",
        gp = gpar(
          fill = alpha(aes$fill, aes$alpha),
          col = aes$colour,
          lwd = aes$size * .pt,
          lty = aes$linetype)
      ))
    } else {
      # GeomRect$draw_panel(subset(data, level==max(data$level)), panel_params, coord)
      # mosaic plots/product plots
      if (!coord$is_linear()) {
        aesthetics <- setdiff(
          names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
        )

        polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
          poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
          aes <- new_data_frame(row[aesthetics])[rep(1,5), ]
          #browser()
          GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
        })

        ggname("bar", do.call("grobTree", polys))
      } else {
        coords <- coord$transform(data, panel_params)
        ggname("geom_rect", grid::rectGrob(
          coords$xmin, coords$ymax,
          width = coords$xmax - coords$xmin,
          height = coords$ymax - coords$ymin,
          default.units = "native",
          just = c("left", "top"),
          gp = gpar(
            col = coords$colour,
            fill = alpha(coords$fill, coords$alpha),
            lwd = coords$size * .pt,
            lty = coords$linetype,
            lineend = "butt"
          )
        ))
      }

    }
  },

  #   # stuff from ggmosaic; for colors
  #   # if (all(is.na(data$colour)))
  #     # data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination
  #
  #   # TODO: check if it's a density plot or not?
  #   if ("xmin" %in% names(data)){
  #     browser()
  #     GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord)
  #   } else {
  #     # stop("density plots not implemented")
  #     browser()
  #     GeomArea$draw_panel(data, panel_params = panel_params, coord = coord)
  #   }
  # },
  # draw_key = ggplot2::draw_key_rect
  draw_key = ggplot2::draw_key_polygon

)

