# CanvasLength: the length of the canvas in numeric
# CanvasHeight: the height of the canvas in numeric
# NumberOfIcon: number of icon for each group in vector
# NumberOfGroup: number of groups in integer, default 1
# connected: whether different groups of icon is connected in boolean, default false
arrangement = function(CanvasLength, CanvasHeight, NumberOfIcon, NumberOfGroups=1, connected=FALSE, ratio=1.0) {

  # color vector
  color = c("blue", "green", "yellow", "red", "gray")

  # return empty list if one of height, length and number of groups is invalid
  if (CanvasHeight <= 0 || CanvasLength <= 0 || NumberOfGroups <= 0) {
    return(list())
  }

  # return empty list if number of icon does not match number of groups
  if (length(NumberOfIcon) != NumberOfGroups) {
    return(list())
  }

  # return empty list if number of icon is invalid
  for (x in NumberOfIcon) {
    if (x <= 0) {
      return(list())
    }
  }

  # return empty list if more than 5 colors are needed
  if (NumberOfGroups > 5) {
    return(list())
  }

  # swap height and length then make the recursive call if height is larger than length
  if (CanvasHeight > CanvasLength) {
    result = arrangement(CanvasHeight, CanvasLength, NumberOfIcon, NumberOfGroups, connected, ratio)
    for (i in seq(1, length(result))) {
      temp = result[[i]][1]
      result[[i]][1] = result[[i]][2]
      result[[i]][2] = temp
    }
    return(result)
  }

  if (NumberOfGroups == 1) {

    # case 1: only one group
    NumberOfIcon = NumberOfIcon[1]

    # simply return the center of the canvas if there's onlyh one icon
    if (NumberOfIcon == 1) {
      return(list(c(CanvasLength / 2, CanvasHeight / 2, color[1])))
    }

    # create a vector holding the differences between the length and height of icon for different icon arrangements
    differences = c()
    for (i in seq(ceiling(sqrt(NumberOfIcon)), NumberOfIcon)) {
      differences = c(differences, abs(CanvasHeight / ceiling(NumberOfIcon / i) - CanvasLength / i))
    }

    # choose the arrangement that has the smallest differences in icon length and height
    iconPerLine = which(differences == min(differences))[1]
    iconPerLine = iconPerLine + ceiling(sqrt(NumberOfIcon)) - 1
    iconSizeHorizontal = CanvasLength / iconPerLine
    iconSizeVertical = CanvasHeight / ceiling(NumberOfIcon / iconPerLine)
    iconSize = min(c(iconSizeVertical, iconSizeHorizontal)) * ratio

    # create list to return using the selected arrangement
    x = 1
    y = 1
    index = 1
    result = list()
    while(index <= NumberOfIcon) {
      result[[index]] = c((x - 1/2) * iconSizeHorizontal, (y - 1/2) * iconSizeVertical, color[1], iconSize)
      index = index + 1
      x = x + 1
      if (x > iconPerLine) {
        x = 1
        y = y + 1
      }
    }
    return(result)

  } else {

    # case 2: multiple group but connected
    if (connected) {

      # recursive call as if all groups are one large group
      result = arrangement(CanvasLength, CanvasHeight, c(sum(NumberOfIcon)), 1, FALSE, ratio)

      # change colors in the return value above to make sure different group has different color
      index = NumberOfIcon[1]
      for (i in seq(2, NumberOfGroups)) {
        for (j in seq(1, NumberOfIcon[i])) {
          result[[index + j]][3] = color[i]
        }
        index = index + NumberOfIcon[i]
      }
      return(result)

    } else {

      # case 3: multiple group not connected
      # create a vector holding the differences between the length and height of icon for different icon arrangements
      differences = c()
      for (i in seq(1, max(NumberOfIcon))) {
        iconHolderPerLine = sum(ceiling(NumberOfIcon / i)) + NumberOfGroups - 1
        differences = c(differences, abs(CanvasHeight / i - CanvasLength / iconHolderPerLine))
      }

      # choose the arrangement that has the smallest differences in icon length and height
      iconHolderPerVerticalLine = which(differences == min(differences))[1]
      iconSizeVertical = CanvasHeight / iconHolderPerVerticalLine
      iconSizeHorizontal = CanvasLength / (sum(ceiling(NumberOfIcon / iconHolderPerVerticalLine)) + NumberOfGroups - 1)
      iconSize = min(c(iconSizeVertical, iconSizeHorizontal)) * ratio

      # create list to return using the selected arrangement
      x = 1
      y = 0
      index = 0
      result = list()
      for (i in seq(1, NumberOfGroups)) {
        for (j in seq(1, NumberOfIcon[i])) {
          index = index + 1
          y = y + 1
          if (y > iconHolderPerVerticalLine) {
            x = x + 1
            y = 1
          }
          result[[index]] = c((x - 1/2) * iconSizeHorizontal, (y - 1/2) * iconSizeVertical, color[i], iconSize)
        }
        x = x + 2
        y = 0
      }
      return(result)
    }
  }

  # return empty list if the code fails somehow
  return(list())
}

icon_grob = function(data, num_of_icon, num_of_group, connected, canvas_length, canvas_height, ratio,
                     name = NULL, gp = gpar(), vp = NULL) {

  gTree(
    data = data, num_of_icon = num_of_icon, num_of_group = num_of_group, connected = connected,
    canvas_length = canvas_length, canvas_height = canvas_height, ratio = ratio, name = name, gp = gp, vp = vp, cl = "icon_grob"
  )
}

#' @import grid
#' @export
makeContent.icon_grob = function(x) {

  # print(1 / convertUnit(unit(1, 'inches'), 'native', valueOnly = TRUE))
  # print(1 / convertUnit(unit(1, 'inches'), 'native', axisFrom = "y", valueOnly = TRUE))
  data = x$data
  canvas_length = 1 / convertUnit(unit(1, 'inches'), 'native', valueOnly = TRUE)
  canvas_height = 1 / convertUnit(unit(1, 'inches'), 'native', axisFrom = "y", valueOnly = TRUE)
  # canvas_length = convertUnit(unit(1, 'npc'), 'native', valueOnly = TRUE)
  # canvas_height = convertUnit(unit(0, 'npc'), 'native', axisFrom = "y", valueOnly = TRUE)
  num_of_icon = x$num_of_icon
  num_of_group = x$num_of_group
  connected = x$connected

  result = arrangement(canvas_length, canvas_height, NumberOfIcon = num_of_icon,
                       NumberOfGroups = num_of_group, connected = connected, x$ratio)

  x_vals = c()
  y_vals = c()
  cols = c()
  for (i in seq(length(result)))
  {
    x_vals = append(x_vals, as.double(result[[i]][1]) / canvas_length)
    y_vals = append(y_vals, as.double(result[[i]][2]) /  canvas_height)
    cols = append(cols, result[[i]][3])
  }

  grobSize = as.double(result[[1]][4]) / 2 / canvas_height

  # print(x_vals)
  # print(y_vals)
  # print(grobSize)

  circleGrob(x_vals, y_vals, grobSize, default.units = "native", gp = x$gp)
}


#' @export
geom_icon <- function(mapping = NULL,
                      data = NULL,
                      stat = "icon",
                      position = "identity",
                      ...,
                      binwidth = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      offset = 0.01,
                      prob.struct = NULL
                      ){

  # same as in geom_bloc
  # parse prob structure
  parsed_mapping <- parse_aes(mapping)
  print(parsed_mapping)


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



  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIcon,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      offset = offset,
      prob.struct = parsed_mapping,
      ...
    )
  )
}


#' @export
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 aes
GeomIcon <- ggplot2::ggproto("GeomIcon", ggplot2::Geom,
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "shape", "colour"),
                    default_aes = ggplot2::aes(
                      shape = 16, colour = "gray", size = 10, fill = NA,
                      alpha = NA, stroke = 0.5, ratio = 1.0
                    ),

                    setup_data = function(data, params) {
                      #browser()
                      data
                    },

                    draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
                      coords <- coord$transform(data, panel_params)
                      str(coords)

                      # assuming the first row of the data is cyl
                      vec = unlist(coords[1])

                      # browser()

                      canvas_length = convertUnit(unit(1, 'npc'), 'native', valueOnly = TRUE)
                      canvas_height = convertUnit(unit(0, 'npc'), 'native', axisFrom = "y", valueOnly = TRUE)

                      group_input = length(levels(as.factor(vec)))
                      icon_input = rep(0, length(levels(as.factor(vec))))
                      for (i in seq(length(levels(as.factor(vec))))) {
                        icon_input[i] = length(which(vec == as.numeric(levels(vec)[i])))
                      }

                      result = arrangement(canvas_length, canvas_height, NumberOfIcon = icon_input,
                                           NumberOfGroups = group_input, connected = FALSE, ratio = coords$ratio[1])

                      x_vals = c()
                      y_vals = c()
                      cols = c()
                      for (i in seq(length(result)))
                      {
                        x_vals = append(x_vals, as.double(result[[i]][1]) / canvas_length)
                        y_vals = append(y_vals, as.double(result[[i]][2]) /  canvas_height)
                        cols = append(cols, result[[i]][3])
                      }

                      # pointsGrob(x_vals, y_vals, default.units = "native", gp = gpar(col = cols,  lwd = 2))
                      icon_grob(data = vec, num_of_icon = icon_input, num_of_group = group_input,
                                connected = FALSE, canvas_length, canvas_height, coords$ratio[1], gp = gpar(fill = cols))
                    }


)

# ===== old geom_icon

#' From geom_point
#' geom_icon <- function(mapping = NULL, data = NULL,
#'                       stat = "icon",
#'                       ...,
#'                       na.rm = FALSE,
#'                       position = "identity",
#'                       show.legend = NA,
#'                       inherit.aes = TRUE) {
#'
#'
#'   # Logic check: stacking does not make sense without x, y aes
#'   # ACHTUNG: this is no longer needed
#'   aes_names <- names(enexpr(mapping))
#'   has_coord_aes <- "x" %in% aes_names | "y" %in% aes_names
#'
#'   if ((!has_coord_aes) & position == "stack"){
#'     message("Warning: Can't stack those aesthetics")
#'   }
#'
#'   # Assign the appropriate position adjustment
#'   if (position == "array"){
#'     position <- mod_position(aes_names)
#'   }
#'
#'
#'   # browser()
#'
#'   ggplot2::layer(
#'     data = data,
#'     mapping = mapping,
#'     stat = stat,
#'     geom = GeomIcon,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = list(
#'       na.rm = na.rm,
#'       ...
#'     )
#'   )
#' }
#'
#'

