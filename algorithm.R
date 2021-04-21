# CanvasLength: the length of the canvas in numeric
# CanvasHeight: the height of the canvas in numeric
# NumberOfIcon: number of icon for each group in vector
# NumberOfGroup: number of groups in integer, default 1
# connected: whether different groups of icon is connected in boolean, default false
arrangement = function(CanvasLength, CanvasHeight, NumberOfIcon, NumberOfGroups=1, connected=FALSE) {

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
    result = arrangement(CanvasHeight, CanvasLength, NumberOfIcon, NumberOfGroups, connected)
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

    # create list to return using the selected arrangement
    x = 1
    y = 1
    index = 1
    result = list()
    while(index <= NumberOfIcon) {
      result[[index]] = c((x - 1/2) * iconSizeHorizontal, (y - 1/2) * iconSizeVertical, color[1])
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
      result = arrangement(CanvasLength, CanvasHeight, c(sum(NumberOfIcon)), 1, FALSE)

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
          result[[index]] = c((x - 1/2) * iconSizeHorizontal, (y - 1/2) * iconSizeVertical, color[i])
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
