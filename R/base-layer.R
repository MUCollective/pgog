
base_layer <- function(data, prob.struct, offset){

  # find the P(1|A), P(1|B) and so on
  for (i in seq_len(nrow(prob.struct))){
    browser()
    if (prob.struct$marginals[i] == 1){


    }

  }

  data
}



#' Fill `data` with coordinates of a square $[0,1]x[0,1]$
#'
#' @param data the data object in `compute_panel()` in Stat*
#'
#' @return data for a unit square in $[0,1]x[0,1]$
dummy_rect <- function(data){
  res <- data[1, ]
  res$ymin <- 0
  res$ymax <- 1
  res$xmin <- 0
  res$xmax <- 1
  res$PANEL <- 1

  res$group <- 1
  res$level <- 1

  browser()

  res
}
