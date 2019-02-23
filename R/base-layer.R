
#' @importMethodsFrom bound productplots
#' @importMethodsFrom divide_once productplots
base_layer <- function(data, prob.struct, offset, bounds = productplots:::bound()){

  # browser()
  margin <- getFromNamespace("margin", "productplots")

  # stuff from prodcalc()
  # this is wrong... need unique()
  marg_var <- paste0("p.", sapply(get_margs(prob.struct), as.character))
  cond_var <- paste0("p.", sapply(get_conds(prob.struct), as.character))
  wt <- margin(data, marg_var, cond_var)

  # stuff from divide()
  # but different logic: just want to get rid of x <- P(1|A), y <- P(1|B)
  first_marg <- prob.struct[1,]$marginals[[1]]
  if (!(first_marg == 1)){ # base case
    return(data)
  }


  margin <- getFromNamespace("margin", "productplots")

  first_aes <- prob.struct$aes[1]
  d <- if (first_aes == "area") 2 else 1
  browser()
  parent_data <- margin(wt, rev(seq_len(d)))


  # TODO: deal with bounds
  # TODO: recurse on base_layer
  parent <- divide_once(parent_data, bounds, )



  parent_data
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
