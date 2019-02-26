#' @importFrom plyr dlply ldply rbind.fill
base_layer <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
  # stuff from divide()
  # but different logic: just want to get rid of x <- P(1|A), y <- P(1|B)

  if (nrow(prob.struct) == 1)
    return(divide_base(data, bounds, prob.struct[1,3], level, offset))


  first_aes <- prob.struct$aes[1]
  d <- if (first_aes == "area") 2 else 1

  margin <- getFromNamespace("margin", "productplots")
  parent_data <- margin(data, rev(seq_len(d)))

  # here, nrow(prob.struct ) > 1
  # just return if the next aes is not a coord one
  next_aes <- prob.struct[2,]$aes[[1]]
  if (!(startsWith(next_aes, "x.") | startsWith(next_aes, "y."))){
    # browser()
    return(divide_base(parent_data, bounds, prob.struct[1,3], level, offset))
  }

  # TODO: recurse on base_layer
  parent <- divide_base(parent_data, bounds, prob.struct[1,3], level, offset)
  pieces <- as.list(dlply(data, seq_len(d)))
  parentc <- parent

  children <- ldply(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    # base_layer <-function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
    partition <- base_layer(data = piece[, -seq_len(d)],
                            prob.struct = prob.struct[-1, ],
                            offset = offset,
                            level = level + 1,
                            bounds = parentc[i,]
                        # cascade = cascade, max_wt = max_wt,
                        )

    labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
    cbind(labels, partition)
  })

  rbind.fill(parent, children)
}

#' @references divide_once from prodplot
divide_base <- function(data, bounds, aes, level=1, offset){
  if (is.list(aes)){
    aes <- aes[[1]]
  }
  d <- if (aes == "area") 2 else 1
  stopifnot(d == 1) # TODO: deal with 2d aes

  wt <- data$.wt
  wt <- wt/sum(wt, na.rm = TRUE)

  # those $l, $r, $b, $t things
  divider <- aes_lookup(aes)
  # divider(data, bounds, offset = offset, max = NULL)
  partition <- divider(wt, bounds, offset)

  cbind(data, partition, level = level)

}


#' Fill `data` with coordinates of a square $[0,1]x[0,1]$
#'
#' @param data the data object in `compute_panel()` in Stat*
#'
#' @return data for a unit square in $[0,1]x[0,1]$
# dummy_rect <- function(data){
#   res <- data[1, ]
#   res$ymin <- 0
#   res$ymax <- 1
#   res$xmin <- 0
#   res$xmax <- 1
#   res$PANEL <- 1
#
#   res$group <- 1
#   res$level <- 1
#
#   browser()
#
#   res
# }
