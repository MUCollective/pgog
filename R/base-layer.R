#' @importFrom plyr dlply ldply rbind.fill
bloc_divide <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
  #browser()
  if (nrow(prob.struct) == 1)
    return(divide_base(data=data, bounds = bounds, aes = prob.struct[1,3],
                       level = level, offset = offset))

  first_aes <- prob.struct$aes[1]
  d <- if (first_aes == "area") 2 else 1

  parent_data <- margin(data, rev(seq_len(d)))

  parent <- divide_base(data = parent_data, bounds = bounds,
                        aes = prob.struct[1,3], level = level, offset = offset)
  pieces <- as.list(dlply(data, seq_len(d)))
  parentc <- parent

  children <- ldply(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    # base_layer <-function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
    partition <- bloc_divide(data = piece[, -seq_len(d)],
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

#' @importFrom dplyr filter
pack_icons <- function(data, bounds, prob.struct, offset){

  base_level <- bounds$level[1]

  base_aes <- prob.struct[base_level, 3][[1]]
  if (grepl("x.", base_aes) | grepl("height", base_aes)){
    direction <- "down"
  } else {
    direction <- "right"
  }

  spacing <- calc_spacing(bounds, direction, offset)

  if (direction == "down"){
    width <- bounds$r[1] - bounds$l[1] - offset
    icons_per_dim <- floor(width / spacing)
  } else {
    height <- bounds$t[1] - bounds$b[1] - offset
    icons_per_dim <- floor(height/spacing)
  }


  conds_var <- names(bounds)[grepl("p.", names(bounds))]
  counts <- data %>%
    filter(level > base_level)


  # calculate the max number of dots within a partition
  if (nrow(counts) != 0){

    counts_by_group <- counts %>%
      group_by_(.dots = conds_var) %>%
      summarise_at(".N", sum)

    max_count <- max(counts_by_group$.N)
  } else {
    max_count <- max(bounds$.N)
  }

  if (nrow(data) == nrow(bounds)){
    # icon_per_dim <- as.integer(sqrt(max(data$.N))/0.618)

    res <- ldply(seq_len(nrow(data)), function(i){
      piece <- data[i, ]
      bound <- bounds[i, ]
      pack_one_partition(piece, bound, spacing, icons_per_dim, offset, direction, max_count)
    })

  } else {
    # need parent bounding box
    parent <- bounds %>%
      filter(level == base_level)
    pieces <- dlply(counts, .variables = conds_var)


    # icon_per_dim <- as.integer(sqrt(max_count / length(pieces)) / 0.618)
    # icon_per_dim <- adjust_N(bounds, icon_per_dim, direction)

    # schema of `coord`: x, y, <marg_var values>
    res <- ldply(seq_along(pieces), function(i) {
      piece <- pieces[[i]]
      bound <- parent[i, ]
      pack_one_partition(piece, bound, spacing, icons_per_dim, offset, direction, max_count)
    })
  }

  repack(res, direction, spacing)
  # res

}


repack <- function(df, direction, spacing){

  if (direction == "down"){

    t <- max(df$y)
    n_ycoords <- length(unique(df$y))
    new_ys <- seq(from=t, by = -1 * spacing, length.out = n_ycoords)
    ys <- sort(unique(df$y), decreasing = TRUE)

    match_ys <- function(i){
      idx <- match(i, ys)
      new_ys[idx]
    }
    df <- df %>% mutate(y = match_ys(y))
    df

  } else {

    l <- min(df$x)
    n_xcoords <- length(unique(df$x))
    new_xs <- seq(from = l, by = spacing, length.out = n_xcoords)
    xs <- sort(unique(df$x))

    match_xs <- function(i){
      idx <- match(i, xs)
      new_xs[idx]
    }

    df <- df %>% mutate(x = match_xs(x))
    df

  }

}

calc_spacing <- function(bounds, direction, offset){

  if (direction == "down"){
    bounds <- bounds %>%
      mutate(n_icon_col = sqrt(.N * (r-l )/(t-b )))
    n_icon_col <- ceiling(max(bounds$n_icon_col))
    # ACHTUNG: hack
    spacing <- (bounds$r[1] - bounds$l[1])/(n_icon_col + 1)
    spacing

  } else {
    bounds <- bounds %>%
      mutate(n_icon_row = sqrt(.N * (t-b)/(r - l)))
    n_icon_row <- ceiling(max(bounds$n_icon_row))
    spacing <- (bounds$t[1] - bounds$b[1]) / (n_icon_row + 1)
    spacing
  }

}


#' @importFrom utils head
pack_one_partition <- function(counts, bound, spacing, N, offset, direction, max_count){

  d <- offset / 2
  all_vars <- names(counts)[grepl("p", names(counts))]


  if (direction == "down"){
    # x.coords <- head(seq(bound$l + d, bound$r - d, length.out = N + 1), -1)
    # y.coords <- seq(bound$b + d, bound$t - d, length.out = ceiling(sum(counts$.N))/N + 1)[-1]

    x.coords <- seq(from = bound$l + spacing/2, by = spacing, length.out = N)
    y.coords <- seq(from = bound$b + spacing/2, by = spacing, length.out = ceiling(max_count/N ))

    counts <- counts %>%
      select(c(.N, all_vars)) %>%
      uncount(weights = .N)

    grid <- expand.grid(x = rev(x.coords), y = rev(y.coords))

    grid <- grid %>%
      group_by_("y") %>%
      mutate(x = rev(x)) %>%
      ungroup() %>%
      head(n = nrow(counts))

    cbind(counts, grid)

  } else { # right

    # y.coords <- seq(bound$b + d, bound$t - d, length.out = N + 1)[-1]
    # x.coords <- head(seq(bound$l + d, bound$r - d, length.out = ceiling(sum(counts$.N))/N + 1), -1)
    y.coords <- seq(from = bound$b + spacing/2, by = spacing, length.out = N)
    x.coords <- seq(from = bound$l + spacing/2, by = spacing, length.out = ceiling(sum(counts$.N)/N))

    counts <- counts %>%
      select(c(.N, all_vars)) %>%
      uncount(weights = .N)

    grid <- expand.grid(x = x.coords, y = rev(y.coords))

    grid <- grid %>%
      arrange(x) %>%
      head(n=nrow(counts))

    cbind(counts, grid)
  }

}


#' @importFrom plyr dlply ldply rbind.fill
icon_divide <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
  # stuff from divide()
  # but different logic: just want to get rid of x <- P(1|A), y <- P(1|B)

  if (nrow(prob.struct) == 1)
    return(divide_base(
      data = data, bounds = bounds,
      aes = prob.struct[1,3],
      level = level,
      max_wt = NULL,
      offset = offset))


  first_aes <- prob.struct$aes[1]
  d <- if (first_aes == "area") 2 else 1

  parent_data <- margin(data, rev(seq_len(d)))


  # TODO: recurse on base_layer
  parent <- divide_base(
    data = parent_data, bounds = bounds,
    aes = prob.struct[1,3],
    level = level,
    max_wt = NULL,
    offset = offset)

  pieces <- as.list(dlply(data, seq_len(d)))

  parentc <- parent


  # here, nrow(prob.struct ) > 1
  # just return if the next aes is not a coord one
  next_aes <- prob.struct[2,]$aes[[1]]
  # if (!(startsWith(next_aes, "x.") | startsWith(next_aes, "y."))){
  if (!(startsWith(next_aes, "x.cond") | startsWith(next_aes, "y.cond"))){
    base_layout <- divide_base(
        data = parent_data, bounds = bounds,
        aes = prob.struct[1,3],
        level = level,
        max_wt = NULL,
        offset = offset)

    # set_attrs(base_layout, pieces = pieces)
    # browser()
    # skips to drawing rectangles
    return(base_layout)
    # return(list(layout = base_layout, data = pieces))

    # TODO: calculate how many dots per row/col
    # max_group_n <- max(parent_data$.N)
    # n_groups <- length(pieces)
    #
    # icon_per_dim <- as.integer(sqrt(max_group_n) / 0.618)
    #
    # ldply(seq_along(pieces), function(i){
    #   piece <- pieces[[i]]
    #   pack_icons(piece, prob.struct, offset, level+1, parentc[i,], icon_per_dim)
    # })
  }


  children <- ldply(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    partition <- icon_divide(data = piece[, -seq_len(d)],
                            prob.struct = prob.struct[-1, ],
                            offset = offset,
                            level = level + 1,
                            bounds = parentc[i,]
                        # cascade = cascade, max_wt = max_wt,
    )

    labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
    cbind(labels, partition)
  })



  # rbind.fill(parent, children)
  children
}

#' @references divide_once from prodplot
divide_base <- function(data, bounds, aes, level=1, max_wt = NULL, offset){
  if (is.list(aes)){
    aes <- aes[[1]]
  }
  d <- if (aes == "area") 2 else 1
  stopifnot(d == 1) # TODO: deal with 2d aes

  wt <- data$.wt
  wt <- wt/sum(wt, na.rm = TRUE)

  if (is.null(max_wt)){
    max_wt <- max(wt, na.rm = TRUE)
  }

  # those $l, $r, $b, $t things
  divider <- aes_lookup(aes)
  # divider(data, bounds, offset = offset, max = NULL)
  partition <- divider(wt, bounds, offset, max = max_wt)

  # browser()
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
