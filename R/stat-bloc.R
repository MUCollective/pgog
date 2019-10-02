

stat_bloc <- function(
  mapping = NULL,
  data = NULL,
  geom = "bloc",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...){
  # custom params
  # offset = 0.01) {


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBloc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      # divider = divider,
      prob.struct,
      offset = offset,
      ...
    )
  )

}


StatBloc <- ggplot2::ggproto(
  "StatBloc", ggplot2::Stat,
  non_missing_aes = "weight",

  # this wouldn't work would it
  # default_aes = aes(y = stat(density)),

  compute_panel = function(self, data, scales, na.rm = FALSE,
                           prob.struct, offset = 0.01,
                           bw = "nrd0",
                           adjust = 1,
                           kernel = "gaussian",
                           n = 512){

    margin <- getFromNamespace("margin", "productplots") # TODO: get rid of productplot dependency
    # stuff from prodcalc()
    # this is wrong... need unique()
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


    # check if these variables are continuous
    all_rvs <- unique(c(marg_var, cond_var))
    is_continuous <- function(i) length(unique(i)) > 7
    rv_levels <- sapply(as.tibble(data[, all_rvs]), is_continuous)
    has_too_many_levels <- sum(rv_levels)


    if (! has_too_many_levels){
      message("Defaulting to mosaic plots. Use `stat=blocdensity` to force density plots")

      # the normal mosaic plot things
      wt <- margin(data, marg_var, cond_var)
      # base_layer <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
      res <- bloc_divide(data = wt, prob.struct = prob.struct, offset = offset)

#
#       pieces <- as.list(dlply(res, 1))
#       res <- ldply(seq_along(pieces), function(i){
#         piece <- pieces[[i]]
#
#         l <- piece$l
#         r <- piece$r
#         b <- piece$b
#         t <- piece$t
#
#         # coords <- data.frame(x = c(l,l,r,r), y=c(b,t,b,t))
#         coords <- new_data_frame(list(
#           y = c(t, t, b, b, t),
#           x = c(l, r, r, l, l)
#         ))
#
#         others <- piece %>%
#           subset(select = -c(l, r, b, t))
#
#         others$group <- i
#         cbind(coords, others)
#       })

      res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
      res$group <- seq_len(nrow(res))

      res

    } else {

      # ==================== density plots ======================
      message("Defaulting to density plots. Use `stat=mosaic` to force mosaic plots")
      is_P_B_given_A <- FALSE
      is_ridges <- FALSE # for P(A|B) defaults to P(A|B), color <- B


      # Need to ignore the x.conds rows
      aeses <- unlist(prob.struct$aes, use.names = FALSE)
      # the real number of terms in pmf, like P(B|A) P(A) has two terms
      # but pmf = P(B | A) only has one
      # n_prob_terms <- sum(!grepl("cond", aeses))
      n_prob_terms <- length(marg_var)

      if (n_prob_terms > 2){
        stop("one simply does not draw density plots with such complexity")
      }

      if (n_prob_terms == 1){

        # there should only be 1 var in the marginals
        stopifnot(length(marg_var) == 1)


        if (is_continuous(data[, marg_var])){
          # is it P(A|...?)
          # all the conds should be discrete
          if (!is.null(cond_var)){
            none_cond_continuous <-
              sum(sapply(as.tibble(data[, cond_var]), is_continuous)) == 0
            stopifnot(none_cond_continuous)
          }

          # the last aes (for P(A|...)) must be one of these
          stopifnot(grepl("x.height", tail(aeses, n = 1)) |
                      grepl("y.width", tail(aeses,n=1)) )

          # base_layout does not consider color, etc aeses
          # sub_prob.struct
          # for (i in seq_len(nrow(prob.struct))){
          #   cur_aes <- prob.struct[i,3][[1]]
          #   if (grepl("colour", cur_aes)){
          #     prob.struct <- prob.struct[-i, ]
          #     break
          #   }
          # }

          # ===== P(A|...) =======

          cont_var <- marg_var
          group_var <- cond_var


          # partition
          # if (is.null(cond_var)){
          if (nrow(prob.struct) == 1){
            # P(A)
            # wt should not be calculated here
            base_layout <- data.frame(.wt = 1, l = 0, r = 1, b = 0, t = 1, level = 1)
          } else {
            # P(A|...)
            # is it P(A|B), color <- B or y <- B

            if (grepl("x",tail(aeses, n= 1))){ # A on x.height
              if (grepl("y", tail(aeses, n=2)[1])){
                # P(A|B), y <- B
                is_ridges <- TRUE
              } else if (grepl("x", tail(aeses, n=2)[1])){
                stop(paste("Check aes mapping on the categorial variable ", group_var))
              }
            } else { # A on y.width
              if (grepl("x", tail(aeses, n=2)[1])){
                # P(A|B), x <- B
                is_ridges <- TRUE
              } else if (grepl("y", tail(aeses, n=2)[1])){
                stop(paste("Check aes mapping on the categorial variable ", group_var))
              }
            }

            # process
            wt <- margin(data, marg_var, cond_var)
            base_layout <- icon_divide(data = wt, prob.struct = prob.struct, offset = offset)



          }
        } else {
          # is it P(B|A, ...)?
          # A should be continuous, but
          # the rest of the conds should be discrete

          if (!is.null(cond_var)){
            one_cond_continuous <-
              sum(sapply(as.tibble(data[, cond_var]), is_continuous)) == 1
            stopifnot(one_cond_continuous)
          }

          if (length(cond_var) == 1){
            A <- cond_var
          } else {
            A <- cond_var[sapply(as.tibble(data[, cond_var]), is_continuous)]
            stopifnot(length(A) == 1)
          }

          # aes on P(B|A,...) must be height or width
          stopifnot(grepl("height", tail(aeses, n=1)) |
                      grepl("width", tail(aeses, n=1)))

          # ============== P(B|A...) =============
          group_var <- marg_var # TODO: wrong
          cont_var <- A

          # Partition
          if (length(cond_var) == 1){
            # P(B|A)
            base_layout <- data.frame(.wt = 1, l = 0, r = 1, b = 0, t = 1, level = 1)

          } else {
            # P(B|A,...)
            cond_mask <- !(sapply(as.tibble(data[, cond_var]), is_continuous))
            wt <- margin(data, NULL, cond_var[cond_mask])

            # TODO: get rid of A in the prob.struct
            repeated_conds <- sapply(unlist(prob.struct$conditionals, use.names = FALSE),
                                     function(i){paste0("p.", as.character(i))})
            A_index <- match(A, unique(repeated_conds))
            base_layout <- icon_divide(data = wt, prob.struct = prob.struct[-A_index,], offset = offset)

          }

          is_P_B_given_A <- TRUE

          # stop("not implemented: P(B|A,...)")
        }


      } else {
        # is it P(B, A|...)?
        stopifnot(length(marg_var) == 2)

        # B should be categorical
        # A should be continuous
        B <- marg_var[1]
        A <- marg_var[2]

        stopifnot(!is_continuous(data[, B]))
        stopifnot(is_continuous(data[, A]))
        # browser()

        if (grepl("height", tail(aeses, n =2)[1])
            && grepl("x", tail(aeses, n =2)[1])){
          if (grepl("height", tail(aeses, n =1))){
            # oops
          } else {
            stop("P(B, A|...)? check P(B|A) orientation")
          }

        } else if (grepl("width", tail(aeses, n =1))
                   && grepl("y", tail(aeses, n =2)[1])){
          if (grepl("width", tail(aeses, n =2)[1])){
            # oops
          } else {
            stop("P(B, A|...)? check P(B|A) orientation")
          }
        } else {
          stop("Invalid aes mapping for P(B|A); only width and height are allowed")
        }

        #  ======= P(B,A|...) ========

        # stop()

        cont_var <- A
        group_var <- B
        is_P_B_given_A <- TRUE

        # partition
        if (is.null(cond_var)){
          # P(B,A)
          # wt should not be calculated here
          base_layout <- data.frame(.wt = 1, l = 0, r = 1, b = 0, t = 1, level = 1)
        } else {
          # P(B,A|...)
          wt <- margin(data, marg_var, cond_var)
          base_layout <- icon_divide(data = wt, prob.struct = prob.struct, offset = offset)
        }

        # stop("not implemented: P(B,A|...), y")
      }


      # ============================= density ========================


      # want number of partitions on x,y axes
      base_layout %<>%
        mutate(x_rank = dense_rank(l), y_rank = dense_rank(b))


      # calculate a single bandwidth for all groups

      if (is_string(bw)){
        group_bw <- data %>%
          pull(!!cont_var) %>%
          bw.nrd0()
      } else { # if bw is a set number
        group_bw <- bw
      }


      # unnecessary
      if (! is.null(group_var))
        group_bw <- group_bw #/ length(unique(data[, group_var]))


      # browser()

      pieces <- as.list(dlply(data,group_var))

      # if (group_var %in% names(base_layout)){
      #   bounds <- as.list(dlply(base_layout, group_var))
      # } else {
      #
      #
      # }

      # get the range of continuous variable

      continous_range <- range(select_(data, cont_var))
      continuous_diff <- continous_range[2]- continous_range[1]


      densities <- ldply(seq_along(pieces), function(i) {
        piece <- pieces[[i]]
        # bound <- bounds[[i]]

        range <- continous_range
        # TODO: what's that $weight
        res <- compute_density(c(t(select(piece, cont_var))), NULL,
                               from = range[1],
                               to = range[2],
                               bw = group_bw,
                               adjust = adjust,
                               kernel = kernel,
                               n=n)

        # keep other vars
        meta_data <- select(piece, -c("x", "y"))[1,]
        meta_data$group <- i

        if (! is.na(res$density)){
          # ====== ACHTUNG =========
          # adjust positions to grids
          # res$x <- res$x + (bound$x_rank - 1) * continuous_diff
          # res$density <- res$density + (bound$y_rank - 1)
          cbind(res, meta_data, row.names = NULL)
        }
      })


      if (is_P_B_given_A){
        # stat(density * n)
        densities$y <- densities$density * densities$n
        # browser()
      } else {
        densities$y <- densities$density
      }

      if (is_ridges){
        # browser()
        # densities$y <-
        densities %<>%
          mutate_(y = group_var) %>%
          mutate(height = density)
      }
      return(densities)

    }
  }
)


create_density <- function(base_layout, cont_var, group_var){


}




compute_density <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian", n = 512) {
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1 / nx, nx)
  }
  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    warning("Groups with fewer than two data points have been dropped.", call. = FALSE)
    return(new_data_frame(list(
      x = NA_real_,
      density = NA_real_,
      scaled = NA_real_,
      ndensity = NA_real_,
      count = NA_real_,
      n = NA_integer_
    ), n = 1))
  }

  # browser()

  dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
                         kernel = kernel, n = n, from = from, to = to)
  new_data_frame(list(
    x = dens$x,
    density = dens$y,
    scaled =  dens$y / max(dens$y, na.rm = TRUE),
    ndensity = dens$y / max(dens$y, na.rm = TRUE),
    count =   dens$y * nx,
    n = nx
  ), n = length(dens$x))
}
