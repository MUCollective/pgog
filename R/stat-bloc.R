

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

  compute_panel = function(self, data, scales, na.rm = FALSE,
                           prob.struct, offset = 0.01){

    margin <- getFromNamespace("margin", "productplots")
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
      # browser()
      res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
      res

    } else {
      message("Defaulting to density plots. Use `stat=mosaic` to force mosaic plots")

      # Need to ignore the x.conds rows
      aeses <- unlist(prob.struct$aes, use.names = FALSE)
      # the real number of terms in pmf, like P(B|A) P(A) has two terms
      # but pmf = P(B | A) only has one
      n_prob_terms <- sum(!grepl("cond", aeses))

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

          # the last aes must be one of these
          stopifnot(grepl("x.height", tail(aeses, n = 1)) |
                      grepl("y.width", tail(aeses,n=1)))


          # partition
          if (is.null(cond_var)){
            # P(A)
            # wt should not be calculated here
            base_layout <- data.frame(.wt = 1, l = 0, r = 1, b = 0, t = 1, level = 1)
          } else {
            # P(A|...)
            wt <- margin(data, marg_var, cond_var)
            base_layout <- icon_divide(data = wt, prob.struct = prob.struct, offset = offset)
          }

          stop("not implemented: P(A|...)")
          return()


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

          browser()

          stop("not implemented: P(B|A,...)")
          return()
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

        if (tail(aeses, n =1) == "height"){
          if (aeses[length(aeses) - 1] == "x.height"){
            # oops
          } else {
            stop("P(B, A|...)? check P(B|A) orientation")
          }

        } else if (tail(aeses, n=1) == "width"){
          if (aeses[length(aeses) - 1] == "y.width"){
            # oops
          } else {
            stop("P(B, A|...)? check P(B|A) orientation")
          }
        } else {
          stop("Invalid aes mapping for P(B|A); only width and height are allowed")
        }


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

        stop("not implemented: P(B,A|...), y")
      }
    }
  }
)


