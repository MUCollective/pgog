

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

    # just making sure that the thing draws
    # dummy_rect(data)

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

    wt <- margin(data, marg_var, cond_var)

    # base_layer <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
    res <- bloc_divide(wt, prob.struct, offset)
    # browser()
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    res
  }
)


