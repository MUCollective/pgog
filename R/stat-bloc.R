

stat_bloc <- function(
  mapping = NULL,
  data = NULL,
  geom = "bloc",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE){
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
    marg_var <- paste0("p.", sapply(get_margs(prob.struct), as.character))
    cond_var <- paste0("p.", sapply(get_conds(prob.struct), as.character))
    wt <- margin(data, marg_var, cond_var)

    # base_layer <- function(data, prob.struct, offset, level=1, bounds = productplots:::bound()){
    res <- base_layer(wt, prob.struct, offset)
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    res

  }
)


