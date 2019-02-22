

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
    base_layer(data, prob.struct, offset)

  }
)


