StatProb<- ggproto("StatProb", Stat,
                   compute_group = function(data, scales) {
                     ret <- data %>%
                       count(x) %>%
                       mutate(prob = n/sum(n)) %>%
                       select(-n)

                     ret
                   },
                   required_aes = c("x")
)


stat_prob <- function(mapping = NULL, data = NULL, geom = "bar",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatProb, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
