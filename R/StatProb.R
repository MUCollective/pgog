# # stat_prob
# StatProb<- ggplot2::ggproto("StatProb", ggplot2::Stat,
#
#                    setup_params = function(data, params) {
#                      params$n_data <- nrow(data)
#                      params
#                    },
#
#                    compute_group = function(data, scales, n_data) {
#                      ret <- data %>%
#                        count(x) %>%
#                        mutate(probability = n/n_data) %>%
#                        select(-n)
#                      print(ret)
#                      ret
#                    },
#
#                    required_aes = c("x"),
#                    default_aes = aes(y = stat(probability))
# )
#
#
# stat_prob <- function(mapping = NULL, data = NULL, geom = "bar",
#                        position = "identity", na.rm = FALSE, show.legend = NA,
#                        inherit.aes = TRUE, ...) {
#
#   # TODO: data needs to be transformed here
#
#   ggplot2::layer(
#     stat = StatProb, data = data, mapping = mapping, geom = geom,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }
#
#
#
#
