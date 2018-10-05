#' New stat?
#'
#' @return a stat

StatConditionalProb <-
  ggproto("StatCount", Stat,
          required_aes = "x",
          default_aes = aes(y = stat(count), weight = 1),

          setup_params = function(data, params) {
            if (!is.null(data$y)) {
              stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
            }
            params
          },

          compute_group = function(self, data, scales, width = NULL) {
            # these conditional probs don't work like this
            positive <- df[df$cancer == 1, ]$test
            negative <- df[df$cancer == 0, ]$test + 2
            x <- c(positive, negative)
            # weight <- data$weight %||% rep(1, length(x))
            weight <- data$weight %||%
              c(rep(1/(nrow(df) / length(positive)), length(positive)), rep(1/(nrow(df) / length(negative)), length(negative)))
            width <- width %||% (resolution(x) * 0.9)

            count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
            count[3:4] <- count[3:4] - length(negative) * 2
            print(count)
            count[is.na(count)] <- 0

            data.frame(
              count = count,
              prop = count / sum(abs(count)),
              x = sort(unique(x)),
              width = width
            )
          }
  )
