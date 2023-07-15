
library(ggforce)
library(ggplot2)


StatElastic = ggproto('StatElastic', Stat,
                      setup_params = function(data, params) {
                        params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
                        params
                      },
                      setup_data = function(data, params) {
                        data$flipped_aes <- params$flipped_aes
                        data
                      },
                      compute_panel = function(data, scales, n = 100, strength = 0.5, flipped_aes = FALSE) {
                        if (empty_data(data)) return(data)
                        data <- flip_data(data, flipped_aes)
                        data$group <- make_unique(as.character(data$group))
                        end <- data
                        end$x <- end$xend
                        end$y <- end$yend
                        data <- vec_rbind(data, end)
                        data$xend <- NULL
                        data$yend <- NULL
                        data <- data[order(data$group), ]
                        data <- add_controls(data, strength)
                        data <- StatBezier$compute_panel(data, scales, n)
                        flip_data(data, flipped_aes)
                      },
                      required_aes = c('x', 'y', 'xend', 'yend'),
                      extra_params = c('na.rm', 'n', 'strength', 'orientation')
                      )

geom_elastic = function() {}
