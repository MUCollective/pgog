GeomUncertaintyArea <- ggproto("GeomUncertaintyArea", Geom,
                               requred_aes = "x",

                               # calculate bounds (from prodplot)


                               # stuff from GeomRect sourcecode
                               draw_panel = function(self, data, panel_params, coord) {
                                 if (!coord$is_linear()) {
                                   aesthetics <- setdiff(
                                     names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
                                   )

                                   polys <- plyr::alply(data, 1, function(row) {
                                     poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                                     aes <- as.data.frame(row[aesthetics],
                                                          stringsAsFactors = FALSE)[rep(1,5), ]

                                     GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
                                   })

                                   ggname("bar", do.call("grobTree", polys))
                                 } else {
                                   coords <- coord$transform(data, panel_params)
                                   ggname("geom_rect", rectGrob(
                                     coords$xmin, coords$ymax,
                                     width = coords$xmax - coords$xmin,
                                     height = coords$ymax - coords$ymin,
                                     default.units = "native",
                                     just = c("left", "top"),
                                     gp = gpar(
                                       col = coords$colour,
                                       fill = alpha(coords$fill, coords$alpha),
                                       lwd = coords$size * .pt,
                                       lty = coords$linetype,
                                       lineend = "butt"
                                     )
                                   ))
                                 }
                               },

                               draw_key = draw_key_polygon
                               )




