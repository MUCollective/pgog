# context("test-x_y_h")
# data(cancer)
#
# plt_x_y_h <- ggplot(data=cancer) +
#   geom_icon(aes(x = factor(test),
#                 y = cancer,
#                 height = P(test),
#                 color = test,
#                 size = 10),
#             position = "array")
# vdiffr::expect_doppelganger("x = factor(A),y = B, height = P(A),", plt_x_y_h)
