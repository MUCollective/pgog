context("test-x_height")
data(cancer)



disp_x_height <- cancer %>%
  ggplot() +
  geom_icon(aes(x = factor(test),
                height = P(cancer),
                color = cancer,
                size = 10
  ), position = "array")

vdiffr::expect_doppelganger("x = A, height = P(A), array", disp_x_height)

