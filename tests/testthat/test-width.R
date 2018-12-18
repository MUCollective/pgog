context("Width aesthetics")
data(cancer)

# x = A, width = P(A)
disp_PA <- cancer %>%
  ggplot() +
  geom_icon(aes(width = P(test),
                color = cancer)) +
  coord_fixed()


# x = A, width = P(A|B)
disp_PAB <- cancer %>%
  ggplot() +
  geom_icon(aes(width = P(cancer | !test),
    color = cancer)) +
  coord_fixed()


vdiffr::expect_doppelganger("x = A, width = P(A)", disp_PA)
vdiffr::expect_doppelganger("x = A, width = P(A|B)", disp_PAB)


remove(cancer)
