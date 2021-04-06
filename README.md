![Build Status](https://github.com/mucollective/pgog/workflows/R-CMD-check/badge.svg)


# A Probabilistic Grammar of Graphics

<img src="https://xiaoyingpu.github.io/images/pgog-thumbnail.jpg" width="100">


This is an R package that accompanies our [CHI 2020 paper](https://osf.io/dy8qv/), _A Probabilistic Grammar of Graphics_ (PGoG). Here we implement the PGoG grammar as two `ggplot2` `geom`s and show that PGoG can be compatible with an existing grammar of graphics-based system. This package is intented as a prototype; not all valid PGoG specifications are supported.


```
devtools::install_github("MUCollective/pgog")
```

## More about the PGoG grammar
- [This vignette](https://github.com/MUCollective/pgog/blob/master/vignettes/pgog.Rmd) lists many PGoG specification examples.
- [Sketch(-y) proof of correctness](https://xiaoyingpu.github.io/note/correctness-proof/)

## PGoG paper
- [Paper pre-print on OSF](https://osf.io/dy8qv/)
- [Paper at ACM DL](https://dl.acm.org/doi/abs/10.1145/3313831.3376466)
- [Probabilistic visualization collection](https://mucollective.github.io/visualization/)

