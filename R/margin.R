prop <- function(x) x / sum(x, na.rm = TRUE)

margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  #browser()
  marginals <- rev(marginals)
  conditionals <- rev(conditionals)

  marg <- weighted.table(table[c(conditionals, marginals)], table$.wt)

  if (length(conditionals) > 0) {
    # Work around bug in ninteraction
    cond <- marg[conditionals]
    cond[] <- lapply(cond, addNA, ifany = TRUE)
    marg$.wt <- stats::ave(marg$.wt, id(cond), FUN = prop)
  }


  marg$.wt[is.na(marg$.wt)] <- 0


  # add in N's
  vars <- table[c(conditionals, marginals)]
  vars[] <- lapply(vars, addNA, ifany = TRUE)
  # ACHTUNG

  if (".N" %in% colnames(table)){
    counts <- table$.N
  } else {
    counts <- rep(1, nrow(vars))
  }
  sums <- tapply(counts, rev(vars), sum, na.rm=TRUE)

  Ns <- as.data.frame.table(sums, responseName = ".N")
  marg$.N <- Ns$.N
  marg$.N[is.na(marg$.N)] <- 0

  marg
}

weighted.table <- function(vars, wt = NULL) {

  # If no weight column, give constant weight
  if (is.null(wt)) {
    wt <- prop(rep(1, nrow(vars)))
  }

  # Ensure missing values are counted
  vars[] <- lapply(vars, addNA, ifany = TRUE)

  # Need to reverse order of variables because as.data.frame works in the
  # opposite way to what I want
  sums <- tapply(wt, rev(vars), sum, na.rm = TRUE)

  df <- as.data.frame.table(sums, responseName = ".wt")


  # Missing values represent missing combinations in the original dataset,
  # i.e. they have zero weight
  df$.wt[is.na(df$.wt)] <- 0
  df[, c(rev(seq_len(ncol(df) - 1)), ncol(df)) ]

}

