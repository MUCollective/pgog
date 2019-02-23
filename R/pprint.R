pprint <- function(aes_mtx){


  m <- matrix(nrow = nrow(aes_mtx), ncol = ncol(aes_mtx))


  # cat("\n")
  # cat(paste(c("marg", "cond", "aes", "level"), collapse = "\t"))
  # cat("\n")

  for (i in seq_len(nrow(aes_mtx))){
    for (j in seq_len(ncol(aes_mtx))){
        m[i, j] <- paste(aes_mtx[i,j][[1]], collapse = ",")

        # cat("\t")
      # for (k in seq_len(length(aes_mtx[i,j]))){
        # cat(paste(aes_mtx[i,j][[k]]))
        # cat("\t")
      # }
    }
    # cat("\n")
  }
  colnames(m) <- c("marg", "cond", "aes", "level")
  print(data.frame(m))
}
