pprint <- function(aes_mtx){
  print(paste(c("marg", "cond", "aes"), sep = "\t"))
  for (i in seq_len(nrow(aes_mtx))){
    for (j in seq_len(ncol(aes_mtx))){
      for (k in seq_len(length(aes_mtx[i,j]))){
        cat(paste(aes_mtx[i,j][[k]]))
        cat("\t")
      }
    }
    cat("\n")
  }
}
