
prob_mtx <- to_mtx(data_mapping)
prob_mtx <- prob_mtx[order(length(prob_mtx$cond)), ]

legit <- TRUE
for (row in prob_mtx){
	if (next_row_exists && legit){
		if(next_row$cond != c(row$marg, row$cond)){
			legit <- FALSE
		}
	}

	legit <- !intercect(row$marg, row$cond)
}

assert(legit)