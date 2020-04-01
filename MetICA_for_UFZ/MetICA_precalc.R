precalc<-function(X, tot_var) {
	### This function scales a given data matrix X and computes the number of principal components needed to keep at least tot_var total variance.
	dd <- dim(X)
	d <- dd[dd != 1L]
	if (length(d) != 2L) 
		stop("data must be matrix-conformal")
	X <- if (length(d) != length(dd)) 
		matrix(X, d[1L], d[2L])
	else as.matrix(X)
	
	X <- scale(X, scale = FALSE) # Centering the dataset
	prx <- prcomp(X,scale=F)
	cum_var <- cumsum(prx$sdev/sum(prx$sdev)) # Cumulated pourcentage of variance
	if (tot_var > 1)
		{message("'tot_var' is too large: reset to ", 1)
		 tot_var=1} # Pourcentage of variance should be larger than 1
	if (tot_var < cum_var[3])
		{message("'tot_var' is smaller than variance of 3 PCs: reset to ", cum_var_3)
		 tot_var=cum_var[3]}
	nbcomp=which(cum_var>=tot_var)[1] # Taking nbcomp PCs will preserve at least tot_var of variance
	D <- diag(c(1/prx$sdev)) 
	K <- D %*% t(prx$rotation)
	K <- matrix(K[1:nbcomp,],nbcomp,ncol(X)) 
	X=t(X)
	Xd <- K %*% X 
	return(list(X=X, K=K, Xd=Xd, nbcomp=nbcomp))
}
