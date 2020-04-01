# The fastICA2 is a slightly modified version of fastICA http://cran.r-project.org/web/packages/fastICA/index.html
fastICA2<-function (X, K, Xd, nbcomp,alg.typ = c("parallel", "deflation"), fun = c("logcosh","exp"), 
                    w.distribution = c("uniform","gaussian","beta"), alpha = 1,
		    maxit = 200, tol = 1e-04,verbose = FALSE) 
{ 
	### Input alg.typ & fun & w.distribution, same as original fastICA
	alg.typ <- match.arg(alg.typ)
	fun <- match.arg(fun)
	w.distribution<- match.arg(w.distribution)
	
	### Input alpha, same as original fastICA
	if (alpha < 1 || alpha > 2) 
		stop("alpha must be in range [1,2]")
	
	### Generation of w.init by the defined distribution
	if (w.distribution=='uniform'){w.init=matrix(runif(nbcomp*nbcomp,0,nbcomp),nbcomp,nbcomp)}
	if (w.distribution=='gaussian'){w.init=matrix(rnorm(nbcomp*nbcomp),nbcomp,nbcomp)}
	if (w.distribution=='beta'){w.init=matrix(rbeta(nbcomp*nbcomp,1,nbcomp),nbcomp,nbcomp)}
	  
	### FastICA algorithm applied on the denoised matrix Xd, same as original fastICA
	a <- if (alg.typ == "deflation") 
		ica.R.def(Xd, nbcomp, tol = tol, fun = fun, alpha = alpha, 
			  maxit = maxit, verbose = verbose, w.init = w.init)
	else if (alg.typ == "parallel") 
		ica.R.par(Xd, nbcomp, tol = tol, fun = fun, alpha = alpha, 
			  maxit = maxit, verbose = verbose, w.init = w.init)
	 
	### Calculation of source and loading matrix & function output, same as original fastICA
	w <- a %*% K 
	S <- w %*% X # Source matrix S = a*K*X
	A <- t(w) %*% solve(w %*% t(w)) # Loading matrix is the pseudo inverse of matrix w
	### Output
	return(list(X = t(X), K = t(K), W = t(a), A = t(A), S = t(S), IC = nbcomp))
}
