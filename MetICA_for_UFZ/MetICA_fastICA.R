# The fastICA2 is a slightly modified version of fastICA http://cran.r-project.org/web/packages/fastICA/index.html
fastICA2<-function (X,nbcomp,alg.typ = c("parallel", "deflation"), fun = c("logcosh","exp"), 
                    w.distribution = c("uniform","gaussian","beta"), alpha = 1, method = c("R", "C"),
		    maxit = 200, tol = 1e-04,verbose = FALSE) 
{ 
	### Input alg.typ & fun & w.distribution, same as original fastICA
	method <- match.arg(method)
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
	  
  
	if(method == 'R') {
		prx <- prcomp(X,scale=F)
		D <- diag(c(1/prx$sdev)) 
		K <- D %*% t(prx$rotation)
		K <- matrix(K[1:nbcomp,],nbcomp,ncol(X)) 
		X=t(X)
		Xd <- K %*% X 
		 
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
	} else if (method == 'C') {
		n <- nrow(X)
		p <- ncol(X)
		a <- .C(fastICA:::icainc_JM, as.double(X), as.double(w.init), as.integer(p), 
			as.integer(n), as.integer(nbcomp), as.double(alpha), 
			as.integer(1), as.integer(FALSE), as.integer(1L + 
			(fun == "exp")), as.integer(maxit), as.double(tol), 
			as.integer(alg.typ != "parallel"), as.integer(FALSE), 
			X = double(p * n), K = double(nbcomp * p), W = double(nbcomp * 
			nbcomp), A = double(p * nbcomp), S = double(nbcomp * 
			n))
		X1 <- matrix(a$X, n, p)
		K <- matrix(a$K, p, nbcomp)
		W <- matrix(a$W, nbcomp, nbcomp)
		A <- matrix(a$A, nbcomp, p)
		S <- matrix(a$S, n, nbcomp)
		return(list(X = X1, K = K, W = W, A = A, S = S, IC=nbcomp))
	}
}
