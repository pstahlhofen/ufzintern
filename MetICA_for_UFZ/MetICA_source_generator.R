MetICA_source_generator<-function(X,tot_var,w.distribution,max_iter){
  
  # This function computes estimated sources from randomly-initialized fastICA algorithm
  # X: data matrix, must be n (nb of observations) * p (number of features), either centered or not
  # tot_var: minimal pourcentage of variance kept for ICA
  # w.distribution: type of distribution used for generation of random initial demixing matrix W, 'gaussian', 'uniform' or 'beta'
  # max_iter:number of simulations, set the highest possible based on computer memory
  
  ### Input max_iter, at least 50
  if (max_iter < 50)
  {message("'max_iter' is too small: reset to ", 50)
   max_iter=50}
  
  ### Iterations 
  type='parallel' # Type of fastICA for the initial half of simulations
  W_sum=c()  # Matrix storing all simulated mixing matrices
  A_sum=c() # Matrix storing the loading matrices
  ### Centering X and calculating the neccessary number of PCs (nbcomp) to keep at least tot_var variance
  precalc_res = precalc(X, tot_var)
  print(precalc_res$nbcomp)

  for (i in 1:max_iter){
    if (i>floor(max_iter/2)){type='deflation'} # Half as deflation, half as parallel
    print(paste0('Iteration:',i))
    wines.ica <- fastICA2(precalc_res$X, precalc_res$K, precalc_res$Xd, nbcomp=precalc_res$nbcomp, w.distribution=w.distribution, alg.typ = type, 
                          fun = "logcosh", alpha = 1, maxit = 300, tol = 1e-04)
    W_dmix=t(wines.ica$K%*%wines.ica$W) # Calculation of demixing matrix W
    W_sum=rbind(W_sum,W_dmix) # Storage of demixing matrix
    A_sum=rbind(A_sum,wines.ica$A) # Storage of loading matrix
  }
  
  source_list=X%*%t(W_sum) # Matrix storing estimated sources from all runs
  colnames(source_list) = paste0('IC', c(1:ncol(source_list)))
  
  ### Algorithm output: combined source matrix, demixing matrix, initial inputs, number of ICs
  
  print(paste('Source generation finished, number of components:', wines.ica$IC))
  source_list_file = paste('../Results/source_list_', wines.ica$IC, '.rda', sep='')
  save(source_list, file=source_list_file)
  print('Source list saved')
  
  return(list(S=source_list,W=W_sum,A=A_sum,nbcomp=wines.ica$IC))}
