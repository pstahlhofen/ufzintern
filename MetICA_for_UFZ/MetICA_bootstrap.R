MetICA_bootstrap_generator <- function(n,c,br){ 
  
  # This function create bootstrap indices
  # n: total number of samples, in data matrix the indices of samples should be 1:n
  # c: number of samples taken for bootstrapping
  # br: number of bootstrap replicates
  
  if (c>n/2)
  {message("'c' is too large: reset to half of the dataset")
   c=floor(n/2)}
  
  boot=1:n 
  bootstrap_history=list() # list storing history of all bootstrap replicates
  
  for (b in 1:br){
    boot_chosen=sample(boot,c) # Indices chosen to be replaced 
    boot_used=sample(setdiff(boot, boot_chosen),c) # Indices chosen to replace boot_chosen 
    boot_result=list(bc=boot_chosen,bu=boot_used)
    bootstrap_history[[b]]=boot_result}
  
  return(bootstrap_history)
}

MetICA_bootstrap<- function(X,c,cluster_center,nbcomp,w.distribution,br,max_iter){ 
  
  # This function evaluates the correlation between centrotypes tested and ICs obtained from bootstrapped data
  # It scores and orders each centrotypes based on how similar they are to bootstrapped simulations
  # X: data matrix, must be n (nb of observations) * p (number of features), either centered or not
  # c: number of samples taken for bootstrapping
  # cluster_center: matrix generated from MetICA_cluster_center, each column=one centrotype
  # The ids of centrotypes are given: OC_1,OC_2...
  # nbcomp: number of principal components used from the data = number of sources computet per run
  # w.distribution: type of distribution used for generation of random initial demixing matrix W, 'gaussian', 'uniform' or 'beta'
  # br: number of bootstrap replicates
  # max_iter:number of input-randomization
  
  n=nrow(X) # Number of bootstrap replicates
  nbc=ncol(cluster_center) # Number of centrotypes tested
  colnames(cluster_center)=paste0('OC_',1:nbc)
  history=MetICA_bootstrap_generator(n,c,br)
  IC_notes_summary=c()
  
  for (r in 1:max_iter){
    print(paste0('Randomized Iteration:',r))
    IC_notes=rep(0,nbc) # vector storing score 
    type='parallel'    
    for (b in 1:br){
      print(paste0('Bootstrapped X:',b))
      if (b>floor(br/2)) {type='deflation'} # Half half deflation parallel
      X_booted=X
      X_booted[history[[b]]$bc,]=X[history[[b]]$bu,] # Create bootstrapped dataset
      set.seed(r) # keep the W0 the same for all bootstrapped dataset
      wines.ica.boot <- fastICA2(X_booted, nbcomp=nbcomp, w.distribution=w.distribution, alg.typ = type, 
                                 fun = "logcosh", alpha = 1, maxit = 300, tol = 1e-04) 
      cor_center_boot=cor(cluster_center,wines.ica.boot$S,method='spearman') # correlation matrix between centrotypes and components simulated from bootstrapped dataset
      IC_notes=IC_notes+apply(abs(cor_center_boot),1,max)}
    IC_notes_summary=rbind(IC_notes_summary,IC_notes)}  
  
  print('Centrotype evaluation finished\n')
  
  # The output IC_notes_summary: each row presents the score of all centrotypes from br bootstrapped data
  # Each column presents the scores given to the same centrotype but with different algorithm inputs 
  colnames(IC_notes_summary)=colnames(cluster_center)
  nbclust = ncol(cluster_center)
  IC_notes_summary_file = paste('../Results/IC_notes_summary_', nbclust, '.csv', sep='')
  write.table(IC_notes_summary, file=IC_notes_summary_file, sep=',', row.names=F, col.names=T)
  IC_notes_summary0=IC_notes_summary
  
  significance_order=order(-apply(IC_notes_summary,2,median)) 
  IC_notes_summary=IC_notes_summary[,significance_order]
  IC_ranking_file = paste('../Results/IC_ranking_', nbclust, '.png', sep='')
  png(IC_ranking_file)
  boxplot(IC_notes_summary,ylab='H-Score',las=2) # Plot the H-scores in a decreased order 
  dev.off()
  return(list(score=IC_notes_summary0))
}
