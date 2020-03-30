MetICA_cluster_generator<-function(S,type_correlation=c('pearson','spearman'),nbcomp){
  
  # This function computes a dissimilarity matrix and uses MetICA_cluster_utility to determine the optimal number of clusters and to calculate centrotypes accordingly.
  # S: estimated source matrices from MetICA_source_generator, n estimations = n coloums
  # type_correlation: type of correlation, 'pearson' or 'spearman' 
  # nbcomp: number of independent components calculated in each run by the MetICA_source_generator
  
  type_correlation <- match.arg(type_correlation)
  if (nbcomp<2)
  {message("'nbcomp' is too small: reset to ", 2)
   nbcomp=2}
  
  if (type_correlation=='pearson')
	  D = 1 - abs(as.dist(fastCor(S, upperTri=TRUE, nSplit=4)))
  else if (type_correlation=='spearman')
	  D = spearman.dist(t(S))
  print('Computation of distance matrix completed')
  
  res = cluster_and_evaluate(D, 2, nbcomp)
  print(paste('Difference between nbcomp and nbclust:', nbcomp - res$nbclust))

  eval_file = paste('../Results/nbclust_eval_', nbcomp, '_', res$nbclust, '.csv', sep='')
  write.table(res$eval, file=eval_file, sep=',', col.names=T, row.names=F)
  centers = S[,res$center_IDs]
  center_file = paste('../Results/center_', res$nbclust, '.rda', sep='')
  save(centers, file=center_file)
  
  return(list(S=S,centers=centers,nbclust=res$nbclust))
}
