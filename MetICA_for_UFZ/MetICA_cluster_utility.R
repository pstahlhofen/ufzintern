center_IDs<-function(cluster_labels, D, nbclust) {
	IDs = c()
	for (i in seq_len(nbclust)) {
		cl = which(cluster_labels==i)
		cl_D = bigdist_subset(D, index=cl, file=paste('Dists/subdist', i, nbclust, sep='_'))
		cl_D_as_disto = disto(objectname='cl_D', object=cl_D, backend='bigdist')
		new_ID = cl[which.min(dapply(cl_D_as_disto, 1, sum))]
		IDs = c(IDs, new_ID)
	}
	names(IDs) = paste0('C', cluster_labels[IDs]) #naming the center_IDs according to their cluster
	return(IDs)
}

silhouette<-function(cluster_labels, D, center_IDs) {
	dists_to_centers = bigdist_extract(D, i=center_IDs, ) # k=1:ncol(D)
	rownames(dists_to_centers) = names(center_IDs) #After bigdist_extract, this must be set manually again
	silhouettes = c()
	for (point in seq_along(cluster_labels)) {
		own_center_ID_name = paste0('C', cluster_labels[point])
		dist_to_own = dists_to_centers[own_center_ID_name, point]
		dists_to_others = dists_to_centers[!rownames(dists_to_centers) %in% own_center_ID_name, ]
		dists_to_others = matrix(dists_to_others, nrow=nrow(dists_to_centers)-1) #To redo automated vector casting
		dist_to_second = min(dists_to_others[,point])
		sil = (dist_to_second - dist_to_own) / max(dist_to_own, dist_to_second)
		silhouettes = c(silhouettes, sil)
	}
	#NaN values occurr only if dist_to_own = dist_to_second = 0,
	#in which case the uncertainty of the correct cluster for the point is maximal.
	#For that reason, I replace the corresponding silhouettes by zeros
	silhouettes[is.nan(silhouettes)] = 0
	return(silhouettes)
}

cluster_and_evaluate<-function(D, k_min, k_max) {
	### This function determines the optimal number of clusters using silhouette evaluation and computes centrotypes for the resulting optimal clustering
	# D: dissimilarity matrix
	# k_min: minimal number of clusters (should be at least 2)
	# k_max: maximal number of clusters (k_min <= k_max < npoints)
	if (k_min > k_max) {
		message("'k_min' must be lower or equal to 'k_max'. Setting k_min = k_max")
		k_min = k_max
	}
	if (k_min < 2) {
		message("'k_min' must be greater or equal to 2. Setting k_min = 2")
		k_min = 2
	}
	n = attributes(D)$Size
	if (k_max >= n) {
		message("'k_max' must be smaller than the number of points. Setting k_max = npoints - 1")
		k_max = n-1
	}
	nbclust_candidates = c(k_min:k_max)
	silhouettes = c()
	center_summary = list()
	dendrogram = hclust(D, method='average')
	D = as_bigdist(D, file=paste('Dists/maindist', k_max, sep='_'))
	for (candidate in nbclust_candidates) {
		cluster_labels = cutree(dendrogram, candidate)
		c_IDs = center_IDs(cluster_labels, D, candidate)
		print(paste('Computed centerIDs for', candidate, 'clusters'))
		center_summary[[candidate]] = c_IDs
		silhouettes = c(silhouettes, mean(silhouette(cluster_labels, D, c_IDs)))
		print(paste('Computed average silhouette for', candidate, 'clusters'))
	}
	nbclust = nbclust_candidates[which.max(silhouettes)]
	eval = cbind(nbclust_candidates, silhouettes)
	return(list(eval=eval, nbclust=nbclust, center_IDs=center_summary[[nbclust]]))
}
