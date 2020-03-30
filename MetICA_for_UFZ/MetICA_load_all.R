## Loading all necessary packages and files before running the program.

library(fastICA)
library(MASS)
library(e1071)
library(HiClimR)
library(bioDist)
library(bigdist)
library(disto)

source('MetICA_precalc.R')
source('MetICA_fastICA.R')
source('MetICA_simulated_generator.R')
source('MetICA_source_generator.R')
source('MetICA_cluster_generator.R')
source('MetICA_bootstrap.R')
source('MetICA_consistent.R')
source('MetICA_cluster_utility.R')
source('bigdist_subset.R')
source('dapply.R')

t=as.numeric(Sys.time())
set.seed((t - floor(t))*1e8->seed)
