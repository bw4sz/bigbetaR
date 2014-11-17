#' Phylogenetic diversity between cells
#' @description Computes mean nearest neighbor distance for a pair of cells
#' @seealso picante package for mulitple methods (phylosor etc.) picante::cophenetic
#' @param comm a community matrix with sites as rows and species as columns 
#' @param coph a species by species cophenetic matrix

#' @export 

#' 
taxF<-function(comm){
  d<-betapart.core(comm)
  bsim<-d$min.not.shared/(d$min.not.shared + d$shared)
  bsim[2]}
