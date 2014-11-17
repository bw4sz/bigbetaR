#' Taxonomic diversity between cells
#' @description Computes betasim for a pair of cells
#' @seealso beta.pairs in the betapart packages
#' @param comm a community matrix with sites as rows and species as columns 
#' @export 
#' 
taxF<-function(comm){
d<-betapart.core(comm)
bsim<-d$min.not.shared/(d$min.not.shared + d$shared)
bsim[2]}
