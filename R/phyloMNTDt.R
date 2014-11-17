#' Phylogenetic diversity between cells
#' @description Computes mean nearest neighbor distance for a pair of cells
#' @seealso picante package for mulitple methods (phylosor etc.) picante::cophenetic
#' @param comm a community matrix with sites as rows and species as columns 
#' @param coph a species by species cophenetic matrix
#' @param sp.lists a species list at each cell

#' @export 


phyloMNTDt<-function(comm,coph,sp.lists){
    
  #walk through each pair of cells and compute phylo betadiversity using MNNTDt
  phyloS <- sapply(rownames(siteXspp_phylo),function(i){
    
    #set iterator
    A<-i
    
    #
    out<-lapply(rownames(siteXspp_phylo)[1:(which(rownames(siteXspp_phylo) == i))], function(B) {MNND(A,B,sp.list=sp.list,dists=coph_cut)})
    names(out)<-rownames(siteXspp_phylo)[1:(which(rownames(siteXspp_phylo) == i))]
    return(out)
  })
  
  return(phyloS[[2]][[1]])
}