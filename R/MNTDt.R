#' Phylogenetic diversity between cells
#' @description Computes mean nearest neighbor distance for a pair of cells
#' @seealso picante package for mulitple methods (phylosor etc.) picante::cophenetic
#' @param comm a community matrix with sites as rows and species as columns 
#' @param coph a species by species cophenetic matrix
#' @param sp.lists a species list at each cell
#' @param nam a column name for the distance matrix, defaults to value

#' @export 


MNTDt<-function(comm,dists,sp.list,nam="value"){
    
  #walk through each pair of cells and compute phylo betadiversity using MNNTDt
  MNdist <- sapply(rownames(comm),function(i){
    
    #set iterator
    A<-i
    
    #
    out<-lapply(rownames(comm)[1:(which(rownames(comm) == i))], function(B) {MNND(A,B,sp.list=sp.list,dists=dists)})
    names(out)<-rownames(comm)[1:(which(rownames(comm) == i))]
    return(out)
  })
  
  MNdist<-melt(MNdist)
  colnames(MNdist)<-c(nam,"To","From")
  return(MNdist)
  
}