#' Mean nearest diversity within a distance matrix
#' @description Computes mean nearest neighbor distance for a pair of cells
#' @seealso picante package for mulitple methods (phylosor etc.) picante::cophenetic
#' @param A a community matrix with sites as rows and species as columns 
#' @param B a species by species cophenetic matrix
#' @param sp.lists a species list at each cell
#' @param dists a species by species matrix of distances

#' @export 

MNND <- function(A,B,sp.list,dists)
{
  
  Asp     <- sp.list[[A]]
  Bsp     <- sp.list[[B]]
  compmat <- dists[Asp,Bsp]
  Ann     <- apply(as.matrix(compmat),1,min)
  Bnn     <- apply(as.matrix(compmat),2,min)
  Dnn     <- mean(c(Ann, Bnn))
  #turn    <- min(c(mean(Ann),mean(Bnn)))
  #nest    <- Dnn - turn
  #res <- c(Dnn,turn,nest)
  res<-c(Dnn)
  #names(res) <- c("MNND","MNNDturn","MNNDnest")
  names(res) <- c("MNND")
  
  res
}