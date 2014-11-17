#' Betadiversity metrics for list of indices
#' @description Computes taxonomic, phylogenetic and trait metrics for each pair of cells
#' @param toScatterMatrix a community matrix with sites as rows and species as columns 
#' @param toScatterIndex an 2 row by n columns index of rows in comm to subset
#' @param coph a species by species cophenetic matrix
#' @param traits a species by species cophenetic matrix

#' @export 

betaPar.scatter<-function(toScatterMatrix,toScatterIndex,coph,traits){
    
  print(paste("Number of within loop calls:", ncol(toScatterIndex)))
  
  #get species lists
  comm<-as.matrix(comm) #needs to be checked for rownames

  out<-beta_all(comm=comm.df[1:10,],traitdist,coph=coph)

  return(holder)
}