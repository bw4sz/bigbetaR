#' Betadiversity metrics for list of indices
#' @description Computes taxonomic, phylogenetic and trait metrics for each pair of cells
#' @param toScatterMatrix a community matrix with sites as rows and species as columns 
#' @param toScatterIndex an 2 row by n columns index of rows in comm to subset
#' @param coph a species by species cophenetic matrix
#' @param traits a species by species cophenetic matrix

#' @export 

betaPar.scatter<-function(toScatterMatrix,toScatterIndex,coph,traitdist){
    
  print(paste("Number of within loop calls:", ncol(toScatterIndex)))
  
  #get species lists
  comm<-as.matrix(comm) #needs to be checked for rownames

  system.time(out<-beta_all(comm=toScatterMatrix,traitdist,coph=coph))

  return(holder)
}