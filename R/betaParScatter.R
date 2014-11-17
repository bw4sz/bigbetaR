#' Betadiversity metrics for list of indices
#' @description Computes taxonomic, phylogenetic and trait metrics for each pair of cells
#' @param toScatterMatrix a community matrix with sites as rows and species as columns 
#' @param toScatterIndex an 2 row by n columns index of rows in comm to subset
#' @param coph a species by species cophenetic matrix
#' @param traits a species by species cophenetic matrix

#' @export 

betaPar.scatter<-function(toScatterMatrix,toScatterIndex,coph,traits){
    
  print(paste("Number of within loop calls:", ncol(toScatterIndex)))
  
  #Within a chunk, loop through the indexes and compute betadiversity
  holder<-apply(toScatterIndex,2,function(x) {
    #get the comm row
    comm.d<-toScatterMatrix[c(x[1],x[2]),]
    out<-beta_all(comm=comm.d,traits=traits,coph=coph)
    return(out)
  }
  )
  
  #bind to a dataframe
  holder<-melt(holder)
  
  return(holder)
}