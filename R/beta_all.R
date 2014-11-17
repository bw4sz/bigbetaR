#' Mean nearest diversity within a distance matrix
#' @description Computes mean nearest neighbor distance for a pair of cells
#' @seealso picante package for mulitple methods (phylosor etc.) picante::cophenetic
#' @param comm a community matrix with sites as rows and species as columns 
#' @param coph a dataframe of trait values with species rownames
#' @param coph a species by species cophenetic matrix
#' @export 


beta_all<-function(comm,traits,coph){
  
  ##Taxonomic Betadiversity
  tax<-taxF(comm) 
  
  ######Phylogenetic Betadiversity using MNTDt
  phylo<-phyloMNTDt(comm,coph)
  
  #Merge with taxonomic
  trait<-traitF(comm,traits)
  
  #Combine with other metrics into one large dataframe
  Allmetrics<-list(Tax=tax,Phylo=phylo,Trait=trait)
  return(Allmetrics)}
