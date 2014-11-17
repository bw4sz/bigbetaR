#' Betadiversity in taxonomic, phylogenetic and trait dimensions
#' @description Computes mean nearest neighbor distance for a pair of cells
#' @param comm a community matrix with sites as rows and species as columns 
#' @param coph a dataframe of trait values with species rownames
#' @param coph a species by species cophenetic matrix
#' @export 


beta_all<-function(comm,traitdist,coph){
  
  sp.list<-apply(comm,1,function(x){ 
    names(x[which(x==1)])
  })
  
  #taxonomic diversity
  tax<-taxF(comm)
  
  #name characters
  tax$To<-as.character(tax$To)
  tax$From<-as.character(tax$From)
  
  #phylogenetic
  phylo<-MNTDt(comm,coph,sp.list,nam="Phylo")
      
  #just use column names in the trait matrix
  comm.trait<-comm[,colnames(comm) %in% rownames(traits)]
  
  #trait species list
  sp.list.trait<-apply(comm.trait,1,function(x){ 
    names(x[which(x==1)])
  })
  
  #trait betadiversity
  trait<-MNTDt(comm=comm.trait,dists=traitdist,sp.list=sp.list.trait,nam="Trait")
  
  #merge together
  merge1<-merge(phylo,tax,by=c("To","From"))
  Allmetrics<-merge(merge1,trait,by=c("To","From"))
  
  #Combine with other metrics into one large dataframe
  return(Allmetrics)}
