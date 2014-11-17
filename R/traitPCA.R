#' Calculate trait space
#' @description Computes trait loadings for species in each assemblage
#' @seealso MNTDt
#' @param comm a community matrix with sites as rows and species as columns 
#' @param traits a species by trait matrix of trait values

#' @export 
#`


traitPCA<-function(comm,traits){
  #subset the data to find intersection of species
  #Trait frame needs to match siteXSpp table
  mon_cut<-traits[rownames(traits) %in% colnames(comm),]

  #species without traits, take them out for just this portion of the analysis, keep the assemblage lsit
  siteXspp_traits<-comm[,colnames(comm) %in% rownames(mon_cut)]

#Data Checks
  if(!is.data.frame(siteXspp_traits)){
  melt.MNTD<-data.frame(MNTD=NA,To=rownames(comm)[1],From=rownames(comm)[2])
  return(melt.MNTD)
  }

  if( nrow(mon_cut)<2){
  melt.MNTD<-data.frame(MNTD=NA,To=rownames(comm)[1],From=rownames(comm)[2])
  return(melt.MNTD)
  }

#if variance of trait column is 0, remove trait
  colvar<-!apply(mon_cut,2,var)==0
  mon_cut<-mon_cut[,colvar]
  if( ncol(mon_cut)==0){
  melt.MNTD<-data.frame(MNTD=NA,To=rownames(comm)[1],From=rownames(comm)[2])
  return(melt.MNTD)
  }
  prc_traits<-stats::prcomp(mon_cut,scale=TRUE)
  newSGdist <- dist(prc_traits$x)

  dists <- as.matrix(newSGdist)

  rownames(dists) <- rownames(mon_cut)
  colnames(dists) <- rownames(mon_cut)

return(dists)}