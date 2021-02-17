
#' @title PLS-SEM results of terminal nodes from the Pathmox Segmentation Trees
#' 
#' @description
#' Calculates basic PLS-SEM results for the terminal nodes of PATHMOX
#' trees
#' 
#' @details
#' The argument \code{xtree} is an object of class \code{"xtree.pls"} returned by 
#' \code{\link{pls.pathmox}}.
#'
#' @param xtree An object of class \code{"xtree.pls"} returned by
#' \code{\link{pls.pathmox}}.
#' @param terminal is string, if equal to \code{TRUE}, just the terminal nodes are considered 
#' for the output reults. when it is equal to \code{FALSE},the PLS-PM results are generated 
#' for all nodes of the tree
#' @param scaled to standardize the latent variables or not
#' @param label is a string. It is false for defect. If it is \code{TRUE}, label.nodes has to be fix. 
#' @param label.nodes is a vector with the name of the nodes. It is null for defect. 
#' @param \dots Further arguments passed on to \code{\link{pls.treemodel}}. 
#' @return An object of class \code{"treemodel.pls"}. Basically a list with the
#' following results:
#' @return \item{weights}{Matrix of outer weights for each terminal node}
#' @return \item{loadings}{Matrix of loadings for each terminal node}
#' @return \item{path_coef}{Matrix of path coefficients for each terminal node}
#' @return \item{path_sgnificance}{Matrix of  path coefficients the significance (p-value) for each terminal node}
#' @return \item{predictive_power_R2}{Matrix of r-squared coefficients for each terminal node}
#' @return \item{total_effects}{list of matrix with the terminal effects for each terminal node}
#'
#' @author Giuseppe Lamberti
#' 
#' @references Lamberti, G. (2021) \emph{Hybrid multigroup partial least squares structural equation modelling: 
#' an application to bank employee satisfaction and loyalty}. 
#' Quality and Quantity; doi: 10.1007/s11135-021-01096-9;  
#'
#' @references Lamberti, G. et al. (2017) \emph{The Pathmox approach for PLS path modeling: 
#' Discovering which constructs differentiate segments.}. 
#' Applied Stochastic Models in Business and Industry; doi: 10.1002/asmb.2270; 
#' 
#' @references Lamberti, G. et al. (2016) \emph{The Pathmox approach for PLS path modeling segmentation}. 
#' Applied Stochastic Models in Business and Industry; doi: 10.1002/asmb.2168;  
#'               
#' @references Lamberti, G. (2015) \emph{Modeling with Heterogeneity.} PhD Dissertation.
#'
#' 
#' 
#' @seealso \code{\link{pls.pathmox}}
#' @export
#' @examples
#' \dontrun{
#'  ## example of PLS-PM in bank customer satisfaction
#'  
#' data(csibank)
#' 
#' # select manifest variables
#' data.bank <-csibank[,6:32]
#' 
#' # define inner model matrix
#' Image 			  = rep(0,6)
#' Expectation	  = c(1,0,0,0,0,0)
#' Quality		    = c(0,1,0,0,0,0)
#' Value			    = c(0,1,1,0,0,0)
#' Satis			    = c(1,1,1,1,0,0)
#' Loyalty       = c(1,0,0,0,1,0)
#' inner.bank = rbind(Image,Expectation, Quality, Value, Satis,Loyalty)
#' colnames(inner.bank) = rownames(inner.bank)
#' 
#' # blocks of indicators (outer model)
#' outer.bank  = list(1:6,7:10,11:17,18:21,22:24,25:27)
#' modes.bank = rep("A", 6)
#' 
#' 
#' # re-ordering those segmentation variables with ordinal scale 
#' seg.bank= csibank[,1:5]
#' 
#' seg.bank$Age = factor(seg.bank$Age, ordered=TRUE)
#' seg.bank$Education = factor(seg.bank$Education, ordered=TRUE)
#' 
#' 
#' # Pathmox Analysis
#' bank.pathmox=pls.pathmox(data.bank, inner.bank, outer.bank, modes.bank,SVAR=seg.bank,signif=0.05,
#'                          deep=2,size=0.2,n.node=20)
#'
#' nodes.models=pls.treemodel(bank.pathmox)
#'  
#'  }
#'
pls.treemodel <- function (xtree,terminal=TRUE,scaled=FALSE, label=FALSE, label.nodes=NULL, ...)
{
  
  if (class(xtree) != "xtree.pls") 
    stop("Argument 'xtree' must be an object of class 'xtree.pls'")
  
  x		      =	  xtree$model$data
  
  inner   	=	  as.matrix(xtree$model$inner)
  outer	  	=	  xtree$model$outer
  mode		  =	  xtree$model$mode
  scaling 	= 	xtree$model$scaling
  scaled		=	  xtree$model$scaled
  scheme		=  	xtree$model$scheme	
  MOX			  =   xtree$MOX
  
  
  if(terminal==TRUE)	{nodes	=	xtree$terminal}
  if(terminal==FALSE) {nodes	=	xtree$nodes}
  
  pls.global=plspm(x,inner,outer,mode,scaling,scheme,scaled=scaled)
  latent		=  	pls.global$scores
  
  
  lvs			=	ncol(inner)
  lvs.names	=	row.names(inner)
  path.labs	=	NULL
  for (j in 1:lvs) for (i in j:lvs) if (inner[i, j] == 1) path.labs <- c(path.labs, paste(lvs.names[j], "->", lvs.names[i],sep = ""))
  
  if(terminal==TRUE) 
  {
    term.nodes <- which(MOX$Terminal == "yes") - 1
    tn.labs <- paste("Node", MOX$Node[term.nodes + 1], sep = "_")
    name.node=c("Root_Node", tn.labs)
    if(label==TRUE){name.node= label.nodes} 
  }
  if(terminal==FALSE) 
  {
    term.nodes <- which(MOX$Depth> 0) - 1
    tn.labs <- paste("Node", MOX$Node[term.nodes + 1], sep = "_")
    name.node=c("Root_Node", tn.labs)
    if(label==TRUE){name.node= label.nodes} 
    
  }
  
  weights=NULL
  loadings=NULL
  paths=NULL
  effect=NULL
  r2=NULL
  total = NULL
  significo=list()
  
  for (i in 1 : length(nodes))
  {
    x.node	= 	x[nodes[[i]],]
    
    pls.node=plspm(x.node,inner,outer,mode,scaling,scheme,scaled=scaled)	
    
    weights		=	round(cbind(weights,pls.node$outer_model[,3]),3)
    loadings	=	round(cbind(loadings,pls.node$outer_model[,4]),3)
    paths		  =	round(cbind(paths,pls.node$path_coefs[pls.node$path_coefs!=0]),3)
    effect		=	round(cbind(effect,pls.node$effects[,4]),3)
    rownames(effect)=pls.node$effects[,1]
    r2			  =	round(cbind(r2,pls.node$inner_summary$R2),3)
    total= round(cbind(total,pls.node$effects[,4]),3)
    
    signific=NULL
    signific=list()
    for(k in 1:length(pls.node$inner_mode))	{signific[[length(signific)+1]]=round(cbind(pls.node$inner_model[[k]][,4]),3)}
    significo[[length(significo)+1]]=signific
    
  }
  
  p2=list()
  for(j in 1:length(pls.node$inner_mode)){
    p1=NULL
    for(i in 1:length(nodes)){
      p1=cbind(p1,significo[[i]][[j]])
    }
    colnames(p1)=name.node
    p2[[length(p2)+1]]=p1
  }
  names(p2)=names(pls.node$inner_model)
  
  
  colnames(weights)=colnames(loadings)=colnames(paths)=colnames(r2)=colnames(effect)= colnames(total) = name.node
  rownames(paths)=path.labs
  rownames(weights)=pls.node$outer_model[,1]
  rownames(loadings)=pls.node$outer_model[,1]
  rownames(r2)=rownames(pls.node$inner_summary)
  rownames(total)=pls.global$effects[,1]
  
  
  res=list(weights=weights,loadings=loadings,path_coef=paths,path_sgnificance=p2, predictive_power_R2=r2, total_effects=total)
  class(res)="treemodel"
  res
}
