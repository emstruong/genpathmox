
#' @title PLS-PM results of terminal nodes from the Pathmox Segmentation Trees
#' 
#' @description
#' Calculates basic PLS-PM results for the terminal nodes of PATHMOX
#' trees
#' 
#' @details
#' The argument \code{xtree} is an object of class \code{"xtree.pls"} returned by 
#' \code{\link{pls.pathmox}}.
#'
#' @param xtree An object of class \code{"xtree.pls"} returned by
#' \code{\link{pls.pathmox}}.
#' @param alpha is numeric value indicating the significance threshold of the invariance test
#' @param terminal is string, if equal to \code{TRUE}, just the terminal nodes are considered 
#' for the output reults. when it is equal to \code{FALSE},the PLS-PM results are generated 
#' for all nodes of the tree
#' @param scaled to standardize the latent variables or not
#' @param label is a string. It is false for defect. If it is \code{TRUE}, label.nodes has to be fix. 
#' @param label.nodes is a vector with the name of the nodes. It is null for defect. 
#' @param \dots Further arguments passed on to \code{\link{pls.treemodel}}. 
#' @return An object of class \code{"treemodel.pls"}. Basically a list with the
#' following results:
#' @return \item{inner}{Matrix of the inner relationship between latent variables of the PLS-PM model}
#' @return \item{invariance.test}{A data frame containing the results of the invariance test}
#' @return \item{weights}{Matrix of outer weights for each terminal node}
#' @return \item{loadings}{Matrix of loadings for each terminal node}
#' @return \item{paths}{Matrix of path coefficients for each terminal node}
#' @return \item{r2}{Matrix of r-squared coefficients for each terminal node}
#' @return \item{sign}{list of matrix with the significance for each terminal node}
#' @return \item{total_effects}{list of matrix with the terminal effects for each terminal node}
#'
#' @author Giuseppe Lamberti
#' 
#' @references Lamberti, G. et al. (2016) \emph{The Pathmox approach for PLS path modeling segmentation}. 
#' Applied Stochastic Models in Business and Industry; doi: 10.1002/asmb.2168; 
#'
#' @references Aluja, T., Lamberti, G., Sanchez, G. (2013). Extending the PATHMOX approach 
#' to detect which constructs differentiate segments. In   H., Abdi,  W. W., Chin,  V., Esposito Vinzi, 
#' G., Russolillo,  and   L., Trinchera (Eds.), Book title: New Perspectives in Partial Least 
#' Squares and Related Methods (pp.269-280). Springer.
#'               
#' @references Lamberti, G. (2014) \emph{Modeling with Heterogeneity.} PhD Dissertation.
#'
#' 
#' 
#' @seealso \code{\link{pls.pathmox}}
#' @export
#' @examples
#'  \dontrun{
#'  ## example of PLS-PM in alumni satisfaction
#'  
#'  data(fibtele)
#'  
#'  # select manifest variables
#'  data.fib <-fibtele[,12:35]
#'  
#'  # define inner model matrix
#'  Image   		= rep(0,5)
#'	Qual.spec	  = rep(0,5)
#'	Qual.gen		= rep(0,5)
#'	Value			  = c(1,1,1,0,0)
#'	Satis			  = c(1,1,1,1,0)
#'  inner.fib = rbind(Image,Qual.spec, Qual.gen, Value, Satis)
#'  colnames(inner.fib) = rownames(inner.fib)
#'  
#'  # blocks of indicators (outer model)
#'  outer.fib  = list(1:8,9:11,12:16,17:20,21:24)
#'  modes.fib  = rep("A", 5)
#'  
#'                  
#'  # re-ordering those segmentation variables with ordinal scale 
#'   seg.fib= fibtele[,2:11]
#'  
#'	 seg.fib$Age = factor(seg.fib$Age, ordered=T)
#'	 seg.fib$Salary = factor(seg.fib$Salary, 
#'			levels=c("<18k","25k","35k","45k",">45k"), ordered=T)
#'	 seg.fib$Accgrade = factor(seg.fib$Accgrade, 
#'			levels=c("accnote<7","7-8accnote","accnote>8"), ordered=T)
#'	 seg.fib$Grade = factor(seg.fib$Grade, 
#'	    levels=c("<6.5note","6.5-7note","7-7.5note",">7.5note"), ordered=T)
#'
#'  # Pathmox Analysis
#'  fib.pathmox=pls.pathmox(data.fib, inner.fib, outer.fib, modes.fib,SVAR=seg.fib,signif=0.05,
#'					deep=2,size=0.2,n.node=20)
#' 
#'  fib.comp=pls.treemodel(fib.pathmox)
#'  
#'  }
#'
#'  library(genpathmox)
#'  data(fibtele)
#'  
#'  # select manifest variables
#'  data.fib <-fibtele[1:50,12:35]
#'  
#'  # define inner model matrix
#'  Image     	= rep(0,5)
#'	Qual.spec		= rep(0,5)
#'	Qual.gen		= rep(0,5)
#'	Value			  = c(1,1,1,0,0)
#'	Satis			  = c(1,1,1,1,0)
#'  inner.fib = rbind(Image,Qual.spec, Qual.gen, Value, Satis)
#'  colnames(inner.fib) = rownames(inner.fib)
#' 
#'  # blocks of indicators (outer model)
#'  outer.fib = list(1:8,9:11,12:16,17:20,21:24)
#'  modes.fib = rep("A", 5)
#'  
#'
#'  # re-ordering those segmentation variables with ordinal scale 
#'  seg.fib = fibtele[1:50,c(2,7)]
#'	seg.fib$Salary = factor(seg.fib$Salary, 
#'			levels=c("<18k","25k","35k","45k",">45k"), ordered=TRUE)
#'
#'  # Pathmox Analysis
#' fib.pathmox = pls.pathmox(data.fib, inner.fib, outer.fib, modes.fib,SVAR=seg.fib,signif=0.5,
#'					deep=1,size=0.01,n.node=10)
#'
#' fib.comp=pls.treemodel(fib.pathmox)
#'
pls.treemodel <- function (xtree,alpha=0.05,terminal=TRUE,scaled=FALSE, label=FALSE, label.nodes=NULL, ...)
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