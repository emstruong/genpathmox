
#' @title PLS-SEM results of a specific terminal node from the Pathmox Segmentation Trees
#' 
#' @description
#' Calculates basic PLS-SEM results for a specific terminal node of PATHMOX
#' trees
#' 
#' @details
#' The argument \code{xtree} is an object of class \code{"xtree.pls"} returned by 
#' \code{\link{pls.pathmox}}.
#'
#' @param xtree An object of class \code{"xtree.pls"} returned by
#' \code{\link{pls.pathmox}}.
#' @param node is numeric value indicating the node that we want to aanalyze
#' @param boot.val is string, if equal to \code{TRUE}, calculates the bootstrap intervals.
#' By default is equal to \code{TRUE}. 
#' @param br is the numebr of boostrap resempling. By default is equal to 500.
#' @return An object of class \code{"treemodel.pls"}. Basically a list with the
#' following results:
#' @return \item{Reliability_indexes_and_unidimensionality}{Classical reliabilitiy indices for PLS-SEM}
#' @return \item{Internal_consistency_and_R2}{AVE and R2 for PLS-SEM}
#' @return \item{loadings}{Outer model loadings}
#' @return \item{weights}{Outer model weights}
#' @return \item{discriminant_validity}{Discriminant validity - Fornell & Larcker criterion}
#' @return \item{path_coef}{Coefficients of the inner model}
#' @return \item{total_effects}{Total effects of the inner model}
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
#' treenode=treenode.pls(bank.pathmox,node=2,br=100)
#'  
#'  }
treenode.pls  	<- function (xtree, node, boot.val = TRUE, br = 500) 
{
  
  # =======================================================
  # checking arguments
  # =======================================================
  if (class(xtree) != "xtree.pls")
    stop("\nAn object of class 'xtree.pls' was expected")
  if (missing(node))
    stop("\nargument 'node' (number of node) is missing")
  if (mode(node)!="numeric" || length(node)!=1 || node<=1 || (node%%1)!=0)
    stop("\nInvalid number of 'node'. Must be an integer larger than 1")
  if (boot.val) {
    if (!is.null(br)) {
      if(!is_positive_integer(br) || length(br) != 1L || br < 10) {
        warning("Warning: Invalid argument 'br'. Default 'br=500' is used.")   
        br = 500
      } 
    } else
      br = 500
  }
  
  
  # =======================================================
  # inputs setting
  # =======================================================  
  
  x		      =	  xtree$model$data
  inner   	=	  as.matrix(xtree$model$inner)
  outer	  	=	  xtree$model$outer
  mode		  =	  xtree$model$mode
  scaling 	= 	xtree$model$scaling
  scaled		=	  xtree$model$scaled
  scheme		=  	xtree$model$scheme	
  MOX			  =   xtree$MOX
  nodes	    =	  xtree$nodes
  
  x.node		= 	x[unlist(nodes[node]),]
  
  pls=plspm(x.node,inner,outer,mode,scaling,scheme,scaled=scaled,boot.val = boot.val,br=br)
  
  cor.lat=round(cor(pls$scores),3)
  diag(cor.lat) = sqrt(pls$inner_summary[,5])
  disc_val<-round(cor.lat,3)
  disc_val[upper.tri(cor.lat)]<-""
  disc_val<-as.data.frame(disc_val)
  
  
  res=list(Reliability_indexes_and_unidimensionality =round(cbind(pls$unidim[,-(1:2)][,c(1:4)]),3), Internal_consistency_and_R2=round(pls$inner_summary[,c(2,5)],3),
           loading=round(pls$boot$loading,3), weights=round(pls$boot$weights,3),discriminant_validity=disc_val, path_coef=round(pls$boot$paths,3),
           total_effects=round(pls$boot$total,3) )
  
  
  
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("PLS-SEM RESULTS FOR PATHMOX TERMINAL NODE: ",node,"\n")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("\n")
  cat("\n")
  
  return(res)
  
  
}