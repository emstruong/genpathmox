#' @title Print function for the Pathmox Segmentation Trees: PLS-PM
#' 
#' @description
#' The function \code{print.xtree.pls} print the \code{pls.pathmox} tree
#' 
#' @param x An object of class \code{"xtree.pls"}.
#' @param \dots Further arguments are ignored.
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
#' @references Sanchez, G. (2009) \emph{PATHMOX Approach: Segmentation Trees in
#' Partial Least Squares Path Modeling.} PhD Dissertation. 
#' 
#' @references Tenenhaus M., Esposito Vinzi V., Chatelin Y.M., and Lauro C.
#' (2005) PLS path modeling. \emph{Computational Statistics & Data Analysis},
#' \bold{48}, pp. 159-205.
#'
#'
#' \code{\link{summary.xtree.pls}}.
#' @method print xtree.pls
#' @S3method print xtree.pls
#' @examples
#'
#'\dontrun{
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
#'  # apply plspm
#'  pls.fib = plspm(data.fib, inner.fib, outer.fib, modes.fib)
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
#'  fib.pathmox=pls.pathmox(pls.fib,seg.fib,signif=0.05,
#'					deep=2,size=0.2,n.node=20)
#'  
#'  print(fib.pathmox)
#'  }
#'
#'  library(genpathmox)
#'  data(fibtele)
#'  
#'  # select manifest variables
#'  data.fib <-fibtele[1:50,12:35]
#'  
#'  # define inner model matrix
#'  Image       = rep(0,5)
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
#'  # apply plspm
#'  pls.fib = plspm(data.fib, inner.fib, outer.fib, modes.fib)
#'                  
#'
#'  # re-ordering those segmentation variables with ordinal scale 
#'  seg.fib = fibtele[1:50,c(2,7)]
#'	seg.fib$Salary = factor(seg.fib$Salary, 
#'			levels=c("<18k","25k","35k","45k",">45k"), ordered=TRUE)
#'
#'  # Pathmox Analysis
#' fib.pathmox = pls.pathmox(pls.fib,seg.fib,signif=0.5,
#'					deep=1,size=0.01,n.node=10)
#'
#' print(fib.pathmox)
#'
print.xtree.pls <- function(x, ...)
{
	cat("\n") 
	print(x$MOX)
	 	
}
