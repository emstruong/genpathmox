#' @title Summary function for the Pathmox Segmentation Trees: PLS-PM
#' 
#' @description
#' The function \code{summary.xtree.pls} returns the most important results obtained 
#' by the function \code{pls.pathmox}. In order, it provides the parameters algorithm (
#' threshold significance, node size limit", tree depth level, and the method used for the 
#' split partition), the essential characteristics of the tree (deep and number of terminals 
#' nodes), the basic characteristics of the nodes and the F-global and the F-coefficient
#' results. For the test results, the significance level is also indicated.
#' 
#' @param x An object of class \code{"xtree.pls"}.
#' @param \dots Further arguments are ignored.
#'
#' @author Giuseppe Lamberti
#'  
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
#' \code{\link{pls.pathmox}}
#' @method summary xtree.pls
#' @S3method summary xtree.pls
#' @examples
#'  
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
#' summary(bank.pathmox)
#'  
#'  }
#'
#' library(genpathmox)
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
#' summary(bank.pathmox)
#'
summary.xtree.pls <- function(x, ...) 
{
  info=x$model
  
  cat("\n")
  cat("PLS-PM SEGMENTATION TREE","\n")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("Info Parameters Algorithm:","\n")
  info.value = rbind(info[[1]],info[[2]],info[[3]],info[[4]])
  dimnames(info.value) = NULL
  info.name = c("Threshold signif","Node size limit(%)","Tree depth level","Method")
  info.tree = data.frame(info.name,info.value)
  names(info.tree) = c("Parameters Algorithm", "value")
  print(info.tree)
  cat("---------------------------------------------")
  cat("\n")
  cat("Info Tree:","\n")
  tree = x$MOX
  info.value = rbind(max(tree[,3]),sum(length(which(tree[,5]=="yes"))))
  dimnames(info.value) = NULL
  info.name = c("Deep tree","Number terminal nodes")
  info.tree = data.frame(info.name,info.value)
  names(info.tree) = c("Parameters Tree", "value")
  print(info.tree)
  cat("---------------------------------------------")
  cat("\n")
  cat("Info nodes:","\n")
  print(tree)
  cat("---------------------------------------------")
  cat("\n")
  cat("Info Splits:","\n")
  cat("\n")
  cat("Variable:","\n")
  print(x$Fg.r[,c(1,4,5,6)])
  cat("\n")
  cat("F.statistic global:","\n")
  printCoefmat(x$Fg.r[,c(1,2,3)], P.values=TRUE, has.Pvalue=TRUE)
  cat("\n")
  cat("F.statistic coefficient:","\n")
  for (i in 1:length(x$Fc.r))
  {
    cat("\n")
    cat(paste("Node",substr(names(x$Fc.r)[i],5,5),":"))
    cat("\n")
    printCoefmat(x$Fc.r[[i]], P.values=TRUE, has.Pvalue=TRUE)
  }
}