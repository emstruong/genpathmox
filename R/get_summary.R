#' @title Summary function for Pathmox Segmentation Trees
#' 
#' @description
#' The function \code{summary.plstrees} returns the most important results obtained 
#' by the function \code{pls.pathmox}. In order, it provides the parameters of the algorithm 
#' (threshold significance,  node size limit,  tree depth level and the method used for the 
#' split partition), the basic characteristics of the tree (depth and number of terminal 
#' nodes), the split results (F-global and F-coefficient). It also returns a ranking of the
#' categorical variables by importance and the terminal node results (path coefficients and R^2).
#' 
#' @param object An object of class \code{"plstree"}.
#' @param \dots Further arguments are ignored.
#'
#' @author Giuseppe Lamberti
#'  
#' @references Lamberti, G. (2021). Hybrid multigroup partial least squares structural equation
#'  modelling: an application to bank employee satisfaction and loyalty. \emph{Quality and Quantity},
#'  doi: 10.1007/s11135-021-01096-9.  
#'
#' @references Lamberti, G. et al. (2017). The Pathmox approach for PLS path modeling: discovering 
#' which constructs differentiate segments.\emph{Applied Stochastic Models in Business and Industry}, 
#' doi: 10.1002/asmb.2270.
#' 
#' @references Lamberti, G. et al. (2016). The Pathmox approach for PLS path modeling segmentation. 
#' \emph{Applied Stochastic Models in Business and Industry}, doi: 10.1002/asmb.2168. 
#'               
#' @references Lamberti, G. (2015). \emph{Modeling with Heterogeneity}, PhD Dissertation.
#'
#' @seealso \code{\link{print.plstree}}, \code{\link{pls.pathmox}},  
#' \code{\link{bar_terminal}}, \code{\link{bar_impvar}} and \code{\link{plot.plstree}}
#' 
#' @exportS3Method summary plstree
#' @examples
#'  \dontrun{
#' # Example of PATHMOX approach in customer satisfaction analysis 
#' # (Spanish financial company).
#' # Model with 5 LVs (4 reflective: Image (IMAG), Value (VAL), 
#' # Satisfaction (SAT), and Loyalty (LOY); and 1 formative construct: 
#' # Quality (QUAL))
#' 
#' # load library and dataset csibank
#' library(genpathmx)
#' data("csibank")
#' 
#' # Define the model using the laavan syntax. Use a set of regression formulas to define 
#' # first the structural model and then the measurement model
#'
#' CSImodel <- "
#' # Structural model
#' VAL  ~ QUAL
#' SAT  ~ IMAG  + QUAL + VAL
#' LOY  ~ IMAG + SAT
#'
#' # Measurement model
#' # Formative
#' QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5 + qual6 + qual7 
#'      
#' # Reflective
#' IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5 + imag6 
#' VAL  <~ val1  + val2  + val3  + val4
#' SAT  =~ sat1  + sat2  + sat3           
#' LOY  =~ loy1  + loy2  + loy3          
#'
#' "
#'
#' # Run pathmox on one single variable
#' age = csibank[,2]
#' 
#' # Transform age into an ordered factor
#' age = factor(age, levels=c("<=25", "26-35", "36-45", "46-55",
#'                                       "56-65", ">=66"),ordered=T)
#'                                       
#' csi.pathmox.age = pls.pathmox(
#'  .model = CSImodel ,
#'  .data  = csibank,
#'  .catvar= age,
#'  .signif = 0.05,
#'  .deep=1
#' )  
#' 
#' # Visualize the Pathmox results
#' summary(csi.pathmox.age)
#'
#' }
#'
summary.plstree <- function(object, ...){
  info=object$other$par_mode
  
  cat("\n")
  cat("PLS-SEM PATHMOX ANALYSIS","\n")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("Info parameters algorithm:","\n")
  info.value = rbind(info[[1]],info[[2]],info[[3]])
  dimnames(info.value) = NULL
  info.name = c("threshold signif","node size limit(%)","tree depth level")
  info.tree = data.frame(info.name,info.value)
  names(info.tree) = c("parameters algorithm", "value")
  print(info.tree)
  cat("---------------------------------------------")
  cat("\n")
  cat("Info tree:","\n")
  tree = object$MOX
  info.value = rbind(max(tree[,3]),sum(length(which(tree[,5]=="yes"))))
  dimnames(info.value) = NULL
  info.name = c("deep tree","number terminal nodes")
  info.tree = data.frame(info.name,info.value)
  names(info.tree) = c("parameters tree", "value")
  print(info.tree)
  cat("---------------------------------------------")
  cat("\n")
  cat("Info nodes:","\n")
  print(tree)
  cat("---------------------------------------------")
  cat("\n")
  cat("Info splits:","\n")
  cat("\n")
  cat("Variable:","\n")
  print(object$Fg.r[,c(1,4,5,6)])
  cat("\n")
  cat("Info F-global test results (global differerences):","\n")
  printCoefmat(object$Fg.r[,c(1,2,3)], P.values=TRUE, has.Pvalue=TRUE)
  cat("\n")
  cat("Info F-coefficient test results (coefficents differerences) :","\n")
  for (i in 1:length(object$Fc.r))
  {
    cat("\n")
    cat(paste("Node",substr(names(object$Fc.r)[i],5,5),":"))
    cat("\n")
    stats::printCoefmat(object$Fc.r[[i]], P.values=TRUE, has.Pvalue=TRUE)
  }
  cat("---------------------------------------------")
  cat("\n")
  cat("Info variable importance ranking:","\n")
  print(object$var_imp)
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("Info terminal nodes PLS-SEM models (path coeff. & R^2):","\n")
  print(object$terminal_paths)
  cat("\n")
}