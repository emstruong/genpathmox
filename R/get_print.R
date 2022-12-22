#' @title Print function for Pathmox Segmentation Trees
#' 
#' @description
#' The function \code{print.plstree} returns the \code{pls.pathmox} results.
#' 
#' @param x An object of class \code{"plstree"}.
#' @param \dots Further arguments are ignored.
#'
#' @author Giuseppe Lamberti
#'  
#' @references Lamberti, G. (2021). Hybrid multigroup partial least squares structural equation
#' modelling: an application to bank employee satisfaction and loyalty. \emph{Quality and Quantity},
#' \doi{10.1007/s11135-021-01096-9}
#'
#' @references Lamberti, G., Aluja, T. B., and Sanchez, G. (2017). The Pathmox approach for PLS path 
#' modeling: Discovering which constructs differentiate segments. \emph{Applied Stochastic Models in 
#' Business and Industry}, \bold{33}(6), 674-689. \doi{10.1007/s11135-021-01096-9}
#' 
#' @references Lamberti, G., Aluja, T. B., and Sanchez, G. (2016). The Pathmox approach for PLS path 
#' modeling segmentation. \emph{Applied Stochastic Models in Business and Industry}, \bold{32}(4), 453-468.
#' \doi{10.1002/asmb.2168}
#' 
#' @references Lamberti, G. (2015). \emph{Modeling with Heterogeneity}, PhD Dissertation.
#' 
#' @references Sanchez, G. (2009). \emph{PATHMOX Approach: Segmentation Trees in
#' Partial Least Squares Path Modeling}, PhD Dissertation.
#' 
#' @seealso \code{\link{summary.plstree}}, \code{\link{pls.pathmox}},  
#' \code{\link{bar_terminal}}, \code{\link{bar_impvar}} and \code{\link{plot.plstree}}
#' 
#' 
#'
#'@exportS3Method print plstree
#' @examples
#'  \dontrun{
#' # Example of PATHMOX approach in customer satisfaction analysis 
#' # (Spanish financial company).
#' # Model with 5 LVs (4 common factor: Image (IMAG), Value (VAL), 
#' # Satisfaction (SAT), and Loyalty (LOY); and 1 composite construct: 
#' # Quality (QUAL)
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
#' # Composite
#' QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5 + qual6 + qual7 
#'      
#' # Common factor
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5 + imag6 
#' VAL  =~ val1  + val2  + val3  + val4
#' SAT  =~ sat1  + sat2  + sat3           
#' LOY  =~ loy1  + loy2  + loy3           
#'
#' "
#'
#' # Run pathmox on one single variable
#' age = csibank[,2]
#' 
#' # Transform age into an ordered factor
#' age = factor(age, levels = c("<=25", "26-35", "36-45", "46-55",
#'                                       "56-65", ">=66"),ordered = T)
#'                                       
#' csi.pathmox.age = pls.pathmox(
#'  .model = CSImodel ,
#'  .data  = csibank,
#'  .catvar= age,
#'  .alpha = 0.05,
#'  .deep = 1
#' )  
#' 
#' # Visualize the Pathmox results
#' print(csi.pathmox.age)
#'
#' }
#'
print.plstree = function(x, ...)
{
  cat("------------------------------------------------------------------------------------")
  cat("\n")
  cat("PLS-SEM PATHMOX ANALYSIS", "\n")
  cat("------------------------------------------------------------------------------------")
  cat("\n   NAME            ", "DESCRIPTION")
  cat("\n1  $MOX            ", "Tree partition")
  cat("\n2  $terminal_paths ", "Info terminal nodes PLS-SEM models (path coeff. & R^2)")
  cat("\n3  $var_imp        ", "Info variable importance ranking:")
  cat("\n4  $Fg.r           ", "Info F-global test results (global differences)")
  cat("\n5  $Fc.r           ", "Info F-coefficient test results (coefficient differences)")
  cat("\n5  $hybrid         ", "Dataset associate to the terminal node prepared
      for MGA with cSEM R package")
  cat("\n6  $other          ", "Other parameters and results")
  cat("\n")
  cat("------------------------------------------------------------------------------------")
  cat("\nYou can also use the function 'summary'", "\n\n")    
  invisible(x)
}
