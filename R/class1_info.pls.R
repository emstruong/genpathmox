#' info.pls class
#'
#' info.pls is an S4 class that contains info on the variable and his levels 
#' that provides the best binary split and the the the Fischers statitistcs: 
#' F-global, F-#' block, F-coefficientes
#'
#' @name info.pls-class
#' @rdname info.pls-class

setClass("info.pls",representation(		
  variable		= "character",
  level  		  = "character",
  fgstatistic	= "numeric",
  fpvalg		  = "numeric",
  fbstatistic	= "numeric",
  fpvalb		  = "numeric",							   					   
  fcstatistic	= "numeric",
  fpvalc	 	  = "numeric",
  candidates 	= "data.frame"
))