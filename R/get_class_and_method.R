#' info class
#'
#' info is an S4 class that contains info about the tree partition
#' and the the the Fischer statitistcs: F-global and F-coefficientes
#'
#' @name info-class
#' @rdname info-class
#' @keywords internal
#' 
setClass("info",representation(		
  variable		= "character",
  level  		  = "character",
  fgstatistic	= "numeric",
  fpvalg		  = "numeric",
  fcstatistic	= "numeric",
  fpvalc	 	  = "numeric",
  candidates 	= "data.frame"
))
#
#' node class
#'
#' node is an S4 class that contains info about the node of the tree
#'
#' @name node-class
#' @rdname node-class
#' @keywords internal
#' 
setClass("node",representation(		
  id			  =	"numeric",
  elements	=	"numeric",
  father		=	"numeric",
  childs		=	"numeric",
  info		  =	"info"
))
#' moxtree class
#'
#' moxtree is an S4 class that contains info about moxtree
#'
#' @name moxtree-class
#' @rdname moxtree-class
#' @keywords internal
#' 
setClass("moxtree",representation(		
  id			  =	"numeric",
  nodes	 	  =	"list",
  info_tree	=	"list"
))
#' @title create method plstree
#' @details
#' Internal function. 
#' @param x the element representing the method.
#' @param \dots Further arguments passed on to \code{\link{plstree}}.
#' @return internal method
#' @keywords internal
#' @export

plstree <- function(x, ...) UseMethod("plstree")

