#' @title PATHMOX-PLS: Extended Segmentation Trees in 
#' Partial Least Squares Structutal Equation Modeling (PLS-SEM)
#' 
#' @description
#' The function \code{pathmox.pls} calculates a binary segmentation tree in 
#' the context PLS-SEM  following the PATHMOX algorithm.
#' The procedure can be resumed in the following way. It starts with the 
#' estimation of the global PLS-SEM Model at the root node. Then, using 
#' the segmentation variables, all possible binary splits of data are produced, 
#' and for each partition local models are calculated. Among all the splits, 
#' the best one is selected by means of the F-test comparing the inner models. 
#' This process is recursively applied for each child node. The stop criterion 
#' is based on the significance level of the p-value associated with the F statistic. 
#' Additionally, two stop parameters are also considered: the number 
#' of individuals in a node and the growing level of the depth of the tree.  
#' This function extends the pathmox algorithm introduced by Sanchez in 2009 
#' including the two new test: the F-block test (to detect the responsible latent 
#' endogenous equations of the difference), the F-coefficient test
#' (to detect the path coefficients responsible of the difference).The F-tests 
#' used in the split process are implemented following the classic lest square 
#' estimation. An implementation of the tests following the LAD regression also 
#' are proposed to overcome the parametric hypothesis of the F-test.
#' 
#' @details
#' 
#' The argument \code{x} must be a data frame containing the manifest variables of the 
#' PLS-SEM model
#' 
#' The argument \code{inner} is a matrix of zeros and ones that indicates
#' the structural relationships between latent variables. \code{inner} 
#' must be a lower triangular matrix; it contains a 1 when column \code{j}
#' affects row \code{i}, 0 otherwise. \cr
#'
#' The argument \code{SVAR} must be a data frame containing segmentation
#' variables as factors. The number of rows in
#' \code{SVAR} must be the same as the number of rows in the data used in
#' \code{x}. 
#'  
#' The argument \code{signif} represent the p-value level takes as reference  
#' to stop the tree partitions. 
#'
#' The argument \code{deep} represent the depth level of the tree takes as reference  
#' to stop the tree partitions. 
#'
#' The argument \code{method} is a string contaning the criterion used to calculate the   
#' tests; if \code{method="lm"} the classic least square approach is used to perform the tests; 
#' if \code{method="lad"} the LAD (least absolute deviation regression) is used.
#'
#' The argument \code{size} is defined as a decimal value (i.e. proportion
#' of elements inside a node). 
#'
#' The argument \code{n.node} is the minimum number of individuals to consider a candidate 
#' partition. If the candidate split produces a partition where the number of individuals is less  
#' then \code{n.node}, the partition is not considered.
#'
#'
#' @param x matrix or data frame containing the manifest variables.
#' @param inner A square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables).
#' @param outer list of vectors with column indices or column names
#' from \code{x} indicating the sets of manifest variables forming 
#' each block (i.e. which manifest variables correspond to each block).
#' @param scaling optional argument for runing the non-metric approach; 
#' it is a list of string vectors indicating the type of 
#' measurement scale for each manifest variable specified in \code{outer}.
#' \code{scaling} must be specified when working with non-metric variables.
#' Possible values: \code{"num"} (linear transformation, 
#' suitable for numerical variables), \code{"raw"} (no transformation), 
#' \code{"nom"} (non-monotonic transformation, suitable for nominal variables), 
#' and \code{"ord"} (monotonic transformation, suitable for ordinal variables).
#' @param mode character vector indicating the type of measurement for each
#' block. Possible values are: \code{"A", "B", "newA", "PLScore", "PLScow"}. 
#' The length of \code{mode} must be equal to the length of \code{outer}.
#' @param scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}.
#' @param scaled whether manifest variables should be standardized. 
#' Only used when \code{scaling = NULL}. By the default (\code{TRUE}, data is 
#' scaled to standardized values (mean=0 and variance=1). 
#' @param SVAR A data frame of factors contaning the segmentation variables.
#' @param signif A numeric value indicating the significance threshold of the
#' F-statistic. Must be a decimal number between 0 and 1.
#' @param deep An integer indicating the depth level of the tree. Must be an
#' integer greater than 1.
#' @param method A string indicating the criterion used to calculate the   
#' the test can be equal to \code{"lm"} or \code{"lad"}.
#' @param size A numeric value indicating the minimum size of elements inside a
#' node.
#' @param X Optional dataset (matrix or data frame) used when argument
#' \code{dataset=NULL} inside \code{pls}.
#' @param n.node It is the minimum number of individuals to consider a candidate 
#' partition (\code{30} by default).
#' 
#'
#' @param \dots Further arguments passed on to \code{\link{pls.pathmox}}. 
#'
#' @return An object of class \code{"xtree.pls"}. Basically a list with the
#' following results:
#' @return \item{MOX}{Data frame with the results of the segmentation tree}
#' @return \item{root}{List of elements contanined in the root node}
#' @return \item{terminal}{List of elements contanined in terminal nodes}
#' @return \item{nodes}{List of elements contanined in all nodes: terminal and intermediate}
#' @return \item{candidates}{List of data frames containing the candidate 
#' splits of each node partition}
#' @return \item{Fg.r}{Data frame containing the results of the F-global test 
#' for each node partition}
#' @return \item{Fb.r}{List of data frames containing the results of the F-block test 
#' for each node partition}
#' @return \item{Fc.r}{A list of data frames containing the results of the F-coefficients test 
#' for each node partition}
#' @return \item{model}{Informations about the internal paramenters} 
#' @return \item{hybrid}{a hybird categorical factor defined according to the final segments idenfied by pathmox} 
#' 
#' @author Giuseppe Lamberti
#' 
#' @references Lamberti, G. et al. (2017) \emph{The Pathmox approach for PLS path modeling: 
#' Discovering which constructs differentiate segments}.Applied Stochastic Models in Business and Industry; 
#' doi: 10.1002/asmb.2270; 
#' 
#' @references Lamberti, G. et al. (2016) \emph{The Pathmox approach for PLS path modeling segmentation}. 
#' Applied Stochastic Models in Business and Industry; doi: 10.1002/asmb.2168; 
#'               
#' @references Lamberti, G. (2014) \emph{Modeling with Heterogeneity.} PhD Dissertation.
#'
#' @export
#' @examples
#'
#' \dontrun{
#'  ## example of PLS-PM in alumni satisfaction
#'  
#'  data(fibtele)
#'  
#'  # select manifest variables
#'  data.fib <-fibtele[,12:35]
#'  
#'  # define inner model matrix
#'  Image 			= rep(0,5)
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
#'                  
#'
#'  # re-ordering those segmentation variables with ordinal scale 
#'  seg.fib = fibtele[1:50,c(2,7)]
#'	seg.fib$Salary = factor(seg.fib$Salary, 
#'			levels=c("<18k","25k","35k","45k",">45k"), ordered=TRUE)
#'
#'  # Pathmox Analysis
#'  fib.pathmox=pls.pathmox(data.fib, inner.fib, outer.fib, modes.fib,SVAR=seg.fib,signif=0.05,
#'					deep=2,size=0.2,n.node=20)
#'
#'
pls.pathmox = function(x,inner,outer,mode,scheme="path",scaling=NULL,scaled=TRUE,SVAR,signif,deep,method="lm",size,X = NULL,n.node=30,...)
{

    if (!is.null(x)) {
      if (!is.matrix(x) && !is.data.frame(x)) 
        stop("Invalid object 'X'. Must be a numeric matrix or data frame.")
      if (nrow(x) != nrow(SVAR)) 
        stop("Arguments 'X' and 'SVAR' are incompatible. Different number of rows")
    }
    else {
      if (is.null(x)) {
        stop("Argument 'X' is missing. No dataset available.")
      }
      else {
        if (nrow(x) != nrow(SVAR)) 
          stop("Arguments 'pls' and 'SVAR' are incompatible. Different number of rows")
      }
    }
    if (!is.data.frame(SVAR)) 
      stop("Argument 'SVAR' must be a data frame containing factors")
    for (j in 1:ncol(SVAR)) if (!is.factor(SVAR[, j])) 
      stop("One or more columns in 'SVAR' are not factors")
    
    if (any(is.na(x)))
    {
      stop("Data contains NA: please provide a complete data-set")
    }
    if (!is.data.frame(SVAR))
    {
      stop("Argument 'SVAR' must be a data frame containing factors")
    }
    if (any(is.na(SVAR)))
    {
      stop("data factors contains NA: please provide a complete data factors")
    }             
    if (mode(signif) != "numeric" || length(signif) != 1 || signif <= 
        0 || signif >= 1) {
      warning("NOTICE: Invalid argument 'signif'. Default value 0.05 was used", 
              "\n")
      signif <- 0.05
    }
    if (mode(method) != "character" || method != "lm" && method != "lad" ){
      warning("NOTICE: Invalid argument 'method'. Default method lm was used", 
              "\n")
      method <- "lm"  
    }
    if (mode(size) != "numeric" || length(size) != 1 || size <= 
        0) {
      warning("NOTICE: Invalid argument 'size'. Default value 0.10 was used", 
              "\n")
      size <- 0.1
    }
    if (mode(deep) != "numeric" ||  deep < 
        1 || (deep%%1) != 0) {
      warning("NOTICE: Invalid argument 'deep'. Default value 1 was used", 
              "\n")
      deep <- 1
    }
    
    
    if ( class(inner)[1] != "matrix")
      stop("\n'inner' must be a matrix.")
    
    if (nrow(inner) == 1)
      stop("\n'inner' must have more than one row")
    
    for (j in 1:ncol(inner)) 
    {
      for (i in 1:nrow(inner)) 
      {
        if (length(intersect(inner[i,j], c(1,0))) == 0)
          stop("\nElements in 'inner' must be '1' or '0'")
      }      
    }
    
    if (!is.list(outer))
      stop("\n'outer' must be a list.")
    
    # no duplicated elements within each block
    mvs_duplicated = unlist(lapply(outer, duplicated))
    if (any(mvs_duplicated))
      stop("\nWrong 'outer'. Duplicated variables in a block are not allowed")
    
    # all elements in blocks of same mode
    mvs_mode = unique(unlist(lapply(outer, mode)))
    if (length(mvs_mode) > 1)
      stop("\nAll elements in 'outer' must have the same mode")
    
    # check indices inside columns range of Data
    if (mvs_mode == "numeric") {
      blocks_in_data = match(unlist(outer), 1:ncol(x))
      if (any(is.na(blocks_in_data)))
        stop("\nIndices in 'outer' outside the number of columns in 'x'")
    }
    
    # convert character blocks to numeric outer
    if (mvs_mode == "character") {
      data_names = colnames(x)
      matched_names = match(unlist(outer), data_names)
      if (any(is.na(matched_names))) {
        bad_names = unlist(outer)[is.na(outer)]
        stop(sprintf("\nUnrecognized name in 'outer': '%s'", bad_names))        
      }
      outer = lapply(outer, function(x, y) match(x, y), data_names)
    }
    
    if (length(outer) != nrow(inner))
      stop("\nNumber of rows in 'path_matrix' different from length of 'outer'.")	

	info.mox.pls(signif,size,deep,SVAR)
	
	model	= list(signif=signif,size=size,deep=deep,method=method,data=x,SVAR=SVAR,
	             inner=inner,outer=outer,mode=mode,scheme=scheme,scaling=scaling, scaled=scaled)
	
	min.ind.node = percent.node(x,size)
	
	hybrid = NULL
	

	min.ind.node = percent.node(x,size)
	
	#init: create the root
	id=0
	t = new ("tree",id=id)
	dim_row = dim(model$data)[1]
	elements = seq(1:dim_row)
	id = id+1
	root = new("node",id=id,elements=elements,father=0,childs=0)
	new_nodes = list(root)
	while(length(new_nodes) > 0){
		n = new_nodes[[1]]
		
		#init
		
		if(length(n@elements)>=min.ind.node$min.n.ind && showDeepth(n) < deep){
				
		d = x[n@elements,]
		s = SVAR[n@elements,]
		cat		=	sapply(s, is.factor)
		s[cat] 	=	lapply(s[cat],factor)
		
		tmp = partopt.pls(d,s,inner,outer,mode,scheme,scaling,scaled,method,n.node)

		if(tmp$pvl.opt <= signif && any(!is.null(tmp$pvl.opt)) == TRUE) {
				
				#before create child nodes
				
				variable		= tmp$variable.opt
				level		= tmp$level.opt
				candidates	= tmp$candidates
				modtwo		= tmp$modtwo.opt
				
				mod=test.partition.pls(d,inner,outer,mode,scheme,scaling,scaled,modtwo,signif,method)
				
				fglobal = mod$Fg
				fblock  = mod$Fb
				fcoef   = mod$Fc
				pvg     = mod$pvg
				pvb     = mod$pvb
				pvc     = mod$pvc
				
				#create child nodes
				
				for(i in 1:2){
						elements = n@elements[which(modtwo==i)]
						child_id = (n@id*2)+i-1
						child = new("node",id=child_id,elements=elements,father=n@id)
						n@childs[i] = child_id
						new_nodes[[length(new_nodes)+1]]=child
					}
				n@info=new("info.pls",
							variable=variable,
							level=level,
							fgstatistic=fglobal,
							fpvalg=pvg,
							fbstatistic=fblock,
							fpvalb=pvb,
							fcstatistic=fcoef,
							fpvalc=pvc, 
							candidates=candidates)
			
		}}
		t@nodes[[length(t@nodes)+1]] = n
		new_nodes[1] = NULL
	}
	if	(length(t@nodes)==1)
	{
		root = root.tree(t)
		cat("No sognificative partition faunded")
		res = list(root=root,model=model)
	}
	else
	{
		MOX = mox.tree.pls(t)
		root = root.tree(t)
		terminal = terminal.tree(t)
		nodes = nodes.tree(t)
		candidates = candidates.tree(t)
		Fg.r = fglobal.tree.pls(t)
		Fb.r = fblock.tree.pls(t)
		Fc.r = fcoef.tree.pls(t)
		for( i in 2: length(terminal))  
		  hybrid = c(hybrid, rep( i-1, length(terminal[[i]])))
		res = list(MOX=MOX,
				root=root,
				terminal=terminal,
				nodes=nodes,
				candidates=candidates,
				Fg.r=Fg.r,
				Fb.r=Fb.r,
				Fc.r=Fc.r,
				model=model,
				hybrid = hybrid)
	class(res) = "xtree.pls"
}
 res
}
