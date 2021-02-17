#' @title PATHMOX-PLS: Extended Segmentation Trees in 
#' Partial Least Squares Structutal Equation Modeling (PLS-SEM)
#' 
#' @description
#' The function \code{multigroup.pls} performs a group comparison test for comparing 
#' path coefficients between two groups (classical multigroup approach for PLS-SEM). 
#' The null and alternative hypotheses to be tested are: H0: path coefficients are not 
#' significantly different; H1: path coefficients are significantly different
#' 
#' @details
#' 
#' The argument \code{x} must be a data frame containing the manifest variables of the 
#' PLS-SEM model.
#' 
#' The argument \code{cat} must be the caterogical variable used for teh comparison. 
#' 
#' \code{ng} number of resampling. 
#'
#' The argument \code{method} is a string contaning the criterion used to calculate the   
#' tests; if \code{method="parametric"} the classic parametric approach is used to perform the tests; 
#' if \code{method="permutation"} the Chin 2003 permutation testis used.
#'
#' @param x matrix or data frame containing the manifest variables.
#' @param inner A square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables).
#' @param outer list of vectors with column indices or column names
#' from \code{x} indicating the sets of manifest variables forming 
#' each block (i.e. which manifest variables correspond to each block).
#' @param mode character vector indicating the type of measurement for each
#' block. Possible values are: \code{"A", "B", "newA", "PLScore", "PLScow"}. 
#' The length of \code{mode} must be equal to the length of \code{outer}.
#' @param scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}.By default is \code{"path"}.
#' @param scaled whether manifest variables should be standardized. 
#' By the default (\code{TRUE}, scaled to standardized values (mean=0 and variance=1). 
#' @param cat the caterogical variable used for the comparison. 
#' @param ng number of resamplig used to calculate the test
#' @param method is the method used to test differences in path coefficients. There
#' are two test availables: the parametric test and the permutation test By
#' defalut is equal to \code{parametric}. 
#' 
#' @param \dots Further arguments passed on to \code{\link{multigroup.pls}}. 
#'
#' @return  Basically a list with the
#' following results:
#' @return \item{global_and_group_path_coeff}{Path coefficients estimated for the global and the local models 
#' defined according to the level of teh categorical variable}
#' @return \item{abs_path_diff}{absolute difference of the path coefficients estimated for the local models}
#' @return \item{multigroup_pvalue}{LP-value test of comparison}
#' 
#' @author Giuseppe Lamberti
#' 
#' @references Hair, J. F. et al.(2018) \emph{Advanced issues in partial least squares structural equation modeling}. 
#' Thousand Oaks: SAGE Publications (2018) 
#'
#'
#' @export
#' @examples
#'  \dontrun{
#'  ## example of PLS-PM in alumni satisfaction
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
#' 
#'
#'  # Multigroup Analysis 
#'  multi=multigroup.pls(x=data.bank, inner=inner.bank, outer=outer.bank, 
#'                         mode=modes.bank, scheme="path",  scaling=NULL,scaled=TRUE,
#'                         cat=seg.bank$Education, ng=100, method="parametric") 
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
#'
#'  # Multigroup Analysis 
#'  multi=multigroup.pls(x=data.bank, inner=inner.bank, outer=outer.bank, 
#'                         mode=modes.bank, scheme="path",  scaling=NULL,scaled=TRUE,
#'                         cat=seg.bank$Education, ng=100, method="parametric") 
#'   
#'
multigroup.pls = function(x,inner,outer,mode,scheme="path",scaled=TRUE, cat, ng=500, method="parametric",... ) {
  
  
  if (!is.null(x)) {
    if (!is.matrix(x) && !is.data.frame(x)) 
      stop("Invalid object 'X'. Must be a numeric matrix or data frame.")
    if (nrow(x) != length(cat)) 
      stop("Arguments 'X' and 'cat' are incompatible. Different number of rows")
  }
  else {
    if (is.null(x)) {
      stop("Argument 'X' is missing. No dataset available.")
    }
  }
  if (!is.factor(cat)) 
    stop("Argument 'cat' must be a factors")
  
  if (any(is.na(x)))
  {
    stop("Data contains NA: please provide a complete data-set")
  }
  if (any(is.na(cat)))
  {
    stop("Argument 'cat' does not admit NA")
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
  
  if (mode(method) != "character" || method != "parametric" && method != "permutation"  && method != "Flebart"){
    warning("NOTICE: Invalid argument 'method'. Default method parametric was used", 
            "\n")
    method <- "parametric"  
  }
  # check number of replicates
  if (is.null(ng) | length(ng) > 1) ng = 100
  if (!is.numeric(ng) | floor(ng) <= 0) ng = 100
  
  
  
  t1 = data.frame(table(cat))
  t2 =data.frame((table(cat)/dim(x)[1])*100)
  
  t3= NULL
  for ( i in 1 : length(levels(cat)))
  {
    data = split(x,cat)[[i]]
    if(nrow(data)< ncol(data) || is.null(data)) {
      t3 = c(t3, "not admisible group size") 
    }
    else {
      t3 = c(t3, "admisible group size")  
      
    }}
  
  
  info= data.frame(t1,round(t2[,2],2),t3)
  colnames(info) = c("Variable level","Freq.", "%", "admisible size")
  
  
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("MULTI_GROUP PLS-SEM ","\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("Group comparison method:",method,"\n")
  cat("\n")
  cat("Categorical variable levels:",levels(cat),"\n")
  cat("\n")
  cat("Categorical variable info:","\n")
  cat("\n")
  print(info)
  cat("\n")
  cat("**not admisible: if size group less than column pls data**","\n")
  
  cat("\n")
  cat("\n")
  
  if (length(t3[t3=="admisible group size"])<2)
    stop("Number of admisible group size lower than 2. No camaprison is viable")
  
  path.list     = list() 
  path.diff     = list()
  data.group    = list()
  length.group  = list()
  
  dgree         = list()
  compname      = list()
  name          = NULL
  
  den.list      = list()
  sd.path.list  = list()
  tstudent      = list()
  
  global        = list()
  
  ###############################################################################################
  
  pls = plspm(x, inner, outer, mode,scheme, scaling=NULL, scaled, maxiter=100000, tol=0.001)
  
  for ( i in 1 : length(levels(cat)))
  {
    data = split(x,cat)[[i]]
    
    if(nrow(data)< ncol(data) || is.null(data)) next
    
    plsg = plspm(data, inner, outer, mode,scheme, scaling=NULL, scaled, maxiter=100000, tol=0.001)
    
    path.list[[length(path.list)+1]]  = plsg$path_coefs[which(inner==1)]
    
    name = c(name,names(split(x,cat)[i]))
    
  }
  
  for ( i in 1 : (length(path.list)-1))
  {
    for ( j in (i+1) : length(path.list))
    {
      path.diff[[length(path.diff)+1]] =    abs(path.list[[i]] - path.list[[j]])
      compname[[length(compname)+1]]  = paste(name[i], "vs.", name[j] )
    }
  }
  
  ################################################################################################
  
  
  coeff = NULL
  diffp = NULL
  for ( i in 1 : length(path.list))
  {
    coeff = cbind(coeff,path.list[[i]])
  }
  coeff=round(cbind(pls$path_coefs[which(inner==1)],coeff),3)
  rownames(coeff)=element(inner)
  colnames(coeff)=c("Global",name)
  
  for ( i in 1 : length(path.diff))
  {
    diffp = cbind(diffp,path.diff[[i]])
  }
  
  diffp=round(diffp,3)
  rownames(diffp)=element(inner)
  colnames(diffp)= unlist(compname)
  
  ################################################################################################
  
  
  
  
  name          = NULL
  path.list     = list() 
  compname      = list()
  path.diff     = list()
  
  
  if(method=="parametric")
  {
    
    for ( i in 1 : length(levels(cat)))
    {
      
      data = split(x,cat)[[i]]
      
      if(nrow(data)< ncol(data) || is.null(data)) next 
      
      name = c(name,names(split(x,cat)[i]))
      
      data.group[[length(data.group)+1]]      = data
      length.group[[length(length.group)+1]]  = dim(data)[1]
      
      pls = plspm(data, inner, outer, mode,scheme, scaling=NULL, scaled, maxiter=100000, tol=0.001)
      
      path.list[[length(path.list)+1]]  = pls$path_coefs[which(inner==1)]
      
      path.mat=mat.or.vec(ng,sum(inner[which(inner==1)]))
      
      for (k in 1:ng)
      {
        
        data.res = x[sample(rownames(data), dim(data)[1], replace=TRUE),]
        plsres    = plspm(data.res, inner, outer, mode, scheme, scaling=NULL,scaled, maxiter=100000, tol=0.001)
        
        path.mat[k,]  = plsres$path_coefs[which(inner==1)]
      }
      
      sd.path.list[[length(sd.path.list)+1]]=apply(path.mat,2,var)
    }
    
    for ( i in 1 : (length(path.list)-1))
    {
      for ( j in (i+1) : length(path.list))
      {
        path.diff[[length(path.diff)+1]] =    abs(path.list[[i]] - path.list[[j]])
        compname[[length(compname)+1]]  = paste(name[i], "vs.", name[j] )
        
        m = length.group[[i]]
        n = length.group[[j]]
        
        tstudent[[length(tstudent)+1]]= (
          
          (abs(path.list[[i]] - path.list[[j]])) /
            
            (sqrt( (1/m) + (1/n) 
            )*
              sqrt(
                ( ( ((m-1)^2)/(m+n-2) ) * sd.path.list[[i]] ) +
                  ( ( ((n-1)^2)/(m+n-2) ) * sd.path.list[[j]] )   
              )
            )
        )
        
        dgree[[length(dgree)+1]] =  (m + n - 2)
        
      } 
    }    
    
    pval=list()
    
    res=NULL
    for ( i in 1 : length(tstudent))
    {
      comp=NULL
      for ( j in 1 : sum(inner[which(inner==1)])) 
      {
        comp=c(comp,pt(tstudent[[i]][j], dgree[[i]], lower.tail=TRUE))
      }
      pval[[length(pval)+1]] <- comp
    }
    
    for ( i in 1 : length(pval))
    {
      unlist(pval[i])
      res = round(cbind(res,unlist(pval[i])),3)
    }
  }
  
  if(method=="permutation")
  {
    for ( i in 1 : length(levels(cat)))
    {
      
      data = split(x,cat)[[i]]
      
      if(nrow(data)< ncol(data) || is.null(data)) next 
      
      name = c(name,names(split(x,cat)[i]))
      
      data.group[[length(data.group)+1]] = data
      
      length.group[[length(length.group)+1]] =dim (data)[1]
      
      pls = plspm(data, inner, outer, mode,scheme, scaling=NULL, scaled, maxiter=100000, tol=0.001)
      
      path.list[[length(path.list)+1]]=  pls$path_coefs[which(inner==1)]
    }
    
    pval=list()
    
    for ( i in 1 : (length(path.list)-1))
    {
      for ( j in (i+1) : length(path.list))
      {
        
        path.diff[[length(path.diff)+1]] =  abs(path.list[[i]] - path.list[[j]])
        
        compname[[length(compname)+1]] = paste(name[i], "vs.", name[j] )
        
        m = length.group[[i]]
        n = length.group[[j]]
        
        dif.perm <- matrix(0, ng, sum(inner))
        
        for (k in 1:ng) {
          
          permu <- sample(1:(m+n), m+n)
          samg1 <- permu[1:m]
          samg2 <- permu[(m+1):(m+n)]
          # apply the selected scaling
          
          
          conbined=rbind(data.group[[i]],data.group[[j]])
          
          X.g1 = conbined[samg1,]
          X.g2 = conbined[samg2,]
          
          plsg1 = plspm(X.g1, inner, outer, mode, scheme, scaling=NULL,scaled, maxiter=100000, tol=0.001)
          plsg2 = plspm(X.g2, inner, outer, mode, scheme, scaling=NULL,scaled, maxiter=100000, tol=0.001)
          
          pp1 = as.vector(plsg1$path_coefs[which(inner==1)])
          pp2 = as.vector(plsg2$path_coefs[which(inner==1)])
          dif.perm[k,] = abs(pp1 - pp2)
          
        }
        
        s.perm <-  abs(path.list[[i]] - path.list[[j]])
        for (r in 1:sum(inner))         
          s.perm[r] <- length(which(abs(path.list[[i]] - path.list[[j]])[r] < dif.perm[,r])) + 1
        pval[[length(pval)+1]] <- (1/(ng+1)) * s.perm 
        
      } 
      
    }
    res=NULL
    for ( i in 1 : length(pval))
    {
      unlist(pval[i])
      res = round(cbind(res,unlist(pval[i])),3)
    }
  }
  if(method=="Flebart")
  {
    for ( i in 1 : length(levels(cat))){
      
      data = split(x,cat)[[i]]
      
      if(nrow(data)< ncol(data) || is.null(data)) next 
      
      name = c(name,names(split(x,cat)[i]))
      
      data.group[[length(data.group)+1]]= data
      length.group[[length(length.group)+1]] = dim(data)[1]
      
      
      pls 		=	plspm(data,inner, outer, mode, scheme, scaling=NULL,scaled, maxiter=100000, tol=0.001)
      LV      = pls$scores
      global[[length(global)+1]]	=	build.block(inner=inner,latent=LV)
    }
    
    path.name		=	element(pls$path_coefs)
    
    pval=list()
    for ( i in 1 : (length(global)-1))
    {
      for ( j in (i+1) : length(global))
      {
        
        compname[[length(compname)+1]]  = paste(name[i], "vs.", name[j] )
        Y1			    =	rbind(global[[i]]$resp,global[[j]]$resp)
        info.block	=	global[[1]]$constructs.label
        X1	=	blockdiag(list(global[[i]]$x.block, global[[j]]$x.block))
        
        n.col	=	ncol(X1)/2
        
        pval[[length(pval)+1]]=Fc.test.pls(Y1,X1,path.name,info.block,method="lm")          
        
      }
    }   
    
    res=NULL
    for ( i in 1 : length(pval))
    {
      res= round(cbind(res,pval[[i]]$pvc ),3)
    }
    
  }
  
  
  rownames(res) = element(inner)
  colnames(res) = unlist(compname)
  
  list(global_and_group_path_coeff= coeff, abs_path_diff=diffp, multigroup_pvalue=res)
}  