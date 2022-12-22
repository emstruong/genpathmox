#' ############################################################################################
#' @title Bart matrix
#' @details
#' Internal function
#' @param x list of matrices used to build the Bart matrix
#' @param \dots further arguments passed on to \code{\link{blockdiag}}.
#' @return the Bart matrix
#' @keywords internal
#' @export
#' 
blockdiag <- function (x,...)
{
  if (!is.list(x)) 
    x <- list(x)
  args <- list(...)
  if (length(args) > 0) 
    args <- c(x, args)
  else args <- x
  idx <- which(!sapply(args, is.matrix))
  if (length(idx) > 0) 
    for (i in idx) args[[i]] <- as.matrix(args[[i]])
  if (length(args) == 1) 
    return(args[[1]])
  nr <- sapply(args, nrow)
  nc <- sapply(args, ncol)
  cumnc <- cumsum(nc)
  NR <- sum(nr)
  NC <- sum(nc)
  rowfun <- function(m, zbefore, zafter) {
    cbind(matrix(0, ncol = zbefore, nrow = nrow(m)), m, matrix(0, 
    ncol = zafter, nrow = nrow(m)))
  }
  ret <- rowfun(args[[1]], 0, NC - ncol(args[[1]]))
  for (i in 2:length(args)) {
    ret <- rbind(ret, rowfun(args[[i]], cumnc[i - 1], NC - 
                               cumnc[i]))
  }
  ret
}
#' ############################################################################################
#' @title Combinations of a vector element
#' @details
#' Internal function
#' @param n size of the source vector
#' @param r size of the target vectors
#' @param v source vector. Defaults to 1:n
#' @param set logical flag indicating whether duplicates should be removed from the 
#' source vector v
#' @param repeats.allowed logical flag indicating whether the constructed vectors may include 
#' duplicated values
#' @return  a matrix of combinations
#' @keywords internal
#' @export
#' 
comb	<-	function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE)
{
  v0 <- vector(mode(v), 0)
  if (repeats.allowed) 
    sub <- function(n, r, v) {
      if (r == 0) 
        v0
      else if (r == 1) 
        matrix(v, n, 1)
      else if (n == 1) 
        matrix(v, 1, r)
      else rbind(cbind(v[1], Recall(n, r - 1, v)), Recall(n - 
                                                            1, r, v[-1]))
    }
  else sub <- function(n, r, v) {
    if (r == 0) 
      v0
    else if (r == 1) 
      matrix(v, n, 1)
    else if (r == n) 
      matrix(v, 1, n)
    else rbind(cbind(v[1], Recall(n - 1, r - 1, v[-1])), 
               Recall(n - 1, r, v[-1]))
  }
  sub(n, r, v[1:n])
}
#' ############################################################################################
#' @title Binary partitions given a segmentation variable (factor). 
#' @details
#' Internal function
#' @param x  single factor or a data.frame of segmenation variables.
#' @return a list of matrices containing  all possibiles binary partions given a 
#' segmenation variable
#' @keywords internal
#' @export
#'
partition <- function(x) 
{
  cat		=	sapply(x, is.factor)
  x[cat] 	=	lapply(x[cat],factor)
  
  nc 		= ncol(x)                                         		
  split	= list()
  
  length(split) = nc
  
  for (j in 1:nc) 
  {                                       		
    
    j.pval.opt	= NULL                            		
    modnum 		= as.numeric(x[,j])               
    nmod		= length(levels(x[,j]))            
    
    if (nmod == 1) next                              
    
    mod = 1:nmod                                	
    if (nmod == 2) {splits = matrix(c(1,2),1,2)}          
    
    if (nmod > 2) 
    {                                      			
      if (class(x[,j])[1] == "ordered") 
      {       	
        splits = matrix(NA,(nmod-1),nmod)         
        np = 0                                     					
        for (m in (2:nmod)) 
        {                      
          np 	= np+1 
          two = rep(1,nmod)                    			
          two[mod>=m] = 2                      		    
          splits[np,] = two                                  
        }
      }
      
      else
      {
        npart		= 2^(nmod-1)-1                     			
        np 			= 0                                   					
        breakflag 	= FALSE                       			
        splits 		= matrix(NA,npart,nmod)          		
        for (k in (1:floor(nmod/2))) 
        {           			
          comb = comb(nmod,k)         	
          for (i in 1:nrow(comb)) 
          {
            np = np + 1
            if (np > npart) breakflag = TRUE    
            if (breakflag) break
            two = rep(2,nmod)              				
            for (l in 1:ncol(comb)) {two[mod==comb[i,l]] = 1}       
            splits[np,] = two             					
          } 
        }
      }
    }
    
    split[[j]] = splits
  } 
  list(split=split)  
}
#' ############################################################################################
#' @title Linear relations between latent variables. 
#' @details
#' Internal function. 
#' @param inner a square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables)
#' @param latent matrix of the latent score
#' @return a list of matrices containing for each endogenous latent variable the 
#' the own predictors
#' @keywords internal
#' @export
#' 
build.block	<-	function(inner,latent)
{
  constructs			= list() 
  constructs.label	= list()
  
  for (i in 1:ncol(inner))
  {
    y = NULL
    y = as.matrix(latent[,i])
    colnames(y) = rownames(inner)[i]
    x = NULL
    for (j in 1:ncol(inner))
    {
      xj = NULL
      if (inner[i,j] == 0) next
      xj = as.matrix(latent[,j])
      colnames(xj) = rownames(inner)[j]
      x = cbind(x,xj)						
    }
    constructs[[length(constructs)+1]] = cbind(y,rep(1,nrow(y)),x)
    constructs.label[[length(constructs.label)+1]] = c("int",colnames(x))		
  }
  
  new.constructs		= list()
  new.constructs.label	= list()	
  k=1
  while (k <= length(constructs))
  {
    if (ncol(constructs[[k]]) > 2)
    {
      new.constructs[[length(new.constructs)+1]] = constructs[[k]]
      new.constructs.label[[length(new.constructs.label)+1]] = constructs.label[[k]]
    }
    k=k+1	
  }
  
  label.block = NULL
  
  for (i in 1:length(new.constructs)) label.block[i] = colnames(new.constructs[[i]])[1]
  for (i in 1:length(new.constructs.label)) names(new.constructs.label)[i] = label.block[i]
  
  resp = NULL
  pred = list()
  
  for (i in 1:length(new.constructs))
  {
    yi		= NULL
    yi		= as.matrix(new.constructs[[i]][,1])	
    resp	= rbind(resp,yi)
    pred[[length(pred)+1]]	= as.matrix(new.constructs[[i]][,-1])
  }
  
  x.block = blockdiag(pred)	
  colnames(x.block) = c(unlist(new.constructs.label))
  
  list(x.block=x.block,
       resp=resp,
       constructs.label=new.constructs.label)
}
#' ############################################################################################
#' @title Labels of a categorical variable a binary partions
#' @details
#' Internal function
#' @param variable segmentation variables (factor)
#' @param z matrix containing the posible combination of levels of factor
#' @return a list of labels
#' @keywords internal
#' @export
#' 
bin.levels	<-	function(z,spl)
{
  new.lab = list()
  for (i in 1:2)
  {
    mod = levels(z)[spl==i]
    for (i in 1:length(mod))
    {
      v.temp = paste(mod,collapse = '/')
    }
    new.lab[[length(new.lab)+1]] = v.temp
  }
  unlist(new.lab)
}
#' ############################################################################################
#' @title Path coefficient labels 
#' @details
#' Internal function
#' @param x matrix containing information of casual relationship of latent variables
#' @return a vector of path coefficients labels
#' @keywords internal
#' @export
#' 
element	<-	function(x,l)
{
  y = mat.or.vec(1,sum(ifelse(x==0,0,1)))
  k = 1
  for (i in 1:dim(x)[1]) 
  {
    for (j in 1:dim(x)[1])
    {
      if(x[i,j] != 0)
      {
        v	= paste(colnames(x)[j],rownames(x)[i],sep = "->")
        y[k]	= v
        k	= k+1
      }
    }
  }
  as.vector(y)
}
#' ############################################################################################
#' @title Vector minimum position  
#' @details
#' Internal function
#' @param x vector of values
#' @return a list containing minimun value, the position of the minimun and the 
#' values different from NA of a vactor
#' @keywords internal
#' @export
#' 
f.min	<-	function(x)
{
  if(!is.null(x))
  {
    v.min	=	max(x[!is.na(x)])
    all.v	=	which(!is.na(x))
    p.min	=	match(v.min,x)
  }
  else
  {
    v.min	=	NULL
    all.v	=	NULL
    p.min	=	NULL
  }	
  list(v.min=v.min,all.v=all.v,p.min=p.min)
}
#' ############################################################################################
#' @title Best partition for a specific segmentation variable
#' @details
#' Internal function
#' @param x matrix or data frame containing the manifest variables.
#' @param inner a square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables)
#' @param .model A description of the user-specified model.
#' @param .scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}.
#' @param .consistent Logical. Should composite/proxy correlations be disattenuated 
#' to yield consistent loadings and path estimates if at least one of the construct 
#' is modeled as a common factor. Defaults to TRUE.
#' @param splits vector indicating the binary partition.
#' @param fact vector indicating the categorical variable.
#' @param size_candidate number indicating the minimum threshold for a node.
#' @return a list containing information about the bets partition for a specific segmentation 
#' variable.
#' @keywords internal
#' @export
#' 
splitopt <- function(x,inner, .model,.scheme,.consistent, splits,fact,size_candidate) 
{	
  Fi				      =	NULL                                             	
  pval.split		  =	NULL                                     
  df.num			    =	NULL
  df.den			    =	NULL
  g1.ind			    =	NULL
  g2.ind			    =	NULL
  new.mod			    =	list()
  modtwo.list		  =	list()
  partition.list	=	list()
  
  #H0
  
  pls 		=	cSEM::csem(.data=x,.model,.PLS_weight_scheme_inner=.scheme, .disattenuate=.consistent)
  
  LV      = pls$Estimates$Construct_scores
  
  global	=	build.block(inner,latent=LV)
  
  n.block	=	ncol(inner)+1
  
  g.resp	=	global$resp
  g.pred	=	global$x.block

  reg0	=	stats::lm(g.resp~g.pred-1)                   	
  SSR0	=	sum(reg0$residuals^2)
  df0	  =	(nrow(g.pred) - ncol(g.pred))

  for (i in 1:nrow(splits)) 
  {                 
    modnum									                    =	as.numeric(fact)                        
    split									                      =	splits[i,]
    partition.list[[length(partition.list)+1]]	=	split
    modtwo 									                    = 	split[modnum]                             
    
    bin.lev									                    =	bin.levels(fact,split)
    new.mod[[length(new.mod)+1]]				        =	bin.lev
    modtwo.list[[length(modtwo.list)+1]]			  =	modtwo
    
    g1.latent 								                  = 	subset(LV,modtwo==1)                    
    g2.latent 								                  = 	subset(LV,modtwo==2)
    
    g1.ind[i]									                  =	nrow(g1.latent)
    g2.ind[i]									                  =	nrow(g2.latent)
    
    if (g1.ind[i] < n.block || g2.ind[i] < n.block) next
    
    if (g1.ind[i] <= size_candidate || g2.ind[i] <= size_candidate) next
    
    g1											=	build.block(inner,latent=g1.latent)
    g2											=	build.block(inner,latent=g2.latent)
    
    g1.resp									=	g1$resp
    g1.pred									=	g1$x.block
    
    g2.resp									=	g2$resp
    g2.pred									=	g2$x.block
    
    if (nrow(g1.pred) <= ncol(g1.pred) || nrow(g2.pred) <= ncol(g2.pred)) next
    
    df1	= (nrow(g1.pred)+nrow(g2.pred))-(ncol(g1.pred)+ncol(g2.pred))
    
    reg11		=	stats::lm(g1.resp~g1.pred-1)        	
    SSR11		=	sum(reg11$residuals^2)   
    reg22		=	stats::lm(g2.resp~g2.pred-1)      	
    SSR22		=	sum(reg22$residuals^2)    
    
    SSR1		=	SSR11+SSR22
    
    Fi[i]		    =	((SSR0-SSR1)/(df0-df1))/(SSR1/df1)         
    pval.split	=	stats::pf(Fi,(df0-df1),df1,lower.tail=FALSE)  
    df.num[i]		=	df0-df1
    df.den[i]		=	df1
  }
  
  fun.min 		    = f.min(Fi)
  
  pos.opt			    =	fun.min$p.min 
  pval.opt			  =	pval.split[pos.opt]                           	
  F.opt			      =	Fi[pos.opt]                                           	                        		
  mod.opt			    =	unlist(new.mod[pos.opt])
  df.num.opt		  =	df.num[pos.opt]
  df.den.opt		  = 	df.den[pos.opt] 
  ind1.opt			  =	g1.ind[pos.opt] 
  ind2.opt			  =	g2.ind[pos.opt]
  modtwo.opt	    =	unlist(modtwo.list[pos.opt])
  partition.opt		=	unlist(partition.list[pos.opt])
  
  list(Fi=F.opt,
       pval=pval.opt,
       mod=mod.opt,
       df.num=df.num.opt,
       df.den=df.den.opt,
       g1.ind=ind1.opt,
       g2.ind=ind2.opt,
       modtwo=modtwo.opt,
       partition=partition.opt)
  
}
#' ############################################################################################
#' @title Candidates to the bets partition for each of segmentation variables  
#' @details
#' Internal function
#' @param x matrix or dataframe containing the dataset.
#' @param y matrix or dataframe or vector of the segmentation variables
#' @param inner a square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables)
#' @param .model A description of the user-specified model.
#' @param .scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}.
#' @param .consistent Logical. Should composite/proxy correlations be disattenuated 
#' to yield consistent loadings and path estimates if at least one of the construct 
#' is modeled as a common factor. Defaults to TRUE.
#' @param size_candidate number indicating the minimum threshold for a node.
#' @return a list containing information of the candidates to the optimum partition 
#' for each of segmentation variables  
#' @keywords internal
#' @export
#' 
all_part<- function(x,y,inner,.model,.scheme,.consistent,size_candidate,...) 
{
  part			=	partition(y)
  
  p.bin		  =	list()                                     
  level		  =	list()                                    
  modtwo		=	list() 
  
  length(p.bin)	=	length(level)	=	length(modtwo)	=	ncol(y) 
  
  Ftest      	= NULL
  df0        	= NULL
  df1        	= NULL
  pvl        	= NULL 
  g1.ind    	= NULL
  g2.ind		  = NULL
  
  for (j in 1:ncol(y))
  {                              
    if (is.null(part$split[[j]])) next
    
    jpart	=	splitopt(x,inner,.model,.scheme,.consistent,splits=part$split[[j]],
                     fact=y[,j],size_candidate) 
    
    if (is.null(jpart$pval)) next
    
    p.bin[[j]]			=	jpart$partition                   
    level[[j]]			=	unlist(jpart$mod)
    pvl[j]				  =	jpart$pval
    Ftest[j]				=	jpart$F
    g1.ind[j]				=	jpart$g1.ind
    g2.ind[j]				=	jpart$g2.ind
    modtwo[[j]]			=	jpart$modtwo
    df0[j]				  =	jpart$df.num
    df1[j]				  =	jpart$df.den
  }
  variable					=	names(y)
  
  list(p.bin=p.bin,
       variable=variable,
       level=level,
       Ftest=Ftest,
       pvl=pvl,
       g1.ind=g1.ind,
       g2.ind=g2.ind,
       df0=df0,
       df1=df1,
       modtwo=modtwo)	
}
#' ############################################################################################
#' @title Best partition given a set of segmentation variables
#' @details
#' Internal function
#' @param x matrix or dataframe containing the dataset
#' @param y matrix or dataframe or vector of the segmentation variables
#' @param inner a square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables)
#' @param .model A description of the user-specified model.
#' @param .scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}.
#' @param .consistent Logical. Should composite/proxy correlations be disattenuated 
#' to yield consistent loadings and path estimates if at least one of the construct 
#' is modeled as a common factor. Defaults to TRUE.
#' @param size_candidate number indicating the minimum threshold for a node
#' @return a list containing information of the best partition given a set of 
#' segmentation variables
#' @keywords internal
#' @export
#' 
partopt	<-	function(x,y,inner,.model,.scheme,.consistent,size_candidate)
{
  a.p	=	all_part(x,y,inner,.model,.scheme,.consistent,size_candidate)
  
  if (any(!is.null(a.p$pvl))){
    
    fun.min	=	f.min(a.p$Ftest)
    
    p.bin.opt		  =	a.p$p.bin[[fun.min$p.min]]                  
    level.opt		  =	a.p$level[[fun.min$p.min]] 
    variable.opt	=	a.p$variable[[fun.min$p.min]]  
    pvl.opt		    =	a.p$pvl[fun.min$p.min]
    Ftest.opt		  =	a.p$Ftest[fun.min$p.min]       
    g1.ind.opt	  =	a.p$g1.ind[fun.min$p.min]      
    g2.ind.opt	  =	a.p$g2.ind[fun.min$p.min]       
    modtwo.opt	  =	a.p$modtwo[[fun.min$p.min]]		
    df0.opt		    =	a.p$df0[fun.min$p.min]
    df1.opt		    =	a.p$df1[fun.min$p.min]
    
    variable		=	a.p$variable[fun.min$all.v]   
    Ftest		    =	a.p$Ftest[!is.na(a.p$Ftest)]
    pvl			    =	a.p$pvl[!is.na(a.p$pvl)]
    df0			    =	a.p$df0[!is.na(a.p$df0)]
    df1			    =	a.p$df1[!is.na(a.p$df1)]
    g1.ind		  =	a.p$g1.ind[!is.na(a.p$g1.ind)]
    g2.ind		  =	a.p$g2.ind[!is.na(a.p$g2.ind)]
    
    i = 1
    level = NULL
    
    while (i < length(unlist(a.p$level)))
    {
      level = rbind(level,unlist(a.p$level)[c(i,i+1)])
      i = i+2
    }
    colnames(level)	=	c("levels G1","levels G2")
    
    candidates	=	data.frame(variable,Ftest=round(Ftest,4),pvl=round(pvl,4),df0,df1,g1.ind,g2.ind,level)
    candidates	=	candidates[order(candidates[,2],decreasing=T),]
    
    list(candidates=candidates,
         p.bin.opt=p.bin.opt,
         variable.opt=variable.opt,
         level.opt=level.opt,
         Ftest.opt=Ftest.opt,
         pvl.opt=pvl.opt,
         indg1.opt=g1.ind.opt,
         indg2.opt=g2.ind.opt,
         df0.opt=df0.opt,
         df1.opt=df1.opt,
         modtwo.opt=modtwo.opt)
  }
  else
  {
    list(candidates=NULL,
         p.bin.opt=NULL,
         variable.opt=NULL,
         level.opt=NULL,
         Ftest.opt=NULL,
         pvl.opt=NULL,
         indg1.opt=NULL,
         indg2.opt=NULL,
         df0.opt=NULL,
         df1.opt=NULL,
         modtwo.opt=NULL)	
  }
}
#' ############################################################################################
#' @title Ranking of variables importance 
#' @details
#' Internal function
#' @param x matrix or dataframe containing the data
#' @param y vector or dataframe containing the categorical variables
#' @return a dataframe containg the ranking of the categorical variable
#' @keywords internal
#' @export
#' 

var_imp_mox = function(x,y){
  
  dt_all=list()
  dt_cat = data.frame(variable=names(y))
  
  
  for (i in 1: length(x))
  {
    dt_rank=x[[i]][,1:2]
    dt_merge=merge(dt_cat,dt_rank,by="variable",all.x=TRUE)
    dt_all[[length(dt_all)+1]] = dt_merge
  }
  
  dt = dt_all[[1]][1]
  
  for (i in 1: length(x))
  {
    dt = cbind(dt,dt_all[[i]][,2])
  }
  
  if(length(x)==1){
    var_imp = data.frame(dt[,1],(dt[,-1]))
  }
  else{
    var_imp = data.frame(dt[,1],rowSums(dt[,-1],na.rm=TRUE)/sum(rowSums(dt[,-1],na.rm=TRUE)))
  }
  
  var_imp = var_imp[order(-var_imp[,2]),]
  colnames(var_imp)=c("variable","ranking")
  
  if (y[1,1] == 1){
    var_imp = var_imp[var_imp$variable!="seg",] 
  } 
  else
  {
    
  }
  var_imp
}
#' ############################################################################################
#' @title Check consistence 
#' @details
#' Internal function
#' @param .model a model in lavaan model syntax
#' @param .data a data.frame or a matrix of indicators
#' @return a logical value
#' @keywords internal
#' @export
#'
check_const = function(.model,.data){
  
  ck_const = cSEM::csem( .data=.data, .model=.model)
  
  x1 <- ck_const$Information
  x2 <- ck_const$Estimates  
  
  stat <- c("1" = FALSE, "2" = FALSE, "3" = FALSE, "4" = FALSE, "5" = FALSE)
  
  if(!(is.null(x1$Weight_info$Convergence_status) || x1$Weight_info$Convergence_status)) {
    stat["1"] <- TRUE
  }
  
  if(max(abs(x2$Loading_estimates)) > 1) {
    stat["2"] <- TRUE
  }
  
  if(!matrixcalc::is.positive.semi.definite(x2$Construct_VCV)) {
    stat["3"] <- TRUE
  }
  
  if(max(x2$Reliabilities) > 1) {
    stat["4"] <- TRUE
  }
  
  check = any(stat)==TRUE
  return(check)
  
}
#' ############################################################################################
#' @title Checks arguments  
#' @details
#' Internal function
#' @param .model a model in lavaan model syntax
#' @param .data a data.frame or a matrix of indicators
#' @param .catvar a vector or dataframe containing the categorical variables
#' @param .scheme Character string. Approach used to obtain composite weights
#' @param .consistent Logical. Should composite/proxy correlations be disattenuated
#' @param .alpha minimum threshold of f-test p-value
#' @param .deep  minimum threshold of deep tree
#' @param .size minimum threshold size node
#' @param .size_candidate minimum size_candidate node threshold
#' @param .tree Logical.Should the tree plot printed
#' @return a list checked arguments
#' @keywords internal
#' @export
#' 
check_arg_mox = function(.model, .data, .catvar, .scheme, .consistent, .alpha, .deep, .size,
                         .size_candidate, .tree)
  
{
  
  if (is.null(.catvar)) {
    stop("Argument '.catvar' is missing. No categorical variables have been provided.")
  }
  
  if(is.null(ncol(.catvar))==TRUE || ncol(.catvar)==1){
    
    cl <-match.call()
    cl2=substr(cl,0,nchar(cl))
    .catvar=data.frame(seg=factor(1),.catvar)
    names(.catvar)[2]=cl2[4]
    .catvar
  }
  
  if (is.null(.model)) {
    stop("Argument '.model' is missing with no default.")
  }
  
  if (is.null(.data)) {
    stop("Argument '.data' is missing. No dataset has been provided.")
  }
  if (!is.null(.data)) {
    if (!is.matrix(.data) && !is.data.frame(.data)) 
      stop("Invalid object '.data'. Must be a '.data' dataframe or a matrix.")
  }
  if (length(grep("\\.", colnames(.data))) > 0) {
    stop("At least one variable name in your dataset contain a `.` (dot).", 
         " Dots are a reserved special character in 'pls.pathmox' Please rename these variables 
         in your data and in the model description.")
  }
  if (any(is.na(.data)))
  {
    stop("Argument '.data' contains NA: please imput or drop it.")
  }
  if (any(is.na(.catvar)))
  {
    stop("Argument '.catvar' contains NA: please imput or drop it.")
  }             
  if (nrow(.data) != nrow(.catvar)) 
    stop("Arguments '.data' and '.catvar' are incompatible. Different number of rows.")
  
  for (j in 1:ncol(.catvar)) if (!is.factor(.catvar[, j])) 
    stop("One or more columns in '.catvar' are not factors.")
 
   if (!.scheme %in% c("path", "centroid", "factorial"))
   {
     warning("NOTICE: Invalid argument '.scheme'. Default value 'path' was used.", 
             "\n") 
     .scheme = "path"
   } 
  if(!.consistent %in% c("FALSE", "TRUE")){
      warning("NOTICE: Invalid argument '.consistent'.It must be logical value. Default value 'TRUE' was used.", 
          "\n") 
     .consistent = TRUE
  }
  if(check_const(.data  = .data, .model = .model)== TRUE)
  {
      warning("NOTICE: Results exhibiting one of the following defects are deemed inadmissible: 
                non-convergence of the algorithm used to obtain weights, loadings and/or (congeneric) 
                reliabilities larger than 1, a construct variance-covariance (VCV) and/or model-implied VCV 
                matrix that is not positive semi-definite. Clasical PLS-SEM algorithm was used.",
                "\n") 
      .consistent = FALSE
  }
  
  if (mode(.alpha) != "numeric" || length(.alpha) != 1 || .alpha <= 
     0 || .alpha >= 1) 
  {
      warning("NOTICE: Invalid argument '.alpha'. Default value 0.05 was used.", 
          "\n")
      .alpha <- 0.05
  }
  if (mode(.size) != "numeric" || length(.size) !=1 || .size <= 
    0 || .size >=1) 
  {
      warning("NOTICE: Invalid argument '.size'. Default value 0.10 was used.", 
          "\n")
    .size <- 0.1
  }
  if (mode(.deep) != "numeric" ||  .deep < 
    1 || (.deep%%1) != 0) 
  {
      warning("NOTICE: Invalid argument '.deep'. Default value 2 was used.", 
          "\n")
    .deep <- 2
  }
  if (mode(.size_candidate) != "numeric" || .size_candidate > nrow(.data) || .size_candidate <= 
    0) 
  {
      warning("NOTICE: Invalid argument '.size_candidate'. Default value 50 was used.", 
          "\n")
    .size_candidate <- 50
  }
  res = list(model=.model, data=.data, catvar=.catvar, scheme=.scheme, consistent=.consistent, alpha =.alpha, 
             deep = .deep, size = .size, size_candidate = .size_candidate, tree = .tree)
  return(res)
}










  
  