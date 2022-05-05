#' ############################################################################################
#' @title Data preprocessing for F-tests
#' @details
#' Internal function
#' @param x matrix or dataframe containing the data.
#' @param inner a square (lower triangular) boolean matrix representing the inner model
#' @param outer list of vectors with column indices indicating which manifest variables 
#' correspond to each block
#' @param mode character vector indicating the type of measurement for each
#' block. Possible values are: \code{"A", "B"}
#' @param scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or \code{"path"}.
#' @param modtwo vector indicating the binary partition
#' @return list containing matrices needed for the tests
#' @keywords internal
#' @export
#' 
F.data	<-	function(x,inner,outer,mode,scheme,modtwo)
{
  pls 		=	pls(x,inner,outer,mode,scheme)
  
  LV      = pls$scores
  
  path.name		=	element(inner)
  
  global		  =	build.block(inner=inner,latent=LV)
  
  Y0			    =	global$resp
  X0			    =	global$x.block
  info.block	=	global$constructs.label
  
  g1.latent = subset(LV,modtwo==1)                    
  g2.latent = subset(LV,modtwo==2)
  
  g1=build.block(inner,latent=g1.latent)
  g2=build.block(inner,latent=g2.latent)
  
  g1.resp	=	g1$resp
  g1.pred	=	g1$x.block
  
  g2.resp	=	g2$resp
  g2.pred	=	g2$x.block
  
  Y.alt	=	rbind(g1.resp,g2.resp)
  X.alt	=	blockdiag(g1.pred,g2.pred)
  
  n.col	=	ncol(X.alt)/2
  
  colnames(X.alt)[1:n.col]	=	paste("g1 -",colnames(X0),sep=" ")
  colnames(X.alt)[(n.col+1):(2*n.col)]	=	paste("g2 -",colnames(X0),sep=" ")
  
  list(Y0=Y0,
       X0=X0,
       Y1=Y.alt,
       X1=X.alt,
       path.name=path.name,
       info.block=info.block)	
}
#' ############################################################################################
#' @title F-global test
#' @details
#' Internal function
#' @param Y0 vector of the concatenate indipendent latent variables of H0 hypothesis global 
#' test
#' @param X0 matrix of the concatenate predictor latent variables of H0 hypothesis global test
#' @param Y1 vector of the concatenate indipendent latent variables of H1 hypothesis global 
#' test
#' @param X1 matrix of the concatenate predictor latent variables of H1 hypothesis global test
#' @return a list containing the statistic and the p-value obtained by applying F-global test
#' @keywords internal
#' @export
#' 
Fg.test	<-	function(Y0,X0,Y1,X1)
{
  reg0	=	stats::lm(Y0~X0-1)                   	
  SSR0	=	sum(reg0$residuals^2)    
  df0	=	(nrow(X0) - ncol(X0))
    
  reg1	=		stats::lm(Y1~X1-1)                   	
  SSR1	=	sum(reg1$residuals^2)    
  df1	=	(nrow(X1) - ncol(X1))                        
 
  Fg		=	((SSR0-SSR1)/(df0-df1))/(SSR1/df1)             
  pval.g	=		stats::pf(Fg,(df0-df1),df1,lower.tail=FALSE)     
  
  list(Fg=round(Fg,4) ,pvg=round(pval.g,4))
}
#' ############################################################################################
#' @title F-coefficient test 
#' @details
#' Internal function
#' @param Y1 vector of the concatenate indipendent latent variables of H1 hypothesis 
#' of the F-coefficient test
#' @param X1 matrix of the concatenate predictor latent variables of H1 hypothesis 
#' of the F-coefficient test
#' @param path.name vector of path coefficients labels
#' @param info.block list contaning information about the endogenous equations of the pls model
#' @return a list containing the statistic and the p-value obtained by applying the F-coefficient 
#' test
#' @keywords internal
#' @export
#' 
Fc.test	<-	function(Y1,X1,path.name,info.block)
{
  Fc			    =	NULL
  pval.c		  =	NULL
  new.Fc		  =	list()
  new.pval.c	=	list()
  k			      =	ncol(X1)/2
  p.name      = list()
  
  reg1	=		stats::lm(Y1~X1-1)                   	
  SSR1	=	sum(reg1$residuals^2)    
  df1	=	(nrow(X1) - ncol(X1))                                                

  for (j in 1:k)
  {
    
    A		= X1	      
    A[,j]	= as.matrix(A[,j]+A[,j+k])         
    X1.c		= A[,-(j+k)]

    df0.c	= (nrow(X1.c)-ncol(X1.c))                             
    SSR0.c	= sum(	stats::lm(Y1~X1.c-1)$residuals^2)
    
    Fc[j]	 =	((SSR0.c-SSR1)/(df0.c-df1))/(SSR1/df1)             
    pval.c[j]	 =		stats::pf(Fc[j],(df0.c-df1),df1,lower.tail=FALSE)     
  }
  
  for (j in 1:length(info.block))
  {
    p.name[[length(p.name)+1]]	=	paste(info.block[[j]],"->",names(info.block)[j])
  }
  
  names(Fc)=names(pval.c)=unlist(p.name)
  
  list(Fc=round(Fc[!substr(names(Fc),1,3) %in% c("int")],4),
       pvc=round(pval.c[!substr(names(pval.c),1,3) %in% c("int")],4))
}
#' ############################################################################################
#' @title Cheking F-tests results 
#' @details
#' Internal function
#' @param x matrix or dataframe containing the data
#' @param inner a square (lower triangular) boolean matrix representing the inner model
#' @param outer list of vectors with column indices indicating which manifest variables 
#' correspond to each block
#' @param mode character vector indicating the type of measurement for each
#' block. Possible values are: \code{"A", "B"} 
#' @param scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or \code{"path"}
#' @param modtwo vector indicating the binary partition
#' @param signific value indicating the threshold a stop condition
#' @return list containing matrices needed for the comparison test
#' @keywords internal
#' @export
#' 
test.partition <- function(x,inner,outer,mode,scheme,modtwo,signif) 
{
  d.info	=	F.data(x,inner,outer,mode,scheme,modtwo)
  
  FG		  =	Fg.test(d.info$Y0,d.info$X0,d.info$Y1,d.info$X1)
  
  if(FG$pvg > signif)
  {
    list(Fg=FG$Fg ,pvg=FG$pvg,Fc=list(),pvc=list())
  }
  else
  {
    
    FC	=	Fc.test(d.info$Y1,d.info$X1,d.info$path.name,d.info$info.block)
    
    list(Fg=FG$Fg ,pvg=FG$pvg,Fc=FC$Fc,pvc=FC$pvc) 	
  }
}
