#' ############################################################################################
#' @title Calculate inner weighting path scheme
#' 
#' @details
#' Internal function
#' 
#' @param inner matrix with paths
#' @param Y matrix of latent score
#' @keywords internal
#' @export
#' 
get_path_scheme <-
  function(inner, Y)
  {
    lvs <- nrow(inner)
    E <- inner
    for (k in 1:lvs) 
    {
      if (length(which(inner[k,]==1)) > 0)
        E[which(inner[k,]==1),k] <- 	stats::lm(Y[,k]~Y[,which(inner[k,]==1)]-1)$coef
      if (length(which(inner[,k]==1)) > 0)
        E[which(inner[,k]==1),k] <- 	stats::cor(Y[,k], Y[,which(inner[,k]==1)])
    }                 
    return(E)
  }
#' ############################################################################################
#' @title Outer Weights
#' 
#' @details
#' Internal function
#' 
#' @note
#' Calculate outer weights (under Lohmoller's algorithm)
#' 
#' @param X scaled data
#' @param inner matrix with path connections
#' @param blocks list with variables in each block
#' @param modes character vector indicating the type of measurement for each
#' block. Possible values are: \code{"A", "B"} 
#' The length of \code{modes} must be equal to the length of \code{blocks}
#' @param scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}
#' @param tol decimal value indicating the tolerance criterion for the
#' iterations (\code{tol=0.000001}). Can be specified between 0 and 0.001
#' @param iter integer indicating the maximum number of iterations
#' (\code{maxiter=100} by default). The minimum value of \code{maxiter} is 100
#' @return a vector with outer weights
#' @keywords internal
#' @export
#' 
pls_weights <-
  function(X, inner, blocks, modes, scheme, tol, iter)
  {
    lvs = nrow(inner)
    mvs = ncol(X)     
    sdv = sqrt((nrow(X)-1) / nrow(X))
    blocklist <- as.list(1:lvs)
    for (j in 1:lvs) blocklist[[j]] = rep(j, blocks[j])
    blocklist = unlist(blocklist)
    ODM = matrix(0, mvs, lvs)
    for (j in 1:lvs) ODM[which(blocklist==j),j] = rep(1, blocks[j])
    
    W = ODM %*% diag(1/(apply(X%*%ODM, 2, 	stats::sd)*sdv), lvs, lvs)          
    
    w.old = rowSums(W)    
    w.dif <- itermax <- 1
    repeat 
    {            
      Y = X %*% W  
      Y =  scale(Y) * sdv
      # matrix of inner weights 'e' 
      E <- switch(scheme, 
                  "centroid" = sign(cor(Y) * (inner + t(inner))),
                  "factor" = 	stats::cor(Y) * (inner + t(inner)),
                  "path" = get_path_scheme(inner, Y))
      Z = Y %*% E  # internal estimation of LVs 'Z'
      Z = Z %*% diag(1/(apply(Z,2,	stats::sd)*sdv), lvs, lvs)  
      # computing outer weights 'w'
      for (j in 1:lvs)
      {
        X.blok = X[,which(blocklist==j)]  
        if (modes[j]=="A")# reflective way
          ODM[which(blocklist==j),j] <- (1/nrow(X)) * Z[,j] %*% X.blok 
        
        
        if (modes[j]=="B")# formative way
          ODM[which(blocklist==j),j] <- solve.qr(qr(X.blok), Z[,j])
        
      }
      W = ODM
      w.new = rowSums(W)                
      w.dif = sum((abs(w.old) - abs(w.new))^2)  
      if (w.dif < tol || itermax == iter) break
      w.old = w.new
      itermax = itermax + 1
    } # end repeat       
    W = ODM %*% diag(1/(apply(X %*% ODM, 2, 	stats::sd)*sdv), lvs, lvs) 
    
    w.new = rowSums(W)                
    names(w.new) = colnames(X) 
    dimnames(W) = list(colnames(X), rownames(inner))      
    res.ws = list(w.new, W, itermax)
    if (itermax == iter) res.ws = NULL
    return(res.ws)
  }
#' ############################################################################################
#' @title Calculate path coefficients for \code{pls}
#' 
#' @details
#' Internal function. 
#' 
#' @param inner path matrix
#' @param Y_lvs matrix of latent variables
#' @return a matrix with path coefs and R2
#' @keywords internal
#' @export
#' 
pls_paths <-  function(inner, Y_lvs)
{
  
  lvs_names = colnames(inner)
  endogenous = as.logical(rowSums(inner))
  num_endo = sum(endogenous)
  Path = inner
  r2 = matrix(0,num_endo) 
  k_names =rep(0,num_endo)
  
  for (aux in 1:num_endo) 
  {
    # index for endo LV
    k1 <- which(endogenous)[aux]
    k_names[aux] = lvs_names[endogenous][aux]
    # index for indep LVs
    k2 = which(inner[k1,] == 1)
    # elimnate warnings
    path= summary(	stats::lm(Y_lvs[,k1] ~ Y_lvs[,k2]))
    Path[k1,k2] = path$coef[-1,1]
    r2[aux,] = path$adj.r.squared
  }
  
  paths = as.matrix(Path[which(inner==1)])
  rownames(paths) = element(inner,paths)
  rownames(r2)=paste("R^2", k_names)
  inner_res = rbind(paths,r2) 
  
  # output
  inner_res
}
#' ############################################################################################
#' @title PLS-SEM: Partial Least Squares Structural Equation Modeling
#'
#' @description
#' Estimate score and path coefficient applying a partial least squares approach
#'
#' @details
#' Internal function
#'
#' @param x matrix or data frame containing the manifest variables
#' @param inner a square (lower triangular) boolean matrix representing 
#' the inner model (i.e. the path relationships between latent variables)
#' @param outer list of vectors with column indices or column names
#' from \code{Data} indicating the sets of manifest variables forming 
#' each block (i.e. which manifest variables correspond to each block)
#' @param modes character vector indicating the type of measurement for each
#' block. Possible values are: \code{"A", "B"} 
#' The length of \code{modes} must be equal to the length of \code{blocks}
#' @param scheme string indicating the type of inner weighting
#' scheme. Possible values are \code{"centroid"}, \code{"factorial"}, or
#' \code{"path"}
#' @param tol decimal value indicating the tolerance criterion for the
#' iterations (\code{tol=0.000001}). Can be specified between 0 and 0.001
#' @param iter integer indicating the maximum number of iterations
#' (\code{maxiter=100} by default). The minimum value of \code{maxiter} is 100
#' 
#' @return a list with score, path coefs and R2
#' @keywords internal
#' @export
pls<-
  function(x, inner, outer, modes, scheme="path", 
           scaled = TRUE, tol = 0.00001, iter = 100,...)
  {
    
    lvs.names = rownames(inner)
    dimnames(inner) = list(lvs.names, lvs.names)
    lvs = nrow(inner)
    blocks = unlist(lapply(outer, length))
    mvs = sum(blocks)
    names(blocks) = lvs.names
    blocklist = outer
    for (k in 1:length(outer))
      blocklist[[k]] <- rep(k,blocks[k])
    blocklist = unlist(blocklist)
    Mode = modes
    Mode[modes=="A"] = "Reflective"
    Mode[modes=="B"] = "Formative"   
    DM = matrix(NA, nrow(x), mvs)
    mvs.names = rep(NA, mvs)
    for (k in 1:lvs)
    {        
      DM[,which(blocklist==k)] = as.matrix(x[,outer[[k]]]) 
      mvs.names[which(blocklist==k)] = colnames(x)[outer[[k]]] 
    }
    dimnames(DM) = list(rownames(x), mvs.names) 
    
    sd.X = sqrt((nrow(DM)-1)/nrow(DM)) * apply(DM, 2, 	stats::sd)
    X = scale(DM, scale = sd.X)
    
    dimnames(X) = list(rownames(x), mvs.names)
    
    out.ws <- pls_weights(X, inner, blocks, modes, scheme, tol, iter)
    if (is.null(out.ws)) {
      print(paste("Iterative process is non-convergent with 'iter'=", 
                  iter, " and 'tol'=", tol, sep=""))
      stop("Algorithm stops") 
    }
    out.weights = out.ws[[1]]
    cor.XY = 	stats::cor(X, X%*%out.ws[[2]])
    
    w.sig = rep(NA, lvs)
    for (k in 1:lvs) 
      w.sig[k] <- ifelse(sum(sign(cor.XY[which(blocklist==k),k]))<=0,-1,1)
    Y.lvs = X %*% out.ws[[2]] %*% diag(w.sig, lvs, lvs)
    dimnames(Y.lvs) = list(rownames(X), lvs.names)
    
    pathmod = pls_paths(inner, Y.lvs)
    Path = pathmod
    res = list(scores = Y.lvs, path=Path)
    class(res) = c("pls")
    return(res)
  }
