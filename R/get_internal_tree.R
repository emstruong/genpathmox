#' ############################################################################################
#' @title Calculating size (numeber of individual of a node)
#' @details
#' Internal function
#' @param x matrix or dataframe with data.
#' @param size value indicating the minimun threshold of number of observations for a node
#' @return the number of observations in a node
#' @keywords internal
#' @export
#'
percent.node	<-	function(x,size)
{
  indiv		=	nrow(x)
  min.n.ind 	= trunc(indiv*size)

  list(min.n.ind=min.n.ind)
}
#' ############################################################################################
#' @title Calculating Deepth stop criterion
#' @details
#' Internal function
#' @param node id that identifies a specicif node
#' @return deepth of the tree
#' @keywords internal
#' @export
#'
showDeepth=function(node)
{
  return (trunc(log2(node@id)))
}
#' ############################################################################################
#' @title Observations belonging to the root node
#' @details
#' Internal function
#' @param moxtree class containing the moxtree elements
#' @return the observations belonging to the root node
#' @keywords internal
#' @export
#'
root.tree	<-	function(moxtree)
{
  root = NULL

  for (n in moxtree@nodes)
  {
    if (n@id == 1)
    {
      root=n@elements
    }
  }
  root
}
#' ############################################################################################
#' @title Observations belonging to the terminal nodes
#' @details
#' Internal function
#' @param moxtree class containing the moxtree element.
#' @return the observations belonging to the terminal nodes
#' @keywords internal
#' @export
#'
terminal.tree	<- function(moxtree)
{
  terminal 	= list()
  id 			= list()

  if (length(moxtree@nodes) > 1)
  {
    for (n in moxtree@nodes)
    {
      if (n@id == 1)
      {
        terminal[[length(terminal)+1]] = n@elements
        id[[length(id)+1]] = "Root"
      }
      if (length(n@childs) == 0)
      {
        terminal[[length(terminal)+1]] = n@elements
        id[[length(id)+1]] = n@id
      }
    }
    for (i in 1:length(terminal)){names(terminal) = paste("node",id)}

    terminal
  }
  else
  {
    terminal = NULL
  }
  terminal
}
#' ############################################################################################
#' @title Observations belonging to the nodes
#' @details
#' Internal function
#' @param moxtree class containing the moxtree elements
#' @return the observations belonging to the nodes
#' @keywords internal
#' @export
#'
nodes.tree	<-	function(moxtree)
{
  nodes 	= list()
  id 		= list()

  if (length(moxtree@nodes) > 1)
  {
    for (n in moxtree@nodes)
    {
      nodes[[length(nodes)+1]] = n@elements
      id[[length(id)+1]] = n@id
    }
    for (i in 1:length(nodes))	{names(nodes) = paste("node",id)}

    nodes
  }
  else
  {
    nodes = NULL
  }
  nodes
}
#' ############################################################################################
#' @title Posibble partions for each node of the tree
#' @details
#' Internal function
#' @param moxtree class containing the moxtree elements
#' @return the Posibble partions for each node of the tree
#' @keywords internal
#' @export
#'
candidates.tree	<-	function(moxtree)
{
  candidates = list()
  id = list()

  if (length(moxtree@nodes) > 1)
  {
    for (n in moxtree@nodes)
    {
      if (length(n@childs)>0)
      {
        candidates[[length(candidates)+1]] = n@info@candidates
        id[[length(id)+1]] = n@id
      }
    }
    for (i in 1:length(candidates))	{names(candidates) = paste("node",id)}

    candidates
  }
  else
  {
    candidates = NULL
  }
  candidates
}
#' ############################################################################################
#' @title F-global test results for each tree partition
#' @details
#' Internal function
#' @param moxtree class containing the moxtree elements
#' @return the F-global test results for each tree partition
#' @keywords internal
#' @export
#'
fglobal.tree	<-	function(moxtree)
{
  fglobal = list()
  fgtable = NULL

  if (length(moxtree@nodes) > 1)
  {
    for (n in moxtree@nodes)
    {
      if (length(n@childs) > 0)
      {
        fglobal[[length(fglobal)+1]] = data.frame(n@id,n@info@fgstatistic,n@info@fpvalg,n@info@variable,t(n@info@level))
      }
    }

    for (i in 1:length(fglobal)) {fgtable = rbind(fgtable,fglobal[[i]])}

    colnames(fgtable)	=	c("node","F value","Pr(>F)","variable","g1.mod","g2.mod")

    Fg.r = fgtable
  }
  else
  {
    Fg.r = NULL
  }
  Fg.r
}
#' ############################################################################################
#' @title F-coefficients test results for each tree partition
#' @details
#' Internal function
#' @param moxtree class containing the moxtree elements
#' @return the F-coefficients test results for each tree partition
#' @keywords internal
#' @export
fcoef.tree	<-	function(moxtree)
{
  fc		= list()
  id		= list()
  fctable	= NULL
  if (length(moxtree@nodes) > 1)
  {
    for (n in moxtree@nodes)
    {
      if (length(n@childs) > 0)
      {
        id[[length(id)+1]] = n@id
        fctable	=	data.frame(as.matrix(n@info@fcstatistic),as.matrix(n@info@fpvalc))
        colnames(fctable)	= c("F value","Pr(>F)")
        fc[[length(fc)+1]]	= fctable
      }
    }

    names(fc) = paste("node",id,sep="")
    Fc.r = fc
  }
  else
  {
    Fc.r=list(fc=NULL,Signif=NULL)
  }
  Fc.r
}
#' ############################################################################################
#' @title General information about the tree
#' @details
#' Internal function
#' @param moxtree class containing the tree elements
#' @return a dataframe containing information about the tree and its nodes
#' @keywords internal
#' @export
#'
mox.tree	<-	function(moxtree)
{
  info.node		= list()
  type			= NULL
  terminal		= NULL
  perc			= NULL
  var			= NULL
  mox			= NULL
  if	(length(moxtree@nodes)>1)
  {
    for (n in moxtree@nodes)
    {
      if (n@id == 1)
      {
        length.root = length(n@elements)
      }
      if (length(n@childs) > 0)
      {
        info.node[[length(info.node)+1]] = data.frame(n@info@variable,n@id,n@childs,n@info@level)
      }
      if	(length(n@childs) == 0)
      {
        type		= "least"
        terminal	= "yes"
      }
      if	(n@father == 0)
      {
        type		= "root"
        terminal	= "no"
      }
      if	(n@father!=0 && length(n@childs) != 0)
      {
        type		= "node"
        terminal	= "no"
      }
      perc = round((length(n@elements)/length.root)*100,2)
      data = data.frame(n@id,n@father,showDeepth(n),type,terminal,length(n@elements),perc)
      mox = rbind(mox,data)
    }

    data.info.node = NULL

    for (i in 1:length(info.node)) {data.info.node = rbind(data.info.node,info.node[[i]])}

    names(data.info.node)[2] = "n.father"
    names(data.info.node)[3] = "n.id"

    MOX =merge (mox, data.info.node,by="n.id",all.x=TRUE)[,-9]

    names(MOX) = c("node","parent","depth","type","terminal","size","%","variable","category")

    MOX
  }
  else
  {
    MOX = NULL
  }
  MOX
}
#' ############################################################################################
#' @title General information about the pathmox algorithm
#' @details
#' Internal function
#' @param signif stop condition 1: significance of the p-value
#' @param size stop condition 2: minimum number of individuals in a node
#' @param deep stop condition 3: maximum tree depth level
#' @param y set of segmentation variables
#' @keywords internal
#' @export
info.mox	<-	function(signif,size,deep,y)
{
  cat("\n")
  cat("PLS-SEM PATHMOX ANALYSIS","\n")
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("Info parameters algorithm","\n")
  info.value = rbind(signif,size,deep)
  dimnames(info.value) = NULL
  info.name = c("threshold signif.","node size limit(%)","tree depth level")
  info.tree = data.frame(info.name,info.value)
  names(info.tree) = c("parameters algorithm", "value")
  print(info.tree)
  cat("\n")
  cat("---------------------------------------------")
  cat("\n")
  cat("Info segmentation variables","\n")
  type.y = rep(0, ncol(y))
  treat.y = rep("binary", ncol(y))
  for (i in 1:length(type.y))
  {
    type.y[i] = ifelse(is.ordered(y[, i]), "ord","nom")

    if (nlevels(y[, i]) > 2)
      if (is.ordered(y[, i]))
        treat.y[i] = "ordinal"
      else treat.y[i] = "nominal"
  }
  df.y = data.frame(nlevels = unlist(lapply(y, nlevels)),ordered = unlist(lapply(y, is.ordered)),
                    treatment = treat.y)
  if (y[1,1] == 1){
    df.y = df.y[-1,]
  }
  else
  {
    df.y
  }
  print(df.y)

}
#' ############################################################################################
#' @title printing the tree structure
#' @details
#' Internal function.
#' @param moxtree moxtree object
#' @return the tree structure
#' @keywords internal
#' @export
#'
printTree	<-	function(moxtree)
{
  for (n in moxtree@nodes){
    print (n)
  }
}
