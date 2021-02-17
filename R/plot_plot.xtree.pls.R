#' @title Plot function for the Pathmox Segmentation Trees: PLS-PM
#' 
#' @title Plot PATHMOX-PLS
#' 
#' @description
#' The function \code{plot.xtree.pls} allows to drow PATHMOX tree for PLS-SEM
#' 
#' 
#' @param xtree An object of class \code{"xtree.pls"} returned by 
#' \code{\link{pls.pathmox}}
#' @param root.col Fill color of root node.
#' @param node.col Fill color of child nodes.
#' @param leaf.col Fill color of leaf.
#' @param shadow.size Relative size of shadows.
#' @param node.shadow Color of shadow of child nodes.
#' @param leaf.shadow Color of shadow of leaf nodes.
#' @param cex A numerical value indicating the magnification to be used for
#' plotting text.
#' @param seg.col The color to be used for the labels of the segmentation
#' variables.
#' @param lwd The line width, a positive number, defaulting to 1
#' @param show.pval Logical value indicating whether the p-values should be
#' plotted.
#' @param pval.col The color to be used for the labels of the p-values.
#' @param main A main title for the plot.
#' @param cex.main The magnification to be used for the main title.
#' @param \dots Further arguments passed on to \code{\link{plot.xtree.pls}}. 
#' @method plot xtree.pls
#' @S3method plot xtree.pls
#' @examples
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
#' plot(bank.pathmox)
#'  
#'  }

plot.xtree.pls	<-	function (xtree, root.col = "grey", node.col = "orange", leaf.col = "green2", 
                            shadow.size = 0.003, node.shadow = "red", leaf.shadow = "darkgreen", 
                            cex = 0.7, seg.col = "blue3", lwd = 1, show.pval = TRUE, 
                            pval.col = "blue", main = NULL, cex.main = 1, ...) 
{
  MOX <- xtree$MOX
  last <- nrow(MOX)
  last.level <- MOX$Depth[last]
  num.levels <- rep(1, last.level + 1)
  for (i in 1:last.level) num.levels[i + 1] <- 2^i
  dev.new()
  par(mar = c(0.4, 0.4, 1, 1.5))
  openplotmat()
  elpos <- coordinates(num.levels)
  fromto <- cbind(MOX[-1, 2], MOX[-1, 1])
  nr <- nrow(fromto)
  arrpos <- matrix(ncol = 2, nrow = nr)
  for (i in 1:nr) arrpos[i, ] <- straightarrow(to = elpos[fromto[i, 
                                                                 2], ], from = elpos[fromto[i, 1], ], lwd = lwd, arr.pos = 0.6, 
                                               arr.length = 0)
  textellipse(elpos[1, ], 0.045, 0.045, lab = c("Root", MOX[1, 
                                                            6]), box.col = root.col, shadow.size = shadow.size, cex = cex)
  for (i in 2:last) {
    posi <- MOX$Node[i]
    nodlab <- c(paste("Node", posi), MOX$Size[i])
    if (MOX$Type[i] == "node") {
      textellipse(elpos[posi, ], 0.05, 0.03, lab = nodlab, 
                  box.col = node.col, shadow.col = node.shadow, 
                  shadow.size = shadow.size, cex = cex)
    }
    else {
      textrect(elpos[posi, ], 0.045, 0.025, lab = nodlab, 
               box.col = leaf.col, shadow.col = leaf.shadow, 
               shadow.size = shadow.size, cex = cex)
    }
  }
  aux <- 1
  for (i in seq(1, nr, by = 2)) {
    if (i == 1) 
      k <- 1
    else k <- 1.15
    x1 <- (arrpos[i, 1] + arrpos[i + 1, 1])/2
    text(x1, k * arrpos[i, 2], MOX$Variable[i + 1], cex = cex, 
         col = seg.col)
    if (show.pval) {
      text(x1, k * arrpos[i, 2], paste("p.val=", round(xtree$Fg.r$fg.pvalue[aux], 
                                                       4), sep = ""), cex = 0.9 * cex, col = pval.col, 
           pos = 1)
    }
    aux <- aux + 1
  }
  for (i in 1:nr) {
    posi <- MOX$Node[i + 1]
    seg.cat <- as.character(MOX$Category[i + 1])
    seg.cat <- unlist(strsplit(seg.cat, "/"))
    if (posi%%2 == 0) {
      for (h in 1:length(seg.cat)) text(arrpos[i, 1] - 
                                          0.03, arrpos[i, 2] + h/55, seg.cat[h], cex = cex)
    }
    else {
      for (h in 1:length(seg.cat)) text(arrpos[i, 1] + 
                                          0.03, arrpos[i, 2] + h/55, seg.cat[h], cex = cex)
    }
  }
  if (is.null(main)) {
    text(0.5, 0.95, c("PATHMOX Tree"), cex = cex.main)
  }
  else {
    text(0.5, 0.95, main, cex = cex.main)
  }
}