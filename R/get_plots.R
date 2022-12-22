#' @title Plot function for the pathmox segmentation tree
#' 
#' @description
#' The function \code{plot.plstree} allows to drow PATHMOX tree
#' 
#' @param x An object of the class \code{"plstree"}.
#' @param .root.col Fill color of root node.
#' @param .node.col Fill color of child nodes.
#' @param .leaf.col Fill color of leaf.
#' @param .shadow.size Relative size of shadows.
#' @param .node.shadow Color of shadow of child nodes.
#' @param .leaf.shadow Color of shadow of leaf nodes.
#' @param .cex A numerical value indicating the magnification to be used for
#' plotting text.
#' @param .seg.col The color to be used for the labels of the segmentation
#' variables.
#' @param .lwd The line width, a positive number, defaulting to 1.
#' @param .show.pval Logical value indicating whether the p-values should be
#' plotted.
#' @param .pval.col The color to be used for the labels of the p-values.
#' @param .main A main title for the plot.
#' @param .cex.main The magnification to be used for the main title.
#' @param \dots Further arguments passed on to \code{\link{plot.plstree}}.
#' 
#' @author Giuseppe Lamberti
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
#' @seealso \code{\link{summary.plstree}}, \code{\link{print.plstree}}, \code{\link{pls.pathmox}},  
#' \code{\link{bar_terminal}}, and \code{\link{bar_impvar}}
#' 
#'@exportS3Method plot plstree
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
#' # Define the model using the lavaan syntax. Use a set of regression formulas to define
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
#' # Identify the categorical variable to be used as input variables 
#' in the split process
#' CSIcatvar = csibank[,1:5]
#' 
#' # Check if variables are well specified (they have to be factors 
#' # and/or ordered factors)
#' str(CSIcatvar)
#'
#' # Transform age and education into ordered factors
#' CSIcatvar$Age = factor(CSIcatvar$Age, levels = c("<=25", 
#'                                      "26-35", "36-45", "46-55", 
#'                                      "56-65", ">=66"),ordered = T)
#'
#' CSIcatvar$Education = factor(CSIcatvar$Education, 
#'                             levels = c("Unfinished","Elementary", "Highschool",
#'                             "Undergrad", "Graduated"),ordered = T)
#'        
#' # Run Pathmox analysis (Lamberti et al., 2016; 2017)
#' csi.pathmox = pls.pathmox(
#'  .model = CSImodel ,
#'  .data  = csibank,
#'  .catvar= CSIcatvar,
#'  .alpha = 0.05,
#'  .deep = 2
#' )                     
#'
#' Visualize the tree
#' plot(csi.pathmox)
#'
#' }
#'
plot.plstree	=	function (x, .root.col = "#CCFFFF", .node.col = "#99CCCC", .leaf.col = "#009999", 
                        .shadow.size = 0.003, .node.shadow = "#669999", .leaf.shadow = "#006666", 
                        .cex = 0.7, .seg.col = "#003333", .lwd = 1, .show.pval = TRUE, 
                        .pval.col = "#009999", .main = NULL, .cex.main = 1, ...) 
{
  
  .MOX = x$MOX
  .last = nrow(.MOX)
  .last.level = .MOX$depth[.last]
  .num.levels = rep(1, .last.level + 1)
  for (i in 1: .last.level) .num.levels[i + 1] <- 2^i
  grDevices::dev.new()
  graphics::par(mar = c(0.4, 0.4, 1, 1.5))
  diagram::openplotmat()
  .elpos = diagram::coordinates(.num.levels)
  .fromto = cbind(.MOX[-1, 2], .MOX[-1, 1])
  .nr = nrow(.fromto)
  .arrpos = matrix(ncol = 2, nrow = .nr)
  for (i in 1: .nr) .arrpos[i, ] = diagram::straightarrow(to = .elpos[.fromto[i, 
                                                                 2], ], from = .elpos[.fromto[i, 1], ], lwd = .lwd, arr.pos = 0.6, 
                                               arr.length = 0)
  diagram::textellipse(.elpos[1, ], 0.045, 0.045, lab = c("Root", .MOX[1, 
                                                            6]), box.col = .root.col, shadow.size = .shadow.size, cex = .cex,font=2)
  for (i in 2: .last) {
    .posi <- .MOX$node[i]
    .nodlab <- c(paste("Node", .posi), .MOX$size[i])
    if (.MOX$type[i] == "node") {
      diagram::textellipse(.elpos[.posi, ], 0.05, 0.03, lab = .nodlab, 
                  box.col = .node.col, shadow.col = .node.shadow, 
                  shadow.size = .shadow.size, cex = .cex)
    }
    else {
      diagram::textrect(.elpos[.posi, ], 0.045, 0.025, lab = .nodlab, 
               box.col = .leaf.col, shadow.col = .leaf.shadow, 
               shadow.size = .shadow.size, cex = .cex)
    }
  }
  aux = 1
  for (i in seq(1, .nr, by = 2)) {
    if (i == 1) 
      k = 1
    else k = 1.15
    .x1 = (.arrpos[i, 1] + .arrpos[i + 1, 1])/2
    graphics::text(.x1, k * .arrpos[i, 2], .MOX$variable[i + 1], cex = .cex, 
         col = .seg.col,font=2)
    if (.show.pval) {
      
      if(x$Fg.r$`Pr(>F)`[aux]<0.001){
        graphics::text(.x1, k * .arrpos[i, 2], paste("p.value <0.001"), cex = 0.9 * .cex, col = .pval.col, 
             pos = 1)
      }
      else
      {
        graphics::text(.x1, k * .arrpos[i, 2], paste("p.value =", round(x$Fg.r$`Pr(>F)`[aux],4), 
                                         sep = ""), cex = 0.9 * .cex, col = .pval.col, 
             pos = 1)
      }
    }
    aux = aux + 1
  }
  for (i in 1: .nr) {
    .posi <- .MOX$node[i + 1]
    .seg.cat = as.character(.MOX$category[i + 1])
    .seg.cat = unlist(strsplit(.seg.cat, "/"))
    if (.posi%%2 == 0) {
      for (h in 1:length(.seg.cat)) graphics::text(.arrpos[i, 1] - 
                                          0.03, .arrpos[i, 2] + h/55, .seg.cat[h], cex = .cex)
    }
    else {
      for (h in 1:length(.seg.cat)) graphics::text(.arrpos[i, 1] + 
                                          0.03, .arrpos[i, 2] + h/55, .seg.cat[h], cex = .cex)
    }
  }
  if (is.null(.main)) {
    graphics::text(0.5, 0.95, c("PLS-SEM PATHMOX TREE"), cex = .cex.main)
  }
  else {
    graphics::text(0.5, 0.95, .main, cex = .cex.main)
  }
}

#' @title Comparative plot for the Pathmox terminal nodes 
#' 
#' @description
#' \code{bar_terminal} returns the path coefficient bar plots of the Pathmox terminal 
#' nodes.
#'
#' @details
#' This function aims to visualize, using bar plots, the path coefficients of
#' the dependnet latent construct associated with the terminal nodes. 
#' The user indicates the dependnet latent construct they want to visualize. 
#' This is done using the same label as used in the structural model definition
#' (lavaan syntax). The comparison is done by analyzing the path coefficient 
#' values for each node, or the values estimated in each node for each path coefficient. 
#' In the former, the plot also returns the R^2. In the latter, the bar corresponding to the 
#' node with the highest path coefficient value shows in a different color. 
#' By default the comparison is done by analyzing the path coefficient values for each node. 
#' 
#' @param x An object of the class \code{"plstree"}.
#' @param .LV A string indicating the name of the dependent  latent variable. The label
#' must be the same as used to define the structural model in the (lavaan syntax).
#' @param .bycoef Logical value indicating if the comparison is done by nodes or by path
#' coefficients. By default, \code{FALSE} means that the comparison is done 
#' by nodes.
#' @param .cex.names Expansion factor for axis names (bar labels).
#' @param .cex.axis Expansion factor for numeric axis labels.
#' @param .cex.main Allows fixing the size of the main. It is equal to 1 to default. 
#' @param \dots Further arguments are ignored.
#'
#' @author Giuseppe Lamberti
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
#' @seealso \code{\link{summary.plstree}}, \code{\link{print.plstree}}, \code{\link{pls.pathmox}},  
#' \code{\link{plot.plstree}}, and \code{\link{bar_impvar}}
#' 
#' @export
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
#' # Define the model using the lavaan syntax. Use a set of regression formulas to define 
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
#' # Visualize the bar plot by comparing the nodes
#' bar_terminal(csi.pathmox.age, .LV = "SAT")
#' 
#' # Visualize the bar plot by comparing path coefficients
#' bar_terminal(csi.pathmox.age, .LV = "SAT", .bycoef = TRUE)
#'
#' }
#'
bar_terminal = function (x,.LV,.bycoef = FALSE,.cex.names = 1,.cex.axis = 1.2,.cex.main = 1,...) 
{	
  
  .nn = rownames(x$terminal_path) 
  .chars = paste("->",.LV,sep="")
  
  if(.bycoef)
  {
    .nodes.name = colnames(.end_eq)
    .chars2 = paste("R^2",.LV,sep=" ")
    .rq = NA
  }
  else{
    .end_eq = (x$terminal_paths[grepl(.chars, .nn, fixed = TRUE),])
    .nodes.name = colnames(.end_eq)
    .chars2 = paste("R^2",.LV,sep=" ")
    .rq = (x$terminal_paths[grepl(.chars2, .nn, fixed = TRUE),])
  }
  
  .rs = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 2, 3, 3, 4, 4)
  .cs = c(1, 2, 3, 2, 3, 3, 4, 4, 3, 5, 4, 4, 4, 4)
  .index.mat = cbind(1:14, .rs, .cs)
  .lvs = nrow(.end_eq)
  .colors = hcl.colors(.lvs, palette = "Teal")
  .nn = ncol(.end_eq)
  par(mfrow = .index.mat[.nn, 2:3])
  par(mar = c(3, 3, 3, 3))
  abline(h = 0)
  ylim <- 1.15 * c(min(.end_eq[1, ]), max(.end_eq[1, ]))
  for (.n in 1: .nn) 
  {
    if(.bycoef){
      .maxfac = 1+(.end_eq[,.n]==max(.end_eq[,.n]))
      barplot(.end_eq[,.n], main = paste(.nodes.name[.n]),col=c("#99CCCC","#009999")[.maxfac],
              cex.names =.cex.names, cex.axis = .cex.axis, cex.main = .cex.main, ylim=
              c(0,max(.end_eq[,.n])+0.1),...)
      
    }
    else{
      barplot(.end_eq[,.n], main = paste(.nodes.name[.n],"\n","R^2:",.rq[.n]),col = .colors,
              cex.names = .cex.names, cex.axis = .cex.axis, cex.main = .cex.main,ylim=
              c(0,max(.end_eq[,.n])+0.1),...)
      
    }
    
  }
}

#' @title Bar Plot of a ranking of categorical variables by importance
#' 
#' @description
#' \code{"bar_impvar"} returns a bar plot to  visualize the ranking
#' of variables by importance  in obtaining the terminal nodes of Pathmox.
#' 
#' @details 
#' The importance of each variable is determined by adding the F-statistic 
#' calculated for the variable in each split node of Pathmox.
#' 
#' @param x An object of the class \code{"plstree"}
#' @param .cex.names Expansion factor for axis names (bar labels)
#' @param .cex.axis Expansion factor for numeric axis labels
#' @param .cex.main Allows fixing the size of the main. Equal to 1 to default
#' @param \dots Further arguments are ignored
#'
#' @author Giuseppe Lamberti
#' 
#' @reference Lamberti, G., et al. (2021). University image, hard skills or soft skills: 
#' Which matters most for which graduate students? \emph{Quality and Quantity}.
#' doi:  \doi{10.1007/s11135-021-01149-z}
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
#' @seealso \code{\link{summary.plstree}}, \code{\link{print.plstree}}, \code{\link{pls.pathmox}},  
#' \code{\link{bar_terminal}}, and \code{\link{plot.plstree}}
#' 
#' @export
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
#' # Define the model using the lavaan syntax. Use a set of regression formulas to define
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
#' # Identify the categorical variable to be used as input variables 
#' in the split process
#' CSIcatvar = csibank[,1:5]
#' 
#' # Check if variables are well specified (they have to be factors 
#' # and/or ordered factors)
#' str(CSIcatvar)
#'
#' # Transform age and education into ordered factors
#' CSIcatvar$Age = factor(CSIcatvar$Age, levels = c("<=25", 
#'                                      "26-35", "36-45", "46-55", 
#'                                      "56-65", ">=66"),ordered = T)
#'
#' CSIcatvar$Education = factor(CSIcatvar$Education, 
#'                             levels = c("Unfinished","Elementary", "Highschool",
#'                             "Undergrad", "Graduated"),ordered = T)
#'        
#' # Run Pathmox analysis (Lamberti et al., 2016; 2017)
#' csi.pathmox = pls.pathmox(
#'  .model = CSImodel ,
#'  .data  = csibank,
#'  .catvar= CSIcatvar,
#'  .alpha = 0.05,
#'  .deep = 2
#' )                     
#'  
#' bar_impvar(csi.pathmox)
#'
#' }
#'
bar_impvar = function (x,.cex.names = 1,.cex.axis = 1.2, .cex.main = 1,...) 
{	
  grDevices::dev.new()
  graphics::par(mai=c(1.4,0.82,0.82,0.42))
  diagram::openplotmat()
  
  .imp = x$var_imp[,2]/sum(x$var_imp[,2])
  names(.imp) = x$var_imp[,1]
  .maxfac = 1+(.imp==max(.imp))
  barplot(.imp, main = "Ranking variable importance",col=c("#99CCCC","#009999")[.maxfac],
          cex.names = .cex.names, cex.axis = .cex.axis, cex.main = .cex.main, 
          ylim = c(0,max(.imp)+0.1),...)
  
}  
