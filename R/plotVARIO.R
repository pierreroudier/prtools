##			plotVARIO
##
## Plots one variogram using ggplot2
##
plotVARIO <- function(
  formula,
  spdf,
  model=c("Exp","Sph","Gau","Mat","Ste","Lin"), # NULL for no model
  cutoff=getCutoff(spdf),
  width=cutoff/50,
  return.model=FALSE,
  np=TRUE,
  size.np=FALSE,
  col.np=FALSE,
  col.pts=tim.colors(),
  col.line="black",
  verbose=TRUE
  ){
  # loading packages
  require(gstat)
  if (!is.null(model)) require(automap)
  require(ggplot2)
  require(fields)

  if (!is.formula(formula)) formula <- as.formula(formula)
  
  # computing variogram and models
  vg <- variogram(formula, spdf, width = width, cutoff = cutoff)
  if (!is.null(model)) {
    vg_auto <- autofitVariogram(formula, spdf, model = model, verbose = FALSE)
    vgm.fitted <- vg_auto$var_model
    vgm.perf <- vg_auto$sserr
    vgmLine <- variogramLine(vgm.fitted, maxdist = max(vg$dist))
    if(return.model) vgm <- vgm.fitted
  }
  # ggplot2 plotting
  vgplot <- ggplot(vg,aes(x = dist, y = gamma))
  if (np) {
    if (size.np && !col.np) {
      vgplot <- vgplot + 
	geom_point(aes(size = np, alpha=0.75)) + 
	scale_area(name="number\n of pairs",to=c(0.3,3)) + 
	scale_alpha(legend=FALSE)
    }
    if (col.np && !size.np) {
      vgplot <- vgplot + 
	geom_point(aes(col = np)) + 
	scale_colour_gradientn(colour = col.pts)
    }
    if (col.np && size.np) {
      vgplot <- vgplot + 
	geom_point(aes(size = np, col=np, alpha=0.75)) + 
	scale_colour_gradientn(colour = col.pts) + 
	scale_area(name="number\n of pairs",to=c(0.3,3)) +
	scale_alpha(legend=FALSE)
    }
    if (!col.np && !size.np) {
      vgplot <- vgplot + 
	geom_point()
      warning("No methods to display the number of pairs has been selected. Try either col.np=TRUE or size.np=TRUE.")
    }
  }
  else vgplot <- vgplot + geom_point()
  if (!is.null(model)) vgplot <- vgplot + geom_line(data = vgmLine, colour = col.line, size = 0.5) 
  vgplot <- vgplot + labs(x= "distance", y = "semivariance", colour = "Number\n of pairs")
  # optional printing of the model fitted
  if(verbose && !is.null(model)) {
    cat("\nBest model fitted:\n")
    print(vgm.fitted)
    cat("\nSums of squares error:\n")
    cat(vgm.perf)
    cat("\n")
  }
  # a ggplot2 graphic object is returned
  if(return.model) vgplot <- list(plot=vgplot, vg=vg, vgm=vgm)
  return(vgplot)
}