##			mapDATA
##
## Mimics spplot using ggplot2
##
mapDATA <- function(spdf,
  attr,
  nlevels=256,
  col=rainbow(nlevels),
  ...
  ){
  require(ggplot2)
  require(sp)

  if (class(spdf) != "data.frame") df <- as.data.frame(spdf)
  if (is.null(any(names(df) == attr))) stop("The attribute you are trying to map does not exist in this data frame.")
  
  gridded.data <- gridded(spdf)
  if (gridded.data) {
    p <- ggplot(df,aes_string(x=coordnames(spdf)[1],y=coordnames(spdf)[2],value=attr)) +
      geom_tile(aes_string(fill=attr)) +
      scale_fill_gradientn(attr,col) +
      coord_equal()
  } else {
    p <- ggplot(df,aes_string(x=coordnames(spdf)[1],y=coordnames(spdf)[2],value=attr)) +
      geom_point(aes_string(colour=attr)) +
      scale_colour_gradientn(attr,col) +
      coord_equal()
  }
  p
}