slider.histogram <- function(y){
  plot.hist <- function(num, y, var.name){
    xrange <- range(y)
    breaks <- seq(xrange[1], xrange[2], length = num + 1)
    hist(y, breaks=breaks, xlab=var.name,
         main=paste("Number of bins =", num))
  }
  var.name <- as.character(match.call())[2]
  manipulate(
    plot.hist(num, y, var.name),
    num = manipulate::slider(1, 100, initial=2, step=1)
  )}
