slider.straighten <- function(x, y){
  power.plot <- function(power.x, power.y, x, y){
    power.x <- ifelse(round(power.x, 2)==0, 1e-04, power.x)
    power.y <- ifelse(round(power.y, 2)==0, 1e-04, power.y)
    reexpressed.x <- (x ^ power.x - 1) / power.x
    reexpressed.y <- (y ^ power.y - 1) / power.y
    fit <- rline(reexpressed.x, reexpressed.y, iter = 3)
    xlb <- "Reexpressed x"
    ylb <- "Reexpressed y"
    tit <- paste("Power.x =", round(power.x, 1), "Power.y =", 
                 round(power.y, 1), "(Half-slope ratio =", 
                 round(fit$half.slope.ratio, 2), ")")
    par(mfrow = c(2, 1))
    plt.default <- par("plt")
    par(plt=c(.117, .940, 0, .84), xaxt="n")
    plot(reexpressed.x, reexpressed.y,
         ylab = ylb, xlab = xlb, main = tit)
    abline(a = fit$a - fit$b * fit$xC, b = fit$b)
    par(plt=c(.117, .940, .25, .98), xaxt="s")
    plot(reexpressed.x, fit$residual,
         xlab = xlb,
         ylab = "Residual", main = "Residual Plot")
    abline(h = 0)
    par(plt = plt.default)
  }
  manipulate(
    power.plot(power.x, power.y, x, y),
    power.x = manipulate::slider(-2, 2, initial=1, step=0.1),
    power.y = manipulate::slider(-2, 2, initial=1, step=0.1)
  )
}
