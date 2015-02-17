slider.compare <- function(y, group){
  power.plot <- function(power, y, group){
    power <- ifelse(round(power, 2)==0, 1e-03, power)
    reexpressed <- (y ^ power - 1) / power
    boxplot(reexpressed ~ group, xlab="Reexpressed Data",
            horizontal=TRUE,
            main=paste("Power =", round(power, 1)))
  }
  manipulate(
    power.plot(power, y, group),
    power = manipulate::slider(-2, 2, initial=1, step=0.1)
  )
}