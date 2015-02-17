slider.power <- function(y){
  power.plot <- function(power, y){
    power <- ifelse(power==0, 1e-04, power)
    reexpressed <- (y ^ power - 1) / power
    hist(reexpressed,
         main=paste("Power =", round(power, 1),", d =", 
                    round(hinkley(reexpressed), 3)))
  }
  manipulate(
    power.plot(power, y),
    power = manipulate::slider(-2, 2, initial=1, step=0.1)
  )
}
