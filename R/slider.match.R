slider.match <- function(x){
  power.plot <- function(power, x){
    power <- ifelse(round(power, 2)==0, 1e-04, power)
    reexpressed <- mtrans(x, power)
    xlb <- "Reexpressed  Data"
    tit <- paste("Power =", round(power, 1), ", d =", 
                 round(hinkley(reexpressed), 2))
    boxplot(data.frame(raw = x, reexpressed),
            main=tit, horizontal=TRUE)
  }
  manipulate(
    power.plot(power, x),
    power = manipulate::slider(-2, 2, initial=1, step=0.1)
  )
}