spread.level.plot=function (response, group) 
{
    b.stats = boxplot(response ~ group, plot = FALSE)
    medians = b.stats$stats[3, ]
    log.median = log10(medians)
    dfs = b.stats$stats[4, ] - b.stats$stats[2, ]
    log.df = log10(dfs)
    k=length(medians)
    if(k>2) fit = line(log.median, log.df) else fit=lm(log.df~log.median)
    plot(log.median, log.df, pch = 19, cex = 1.3, xlab = "LOG MEDIAN", 
        ylab = "LOG DF", main = paste("SPREAD VS LEVEL PLOT, slope = ", 
            round(fit$coef[2], 2)))
    abline(fit, lwd = 3, col = "red")
    GROUP = unique(group)
    return(data.frame(GROUP, medians, dfs, log.median, log.df))
}
