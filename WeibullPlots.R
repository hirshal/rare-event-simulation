#create density plots dweibull(x, k, β)

curve(dweibull(x, 0.25, 1), from=20, to=50, ylab = "Probability Density", xlab = "",lwd=4,ylim=c(0,0.003))
curve(dweibull(x, 0.5, 1), from=20, to=50, col='red', add=TRUE,lwd=4)
curve(dweibull(x, 0.75, 1), from=20, to=50, col='blue', add=TRUE,lwd=4)
curve(dweibull(x, 1, 1), from=20, to=50, col='green', add=TRUE,lwd=4)

#add legend
legend(5.5, 0.8, legend=c("beta = 1, k = 0.25", "beta = 1, k = 0.5", "beta = 1, k = 0.75","beta = 1, k = 1"),
       col=c("black", "red", "blue","green"), lty=1, cex=1.2,lwd=4)

