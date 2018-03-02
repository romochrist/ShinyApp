library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
   
    output$distPlot <- renderPlot({
    
        # number of simulations
        nSims <- input$simulations
        # number of exponentials
        nExp <- input$exponentials
        # lambda
        lambdaExp <- input$lambda
        # mean of exponential distribution
        meanExp <- 1/lambdaExp
        # standard deviation
        sdExp <- 1/lambdaExp
        
        expMeans <- matrix(rexp(nExp * nSims, rate = lambdaExp), ncol = nExp)
        expMeans <- apply(expMeans, 1, mean)
        expMeans <- (expMeans - meanExp) / (sdExp / sqrt(nExp))
        dty <- density(expMeans)
        plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-5, 5), ylim = c(0, .5))
        title(paste("Sample mean of", nExp, "exponentials"))
        lines(dty$x, dty$y, lwd = 2)
        if(input$showNormal) {
            lines(seq(-5, 5, length = 100), dnorm(seq(-5, 5, length = 100)), col = grey(.8), lwd = 3)
        }
  })
  
})
