library("shiny")
library("ggplot2")
library("tibble")

#----------------------------------------------------
sim = function(inp, n = 20, m = 2000) {
  x = matrix(
    do.call(inp$dist, list(n = n * m)), 
    ncol = m, nrow = n)
  rownames(x) = letters[1 + (seq_len(n)-1) %/% (n/2)]
  stopifnot(nrow(x) >= 2)
  for (j in 2:nrow(x))
    x[j, ] = x[j-1, ] * inp$eps + x[j, ] * (1 - inp$eps)
  x
}

#----------------------------------------------------
ui = fluidPage(
  titlePanel(title = h4("t-test", align = "center")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("eps", 
        "Correlation strength", 
        min = 0, max = .8, value = 0),
      radioButtons("dist", "Distribution of the data:",
        c("Normal" = "rnorm", "Uniform" = "runif",
        "Log-normal" = "rlnorm", "Exponential" = "rexp")),
    uiOutput("plot_ui")
  ),
  mainPanel(
    plotOutput("plot"),    
    HTML(paste("The histogram shows the distribution of", formals(sim)$m, "p-values from calling the t-test on a comparison between two groups, each with", formals(sim)$n/2, "data points. There is no difference between the statistical distributions of the data between the two groups (in particular, no difference between their means). Thus, the histogram should be flat, corresponding to a uniform distribution of null p-values in [0,1]. You can experiment with different elementary statistical distributions from which the random numbers are drawn, and you can introduce and control the strength of serial correlations between subsequent values. <a href='https://github.com/wolfganghuber/t-test-normality-and-independence'>The source code for this app is here: https://github.com/wolfganghuber/t-test-normality-and-independence</a>.")),
  )
))


server = function(input, output) {
  output$plot = renderPlot({
    x = sim(input)
    d = tibble(p.value = vapply(seq_len(ncol(x)), function(i) 
      t.test(x[, i] ~ rownames(x))$p.value, numeric(1)))
    ggplot(d, aes(x = p.value)) + geom_histogram(breaks = seq(0, 1, by = 0.025)) 
  })
}

#----------------------------------------------------
shinyApp(ui, server)