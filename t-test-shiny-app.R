library("shiny")
library("ggplot2")

#----------------------------------------------------
ui = fluidPage(sidebarLayout(
  titlePanel(title = h4("t-test", align = "center")),
  sidebarPanel(
    sliderInput("eps", 
        "Correlation strength", 
        min = 0, max = .8, value = 0),
    radioButtons("dist", "Distribution:",
      c("Normal" = "rnorm", "Uniform" = "runif",
        "Log-normal" = "rlnorm", "Exponential" = "rexp")),
    uiOutput("plot_ui")
  ),
  mainPanel(
    plotOutput("plot")
  )
))

#----------------------------------------------------
server <- function(input,output){
  dat <- reactive({
    test <- df[df$num %in% seq(from=min(input$num),to=max(input$num),by=1),]
    print(test)
    test
  })
  output$plot2<-renderPlot({
    ggplot(dat(),aes(x=date,y=num))+geom_point(colour='red')},height = 400,width = 600)}

sim = function(inp, n = 20, m = 10000) {
  
  x = matrix(
    do.call(inp$dist, list(n = n * m)), 
    ncol = n, nrow = m)
  colnames(x) = letters[1 + (seq_len(n)-1) %/% (n/2)]
  
  for (j in 2:ncol(x))
    x[, j] = x[, j-1] * inp$eps + x[, j] * (1 - inp$eps)
  x
}

server = function(input, output) {
  rt = reactive({
    xc = sim(input)
    genefilter::rowttests(xc, factor(colnames(xc)))
  })
  
  output$plot = renderPlot({
    ggplot(rt, ~p.value) + geom_histogram(breaks = seq(0, 1, by = 0.0125)) 
  })
}

#----------------------------------------------------
shinyApp(ui, server)