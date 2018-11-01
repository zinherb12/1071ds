library(shiny)
library(ggvis)
shinyServer(function(input, output, session) {
  mtc <- reactive({ mtcars[1:input$n, ] })
  mtc %>%
    ggvis(~wt, ~mpg) %>%
    layer_points() %>%
    bind_shiny("plot", "plot_ui")
  output$mtc_table <- renderTable({
    mtc()[, c("wt", "mpg")]
  })
})