library(shiny)

ui <- fluidPage(
    titlePanel("Hello Shiny!"),
    mainPanel(
        numericInput("mean", label = "mean", value = 1),
        uiOutput("tableUI")
    )
)

server <- function(input, output) {
    
    output$table <- renderTable({
        x <- rnorm(2)
        y <- rnorm(2, input$mean)
        tab <- data.frame(x = x, y = y)
        rownames(tab) <- c("\\(\\alpha\\)", 
                           "\\( \\bar{X_i} \\)")
        tab
    },
    include.rownames = TRUE,
    include.colnames = TRUE)
    
    output$tableUI <- renderUI({
        input$mean # in order to re-render when input$mean changes
        tagList(
            withMathJax(),
            withMathJax(tableOutput("table"))
        )
    })
    
}

shinyApp(ui = ui, server = server)


#Name = c("\\( n_i )\\", "\\( \\bar{X_i} \\)", "\\( S_{i} \\)", "\\( U_{i} \\)", "\\( $V_{i} \\)", "\\( \\tilde{X_i} \\)", "Center", "LDL", "UDL"),
