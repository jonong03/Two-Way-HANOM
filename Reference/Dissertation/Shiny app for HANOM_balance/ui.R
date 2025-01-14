ui <-shinyUI(fluidPage(
  theme = "Spacelab.css",
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: white;  color:#013B63}
    .tabbable > .nav > li[class=active]    > a {background-color: #013B63; color:white}
  ")),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: white;
        color: black;
      }
      h2 {
        font-family: 'Arial', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
  ),
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #013B63;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),

  titlePanel("Single-stage Sampling Procedure for Heteroscedasticity ANOM"),sidebarLayout(
    #file input & input number
    sidebarPanel(id="sidebar",
                 fileInput("file", label = h3("File input", style = "color:#FFFFFF;font-size:20px")),
                 tags$style("[type = 'number'] {font-size:20px;height:20px;}"),
                 downloadButton('dl', 'Download Toy Dataset'),
                 textInput("num.3", label = h3(withMathJax("Type I Error Rate (\\(\\alpha\\)):"), style = "color:#FFFFFF;font-size:20px"), value = 0.05),
                 # Button
                 h3("", style = "color:#FFFFFF"),
                 actionButton("goButton","Run")
                 #, h4("Example"),
                 #img(src = "example.PNG")
    ),
    mainPanel(
    p(h4('Data type: ',style="display:inline"),span(textOutput("equal_or_not_determine",inline = TRUE),style="color:blue")),
      tabsetPanel(
              tabPanel(h3("Describe statistic", style = "font-size:20px" ), shinycssloaders::withSpinner(tableOutput("ss"))),
              tabPanel(h3("Test for normality", style = "font-size:20px" ), shinycssloaders::withSpinner(tableOutput("aa"))),
              tabPanel(h3("Test for variances", style = "font-size:20px" ), shinycssloaders::withSpinner(tableOutput("vv"))),
              tabPanel(
                h3("HANOM", style = "font-size:20px"),
                h3("HANOM Chart", style = "color:black"), shinycssloaders::withSpinner(plotOutput("distPlot.1")),
                h3("Table. Summary Statistics", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.1")),
                h3("Critical Value (standard error)", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.2")),
                h3("P-value", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.3")))
                #h3(type="button", class="btn btn-P-value"), shinycssloaders::withSpinner(tableOutput("table.3")))
                
    )))))


