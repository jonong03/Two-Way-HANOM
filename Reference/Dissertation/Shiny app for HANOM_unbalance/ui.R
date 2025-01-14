library(shiny)
library(purrr)
library(rlang)
library(httpuv)
library(Rcpp)
library(rsconnect)
library(shinycssloaders)
library(dplyr)
library(RTextTools)
library(car)
library(shinythemes)
library(nortest)


# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram
  shinyUI(
    fluidPage(
      withMathJax(),
      theme = shinytheme("cerulean"),
      tags$style(HTML("
                  .tabbable > .nav > li > a                  {background-color: white;  color:#013B63}
                  .tabbable > .nav > li[class=active]    > a {background-color: #013B63; color:white}
                  ")
      ),
      
      tags$head(tags$style(
        HTML('
         #sidebar {
         background-color: #013B63;
                  }
         
         body, label, input, button, select { 
         font-family: "Arial";
         }')
      )
      ),
      # Application title
      titlePanel("Modified Single-stage Sampling Procedure for Heteroscedasticity ANOM (HANOM)"),
      fluidRow(class= "R1",
               tabsetPanel(type = "pills",
                           tabPanel(h3("HANOM", style = "font-size:150%"),
                                    titlePanel(h2("The HANOM Chart and Summary Statistics", style = "color:#013B63")),sidebarLayout(
                                      
                                      #file input & input number
                                      sidebarPanel(id="sidebar",
                                                   h3("Toy Dataset", style = "color:#FFFFFF;font-size:20px"),
                                                   downloadButton("dl", "Download file"),
                                                   tags$style("[type = 'number'] {font-size:24px;height:20px;}"),
                                                   fileInput("file", label = h3("File input", style = "color:#FFFFFF;font-size:20px")),
                                                   textInput("num.3", label = h3(withMathJax("Type I Error Rate (\\(\\alpha\\)):"), style = "color:#FFFFFF;font-size:20px"), value = 0.05),
                                                   # Button
                                                   h3("", style = "color:#FFFFFF;font-size:20px"),
                                                   actionButton("goButton3","Run")
                                                   #, h4("Example"),
                                                   #img(src = "example.PNG")
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel(h3("Describe statistic", style = "font-size:20px" ), shinycssloaders::withSpinner(tableOutput("ss"))),
                                          tabPanel(h3("Test for normality", style = "font-size:20px" ), shinycssloaders::withSpinner(tableOutput("aa"))),
                                          tabPanel(h3("Test for variances", style = "font-size:20px" ), shinycssloaders::withSpinner(tableOutput("vv"))),
                                          tabPanel(
                                            h3("HANOM(P1)", style = "font-size:20px"),
                                            h3("HANOM Chart", style = "color:black"), shinycssloaders::withSpinner(plotOutput("distPlot.1")),
                                            h3("Table. Summary Statistics", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.1")),
                                            h3("Critical Value", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.2")),
                                            h3("P-value", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.3"))),
                                          tabPanel(
                                            h3("HANOM(P2)", style = "font-size:20px"),
                                            h3("HANOM Chart", style = "color:black"), shinycssloaders::withSpinner(plotOutput("distPlot.2")),
                                            h3("Table. Summary Statistics", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.4")),
                                            h3("Critical Value", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.5")),
                                            h3("P-value", style = "color:black"), shinycssloaders::withSpinner(tableOutput("table.6")))
                                        ))
                                    )
                           ),
                           tabPanel(h3("Calculate Power", style = "font-size:150%"),
                                    titlePanel(h2("The Critical Value and Power for Heteroscedasticity ANOM", style = "color:#013B63")),sidebarLayout(
                                      #file input & input number
                                      sidebarPanel(id="sidebar",
                                                   # tags$style("#num1,#num {font-size:30px;height:50px}"),
                                                   textInput("num.alpha", label = h3(withMathJax("Type I Error Rate (\\(\\alpha\\)):"), style = "color:#FFFFFF;font-size:20px"), value = 0.05),
                                                   textInput("num.delta", label = h3(withMathJax('Effect Size (\\(\\delta\\)):'), style = "color:#FFFFFF;font-size:20px"), value = 1),
                                                   textInput("num.sk", label = h3(withMathJax('Maximum Standard Deviation (\\(S_{[k]}\\)):'), style = "color:#FFFFFF;font-size:20px"), value = 0.5),
                                                   textInput("num.k", label = h3(withMathJax('Number of Treatment (\\(k\\)):'),
                                                                                    style = "color:#FFFFFF;font-size:20px"), value = 4),
                                                   h3("Size of Each Treatment", style = "color:#FFFFFF;font-size:20px"),
                                                   uiOutput("col"),
                                                   h3("", style = "color:#FFFFFF;font-size:20px"),
                                                   actionButton("goButton","Run")
                                                   
                                      ),
                                      mainPanel(h3("Critical Value", style = "color:#013B63;font-size:30px" ),
                                                div(textOutput("Value.critical")%>% withSpinner(color="#013B63"), style = "color:black;font-size:26px"),
                                                h3("Power", style = "color:#013B63;font-size:30px" ),
                                                div(textOutput("Value.power")%>% withSpinner(color="#013B63"), style = "color:black;font-size:26px")
                                                
                                      )
                                    )
                           ),
                           tabPanel(h3("Calculate Sample Size", style = "font-size:150%"),
                                    #div(style = "height:800px; background-color: yellow;", "This is an example")
                                    titlePanel(h2("The Required Sample Size for Heteroscedasticity ANOM", style = "color:#013B63")),sidebarLayout(
                                      #file input & input number
                                      sidebarPanel(id="sidebar",
                                                   # tags$style("#num1,#num {font-size:30px;height:50px}"),
                                                   textInput("num.alpha2", label = h3(withMathJax("Type I Error Rate (\\(\\alpha\\)):"), style = "color:#FFFFFF;font-size:20px"), value = 0.05),
                                                   textInput("num.reachpower", label = h3(withMathJax("Required Power (\\(1-\\beta\\)):"), style = "color:#FFFFFF;font-size:20px"), value = 0.9),
                                                   textInput("num.delta2", label = h3(withMathJax('Effect Size (\\(\\delta\\)):'), style = "color:#FFFFFF;font-size:20px"), value = 1),
                                                   textInput("num.sk2", label = h3(withMathJax('Maximum Standard Deviation (\\(S_{[k]}\\)):'), style = "color:#FFFFFF;font-size:20px"), value = 0.5),
                                                   textInput("num.k2", label = h3(withMathJax('Number of Treatment (\\(k\\)):'),style = "color:#FFFFFF;font-size:20px"), value = 4),
                                                   #textInput("num.n", label = h3("The initial value of n",
                                                   #                                  style = "color:#FFFFFF;font-size:24px"), value = 4),
                                                   h3("", style = "color:#FFFFFF;font-size:20px"),
                                                   actionButton("goButton2","Run")
                                                   
                                                   # alpha = 0.1       ##Determined alpha of the test
                                                   # reach.power =0.95  ##Expect to reach the power of the test
                                                   # k=4               ##Number of treatments
                                                   # sk=4              ##The largest standard deviation if each treatment
                                                   # delta=10          ##The difference between any two treatment means that will lead to rejection
                                                   
                                                   
                                      ),
                                      mainPanel(h3("Required Sample Size", style = "color:#013B63;font-size:30px" ),
                                                div(textOutput("Value.sample.size")%>% withSpinner(color="#013B63"), style = "color:black;font-size:26px"),
                                                h3("Calculated Power", style = "color:#013B63;font-size:30px" ),
                                                div(textOutput("Value.power2")%>% withSpinner(color="#013B63"), style = "color:black;font-size:26px")
                                      )
                                    )
                           )
               )  # end tabsetPanel
               
      ), tags$head(tags$style(".R1{background-color: white;}"))
      
    )  # fluidPage
  )  # end shinyUI



