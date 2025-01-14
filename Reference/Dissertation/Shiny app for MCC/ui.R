library(shiny)
library(purrr)
library(rlang)
library(httpuv)
library(Rcpp)
library(rsconnect)
library(shinycssloaders)

shinyUI(fixedPage(
  theme = "Spacelab.css",
  tags$style(HTML("
                  .tabbable > .nav > li > a                  {background-color: white;  color:#013B63}
                  .tabbable > .nav > li[class=active]    > a {background-color: #013B63; color:white}
                  ")),
  
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #013B63;
                  }
         
         body, label, input, button, select { 
         font-family: "Arial";
         }')
  )),
  
  titlePanel("The Critical Value for Multiple Comparison with a Control under Heteroscedasticity"),sidebarLayout(
    #file input & input number
    sidebarPanel(id="sidebar",
                 # tags$style("#num1,#num {font-size:30px;height:50px}"),
                 textInput("num.1", label = h3(withMathJax("Type I Error Rate (\\(\\alpha\\)):"), style = "color:#FFFFFF;font-size:24px"), value = 0.05),
                 textInput("num.2", label = h3("Size of Control", style = "color:#FFFFFF;font-size:24px"), value = 10), 
                 numericInput("n", label = h3("Number of Treatment and Size of Each Treatment", style = "color:#FFFFFF;font-size:24px"), value = 3, min = 2),
                 uiOutput("col"),
                 h3("Carry out", style = "color:#FFFFFF"),
                 actionButton("goButton","Run")

    ),
    mainPanel(h3("Critical Value (One-sided)", style = "color:#013B63;font-size:30px" ),
              div(textOutput("Value.1")%>% withSpinner(color="#013B63"), style = "color:black;font-size:26px"),
              h3("Critical Value (Two-sided)", style = "color:#013B63;font-size:30px" ),
              div(textOutput("Value")%>% withSpinner(color="#013B63"), style = "color:black;font-size:26px")
      
      ))))


