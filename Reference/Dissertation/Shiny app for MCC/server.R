library(shiny)
library(purrr)
library(rlang)
library(httpuv)
library(Rcpp)
library(rsconnect)
library(shinycssloaders)

tempfun2 <- function(x,K,NC,NG,h){
  NG  = as.vector(NG)
  tmp = 1
  for (i in 1:K){
    tmp2 <- pt(x+h,df=NG[i]-2) - pt(x-h,df=NG[i]-2) # df=n_i-2
    tmp = tmp*tmp2
  }
  tmp*dt(x,df=NC-2) 
}

tempfun3<-function(x,K,NC,NG,h){
  NG  = as.vector(NG)
  tmp = 1
  for (i in 1:K){
    tmp3<-pt(x+h,df=NG[i]-2)
    tmp = tmp*tmp3
  }
  tmp*dt(x,df=NC-2) 
}

default_val<-function(x,value) {
  if(isTruthy(x)){
    x
  }else{value}}


server <- function(input, output, session){
  col_names <- reactive(paste0("col", seq_len(input$n)))
  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
  })
  #Computing
  Value.1<-reactive({
    input$goButton
    cols <- map_chr(col_names(),~isolate(default_val(input[[.x]],1)))
    return(list(V=cols))
  })
  #tempfun2
  output$Value <- renderPrint({
    group<-isolate(as.numeric(input$n))
    NumG<- as.numeric(Value.1()[[1]])
    NumC<- isolate(as.numeric(input$num.2))
    tmp<- 0 # Init
    h <- 0 # Init
    alpha<-1-c(isolate(as.numeric(input$num.1)))
    while (abs(tmp-alpha)>0.001) {
      h=h+0.001 # add 0.001 each iteration until converge 
      tmp <-integrate(tempfun2, lower = -Inf, upper = Inf,K=group,NC=NumC,NG=NumG,h=h,rel.tol = .Machine$double.eps^.05)$value
    }
    mm <- matrix(h, ncol = 1, nrow = 1)
    df <- as.data.frame(mm)
    colnames(df) <- ""
    rownames(df) <- ""
    df
    # return(h)
  })
  #tempfun3
  output$Value.1 <- renderPrint({
    group<-isolate(as.numeric(input$n))
    NumG<- as.numeric(Value.1()[[1]])
    NumC<- isolate(as.numeric(input$num.2))
    tmp<- 0 # Init
    h.3 <- 0 # Init
    alpha<-1-c(isolate(as.numeric(input$num.1)))
    while (abs(tmp-alpha)>0.001) {
      h.3=h.3+0.001 # add 0.001 each iteration until converge 
      tmp <-integrate(tempfun3,lower = -Inf,upper = Inf,K=group,NC=NumC,NG=NumG,h=h.3,rel.tol = .Machine$double.eps^.05)$value
    }
    m3 <- matrix(h.3, ncol = 1, nrow = 1)
    df3 <- as.data.frame(m3)
    colnames(df3) <- ""
    rownames(df3) <- ""
    df3
    # return(h.3)
  })
}