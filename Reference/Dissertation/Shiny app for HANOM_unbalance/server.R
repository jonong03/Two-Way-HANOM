# Unbalanced

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


aa=function(data){
  data=data.frame(data)
  colnames(data) <- c("value","group")
  k=length(unique(data[,2]))
  group.within<-sapply(1:k,function(w)length(data[data[,2]==w,1]))
  if(length(which(group.within<=7))>0){
    #Shapiro
    a=data.frame(aggregate(formula = value ~ Group,data = data,FUN = function(x) {y <-shapiro.test(x);sprintf("%.3f",y$p.value)}))
    names(a)[2]="p-value(Shapiro)"
    for(i in 1:k){
      if(a[i,2] > 0.05){
        a[i,3]="Normal"
      }else{
        a[i,3]="NotNormal"
      } }
    c<-a
    names(c)[3]="Decision"
  }else{
    #Shapiro
    a=data.frame(aggregate(formula = data[,1] ~ data[,2],
                           data = data,
                           FUN = function(x) {y <-shapiro.test(x);sprintf("%.3f",y$p.value)}))
    names(a)[2]="p-value(Shapiro)"
    #Anderson-Darling
    b=data.frame(aggregate(formula = data[,1] ~ data[,2],
                           data = data,
                           FUN = function(x) {y <-ad.test(x);sprintf("%.3f",y$p.value)}))
    names(b)[2]="p-value(Anderson-Darling)"
    for(i in 1:k){
      if(a[i,2] > 0.05){
        a[i,3]="Normal"
      }else{
        a[i,3]="NotNormal"
      } }
    
    for(i in 1:k){
      if(b[i,2] > 0.05){
        b[i,3]="Normal"
      }else{
        b[i,3]="NotNormal"
      }}
    c=cbind(a,b[,2:3])
    names(c)[4]="p-value(A-D)"
    names(c)[3]="Decision"
    names(c)[5]="Decision"
  }
  
  return(c)
  return(a)
}

#同質檢定
vv=function(data){
  a=leveneTest(data[,1],as.factor(data[,2]),data=data,center="mean" ,trim=0.001)
  a=sprintf("%.3f",a[1,3])
  aa="Levene_test"
  b=data.frame(Statistical.testing.method=aa,p.value=a)
  if(b$p.value>0.05){b$Decision="homogeneity"}
  else{b$Decision="heteroscedasticity"}
  print(b)
}


#描述統計
ss=function(data){
  data=data.frame(data)
  colnames(data) <- c("value","group")
  k=length(unique(data[,2]))
  a=data.frame(aggregate(data$value, by=list(data$group), FUN=length))
  b=data.frame(aggregate(data$value, by=list(data$group), FUN=mean))
  c=data.frame(aggregate(data$value, by=list(data$group), FUN=sd))
  d=data.frame(aggregate(data$value, by=list(data$group), FUN=max))
  e=data.frame(aggregate(data$value, by=list(data$group), FUN=min))
  b=sprintf("%.3f",b[,2])
  c=sprintf("%.3f",c[,2])
  d=sprintf("%.3f",d[,2])
  e=sprintf("%.3f",e[,2])
  f=cbind(a,b,c,d,e)
  names(f)=c("Group","Sample size","Mean","Standard deviation","Minimum","Maximum")
  print(f)
}


default_val<-function(x,value) {
  if(isTruthy(x)){
    x
  }else{value}}


calc.power <- function(k,n,alpha,sk,delta){
  ni=0
  for (i in 1:k){
    ni[i]=n 
  }
  u3<-rep(0, times=k-2)
  ui = c(delta/2,-delta/2,u3)
  #Calculate for hs
  #simulation for critical value
  set.seed(3)
  time <- 4      
  sim <- 10000
  scv <- matrix(0, sim, 2*time)
  pow <- matrix(0, sim, 2*time)
  for(a in 1:time){
    for (i in 1:sim){
      ti <- sapply(1:k, function(p) rt(1, ni[p]-2))
      ttildei <- sapply(1:k, function(p) ((k-1)/k)*ti[p]-(sqrt(ni[p])/k)*sum(ti[-p]/sqrt(ni[-p])))
      thati <- sapply(1:k, function(p) ((k-1)/k)*ti[p]-(sqrt(ni[p])/k)*sum(ti[-p]/sqrt(ni[-p]))+ui[p]*sqrt(ni[p])/sk)
      scv[i, a] <- max(ttildei)
      scv[i, time + a] <- min(ttildei)
      pow[i, a] <- max(thati)  
      pow[i, time + a] <- min(thati)
    }
  }
  
  #Result of Critical 
  CV.k <- sapply(1:time, function(a) quantile(scv[, a], 1-alpha/2))
  CV.1 <- -(sapply(1:time, function(a) quantile(scv[, time + a], alpha/2)))
  CV.max <- sapply(1:time, function(a) max(CV.k[a],CV.1[a]))
  hs <- mean(CV.max)
  #Computing Power
  beta <- sapply(1:time, function(a) prop.table(table(pow[,a] > hs|pow[,time+a] < -hs)))%>% as.data.frame()
  test.power <- mean(c(beta$V1[2], beta$V2[2], beta$V3[2], beta$V4[2]))
  return(c(hs, test.power))
}



server <- function(input, output, session){
  sample_data.1<-reactive({
  value=c(97.1, 96.6,	97.4,	95.4,	96.3,	98.2,	95.1,	98.2,	98.3,	96.4,	98.5,	97.4,	97.3,	99.2,	98.1,
            95, 93.4,	94.3,	96.1,	94,	96.2,	92.3,	93.4,	94.5,	95.4,	94.1,	96,	94.2,	92.3,	94, 95.4,
            93.5,	97.6,	98.8,	94.9,	93.7,	92.6,	94.5,	94.4,	92.6,	94.7,	98.8,	92.9,	97.8,	94.7,	96.8,	93.8,	95.9,	93.5,	96.6,	96.7,	96.3, 96.6,	
            96.7,	96.3,	95.9,	96.7,	98.4,	97.7,	97.3,	97.2,	97.5,	98.5,	98.1,	98.1,	96.6)
  group=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
          3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
          4,4,4,4,4,4,4,4,4,4,4,4,4)
  sample_data=as.data.frame(cbind(value,group))
  return(list(sample_data=sample_data))
  })
  
  output$dl <- downloadHandler(
    filename = function() {
      paste("Sample",".csv", sep = "")
    },
    content = function(file) {
      write.csv(sample_data.1(),file,row.names = FALSE)
    }
  )
  
    
  datasetInput<- eventReactive(input$goButton3,{
    input$goButton3
    inFile<-isolate(input$file)
    validate(need(inFile!= "","Please select a data set"))
    Alpha<-isolate(as.numeric(input$num.3))
    
    if(is.null(inFile)){return("No Data")}else{
      data<-read.csv(inFile$datapath)
      #---------------------------------
      #number of group
      colnames(data) <- c("data","group")
      k <- max(data[, 2])
      #sample size of each group
      ni <- sapply(1:k, function(a) sum(data[, 2] == a))
      n0 <- min(ni-1)
      #xbar_i
      xbari <- sapply(1:k, function(a) mean(data[, 1][data[, 2] == a]))
      #si^2
      vari <- sapply(1:k, function(a) var(data[, 1][data[, 2] == a]))
      #xbar_i(ni-1), xbar_i(n0-1)
      xbari1_i <- sapply(1:k, function(a)  mean(data[, 1][data[, 2] == a][-ni[a]]))
      xbari2_i <- sapply(1:k, FUN = function(x){
        mean(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE)
      })
      #si^2_(n-1) si^2_(n0-1)
      vari1_i <- sapply(1:k, function(a)  var(data[, 1][data[, 2] == a][-ni[a]]))
      s1_i <- sapply(1:k, function(a)  sd(data[, 1][data[, 2] == a][-ni[a]]))
      vari2_i  <- sapply(1:k, FUN = function(x){
        var(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE)
      })
      s2_i <- sapply(1:k, FUN = function(x){
        sd(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE)
      })
      #z*
      z1 <- max(vari1_i)
      z2 <- max(vari2_i/ni)
      #weight
      rev1 <- sapply(1:k, function(a) (((z1/vari1_i[a])-1)))
      zz1 <- ifelse(rev1 < 0, 0, rev1)
      U1i <- sapply(1:k, function(a) (1/ni[a]) + (1/ni[a]) * sqrt((1/(ni[a]-1))*zz1[a]))
      V1i <- sapply(1:k, function(a) (1/ni[a]) - (1/ni[a]) * sqrt((ni[a]-1)*zz1[a]))
      rev2 <- sapply(1:k, function(a) (((z2*ni[a]/vari2_i[a])-1)))
      zz2 <- ifelse(rev2 < 0, 0, rev2)
      U2i <- sapply(1:k, function(a) (1/ni[a]) + (1/ni[a]) * sqrt(((ni[a]-n0)/n0)*zz2[a]))
      V2i <- sapply(1:k, function(a) (1/ni[a]) - (1/ni[a]) * sqrt((n0/(ni[a]-n0))*zz2[a]))
      #weighted sample mean
      #weighted sample mean
      xtildei1 <- sapply(1:k, function(a) U1i[a] * sum(data[, 1][data[, 2] == a][-ni[a]])
                         + V1i[a] * data[, 1][data[, 2] == a][ni[a]])
      inf1 <- round(rbind(ni, xbari, vari, xbari1_i, s1_i, U1i, V1i, xtildei1), 3)
      
      Xu <- sapply(1:k, FUN = function(x){
        sum(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE)  })
      Xv <- sapply(1:k, FUN = function(x){
        sum(data[data[, 2] == x, "data"][-c(1:n0)], na.rm = TRUE)  })
      xtildei2 <- sapply(1:k, function(a) U2i[a] * Xu[a] + V2i[a] * Xv[a])
      inf2 <- round(rbind(ni, n0, xbari, vari, xbari2_i, s2_i, U2i, V2i, xtildei2), 3)
      
      #simulation for critical value
      set.seed(3)
      time <- 4      
      sim <- 100000
      scv1 <- matrix(0, sim, 2*time)
      for(a in 1:time){
        for (i in 1:sim){
          ti <- sapply(1:k, function(p) rt(1, ni[p]-2))
          ttildei <- sapply(1:k, function(p) ((k-1)/k)*ti[p]-(sqrt(ni[p])/k)*sum(ti[-p]/sqrt(ni[-p])))
          scv1[i, a] <- max(ttildei)
          scv1[i, time + a] <- min(ttildei)
          invisible(scv1)
        }
      }
      
      set.seed(1)
      X <- matrix(0, sim, time)
      scv2 <- sapply(1:time, function(z) { ran <- rep(0, sim)
      for (i in 1:sim){
        t <- rt(n = k, df = n0-1)
        ran[i] <- max(t - mean(t))
      }
      X[, z] <- ran
      }
      )
      
      #Result of Critical Value
      alpha<-Alpha
      
      CV.k <- sapply(1:time, function(a) quantile(scv1[, a], 1-alpha/2))
      CV.mean.k <- mean(CV.k)
      CV.se.k <- sd(sapply(1:time, function(a) quantile(scv1[, a], 1-alpha/2)))
      CV.1 <- -(sapply(1:time, function(a) quantile(scv1[, time + a], alpha/2)))
      CV.mean.1 <- mean(CV.1)
      CV.se.1 <- sd(sapply(1:time, function(a) quantile(scv1[, time + a], alpha/2)))
      CV.max <- sapply(1:time, function(a) max(CV.k[a],CV.1[a]))
      CV.mean1 <- mean(CV.max)
      CV.se1 <- sd(CV.max) 
      CV.table1 <- round(cbind(CV.mean1,CV.se1), 3)
      rownames(CV.table1) <- c("Critical Value")
      colnames(CV.table1) <- c("Mean", "S.E.")
      
      CV.mean2 <- mean(sapply(1:time, function(a) quantile(scv2[, a], 1-alpha/2)))
      CV.se2 <- sd(sapply(1:time, function(a) quantile(scv2[, a], 1-alpha/2)))
      CV.table2 <- round(rbind(c(CV.mean2, CV.se2)), 3)
      rownames(CV.table2) <- c("Critical Value")
      colnames(CV.table2) <- c("Mean","S.E.")
      
      
      #Center, Lower and Upper Decision Line
      center1 <- mean(xtildei1)
      LDL1 <- mean(xtildei1) - CV.mean1 * sqrt(z1) / sqrt(ni)
      UDL1 <- mean(xtildei1) + CV.mean1 * sqrt(z1) / sqrt(ni)
      
      center2 <- mean(xtildei2)
      LDL2 <- mean(xtildei2) - CV.mean2 * sqrt(z2)
      UDL2 <- mean(xtildei2) + CV.mean2 * sqrt(z2)
      
    }
    
    #Computing p-Value
    Pseq <- seq(0, 1, 0.001)
    Pquantile1 <- sapply(Pseq,
                         function(w) mean(sapply(1:time, function(z) quantile(scv1[, z], 1-w/2))))
    Pequation1 <- abs(Pquantile1 - max(abs((xtildei1 - mean(xtildei1)) / (sqrt(z1) / sqrt(ni)))))
    P.value1 <- Pseq[Pequation1 == min(Pequation1)]
    if(P.value1 > 0 && P.value1 < 1){
      PV1<-sprintf("%.3f",P.value1)
    }else if(P.value1 == 0){
      PV1<-"< 0.001 "
    }else{PV1 <-"> 0.999"}
    
    Pquantile2 <- sapply(Pseq,
                         function(w) mean(sapply(1:time, function(z) quantile(scv2[, z], 1-w/2))))
    Pequation2 <- abs(Pquantile2 - max(abs((xtildei2 - mean(xtildei2)) / sqrt(z2))))
    P.value2 <- Pseq[Pequation2 == min(Pequation2)]
    if(P.value2 > 0 && P.value2 < 1){
      PV2<-sprintf("%.3f",P.value2)
    }else if(P.value2 == 0){
      PV2<-"< 0.001 "
    }else{PV2 <-"> 0.999"}
    
    #HANOM Decision Chart
    #-------------------------------
    Uylim1 <- max(xtildei1, UDL1)                                
    Lylim1 <- min(xtildei1, LDL1)   
    Uylim2 <- max(xtildei2, UDL2)                                
    Lylim2 <- min(xtildei2, LDL2)   
    
    return(C=list(Data=data,Uylim1=Uylim1,Lylim1=Lylim1,k=k,xtildei1=xtildei1,LDL1=LDL1,UDL1=UDL1,Center1=center1,inF1=inf1,CV.table1=CV.table1,
                PV1=PV1,Uylim2=Uylim2,Lylim2=Lylim2,xtildei2=xtildei2,LDL2=LDL2,UDL2=UDL2,Center2=center2,inF2=inf2,CV.table2=CV.table2,PV2=PV2))
  })
  
  output$distPlot.1<-renderPlot({
    data<-datasetInput()[[1]];Uylim1<-datasetInput()[[2]];Lylim1<-datasetInput()[[3]]
    k<-datasetInput()[[4]];xtildei1<-datasetInput()[[5]];LDL1<-datasetInput()[[6]];UDL1<-datasetInput()[[7]];
    center1<-datasetInput()[[8]]
    k.1<-k+0.3
    par(mar=c(4.5,4.5,4,6))
    plot(xtildei1,pch=16,las = 2,xlab=colnames(data)[2],ylab=NA,xlim=c(0.7, k+0.3),ylim = c(Lylim1, Uylim1),main = "P1 HANOM Chart", xaxt = "n")                            
    xaxis <- seq(1, k, 1)                                        
    axis(1, at=1:k, labels = formatC(xaxis))                     
    abline(h = center1)                                              
    sapply(1:k, function(a) arrows(a, min(xtildei1[a], mean(xtildei1)), a, max(xtildei1[a], mean(xtildei1)), length = 0))
    lines(1:(k+1)-.5, c(LDL1, LDL1[k]), type = "s", lty = 2, lwd = 2)  
    lines(1:(k+1)-.5, c(UDL1, UDL1[k]), type = "s", lty = 2, lwd = 2)  
    sapply(1:k, function(a) arrows(a, min(xtildei1[a], mean(xtildei1)), a, max(xtildei1[a], mean(xtildei1)), length = 0))
    mtext(paste("LDL = ", sprintf("%.3f", LDL1)), side = 4, at = LDL1, las = 2, cex = 0.9, line = 0.3)
    mtext(paste("UDL = ", sprintf("%.3f", UDL1)), side = 4, at = UDL1, las = 2, cex = 0.9, line = 0.3)
    mtext(bquote(bar(widetilde(X)) == .(sprintf("%.3f", mean(xtildei1)))),side = 4, at = mean(xtildei1), las = 2, cex = 0.9, line = 0.4)
  })
  
  output$table.1<-renderTable(rownames = T,quoted = F, digits = 3,{
    k<-datasetInput()[[4]]
    V1<-datasetInput()[[9]]
    V1<-V1[-c(2,3),]
    NB1<-c(1:k)
    NB.1<-paste0("Treat",".",NB1)
    colnames(V1)<-NB.1
    row.names(V1)<-c("n_i","X_bar_i","S_i","U_i","V_i","X_tilde_i")
    return(V1)
    
  })
  
  output$table.2<-renderTable(rownames = T,quoted = F, digits = 3,{
    W1<-datasetInput()[[10]]
    colnames(W1) <- c("Mean","S.E.")
    row.names(W1)<- c("Critical Value")
    return(W1)
    
  })
  
  output$table.3<-renderTable(rownames = F,colnames = F,quoted = F, digits = 3,{
    p1<-datasetInput()[[11]]
    return(p1[1])
  })
  
  
  output$distPlot.2<-renderPlot({
    data<-datasetInput()[[1]];Uylim2<-datasetInput()[[12]];Lylim2<-datasetInput()[[13]]
    k<-datasetInput()[[4]];xtildei2<-datasetInput()[[14]];LDL2<-datasetInput()[[15]];UDL2<-datasetInput()[[16]];
    center2<-datasetInput()[[17]]
    k.1<-k+0.3
    par(mar=c(4.5,4.5,4,6))
    plot(xtildei2, pch=16,las = 2,xlab=colnames(data)[2],ylab=NA,xlim=c(0.7, k+0.3),ylim = c(Lylim2, Uylim2),main = "P2 HANOM Chart", xaxt = "n")                            
    xaxis <- seq(1, k, 1)                                        
    axis(1, at=1:k, labels = formatC(xaxis))                     
    abline(h = LDL2, lty = 2, lwd = 2)
    abline(h = UDL2, lty = 2, lwd = 2)
    abline(h = center2)
    sapply(1:k, function(a) arrows(a, min(xtildei2[a], mean(xtildei2)), a,max(xtildei2[a], mean(xtildei2)), length = 0))
    mtext(paste("LDL = ", sprintf("%.3f", LDL2)), side = 4, at = LDL2,las = 2, cex = 0.9, line = 0.3)
    mtext(paste("UDL = ", sprintf("%.3f", UDL2)), side = 4, at = UDL2,las = 2, cex = 0.9, line = 0.3)
    mtext(bquote(bar(widetilde(X)) == .(sprintf("%.3f", mean(xtildei2)))),side = 4, at = mean(xtildei2), las = 2, cex = 0.9, line = 0.4)
  })
  
  
  output$table.4<-renderTable(rownames = T,quoted = F, digits = 3,{
    k<-datasetInput()[[4]]
    V2<-datasetInput()[[18]]
    V2<-V2[-c(3,4),]
    NB2<-c(1:k)
    NB.2<-paste0("Treat",".",NB2)
    colnames(V2)<-NB.2
    row.names(V2)<-c("n_i","n_0","X_bar_i_n0","S_i_n0","U_i","V_i","X_tilde_i")
    return(V2)
    
  })
  
  output$table.5<-renderTable(rownames = T,quoted = F, digits = 3,{
    W2<-datasetInput()[[19]]
    colnames(W2) <- c("Mean","S.E.")
    row.names(W2)<- c("Critical Value")
    return(W2)
    
  })
  
  output$table.6<-renderTable(rownames = F,colnames = F,quoted = F, digits = 3,{
    p2<-datasetInput()[[20]]
    return(p2[1])
  })
  
  #常態
  output$aa <-  renderTable({
    data=datasetInput()[[1]][1:2]
    aa(data)})
  #變異數
  output$vv <-  renderTable({
    data=datasetInput()[[1]][1:2]
    vv(data)})
  
  #描述統計
  output$ss <-  renderTable({
    data=datasetInput()[[1]][1:2]
    ss(data)})
  
  
  
  
  
  #Computing power
  
  col_names <- reactive(paste0("col", seq_len((as.numeric(input$num.k) ))))
  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
  })
  #Computing
  Value.0<-reactive({
    input$goButton
    cols <- map_chr(col_names(),~isolate(default_val(input[[.x]],-99)))
    return(list(V=cols))
  })
  
  Value.1<-eventReactive(input$goButton,{
      input$goButton
      Num.k<-isolate(as.numeric(input$num.k)) 
      Num.alpha<-isolate(as.numeric(input$num.alpha)) 
      Num.delta<-isolate(as.numeric(input$num.delta)) 
      Num.sk<-isolate(as.numeric(input$num.sk)) 
      NumG <-as.numeric(Value.0()[[1]])
      ui_c = c(as.numeric(Num.delta)/2,-as.numeric(Num.delta)/2,rep(0,times=Num.k-2))
      set.seed(3)
      time <- 4      
      sim <- 10000
      scv <- matrix(0, sim, 2*time)
      pow <- matrix(0, sim, 2*time)
      for(a in 1:time){
        for (i in 1:sim){
          ti <- sapply(1:Num.k, function(p) rt(1, NumG[p]-2))
          ttildei <- sapply(1:Num.k, function(p) ((Num.k-1)/Num.k)*ti[p]-(sqrt(NumG[p])/Num.k)*sum(ti[-p]/sqrt(NumG[-p])))
          thati <- sapply(1:Num.k, function(p) ((Num.k-1)/Num.k)*ti[p]-(sqrt(NumG[p])/Num.k)*sum(ti[-p]/sqrt(NumG[-p]))+ui_c[p]*sqrt(NumG[p])/as.numeric(Num.sk))
          scv[i, a] <- max(ttildei)
          scv[i, time + a] <- min(ttildei)
          pow[i, a] <- max(thati)  
          pow[i, time + a] <- min(thati)
        }
      }
      #Result of Critical 
      CV.k <- sapply(1:time, function(a) quantile(scv[, a], 1-as.numeric(Num.alpha)/2))
      CV.1 <- -(sapply(1:time, function(a) quantile(scv[, time + a], as.numeric(Num.alpha)/2)))
      CV.max <- sapply(1:time, function(a) max(CV.k[a],CV.1[a]))
      hs <- mean(CV.max)
      #Computing Power
      beta <- sapply(1:time, function(a) prop.table(table(pow[,a] > hs|pow[,time+a] < -hs)))%>% as.data.frame()
      test.power <- mean(c(beta$V1[2], beta$V2[2], beta$V3[2], beta$V4[2]))
      answer=c(hs,test.power)
  
    return(list(V=answer))
    
  })
  
  
  output$Value.critical<- renderPrint({
    Value.1()[[1]][1]
  })
  
  output$Value.power<- renderPrint({
    Value.1()[[1]][2]
  })
  
  
  #Computing sample size
  
  Value.2 <- eventReactive(input$goButton2, {
    input$goButton2
    Num.k2<-isolate(as.numeric(input$num.k2))
    Num.alpha2<-isolate(as.numeric(input$num.alpha2))
    Num.delta2<-isolate(as.numeric(input$num.delta2))
    Num.sk2<-isolate(as.numeric(input$num.sk2))
    Num.reachpower<-isolate(as.numeric(input$num.reachpower))
    n=0
    n[2]=3
    result <- calc.power(Num.k2,n[2],Num.alpha2,Num.sk2,Num.delta2)
    while(result[2] < Num.reachpower){
      n[2] <- n[2] + 1
      result <- calc.power(Num.k2,n[2],Num.alpha2,Num.sk2,Num.delta2)
    }
    answer=c(sprintf("%.0f", n[2]), sprintf("%.3f", result[2]))
    return(list(V=answer))
  })
  
  output$Value.sample.size<- renderPrint({
    Value.2()[[1]][1]
  })
  output$Value.power2<- renderPrint({
    Value.2()[[1]][2]
  })
  

  
}





