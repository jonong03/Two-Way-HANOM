# Balanced
#=========Shiny===============
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


#常態檢定
aa=function(data){
  data=data.frame(data)
  colnames(data) <- c("value","group")
  k=length(unique(data$group))
  group.within<-sapply(1:k,function(w)length(data[data$group==w,1]))
  if(length(which(group.within<=7))>0){
    #Shapiro
    a=data.frame(aggregate(formula = value ~ group,
                           data = data,
                           FUN = function(x) {y <-shapiro.test(x);sprintf("%.3f",y$p.value)}))
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
    a=data.frame(aggregate(formula = value ~ group,
                           data = data,
                           FUN = function(x) {y <-shapiro.test(x);sprintf("%.3f",y$p.value)}))
    names(a)[2]="p-value(Shapiro)"
    #Anderson-Darling
    b=data.frame(aggregate(formula = value ~ group,
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
  k=length(unique(data$group))
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









#================================================================================
server<-function(input, output, session){

  output$equal_or_not_determine<- renderText({
    equal<-datasetInput()[[12]]
    
    if(equal){
      paste('Equal treatment')
    }else{
      paste('Unequal treatment')
    }
    
  })
  
  
  
  data_sample=c(98.29,95.58,97.39,98.43,95.41,96.87,98.66,95.29,98.3,98.59,95.28,98.89,97.41,97.52,
         94.63,96.44,98.47,94.67,95.68,97.52,97.52,92.43,96.86,97.57,94.03,97.21,98.06,98.2,
         92.09,93.63,94.61,95.28,94.14,93.09,94.47,92.29,91.57,94.13,98.05,91.43,93.38,97.55,
         98.93,94.96,93.61,92.52,94.2,94.21,92.65,92.68,98.79,92.42,97.48,94.34,96.89,93.86,
         95.31,93.76,96.27,96.06,96.33,95.9,96.71,98.38,97.97,97.13,97.42,97.65,98.05,98.11)
  group=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,
          3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4)
  sample_data=as.data.frame(cbind(data_sample,group))
  
  output$dl <- downloadHandler(
    filename = function() {"sample.csv"},
    content = function(file) {write.csv(sample_data, file,row.names = F)}
  )
  
  datasetInput<-reactive({
    input$goButton
    inFile<-isolate(input$file)
    validate(need(inFile!= "","Please select a data set"))
    Alpha<-isolate(as.numeric(input$num.3))
    
    if(is.null(inFile)){return("No Data")}else{
      data<-read.csv(inFile$datapath)
      #---------------------------------
      colnames(data) <- c("data","group")
      k <- max(data[, 2])
      ni <- sapply(1:k, function(a) sum(data[, 2] == a))
      n0 <- floor(1/mean(1/ni))
   
      #number of group

      #Number of Observations
      n <- sum(data[, 2] == 1)
     
      #To see the data equal or not
      equal_or_not=sum(sum(ni)/length(ni)==ni)==length(ni)
  
      if(equal_or_not){
      
        #Xbar_i(n-1)
        MEANi_f <- sapply(1:k, function(z) mean(data[, 1][data[, 2] == z][-n]))
        #Si^2(n-1)
        VARi <- sapply(1:k, function(z) var(data[, 1][data[, 2] == z][-n]))
        #Weights
        rev0 <- sapply(1:k, function(a) ((max(VARi)/VARi[a])-1))
        zz <- ifelse(rev0 < 0, 0, rev0)
        Ui <- (1/n) + (1/n) * sqrt((1/(n-1)) * zz)
        Vi <- (1/n) - (1/n) * sqrt((n-1) * zz)
        #Ui <- sapply(1:k,
        #             function(z) (1/n) + (1/n) * sqrt((1/(n-1)) * ((max(VARi)/VARi[z])-1)))
        #Vi <- sapply(1:k,
        #             function(z) (1/n) - (1/n) * sqrt((n-1) * ((max(VARi)/VARi[z])-1)))
        
        #Weighted Sample Mean
        Xtilde_i <- sapply(1:k, function(z) Ui[z] * sum(data[, 1][data[, 2] == z][-n])
                           + Vi[z] * data[, 1][data[, 2] == z][n])
        weight <- round(rbind(rep(n, k), MEANi_f, sqrt(VARi), Ui, Vi, Xtilde_i), digits = 3)
        rownames(weight) <- c("n", "Xbar_i(n-1)", "Si(n-1)", "Ui", "Vi","Xtilde_i")
        colnames(weight) <- paste("k", 1:k, sep = "")
        
      }else{
        
        #Xbar_i(n0)
        MEANi <- sapply(1:k, function(z) {
          mean(data[data[, 2] == z, "data"][c(1:n0)], na.rm = TRUE)})
        
        #Si^2(n0)
        VARi <- sapply(1:k, function(z){
          var(data[data[, 2] == z, "data"][c(1:n0)], na.rm = TRUE)})
        
        #Weighted Sample Mean
        Xtilde_i=rep(0,length(ni))
        options (warn = -1) # close warning message
        for(i in 1:length(ni)){
          if(ni[i] > n0){
            xbari_i <- sapply(1:k, FUN = function(x){
              mean(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE)})
            vari_i  <- sapply(1:k, FUN = function(x){
              var(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE)})
            z=max(VARi,vari_i)
            #Weights
            rev0 <- sapply(1:k, function(a) (((z*ni[a]/vari_i[a])-1)))
            zz <- ifelse(rev0 < 0, 0, rev0)
            Ui <- sapply(1:k, function(a) (1/ni[a]) + (1/ni[a]) * sqrt(((ni[a]-n0)/n0^2)*zz[a]))
            Vi <- sapply(1:k, function(a) (1/ni[a]) - (1/ni[a]) * sqrt((1/(ni[a]-n0))*zz[a]))
            Xu <- sapply(1:k, FUN = function(x){
              sum(data[data[, 2] == x, "data"][c(1:n0)], na.rm = TRUE) })
            Xv <- sapply(1:k, FUN = function(x){
              sum(data[data[, 2] == x, "data"][-c(1:n0)], na.rm = TRUE) })
            Xtilde_i[i] <- sapply(1:k, function(a) Ui[a] * Xu[a] + Vi[a] * Xv[a])[i]
          }else{
            Xtilde_i[i] <- sapply(1:k, function(z) mean(data[, 1][data[, 2] == z]))[i]
          }
        }
        
        weight <- round(rbind(ni, MEANi, sqrt(VARi), Ui, Vi, Xtilde_i), digits = 3)
        rownames(weight) <- c("n", "Xbar_i(n0)", "Si(n0)", "Ui", "Vi", "Xtilde_i")
        colnames(weight) <- paste("k", 1:k, sep = "")
        
      }
      
  
      #Simulation for Critical Value
      set.seed(1)
      time <- 4
      sim <- 100000
      X <- matrix(0, sim, time)
      scv <- sapply(1:time, function(z) { ran <- rep(0, sim)
      for (i in 1:sim){
        t <- rt(n = k, df = n-2)
        ran[i] <- max(t - mean(t))
      }
      X[, z] <- ran
      }
      )
      #Result of Critical Value
      alpha<-Alpha
      
      CV.mean <- mean(sapply(1:time, function(z) quantile(scv[, z], 1-alpha/2)))
      CV.se <- sd(sapply(1:time, function(z) quantile(scv[, z], 1-alpha/2)))
      CV <- paste0(round(CV.mean,3)," (", round(CV.se,3),")")
      #Lower and Upper Decision Line (LDL and UDL)
      LDL <- mean(Xtilde_i) - CV.mean * max(sqrt(VARi))/sqrt(n0)
      UDL <- mean(Xtilde_i) + CV.mean * max(sqrt(VARi))/sqrt(n0)
    }
    
    #Computing p-Value
    Pseq <- seq(0, 1, 0.001)
    Pquantile <- sapply(Pseq,
                        function(w) mean(sapply(1:time, function(z) quantile(scv[, z], 1-w/2))))
    Pequation <- abs(Pquantile - max(abs((Xtilde_i - mean(Xtilde_i))
                                         / (max(sqrt(VARi))/sqrt(n0)))))
    P.value <- Pseq[Pequation == min(Pequation)]
    
    
    #new HANOM Decision Chart
    #-------------------------------
    center <- mean(Xtilde_i)
    Uylim <- max(Xtilde_i, UDL)                               
    Lylim <- min(Xtilde_i, LDL)                              
    #
    return(list(Data=data,Uylim=Uylim,Lylim=Lylim,k=k,Xtilde_i=Xtilde_i,LDL=LDL,UDL=UDL,center=center,
                weight=weight,CV=CV,P.value=P.value,determine=equal_or_not))
  })
  
  output$distPlot.1<-renderPlot({
    data<-datasetInput()[[1]];Uylim<-datasetInput()[[2]];Lylim<-datasetInput()[[3]]
    k<-datasetInput()[[4]];Xtilde_i<-datasetInput()[[5]];LDL<-datasetInput()[[6]];UDL<-datasetInput()[[7]];
    center<-datasetInput()[[8]]
    k.1<-k+0.3
    par(mar=c(4.5,4.5,4,6))
    plot(Xtilde_i, pch=16, las = 2,xlab=colnames(data)[2],ylab=NA,xlim=c(0.7, k+0.3),ylim = c(Lylim, Uylim),main = "HANOM Chart", xaxt = "n")                            
    xaxis <- seq(1, k, 1)                                        
    axis(1, at=1:k, labels = formatC(xaxis))                     
    abline(h = LDL, lty = 2, lwd = 2)
    abline(h = UDL, lty = 2, lwd = 2)
    abline(h = center)
    sapply(1:k, function(a) arrows(a, min(Xtilde_i[a], mean(Xtilde_i)), a, max(Xtilde_i[a], mean(Xtilde_i)), length = 0))
    mtext(paste("LDL = ", sprintf("%.3f", LDL)), side = 4, at = LDL,las = 2, cex = 0.9, line = 0.3)
    mtext(paste("UDL = ", sprintf("%.3f", UDL)), side = 4, at = UDL,las = 2, cex = 0.9, line = 0.3)
    mtext(bquote(bar(widetilde(X)) == .(sprintf("%.3f", mean(Xtilde_i)))),side = 4, at = mean(Xtilde_i), las = 2, cex = 0.9, line = 0.4)
  })
  

  output$table.1<-renderTable(rownames = T,quoted = F, digits = 3,{
    k<-datasetInput()[[4]]
    V<-datasetInput()[[9]]
 #   V<-V[-c(2,3),]
    NB<-c(1:k)
    NB.1<-paste0("Treat",".",NB)
    colnames(V)<-NB.1
    row.names(V)<-c("n","X_bar_i","S_i","U_i","V_i","X_tilde_i")
    return(V)
    
  })
  
  output$table.2<-renderTable(rownames = F,colnames = F,quoted = F, digits = 3,{
    W<-datasetInput()[[10]]
    return(W[1])
  })
  
  output$table.3<-renderTable(rownames = F,colnames = F,quoted = F, digits = 3,{
    p<-datasetInput()[[11]]
    return(p[1])
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
  
  return(df)
  
}


