# Assemble a driver, throw in data, specify p-value, get output: summary and chart
pacman::p_load(shiny, dplyr, data.table, shinythemes, shinycssloaders, waiter, ggplot2, ggtext, ggthemr, waiter)
one_run<- function(data, alpha= 0.05, iter= 10000, time= 4){
  
  data<- as.data.frame(data)
  varname<- colnames(data)
  varname<- c(varname, paste0(varname[2], varname[3], sep=":"))
  # Predefined functions:
  # Functions ---------------------------------------------------------------
  # Calculate Statistics
  MeanP1<- function(data){
    
    #Current Restriction: 1st column as value, 2nd column as treatment/ factor
    data<- data.frame(data)
    tr<- as.factor(data[,2])
    val<- as.numeric(data[,1])
    
    k <- levels(tr)
    nk <- nlevels(tr)    #levels of treatment
    ni <- tr %>% table #number of samples in each treatment
    
    # Random sample ni-1 ro calculate sample mean and sample variance
    xbari <- sapply(1:nk, function(a)  mean(val[tr == k[a]][-ni[a]]))
    vari <- sapply(1:nk, function(a) var(val[tr == k[a]][-ni[a]]))
    
    # Calculate weighted sample mean: Xtilde
    maxvar <- max(vari)
    Ui <- sapply(1:nk, function(a) (1/ni[a]) + (1/ni[a]) * sqrt(max(0, (1/(ni[a]-1))*(maxvar/vari[a] - 1)) ) )#Eq4
    Vi <- sapply(1:nk, function(a) (1/ni[a]) - (1/ni[a]) * sqrt(max(0, (ni[a]-1)*(maxvar/vari[a] - 1)) ) )  #Eq5
    xtildei <- sapply(1:nk, function(a) Ui[a] * sum(val[tr == k[a]][-ni[a]]) + Vi[a] * val[tr == k[a]][ni[a]])
    
    out<- rbind(ni, xbari, vari, Ui, Vi, xtildei)
    return(out)
  }
  MeanP2<- function(data){
    
    #Current Restriction: 1st column as value, 2nd column as treatment/ factor
    data<- data.frame(data)
    tr<- as.factor(data[,2])
    val<- as.numeric(data[,1])
    
    k <- levels(tr)
    nk <- nlevels(tr)    #levels of treatment
    #k<- unique(tr)
    #nk<- length(k)
    ni <- tr %>% table  #number of samples in each treatment
    n0<- min(ni-1)
    
    # Random sample ni-1 ro calculate sample mean and sample variance
    xbari <- sapply(1:nk, function(a) mean(val[tr==k[a]][c(1:n0)], na.rm=TRUE) )
    vari <- sapply(1:nk, function(a)  var(val[tr == k[a]][c(1:n0)], na.rm=TRUE) )
    
    # Calculate weighted sample mean: Xtilde
    maxvar <- max(vari/ni)
    Ui <- sapply(1:nk, function(a) (1/ni[a]) + (1/ni[a]) * sqrt( max(0, ((ni[a]-n0)/n0) *(ni[a]*maxvar/vari[a] - 1)) )) #Eq15
    Vi <- sapply(1:nk, function(a) (1/ni[a]) - (1/ni[a]) * sqrt( max(0, (n0/(ni[a]-n0) * (ni[a]*maxvar/vari[a] - 1))) )) #Eq16
    xtildei <- sapply(1:nk, function(a) Ui[a] * sum(val[tr == k[a]][c(1:n0)]) + Vi[a] * sum(val[tr == k[a]][c((n0+1):ni[a])]) )
    
    out<- rbind(ni, n0, xbari, vari, Ui, Vi, xtildei)
    return(out)
  }
  
  # Distribution functions
  hdist<- function(ni, iter){
    k<- length(ni)
    ti<- sapply(1:k, function(p) rt(iter, ni[p]-2))   #t(ni-2) distribution
    ttildei<- sapply(1:iter, function(i) {
      sapply(1:k, function(p) ((k-1)/k)*ti[i,p]-(sqrt(ni[p])/k)*sum(ti[i,-p]/sqrt(ni[-p])))  #Eq9 (transform student-t dis into ttilde)
    })
    cv<- data.frame(Min= apply(ttildei, 2, min), Max= apply(ttildei, 2, max))
    return(cv)
  }
  ddist<- function(n0, iter){
    k<- length(n0)
    ti <- sapply(1:iter, function(i) rt(n = k, df = n0[1]-1))
    out<- apply(ti, 2, function(t) max(t - mean(t)))
    return(out)
  }
  
  # Compute P-Value 
  # W must be a WeightedMean() object
  # cvdist must be a SimCV() object
  pvalP1<- function(W, cvdist){
    xbari<- W['xbari',]
    vari<- W['vari',]
    xtildei<- W['xtildei',]
    ni<- W['ni',]
    
    tval<- (xtildei- mean(xtildei))/(sqrt(max(vari)/ni))
    
    pseq<- seq(0, 1, by= 0.001)
    pquantile<- sapply(pseq, function(w) quantile(cvdist, 1-w/2))
    pequation<- abs(pquantile- max(abs(tval)))
    pval<- pseq[pequation== min(pequation)]
    
    return(pval)
    
  }
  pvalP2<- function(W, cvdist){
    xbari<- W['xbari',]
    vari<- W['vari',]
    xtildei<- W['xtildei',]
    ni<- W['ni',]
    
    tval<- (xtildei- mean(xtildei))/(sqrt(max(vari/ni)))
    
    pseq<- seq(0, 1, by= 0.001)
    pquantile<- sapply(pseq, function(w) quantile(cvdist, 1-w/2))
    pequation<- abs(pquantile- max(abs(tval)))
    pval<- pseq[pequation== min(pequation)]
    
    return(pval)
  }
  
  MetricsP1<- function(W, criticalvalue){
    xtildei <- W["xtildei",]
    k<- ncol(W)
    ni <- W["ni",]
    z1<- max(W["vari",]) 
    
    center <- mean(xtildei)
    LDL <- mean(xtildei) - criticalvalue * sqrt(z1) / sqrt(ni)
    UDL <- mean(xtildei) + criticalvalue * sqrt(z1)/ sqrt(ni)
    
    out<- rbind(W, center=center, LDL=LDL, UDL=UDL)
    #rownames(out) <- c("\\( n_{i} )\\", "\\( \\bar{X_i} \\)", "\\( S_{i} \\)", "\\( U_{i} \\)", "\\( V_{i} \\)", "\\( \\tilde{X_i} \\)", "Center", "LDL", "UDL")
    
    return(out)
  }
  MetricsP2<- function(W, criticalvalue){
    xtildei <- W["xtildei",]
    k<- ncol(W)
    ni <- W["ni",]
    z1<- max(W["vari",]/ni)
    
    center <- mean(xtildei)
    LDL <- mean(xtildei) - criticalvalue * sqrt(z1)
    UDL <- mean(xtildei) + criticalvalue * sqrt(z1)
    
    out<- rbind(W, center=center, LDL=LDL, UDL=UDL)
    #rownames(out) <- c("\\( n_{i} )\\", "\\( \\bar{X_i} \\)", "\\( S_{i} \\)", "\\( U_{i} \\)", "\\( V_{i} \\)", "\\( \\tilde{X_i} \\)", "Center", "LDL", "UDL")
    
    return(out)    
  }
  
  ggChart2 <- function(charttable, text1, text2, text3){
    k<- colnames(charttable)
    M<- data.frame(t(charttable))
    M$xlabel <- k
    M$xnum <- as.factor(k) %>% as.numeric
    q<- nrow(M)
    
    
    #g<- ggplot(M, aes(x = xnum, xtildei, group=1)) +
    #  geom_point()+
    #  geom_segment(aes(xend= xnum, yend = pmin(xtildei, center))) +
    #  geom_segment(aes(xend= xnum, yend = pmax(xtildei, center))) +
    #  geom_line(aes(y = LDL), linetype = "dashed", linewidth=0.5, color="black") +
    #  geom_line(aes(y = UDL), linetype = "dashed", linewidth=0.5, color="black") +
    #  geom_hline(aes(yintercept= center)) + 
    #  theme_minimal()
    
    g<- ggplot(M, aes(xnum, xtildei, group=1)) +
      geom_point() +
      geom_segment(aes(xend= xnum, yend = pmin(xtildei, center))) +
      geom_segment(aes(xend= xnum, yend = pmax(xtildei, center))) +
      
      #coord_cartesian(xlim=c(0.8,q+0.2)) +
      geom_hline(aes(yintercept= center)) + 
      
      # Draw boundaries
      geom_step(aes(x=xnum-0.5, y= UDL), linetype="dashed", linewidth=0.5) +
      geom_step(aes(x=xnum-0.5, y= LDL), linetype="dashed", linewidth=0.5) +
      geom_segment(aes(x=0, xend =0.5, y= UDL[1], yend= UDL[1]), linetype= "dashed", linewidth=0.5) +
      geom_segment(aes(x=Inf, xend = q-0.5, y= UDL[q], yend= UDL[q]), linetype= "dashed", linewidth=0.5)+
      geom_segment(aes(x=0, xend = 0.5, y= LDL[1], yend= LDL[1]), linetype= "dashed", linewidth=0.5)  +
      geom_segment(aes(x=Inf , xend =q-0.5, y= LDL[q], yend= LDL[q]), linetype= "dashed", linewidth=0.5)
    
    # Set x-limits
    g<- g + coord_cartesian(xlim=c(0.8,q+0.2)) + 
      scale_x_continuous(breaks= c(1:q), labels = M$xlabel) +
      labs(title=text1, subtitle= text2, x= text3, y= "Xtilde")+
      theme(plot.margin= margin(20,50,25,25), 
            plot.caption = element_text(hjust=0),
            axis.text.x = element_text(size=14)) 
    ggthemr('pale')
    return(g)
  }
  
  # First: Given data, simulate distribution
  simdist<- function(data){
    # Calculate Statistics
    newdt<<- data.frame(data, inter= paste(data[,2],data[,3], sep=":"))  #Define interaction column
    cn<- colnames(data)
    colnames(newdt)[4]<- paste(cn[2], cn[3], sep="*")
    
    p1.mean<- lapply(list(newdt[,c(1,2)], newdt[,c(1,3)], newdt[,c(1,4)]), MeanP1)
    p2.mean<- lapply(list(newdt[,c(1,2)], newdt[,c(1,3)], newdt[,c(1,4)]), MeanP2)
    
    # Simulate distribution
    ni <- lapply(p1.mean, function(x) x['ni',])
    n0 <- lapply(p2.mean, function(x) x['n0',])
    distP1<- lapply(1:3, function(i) sapply(1:time, function(q) hdist(ni[[i]], iter)))
    distP2<- lapply(1:3, function(i) sapply(1:time, function(q) ddist(n0[[i]], iter)))
    return(list(p1.mean= p1.mean, p2.mean= p2.mean, distP1= distP1, distP2= distP2))
  }
  
  # Second: Given alpha and sumulated distribution, calculate critical values and p-values
  critical_f<- function(simdist_object, alpha){ 
    
    cv.p1<-sapply(1:3, function(i) sapply(1:time, function(q) quantile(simdist_object$distP1[[i]][2,q]$Max, 1-alpha/2)) )
    pval.p1<- sapply(1:3, function(i) sapply(1:time, function(q) pvalP1(simdist_object$p1.mean[[i]], simdist_object$distP1[[i]][2,q]$Max)))
    out.p1<- cbind(cvmean= apply(cv.p1,2, mean), cvse = apply(cv.p1,2, sd),pval = apply(pval.p1,2, mean))
    
    cv.p2<-sapply(1:3, function(i) apply(simdist_object$distP2[[i]], 2, function(q) quantile(q, 1-alpha/2)) )
    pval.p2<- sapply(1:3, function(i) sapply(1:time, function(q) pvalP2(simdist_object$p2.mean[[i]], simdist_object$distP2[[i]][,q])))
    out.p2<- cbind(cvmean= apply(cv.p2,2, mean), cvse = apply(cv.p2,2, sd),pval = apply(pval.p2, 2, mean))
    
    return(list(outP1= out.p1, outP2= out.p2))
    
  }

  # Third: Given critical values and p-values, update statistics and chart values
  chartval<- function(simdist_object, critical_f_object){
    v1.p1<- MetricsP1(simdist_object$p1.mean[[1]], critical_f_object$outP1[1,1]) 
    v2.p1<- MetricsP1(simdist_object$p1.mean[[2]], critical_f_object$outP1[2,1])
    v3.p1<- MetricsP1(simdist_object$p1.mean[[3]], critical_f_object$outP1[3,1])
    
    v1.p2<- MetricsP2(simdist_object$p2.mean[[1]], critical_f_object$outP2[1,1]) 
    v2.p2<- MetricsP2(simdist_object$p2.mean[[2]], critical_f_object$outP2[2,1] )
    v3.p2<- MetricsP2(simdist_object$p2.mean[[3]], critical_f_object$outP2[3,1])
    
    return(list(MainEffect1.p1=v1.p1, MainEffect2.p1=v2.p1, InteractionEffect.p1=v3.p1, MainEffect1.p2=v1.p2, MainEffect2.p2= v2.p2, InteractionEffect.p2=v3.p2))
  }
  
  # Run and assemble all outputs
  sim<- simdist(data)
  cri<- critical_f(sim, alpha)
  out.chartval<- chartval(sim,cri)

  # Plotting  
  output<- list()
  output$plot1.p1<- ggChart2(out.chartval$MainEffect1.p1, text1= paste0("P1 Main Effect: ",varname[2]), text2= "Treatment mean outside of decision limits suggests statistical difference from the overall average.", text3= "" )
  output$plot2.p1<- ggChart2(out.chartval$MainEffect2.p1, text1= paste0("P1 Main Effect: ",varname[3]), text2= "Treatment mean outside of decision limits suggests statistical difference from the overall average.", text3= "" )
  output$plot3.p1<- ggChart2(out.chartval$InteractionEffect.p1, text1= paste0("P1 Interaction Effect: ",varname[4]), text2= "Treatment mean outside of decision limits suggests statistical difference from the overall average.", text3= "" ) + 
    labs(caption = "The colon operator ':' indicates interaction among two or more treatments")
  
  output$plot1.p2<- ggChart2(out.chartval$MainEffect1.p2, text1= paste0("P2 Main Effect: ",varname[2]), text2= "Treatment mean outside of decision limits suggests statistical difference from the overall average.", text3= "" )
  output$plot2.p2<- ggChart2(out.chartval$MainEffect2.p2, text1= paste0("P2 Main Effect: ",varname[3]), text2= "Treatment mean outside of decision limits suggests statistical difference from the overall average.", text3= "" )
  output$plot3.p2<- ggChart2(out.chartval$InteractionEffect.p2, text1= paste0("P2 Interaction Effect: ",varname[4]), text2= " ", text3= "" ) + 
    labs(caption = "The colon operator ':' indicates interaction among two or more treatments")
  
  return(list(out_summary= out.chartval, out_plot= output))
}

dt2002_d1<- fread("Data/2002data/2002data_S1.csv", stringsAsFactors = T)
one_run(dt2002_d1, iter=100)
