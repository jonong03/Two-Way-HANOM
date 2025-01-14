rm(list=ls()); gc()
pacman::p_load(dplyr)

# Data Generation ---------------------------------------------------------

dt <- data.frame(ToothGrowth)
dt<-  dt %>% 
  mutate(dose= as.factor(dose) %>% as.numeric, supp= as.numeric(supp)) %>%
  rename(value=len)

value=c(97.1, 96.6,	97.4,	95.4,	96.3,	98.2,	95.1,	98.2,	98.3,	96.4,	98.5,	97.4,	97.3,	99.2,	98.1,
        95, 93.4,	94.3,	96.1,	94,	96.2,	92.3,	93.4,	94.5,	95.4,	94.1,	96,	94.2,	92.3,	94, 95.4,
        93.5,	97.6,	98.8,	94.9,	93.7,	92.6,	94.5,	94.4,	92.6,	94.7,	98.8,	92.9,	97.8,	94.7,	96.8,	93.8,	95.9,	93.5,	96.6,	96.7,	96.3, 96.6,	
        96.7,	96.3,	95.9,	96.7,	98.4,	97.7,	97.3,	97.2,	97.5,	98.5,	98.1,	98.1,	96.6)
group=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,4,4,4)
sample_data=as.data.frame(cbind(value,group))
data <- sample_data
data <- ToothGrowth

# Functions ---------------------------------------------------------------

WeightedMean<- function(data){
  
  #Current Restriction: 1st column as value, 2nd column as treatment/ factor
  data<- data.frame(data)
  tr<- as.factor(data[,2])
  val<- as.numeric(data[,1])
  
  k <- levels(tr)
  nk <- nlevels(tr)    #levels of treatment
  #k<- unique(tr)
  #nk<- length(k)
  ni <- tr %>% table  #number of samples in each treatment
  
  # Random sample ni-1 ro calculate sample mean and sample variance
  xbari <- sapply(1:nk, function(a)  mean(val[tr == k[a]][-ni[a]]))
  vari <- sapply(1:nk, function(a) var(val[tr == k[a]][-ni[a]]))
  
  # Calculate weighted sample mean: Xtilde
  maxvar <- max(vari)
  Ui <- sapply(1:nk, function(a) (1/ni[a]) + (1/ni[a]) * sqrt((1/(ni[a]-1))*(maxvar/vari[a] - 1)) ) #Eq4
  Vi <- sapply(1:nk, function(a) (1/ni[a]) - (1/ni[a]) * sqrt((ni[a]-1)*(maxvar/vari[a] - 1)) )  #Eq5
  xtildei <- sapply(1:nk, function(a) Ui[a] * sum(val[tr == k[a]][-ni[a]]) + Vi[a] * val[tr == k[a]][ni[a]])

  out<- rbind(ni, xbari, vari, Ui, Vi, xtildei)
  return(out)
}

# Simulate h* distribution (critical values)
SimCV<- function(ni, iter=100){
  OneRun<- function(ni){
    k<- length(ni)
    ti<- sapply(1:k, function(p) rt(1, ni[p]-2))   #t(ni-2) distribution
    ttildei<- sapply(1:k, function(p) ((k-1)/k)*ti[p]-(sqrt(ni[p])/k)*sum(ti[-p]/sqrt(ni[-p])))  #Eq9 (transform student-t dis into ttilde)
    cv<- c(Min= min(ttildei), Max= max(ttildei))
    
    return(cv)
  }
  out<- sapply(1:iter, function(i) OneRun(ni)) %>% t 
  return(out)
}

# Calculate critical values at level alpha, based on cvdist (must be an object from SimCV() ).
CalcCV<- function(cvdist, alpha){
  CV.k<- quantile(cvdist[,2], 1-alpha/2)
  CV.1<- -quantile(cvdist[,1], alpha/2)
  max(CV.1, CV.k)  #Eq11
}

# SimCV distributions for all effects:
SimCV_all<- function(wlist){
  
  # Calculate Critical Values
  # Repeat critical value calculation process for a number of times. 
  time<- 4; iter<- 1000
  cv.dist<- lapply(1:2, function(i) lapply(1:time, function(j) SimCV(wlist[[i]]['ni',])) %>% do.call(cbind,.))

  return(cv.dist)
}

Wlist<- lapply(list(ToothGrowth[,c(1,2)], ToothGrowth[,c(1,3)]), WeightedMean)
SimCV_all(Wlist)

# Compute P-Value 
# W must be a WeightedMean() object
# cvdist must be a SimCV() object
CalcP<- function(W, cvdist){
  xbari<- W['xbari',]
  vari<- W['vari',]
  xtildei<- W['xtildei',]
  ni<- W['ni',]
  
  tval<- (xtildei- mean(xtildei))/(sqrt(max(vari)/ni))
  
  pseq<- seq(0, 1, by= 0.001)
  pquantile<- sapply(pseq, function(w) quantile(cvdist[,2], 1-w/2))
  pequation<- abs(pquantile- max(abs(tval)))
  pval<- pseq[pequation== min(pequation)]

  return(pval)
  
  #if(P.value1 > 0 && P.value1 < 1){
  #  PV1<-sprintf("%.3f",P.value1)
  #}else if(P.value1 == 0){
  #  PV1<-"< 0.001 "
  #}else{PV1 <-"> 0.999"}
  
}


# Chart for P1 Procedure
ChartP1 <-function(W, criticalvalue){
  xtildei <- W["xtildei",]
  k<- ncol(W)
  z1<- max(W["vari",])
  ni <- W["ni",]
  
  center <- mean(xtildei)
  LDL <- mean(xtildei) - criticalvalue * sqrt(z1) / sqrt(ni)
  UDL <- mean(xtildei) + criticalvalue * sqrt(z1) / sqrt(ni)
  
  Uylim <- max(xtildei, UDL)                                
  Lylim <- min(xtildei, LDL)   
  
  k.1<-k+0.3
  par(mar=c(4.5,4.5,4,6))
  plot(xtildei, pch=16,las = 2,ylab=NA,xlim=c(0.7, k+0.3),ylim = c(Lylim, Uylim), main = "P1 HANOM Chart", xaxt = "n")                            
  xaxis <- seq(1, k, 1)                                        
  axis(1, at=1:k, labels = formatC(xaxis))                     
  abline(h = center)                                              
  sapply(1:k, function(a) arrows(a, min(xtildei[a], mean(xtildei)), a, max(xtildei[a], mean(xtildei)), length = 0))
  lines(1:(k+1)-.5, c(LDL, LDL[k]), type = "s", lty = 2, lwd = 2)  
  lines(1:(k+1)-.5, c(UDL, UDL[k]), type = "s", lty = 2, lwd = 2)  
  sapply(1:k, function(a) arrows(a, min(xtildei[a], mean(xtildei)), a, max(xtildei[a], mean(xtildei)), length = 0))
  mtext(paste("LDL = ", sprintf("%.3f", LDL)), side = 4, at = LDL, las = 2, cex = 0.9, line = 0.3)
  mtext(paste("UDL = ", sprintf("%.3f", UDL)), side = 4, at = UDL, las = 2, cex = 0.9, line = 0.3)
  mtext(bquote(bar(widetilde(X)) == .(sprintf("%.3f", mean(xtildei)))),side = 4, at = mean(xtildei), las = 2, cex = 0.9, line = 0.4)
        
} 

# ANOVA Test for interaction
# data must only consists three columns: one numerical value column and two factor columns
interaction_test<-function(data){
  out_aov<- aov(value~.^2, data)
  out2<- data.frame(summary(out_aov)[[1]])
  row_name<- rownames(out2)
  p_val<- out2[grepl(":", row_name),5]
  inter<- sum(which(p_val<= 0.05)) >0  #Interaction effect is significant if result is TRUE
  
  return(inter)
}

# P1: Single Stage Sampling Procedure -------------------------------------
alpha=0.05

# Example for one factor HANOM
HANOM_P1<- function(data, alpha=0.05){
  
  X<- WeightedMean(data)
  ni<- X['ni',]
  
  # Calculate Critical Values
  # Repeat critical value calculation process for a number of times. 
  time<- 4
  iter<- 10000
  cv.dist<- array(0, dim=c(iter, 2, time))
  
  set.seed(NULL)
  for (i in 1:time){
    cv.dist[,,i]<- SimCV(ni, iter)
  }
  h<- sapply(1:time, function(i) CalcCV(cv.dist[,,i], alpha))
  h<- data.frame(Mean= mean(h), `S.E.`= sd(h))
  rownames(h) <- c("Critical Value")
  
  p.val<- sapply(1:time, function(i) CalcP(X, cv.dist[,,i]) ) %>% mean
  h<- cbind(h, pval= p.val)  
  
  return(list(summary= X, result= h))
        
}

chart<- ChartP1(W=X, criticalvalue= h$Mean)


aa<- HANOM_P1(data, alpha=0.05)
aa$chart

# Example for a two-factor HANOM
data<- dt[,c(1,4)]
HANOM_P1(dt[,c(1,4)])

dt <- ToothGrowth %>% 
  mutate(dose= as.factor(dose) %>% as.numeric, supp= as.factor(supp)) %>%
  rename(value=len)
if( interaction_test(dt) ) {
  dt$inter <- paste(dt[,2],dt[,3], sep="-") %>% as.factor
  dt$inter <- as.numeric(as.factor(dt$inter))
}

out<- lapply(list(dt[,c(1,2)], dt[,c(1,3)], dt[,c(1,4)]), function(x) HANOM_P1(data=x, alpha=alpha))
out[[1]]$summary


# Interaction Effect ------------------------------------------------------



# P2 Procedure ------------------------------------------------------------

data= sample_data
data= ToothGrowth
data$inter= paste(data[,2], data[,3], sep= "-")
data= data[,c(1,4)]
W<- MeanP2(data)
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

OneRunP2<- function(W){
  n0<- W['n0',] %>% unique()
  k<- ncol(W)
  t <- rt(n = k, df = n0-1)
  max(t - mean(t))
}
OneRunP2(W)

# Calculate h distribution
iter<- 100000
times<- 4
totalloop<- iter*times*3
dtemp<- array(0, dim=c(iter,times))  
out.ddist<- list(dtemp, dtemp, dtemp)
  
for(h in 1:3){
  for(j in 1:times){
    for(i in 1:iter){
      out.ddist[[h]][i,j] <- OneRunP2(W) #Min, Max
    }
  }
}

alpha= 0.05

dist= out.ddist
wlist=list(W,W,W)
CalcP.P2<- function(W, cvdist){
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

testP2<- function(wlist, dist, alpha){
  k<- length(dist)
  time<- 4
  out<- NULL
  
  for(q in 1:k){
    # Calculate critical values
    d<- sapply(1:time, function(i) quantile(dist[[q]][,i], 1-alpha/2))
    h<- data.frame(`CV Mean`= mean(d), `CV S.E.`= sd(d) )
    
    # Calculate p-value
    p.val<- sapply(1:time, function(i) CalcP.P2(wlist[[q]], dist[[q]][,i]) ) %>% mean
    h<- cbind(h, pval= p.val) 
    out<- rbind(out, h)
  }
  rownames(out) <- c(1:k)
  return(out)
}

ooo<- testP2(wlist, dist, alpha)

MetricsP2(wlist[[1]], ooo[1,1])

LDL2
UDL2


data$inter <- paste(data[,2], data[,3],sep="-")
W<- lapply(list(data[,c(1,2)], data[,c(1,3)], data[,c(1,4)]), MeanP1)

iter<- 1000
times<- 4
totalloop<- iter*times*3
htemp<- array(0, dim=c(iter,2*times))
out.hdist<- list(htemp, htemp, htemp)

for(h in 1:3){
  for(j in 1:times){
  for(i in 1:iter){
  out.hdist[[h]][i,(2*j-1):(2*j)] <- OneRunP1(W[[h]]['ni',]) #Min, Max
}
}
}

p1.test<- testP1(W, out.hdist, 0.05)

v1<- MetricsP1(W[[1]], p1.test[1,1]) 
v2<- MetricsP1(W[[2]], p1.test[2,1] )
v3<- MetricsP1(W[[3]], p1.test[3,1])


ggChart2 <- function(charttable, text1, text2, text3){
  k<- colnames(charttable)
  M<- data.frame(t(charttable))
  M$xlabel <- k
  M$xnum <- as.factor(k) %>% as.numeric
  q<- nrow(M)
  
  g<- ggplot(M, aes(xnum, xtildei, group=1)) +
    geom_point() +
    geom_segment(aes(xend= xnum, yend = pmin(xtildei, center))) +
    geom_segment(aes(xend= xnum, yend = pmax(xtildei, center))) +
    
    #coord_cartesian(xlim=c(0.8,q+0.2)) +
    geom_hline(aes(yintercept= center)) + 
    
    # Draw boundaries
    geom_step(aes(x=xnum-0.5, y= UDL), linetype="dashed", size=0.5) +
    geom_step(aes(x=xnum-0.5, y= LDL), linetype="dashed", size=0.5) +
    geom_segment(aes(x=0, xend =0.5, y= UDL[1], yend= UDL[1]), linetype= "dashed", size=0.5) +
    geom_segment(aes(x=Inf, xend = q-0.5, y= UDL[q], yend= UDL[q]), linetype= "dashed", size=0.5) +
    geom_segment(aes(x=0, xend = 0.5, y= LDL[1], yend= LDL[1]), linetype= "dashed", size=0.5)  +
    geom_segment(aes(x=Inf , xend =q-0.5, y= LDL[q], yend= LDL[q]), linetype= "dashed", size=0.5)
  
  
  # Set x-limits
  g<- g + coord_cartesian(xlim=c(0.8,q+0.2)) + 
    scale_x_continuous(breaks= c(1:q), labels = M$xlabel)+
    theme(plot.margin= margin(50,50,20,20))
   # labs(title=text1, subtitle= text2, x= text3, y= "Xtilde") +
    
  ggthemr('pale') 
  
  return(g)
  
  
}

ggChart2(v3, "Main Effect: Supp", "HANOM P1 Results: p-value", "Supp")


pacman::p_load(devtools,ggthemr)
devtools::install_github('Mikata-Project/ggthemr')

