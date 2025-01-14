rm(list=ls()); gc()
pacman::p_load(data.table, dplyr, car)

data= fread("C:/Users/jonog/OneDrive/Documents/Projects/HANOM/TwoWayHANOM/Data/Sample Data/Diet2.csv")

desc_f<- function(dt){
  
  data<- data.table(dt[,c(1,2)])
  cn <- colnames(dt)
  
  setnames(data,new=c("val","tr"))
  
  data[,`:=`(val= as.numeric(val), tr= as.factor(tr))]
  
  grandmean<- data[,mean(val, na.rm=TRUE)]
  summary<- data[,.(ni=.N, xbar=mean(val, na.rm=TRUE),
                 var= var(val, na.rm=TRUE)%>% round(.,2) ), by=tr]
  
  lev<- leveneTest(val~tr, data)
  sha<- shapiro.test(data$val)
  
  setnames(summary,old="tr",new= cn[2])
  return(list(variables= c(paste("Factor:", cn[2]),
                           paste("Value:", cn[1]),
                           paste("Grandmean:", grandmean)),
              summary= summary, levene= lev, shapiro= sha))
}
aov_f<- function(dt){

  #Current Restriction: 1st column as value, 2nd column as treatment/ factor
  cn <- colnames(dt)
  cn<- c(cn,paste0(cn[2],":",cn[3]))
  
  data<- data.table(dt)
  setnames(data,new=c("val","tr1","tr2"))
  data[,tr3:= paste0(tr1,"-",tr2)]
  data[,`:=`(val= as.numeric(val), tr1= as.factor(tr1), tr2= as.factor(tr2), tr3= as.factor(tr3))]

  # out1: data summary: contains treatment levels, and sample size in each treatment groups
  # tr | samplesize | xbar | varx
  # 3 tables in total: factor A, factor B, factor AB
  
  setnames(data,new=cn)
  out1<- lapply(list(data[,c(1,2)], data[,c(1,3)], data[,c(1,4)]), desc_f)
  #names(out1)<- cn[2:4]
  
  setnames(data, old= cn[1], new="val")
  out2<- Anova(lm(val~.*., data= data[,1:3]), type="3")
  
  return(list(out1= out1, out2= out2))
  
}

out<- aov_f(data)
out

