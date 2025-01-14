#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pacman::p_load(shiny, vroom, dplyr, data.table, shinythemes, waiter)
  
#Things to fix:
# 1- Regenerate plots with ggplot2
# 2- only reactive/ updated when "Start Analysis" button is clicked
# 3- Generate Summary Statistics for data
# 4- Generate Summary Statistics for HANOM: xtilder, vari, lDL, UDL

# UI ----------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("united"),
                
                # Application title
                titlePanel("Two-way HANOM"),
                
                fluidRow(column(12, "A",
                    fluidRow(column(3, "B",
                                    fileInput("upload", "Upload Data")),
                             column(2," ",
                                    numericInput("alpha","Type I error rate", value=0.05, min=0, max=1, step = 0.01))
                    ),
                    fluidRow(column(3, "C",
                                    #useWaitress(),
                                    #br(),
                                    actionButton("run","Start Analysis"),
                                    tableOutput("desc.summary")),
                             column(9,"C1: Set1",
                                    tabsetPanel(
                                        tabPanel("HANOM P1",
                                                 fluidRow(
                                                     column(6, "C1-1: plot1", plotOutput("plot1", height="350px")),
                                                     column(3, "C1-2: table1", 
                                                            tabsetPanel(
                                                                tabPanel("Result", textOutput("result1_cv"),
                                                                         textOutput("result1_pval")),
                                                                tabPanel("Summary", tableOutput("m1")))
                                                     )),
                                                 fluidRow(
                                                     column(6, "C2-1: plot2", plotOutput("plot2", height="350px")),
                                                     column(3, "C2-2: table2",
                                                            tabsetPanel(
                                                                tabPanel("Result", textOutput("result2_cv"),
                                                                            textOutput("result2_pval")),
                                                                tabPanel("Summary", tableOutput("m2")))
                                                     )),
                                                 fluidRow(
                                                     column(6, "C3-1: plot3", plotOutput("plot3", height="350px")),
                                                     column(3, "C3-2: table3", 
                                                            tabsetPanel(
                                                                tabPanel("Result", textOutput("result3_cv"),
                                                                         textOutput("result3_pval")),
                                                                tabPanel("Summary", tableOutput("m3")),
                                                                tabPanel("Interaction Test", tableOutput("interaction.result")))))
                                        ),
                                        tabPanel("HANOM P2")))
                    )
                )
                )
)

# Functions ---------------------------------------------------------------
ChartP1 <-function(chartvalue, criticalvalue){
  xtildei <- chartvalue["xtildei",]
  k<- ncol(chartvalue)
  z1<- max(chartvalue["vari",])
  ni <- chartvalue["ni",] %>% as.integer()
  
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

ggChart <- function(chartvalue, criticalvalue, title0) {
  xtildei <- chartvalue["xtildei",]
  k <- ncol(chartvalue)
  z1 <- max(chartvalue["vari",])
  ni <- chartvalue["ni",] %>% as.integer()
  
  center <- chartvalue["center",] 
  LDL <- chartvalue["LDL",]
  UDL <- chartvalue["UDL",]
  
  Uylim <- max(xtildei, UDL)*1.1
  Lylim <- min(xtildei, LDL)*0.9
  
  k.1 <- k + 0.3
  
  x<- colnames(chartvalue)
  data <- data.frame(x = x, y = xtildei)
  adjposition<- sapply(1:k, function(i) ifelse(xtildei[i] < mean(xtildei),-1,1) )
  
  p<- ggplot(data, aes(x = x, y = y)) +
    geom_point(shape = 16, color = "#0072B2", size = 4) +
    labs(x = NULL, y = NULL, title = title0) +
    ylim(Lylim, Uylim) +
    
    # Add horizontal lines
    #geom_hline(aes(yintercept = center), linetype = "solid", color = "dodgerblue4", size = 0.75) +
    #geom_hline(aes(yintercept = LDL), linetype = "dashed", size = 0.75, color = "dodgerblue3") +
    #geom_hline(aes(yintercept = UDL), linetype = "dashed", size = 0.75, color = "dodgerblue3") +
    #
    geom_vline(xintercept=seq(1,k-1,1)+.5,color="gray20", linetype= "dotted", size = 0.5)+
    
    # Add labels for center, LDL, UDL
    geom_text(aes(x = -Inf, y = center, label = paste("Center=", round(center,2))),
              hjust = -0.05, vjust = 0, size = 4, color = "#D55E00", fontface = "bold") +
    geom_text(aes(x = -Inf, y = LDL, label = paste("LDL=", round(LDL,2))),
              hjust = -0.05, vjust = 0, size = 4, color = "#009E73", fontface = "bold") +
    geom_text(aes(x = -Inf, y = UDL, label = paste("UDL=",round(UDL,2))),
              hjust = -0.05, vjust = 0, size = 4, color = "#009E73", fontface = "bold") +
    
    # Add segments
    geom_segment(aes(xend = x, yend = pmin(y, center)), size = 1.5, color = "skyblue", lineend="butt") +
    geom_segment(aes(xend = x, yend = pmax(y, center)), size = 1.5, color = "skyblue") +
    
    # Add data point labels
    geom_text(aes(label = round(y, 2)), nudge_y = adjposition * 1, size = 4, color = "#0072B2") +
    
    # Adjust the label position
    #theme_minimal() +
    theme(axis.text.x = element_text(hjust = 0.5, size=14, face= "bold"),
          axis.text.y = element_text(size= 8),
          #panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          plot.caption = element_text(hjust = 0.5, size = 8),
          plot.margin = margin(30, 50, 50, 30),
          axis.ticks = element_blank()
    )
  
  return(p)
}
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

OneRun<- function(ni){
    k<- length(ni)
    ti<- sapply(1:k, function(p) rt(1, ni[p]-2))   #t(ni-2) distribution
    ttildei<- sapply(1:k, function(p) ((k-1)/k)*ti[p]-(sqrt(ni[p])/k)*sum(ti[-p]/sqrt(ni[-p])))  #Eq9 (transform student-t dis into ttilde)
    cv<- c(Min= min(ttildei), Max= max(ttildei))
    return(cv)
}


SimCV<- function(ni, iter){
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

SimCV_all<- function(wlist){
    
    # Calculate Critical Values
    # Repeat critical value calculation process for a number of times. 
    time<- 4
    iter<- 1000
    cv.dist<- lapply(1:length(wlist), function(i) lapply(1:time, function(j) SimCV(wlist[[i]]['ni',], iter)) %>% do.call(cbind,.))
    return(cv.dist)
}

# Calculate critical values at level alpha, based on cvdist (must be an object from SimCV() ).
CalcCV<- function(cvdist, alpha){
    CV.k<- quantile(cvdist[,2], 1-alpha/2)
    CV.1<- -quantile(cvdist[,1], alpha/2)
    max(CV.1, CV.k)  #Eq11
}

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

Calc_all<- function(wlist, dist, alpha){
    k<- length(dist)
    time<- 4
    out<- NULL
    
    for(q in 1:k){
        h<- sapply(1:time, function(i) CalcCV(dist[[q]][,c(1:2)*i], alpha))
        h<- data.frame(`CV Mean`= mean(h), `CV S.E.`= sd(h))
        
        p.val<- sapply(1:time, function(i) CalcP(wlist[[q]], dist[[q]][,c(1:2)*i]) ) %>% mean
        h<- cbind(h, pval= p.val) 
        out<- rbind(out, h)
        
    }
    rownames(out) <- c(1:k)
    return(out)
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
    colnames(data)[1]<- "value"
    out_aov<- aov(value~.^2, data)
    out2<- data.frame(summary(out_aov)[[1]])
    row_name<- rownames(out2)
    p_val<- out2[grepl(":", row_name),5]
    inter<- sum(which(p_val<= 0.05)) >0  #Interaction effect is significant if result is TRUE
    return(list(result= summary(out_aov)[[1]], sig= inter))
}

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
    
    return(list(summary= X, result= h, cv.dist= cv.dist))
    
}

DescStat<- function(DT){
    #x<- DT[,(sapply(DT, is.numeric))]
    #y<- DT[,!(sapply(DT, is.numeric))]
    data.frame(Class= class(x), Obs= length(x), Mean=round(mean(x),2), SD= round(sd(x),2), Min= min(x), Max= max(x))
}



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
    
    out.set1<- eventReactive(input$run,{
        req(input$upload) 
        data<- fread(input$upload$datapath) %>% as.data.frame()
        dt<- data.frame(data, inter= paste(data[,2],data[,3], sep="-"))
        
        out.summary<- lapply(list(dt[,c(1,2)], dt[,c(1,3)], dt[,c(1,4)]), WeightedMean)
        
        out.hdist<- SimCV_all(out.summary)
        
        #iter<- 10000
        #times<- 4
        #ndist<- length(out.summary)
        #withProgress(message = "Simulating h* distribution", {
        #   for (i in seq_len(iter)) {
        #       
        #            incProgress(1 / iter)
        #   }
        #})
        
        iter<- 1000
        times<- 4
        htemp<- array(0, dim=c(iter,2*times))
        out.hdist<- list(htemp, htemp, htemp)
        
        for(h in 1:3){
                for(j in 1:times){
                    for(i in 1:iter){
                        out.hdist[[h]][i,(2*j-1):(2*j)] <- OneRun(out.summary[[h]]['ni',]) #Min, Max
                        #incProgressWaitress(1/(iter*times*3))
                        #incProgressWaitress(1)
                        #Sys.sleep(0.25)
                    }
                }
            }
        
        return(list(data0= data, data=dt, summary= out.summary, hdist= out.hdist))
    
    })
    
    #observeEvent(input$run,
    #             withProgressWaitress({
    #                         for(i in 1:1000){
    #                             incProgressWaitress(1)
    #                             Sys.sleep(0.25)
    #                         }
    #             },  selector = "#load", max = 15, theme = "overlay-percent")
    #)
    
    output$m1 <- renderTable(out.set1()$summary[[1]], rownames=TRUE, striped=TRUE, hover= TRUE, bordered= TRUE, align= "c")
    output$m2 <- renderTable(out.set1()$summary[[2]], rownames=TRUE, striped=TRUE, hover= TRUE, bordered= TRUE, align= "c")
    output$m3 <- renderTable(out.set1()$summary[[3]], rownames=TRUE, striped=TRUE, hover= TRUE, bordered= TRUE, align= "c")
    
    out.ptable<- reactive(Calc_all(out.set1()$summary, out.set1()$hdist, input$alpha))

    output$plot1<- renderPlot(ChartP1(out.set1()$summary[[1]], out.ptable()[1,1]))
    output$plot2<- renderPlot(ChartP1(out.set1()$summary[[2]], out.ptable()[2,1]))
    output$plot3<- renderPlot(ChartP1(out.set1()$summary[[3]], out.ptable()[3,1]))
    
    output$result1_cv<- renderText(paste0("Critical value: ", out.ptable()[1,1] %>% round(.,3)," (s.e.: ", out.ptable()[1,2] %>% round(.,3), ")" ))
    output$result1_pval<- renderText(paste0("p-value: ", out.ptable()[1,3] %>% round(.,3)))
    output$result2_cv<- renderText(paste0("Critical value: ", out.ptable()[2,1] %>% round(.,3)," (s.e.: ", out.ptable()[2,2] %>% round(.,3), ")" ))
    output$result2_pval<- renderText(paste0("p-value: ", out.ptable()[2,3] %>% round(.,3)))
    output$result3_cv<- renderText(paste0("Critical value: ", out.ptable()[3,1] %>% round(.,3)," (s.e.: ", out.ptable()[3,2] %>% round(.,3), ")" ))
    output$result3_pval<- renderText(paste0("p-value: ", out.ptable()[3,3] %>% round(.,3)))
    
    output$interaction.result<- renderTable(interaction_test(out.set1()$data0)$result, rownames=TRUE, align= "c" )
    
    #data<- reactive({
    #    req(input$upload) 
    #    out<- fread(input$upload$datapath) %>% as.data.frame()
    #    out<- data.frame(out, inter= paste(out[,2],out[,3], sep="-"))
    #})
    
    #output$head <- renderTable( head(data(), input$n) )  
    
    #observeEvent(data(),{
    #    choices<- colnames(data())
    #    updateSelectInput(inputId= "numvar", choices= choices)
    #})
    
    #data2 <- reactive({
    #    req(input$numvar)
    #    value.col<- colnames(data())==input$var
    #    data()[,which(value.col)] <- as.numeric(data()[,which(value.col)])
    #    data()[,which(!value.col)]<- sapply(data()[,which(!value.col)], function(x) as.factor(x) )
    #})
    
    #output$desc.summary<- renderTable( sapply(data(), DescStat), digits=2, striped=TRUE, rownames = TRUE )
    
    #inter.result<- interaction_test(data())
    #data()$inter <- paste(data()[,2],data()[,3], sep="-") %>% as.factor
    
    #out.m<-  reactive({
    #    lapply(list(data()[,c(1,2)], data()[,c(1,3)], data()[,c(1,4)]), WeightedMean)
    #})
    
    
    #output$m1<- renderTable(out.m()[[1]][c(1,2,3,6),], rownames=TRUE, striped=TRUE, hover= TRUE, bordered= TRUE, align= "c")
    #output$m2<- renderTable(out.m()[[2]][c(1,2,3,6),], rownames=TRUE, striped=TRUE, hover= TRUE, bordered= TRUE, align= "c")
    #output$m3<- renderTable(out.m()[[3]], rownames=TRUE, striped=TRUE, hover= TRUE, bordered= TRUE, align= "c")
    

    #out.hdist<- reactive(SimCV_all(out.m()))
    
    #out1<- HANOM_P1(data()[,c(1,2)], alpha= 0.05)
    #output$tr1.weightedsummary<- renderTable(out1$summary, rownames = TRUE)
    #output$plot1<- renderPlot(out1$chart)
    #
    ##output$tr2.weightedsummary<- renderTable(WeightedMean(data()[,c(1,3)]), rownames = TRUE)
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#data<- ToothGrowth
#data<- data.frame(data, inter= paste(data[,2],data[,3], sep="-"))
#
#out.m<- lapply(list(data[,c(1,2)],data[,c(1,3)],data[,c(1,4)]), WeightedMean)
#out.hist<- SimCV_all(out.m)
#out.ptable<- Calc_all(out.m, out.hist, alpha=0.05)
#ChartP1(out.m[[1]], out.ptable[1,1])
#
#
#out.m
#times=4
#iter<- 10
#
#
#htemp<- array(0, dim=c(iter,2*times))
#hdist<- list(htemp, htemp, htemp)
#for(h in 1:3){
#    for(j in 1:times){
#        for(i in 1:iter){
#            hdist[[h]][i,(2*j-1):(2*j)] <- OneRun(out.m[[h]]['ni',]) #Min, Max
#        }
#    }
#}
