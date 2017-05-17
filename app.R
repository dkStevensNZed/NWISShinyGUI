#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(DT)
setwd('d:/usgs/Final versions')
source('SiteSelection.r')
source('ProbabilityPlots.r')
source('extractDataShiny.r')

site.df <<- getAllStations()
# # load auxiliary R files
# #save(Result.all.lst,file='Result.SFBay.rData')
if(!exists('r.x')) {
  cat('Loading database ... ');  flush.console()
  load('r.x.rData')
  cat('Done\n');flush.console()
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyUI(fluidPage(
    
    ## custom CSS for 3 column layout (used below for mechanics filter options)
    tags$head(
      tags$style(HTML("
     .multicol {
       -webkit-column-count: 3; /* Chrome, Safari, Opera */
       -moz-column-count: 3; /* Firefox */
       column-count: 3;
     }"
                      ))
      
    ),
    
   tags$style(type='text/css', ".selectize-input { font-size: 10px; line-height: 10px;} .selectize-dropdown { font-size: 10px; line-height: 10px; }"),

   # Application title
   titlePanel("Trial USGS Application"),
   
   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#      sidebarPanel(
   fluidRow(
      column(4,wellPanel(
          
          tags$div(class = "multicol", 
                   checkboxGroupInput(inputId="wateryear", choices = rev(sort(names(r.x))), 
                                      label = "Water Year", selected = 'WY2016'))
      )),
      
      column(2,wellPanel(
           checkboxGroupInput(inputId="site",label="Site",
                            choices=names(r.x[['WY2016']][[1]]$Data),
                            selected='RICH')
       )),
         
       column(2,wellPanel(
           checkboxGroupInput(inputId="parameter",label="Parameters",
                            choices=names(r.x[['WY2016']]),
                            selected='Tempw')
       )),
      
       column(4,wellPanel(sliderInput(inputId="month",
                    label="Month of the year:",
                    min = 1,
                    max = 12,
                    value = 1)
       ))
      
      ),
   
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(
          tabPanel("Time series", plotOutput("tsPlot")), 
          tabPanel("Distribution", plotOutput("distPlot")), 
          tabPanel("Probability", plotOutput("probPlot")), 
          tabPanel("Box/Whisker", plotOutput("boxPlot")), 
          tabPanel("Summary", verbatimTextOutput("sumTable")),
          tabPanel("Table", dataTableOutput("dataTable"))
        )
#        column(8,wellPanel(plotOutput("tsPlot"))),
#        column(8,wellPanel(plotOutput("distPlot")))
         
         
      )
   ))
)

# Define server logic required to draw a histogram

server <- function(input, output) {

   output$tsPlot <- renderPlot({
       WY <- sort(input$wateryear)
       nWY <- length(WY)
       ST <- sort(input$site)
       nST <- length(ST)
       VR <- sort(input$parameter)
       nVR <- length(VR)
       cat(WY,VR,ST,'\n')
       ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     
        w.ind <- 1:nrow(ds.cur)  #which(as.POSIXlt(dt)$mon + 1 == input$month)
        x  <- ds.cur[, 4]
        dt <- ds.cur[, 'dateTime']
        x <- x[w.ind]
        dt <- dt[w.ind]
        st.c <- paste(ST,collapse=', ')
        cols <<- add.alpha(getCols(length(ST)),0.5)
      
      plot(x~dt, type='n', main=paste('Time series for ',st.c,sep=''), 
           col = 'darkgray', xlab='Date/Time',ylab=input$parameter, tcl=.5, las=1)
      u <- par('usr')
      gap.height <- (u[4] - u[3])/2/30
      iPlt <<- 1
      
      sapply(ST,function(st) {
        sn <- as.character(site.df$All[which(site.df$All$Site_name==st),'Site_no'])
        d.tmp <- ds.cur[which(ds.cur$site_no==sn),]
        gap.times <- getGapTimes(d.tmp)
        ds.tmp <- data.frame(dateTime=seq.POSIXt(min(d.tmp$dateTime),
                                                 max(d.tmp$dateTime),by='15 min'))
        d.tmp <- merge(ds.tmp,d.tmp,by='dateTime',all.x=T)
        
        lines(d.tmp[,4]~d.tmp[,'dateTime'],col=cols[iPlt])
        rect(gap.times[,1],u[3]+gap.height*(iPlt-1),
             gap.times[,2],u[3] + gap.height*(iPlt),border=NA,col=cols[iPlt])
        iPlt <<- iPlt + 1
      })
      legend('topleft',legend=ST,cex=0.8,bty='n',fill=cols,border=NA)
      box()
      
   })
   
   output$distPlot <- renderPlot({
     WY <- sort(input$wateryear)
     cat('dP: ',nrow(ds.cur),'\n')
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     nCol <- min(nST,3)
     nRow <- ceiling(nST/3)
     par(mfrow=c(nRow,nCol))
     yLim <- range(ds.cur$ResultU)
     iPlt <<- 1
     cols <<- add.alpha(getCols(length(ST)),0.5)
     
     sapply(ST,function(st) {
       sn <- as.character(site.df$All[which(site.df$All$Site_name==st),'Site_no'])
       hist(ds.cur$ResultU[which(ds.cur$site_no==sn)],col=cols[iPlt],main=WY,
            xlim=yLim,xlab=VR)
       u <- par('usr')
       text(u[1],u[4]-(u[4]-u[3])/20,labels=st,pos=4)
       iPlt <<- iPlt + 1
     })
   })

   output$probPlot <- renderPlot({
     WY <- sort(input$wateryear)
     cat('dP: ',nrow(ds.cur),'\n')
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     cat(WY,ST,VR,'\n')
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     nCol <- min(nST,3)
     nRow <- ceiling(nST/3)
     
     #     par(mfrow=c(nRow,nCol))
     par(mfrow=c(1,1))
     xLim <- range(ds.cur$ResultU)
     iPlt <<- 1
     cols <<- add.alpha(getCols(length(ST)),0.5)
     addData <<- F
     maxN <- max(table(ds.cur$site_no))
     
     sapply(ST,function(st) {
       sn <- as.character(site.df$All[which(site.df$All$Site_name==st),'Site_no'])
       wy <- paste(WY,collapse=', ')
       Log <- ''
       yLim <- c(qnorm(.9/maxN),-qnorm(.9/maxN))
       pPlotxy(ds.cur$ResultU[which(ds.cur$site_no==sn)],col=cols[iPlt],grid=T, xfgrid=T, line=F, Log=Log,
               main = wy,xLim=xLim,yLim=yLim,xlab=VR,
               ylab="Frequency of observations\nless than, %", addData=addData)
       #               cex.axis=psz$cex.axis/1.2,cex.lab=psz$cex.axis,cex=psz$pt.cex,pSize=pSize,addData=addData)  #,addMedian=svalue(gchkAddMedianPP))
       #       xlab=paste(v$fullname,', ',v$units,sep=''),
       u <- par('usr')
       iPlt <<- iPlt + 1
       addData <<- T
     })
     legend('topleft',legend=ST,bty='n',fill=cols,border=NA)
   }) 
   
   output$boxPlot <- renderPlot({
     WY <- sort(input$wateryear)
     cat('dP: ',nrow(ds.cur),'\n')
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- (input$site)
     SN <- as.character(site.df$All[which(site.df$All$Site_name %in% ST),'Site_no'])
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     cat(WY,ST,VR,'\n')
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     ds.cur <- ds.cur[which(!is.na(ds.cur$ResultU)),]
     ds.cur$site_no <- factor(ds.cur$site_no,levels=SN)
     xLim <- range(ds.cur$ResultU)
     iPlt <<- 1
     cols <<- add.alpha(getCols(length(ST)),0.5)
     addData <<- F
     maxN <- max(table(ds.cur$site_no))
     sf <- 1
     main <- paste('Water years(s): ',paste(WY, collapse=', '),sep='')
     par(mfrow=c(1,1),las=1,tcl=.5,mar=c(5.1,13.1,4.1,2.1),mgp=c(3.5,.5,0))
     bp <- boxplot(ResultU/sf~site_no,xlim=xLim,data=ds.cur,plot=T,na.rm=T)
     if(length(bp$group) > 0) 
       rnge <- range(bp$group)
     else
       rnge <- 1:length(ST)
     
     rnge <- c(min(rnge)-1,max(rnge)+1)
   
     site_f <- ds.cur$site_no
     log <- ''
     plot(0,.5,type='n',ylim=rnge,xlim=xLim,yaxt='n',ylab='',xlab='',
          yaxt='n',xaxt='n',main='',cex.axis=.8)
     boxplot(ResultU/sf~site_no,xlim=xLim,data=ds.cur,add=T,
             col=cols,horizontal=T,varwidth=T,outline=F,
             xlab=VR,ylab='',yaxt='n',
             main=main,log=log,cex=0.7,pch=19)
     points(jitter(bp$group,0.75)~bp$out,pch=19,cex=.25,col='grey80')
#paste(v$fullname,', ',v$units)
 
     yScale <- paste(SN,ST,sep=' - ')
     axis(2,at=1:length(ST),labels=yScale,las=1)
     mtext(text='Site number', side=2, line=11, las=0)
   }) 
   
   output$sumTable <- renderPrint({
     WY <- sort(input$wateryear)
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     d.tmp <- sapply(unique(ds.cur$site_no), function(sn) {
       d <- ds.cur[which(ds.cur$site_no==sn & !is.na(ds.cur$ResultU)),]
       nObs <- nrow(d)
       d.tmp <- data.frame(dateTime=seq.POSIXt(min(d$dateTime),max(d$dateTime),by='15 min'))
       d.tmp <- merge(d.tmp,d[,c('dateTime','ResultU')],by='dateTime',all.x=T)
       nMiss <- nrow(d.tmp) - nObs
       buildSummary(d,rCol=4)
     })
     d.tmp
   })
   
   output$dataTable <- renderDataTable({
     WY <- sort(input$wateryear)
     cat('dP: ',nrow(ds.cur),'\n')
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     d.tmp <- datatable(head(ds.cur[,2:7],100))
     d.tmp
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

