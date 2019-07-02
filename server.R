# server.R

library(plotly)
library(shiny)
library(ggplot2)
library(rgdal)
library(dplyr)
library(maptools)
library(sp)
library(networkD3)
library(datasets)
library(chorddiag)
library(circlize)
#gpclibPermit()




# Read data
df <- read.csv(file="df5d.csv",
               head=TRUE, fileEncoding='UTF-8-BOM')




shinyServer(function(input, output) {
  #when select input , main panel can change with input changing
observeEvent(input$Regionname, {
   Regionname<- switch(input$Regionname,
                  'Northern Metropolitan'='Northern Metropolitan' , 
                  'Southern Metropolitan'='Southern Metropolitan', 
                  'South-Eastern Metropolitan'='South-Eastern Metropolitan', 
                  'Western Metropolitan'='Western Metropolitan', 
                  'Eastern Metropolitan' = 'Eastern Metropolitan')})
   
  #make ggplot to analyze
  output$trend <- renderPlot({
    if (input$ggplot_type == "scatterplot"){
    df2 <- subset(df, Regionname == input$Regionname)
    ggplot(data=df2, mapping=aes(x=Distance, y=Price,  colour=Regionname,
              text=paste('</br>distance to CBD: ',Distance,'</br>price: ',Price))) +
      geom_point() + labs(x="distance to CBD", y="price") +
      ggtitle(paste('Distance and region affect price')) +
      theme(plot.title=element_text(hjust=0.5, face="bold", size=12))
   
      # ggplotly(p,tooltip = c("text"))
    }  else if (input$ggplot_type == "area") {
      df2 <- subset(df, Regionname == input$Regionname)
     
      ggplot(data=df2, mapping=aes(x=Distance, y=Price,  colour=Regionname, fill=Regionname,
                                   text=paste('</br>distance to CBD: ',Distance,'</br>price: ',Price))) +
       geom_area(size=5) + geom_smooth(method="lm")+ labs(x="distance to CBD", y="price") +
        ggtitle(paste('Distance and region affect price')) +
        theme(plot.title=element_text(hjust=0.5, face="bold", size=12))
    }
    
  })
  #make tooltip to show additional infor
  output$hover_info <- renderUI({
    df2 <- subset(df, Regionname == input$Regionname)
   
     hover <- input$plot_hover
    point <- nearPoints(df2, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct/2 * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct /2* (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:50; background-color: rgba(145, 245, 245, 0.85); ",
                    "left:", left_px + 1, "px; top:", top_px + 1, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0('</br>distance to CBD: ',point$Distance,'km','</br>price: ',point$Price,'$')))
    )
  })

#make sankey graph to analyze
  output$graph <- renderSankeyNetwork({
    if (input$sankey_type == "price"){
      myData1 <-read.csv(("df5d.csv"), header=T,stringsAsFactors=FALSE,check.names=FALSE)
      attach(myData1)
      myData1$Pricecategory[Price > 1000000] <- "over 1 million"
      myData1$Pricecategory[Price > 500000 & Price <= 750000] <- "500k-750k"
      myData1$Pricecategory[Price > 750000 & Price <= 1000000] <- "750k-1000k"
      myData1$Pricecategory[Price <= 500000] <- "below 500k"
      detach(myData1)
      myData2<-aggregate(myData1[,9],by=list(myData1$Type,myData1$Regionname),FUN=sum)
      myData1<-aggregate(myData1[,9],by=list(myData1$Regionname,myData1$Pricecategory),FUN=sum)
      names(myData2)<-c("source","target","value")
      names(myData1)<-c("source","target","value")
      myData<-rbind(myData2,myData1)
      library(networkD3)
      Sankeylinks<-myData
      Sankeynodes<-data.frame(name=unique(c(Sankeylinks$source,Sankeylinks$target)))
      Sankeynodes$index<-0:(nrow(Sankeynodes) - 1)
      Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="source",by.y="name")  
      Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="target",by.y="name")  
      Sankeydata<-Sankeylinks[,c(4,5,3)] 
      names(Sankeydata)<-c("Source","Target","Value")  
      Sankeyname<-Sankeynodes[,1,drop=FALSE]  
      sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",  
                    Target = "Target", Value = "Value", NodeID = "name",fontSize = 10, nodeWidth = 50,
                    height =600, width = 650, sinksRight=FALSE)}
    else if (input$sankey_type == "distance") {
      myData1 <-read.csv(("df5d.csv"), header=T,stringsAsFactors=FALSE,check.names=FALSE)
      attach(myData1)
      myData1$Distancecategory[Distance > 25] <- "over 25 km"
      myData1$Distancecategory[Distance  > 15 & Distance <= 25] <- "15km-25km"
      myData1$Distancecategory[Distance  > 5 & Distance <= 15] <- "5km-15km"
      myData1$Distancecategory[Distance  <= 5] <- "below 5km"
      detach(myData1)
      myData2<-aggregate(myData1[,9],by=list(myData1$Type,myData1$Regionname),FUN=sum)
      myData1<-aggregate(myData1[,9],by=list(myData1$Regionname,myData1$Distancecategory),FUN=sum)
      names(myData2)<-c("source","target","value")
      names(myData1)<-c("source","target","value")
      myData<-rbind(myData2,myData1)
      library(networkD3)
      Sankeylinks<-myData
      Sankeynodes<-data.frame(name=unique(c(Sankeylinks$source,Sankeylinks$target)))
      Sankeynodes$index<-0:(nrow(Sankeynodes) - 1)
      Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="source",by.y="name")  
      Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="target",by.y="name")  
      Sankeydata<-Sankeylinks[,c(4,5,3)] 
      names(Sankeydata)<-c("Source","Target","Value")  
      Sankeyname<-Sankeynodes[,1,drop=FALSE]  
      sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",  
                    Target = "Target", Value = "Value", NodeID = "name",fontSize = 10, nodeWidth = 50,
                    height =600, width = 650, sinksRight=FALSE)}
    })
    #make chord graph to analyze
      output$distPlot <- renderPlot({
        if (input$chord_type == "price"){
        myData1 <- read.csv(file="df5d.csv",
                     head=TRUE, fileEncoding='UTF-8-BOM')
      attach(myData1)
      myData1$Pricecategory[Price > 1000000] <- "over 1 million"
      myData1$Pricecategory[Price > 500000 & Price <= 750000] <- "500k-750k"
      myData1$Pricecategory[Price > 750000 & Price <= 1000000] <- "750k-1000k"
      myData1$Pricecategory[Price <= 500000] <- "below 500k"
      detach(myData1)
      #Create data
      name=paste("region",c(myData1$Regionname))
      
      feature=paste("Price ", c(myData1$Pricecategory) , sep="")
      dat <- data.frame(name,feature)
      dat <- with(dat, table(name, feature))
      
      # Charge the circlize library
      library(circlize)
      
      # Make the circular plot
      chordDiagram(as.data.frame(dat), transparency = 0.5)}
        else if (input$chord_type == "distance") {
          myData1 <- read.csv(file="df5d.csv",
                              head=TRUE, fileEncoding='UTF-8-BOM')
          attach(myData1)
          myData1$Distancecategory[Distance > 25] <- "over 25 km"
          myData1$Distancecategory[Distance  > 15 & Distance <= 25] <- "15km-25km"
          myData1$Distancecategory[Distance  > 5 & Distance <= 15] <- "5km-15km"
          myData1$Distancecategory[Distance  <= 5] <- "below 5km"
          detach(myData1)
          #Create data
          name=paste("distance",c(myData1$Distancecategory))
          
          feature=paste("region ", c(myData1$Regionname) , sep="")
          dat <- data.frame(name,feature)
          dat <- with(dat, table(name, feature))
          
          # Charge the circlize library
          library(circlize)
          
          # Make the circular plot
          chordDiagram(as.data.frame(dat), transparency = 0.5)
        }
      })
      output$info <- renderText({
        hover <- input$plot_hover
        
        paste0("region 1: Eastern Metropolitan","\nregion 2: Eastern Victoria","\nregion 3: Northern Metropolitan","\nregion 4: Northern Victoria","\nregion 5: Southern-East Metropolitan","\nregion 6: Southern Metropolitan","\nregion 7: Western Metropolitan", "\nregion 8: Western Victoria")
      })
      # make a linear regression to predict housing price
      runRegression <- reactive({
        df <- head(df,3000)
        line_fit <- lm(Price~Distance, data =df)
      })
      #showing analysis of linear regression
      output$coeff <- renderTable({
        details<-summary(runRegression())$coefficients
        as.data.frame(details)
      })
      #modelling results
      output$text <- renderText({
        'housing property price = distance * (-33828) +1804378'
      })
      output$pic <- renderPlot({
        df <- head(df,3000)
        line_fit <- lm(Price~Distance, data =df)
        par(mfrow=c(2,2))
        plot(line_fit)
      })
      #show forcast result when input distance to CBD
      ntext <- eventReactive(input$goButton, {
        input$n
      })
      
      output$nText <- renderText({
        1804378-(ntext()*33828)
      })
    })

