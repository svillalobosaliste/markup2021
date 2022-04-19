#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(eurostat)
library(sf)
library(geojsonsf)
library(RColorBrewer)
library(tmap)

mig<-get_eurostat("migr_imm8")
mig<-mig[which(mig$age=="TOTAL" & mig$sex=="T" & mig$agedef=="COMPLET"),]
radata<-geojson_sf("https://raw.githubusercontent.com/eurostat/Nuts2json/master/2021/4326/20M/nutsrg_0.json")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(title = "Inmigration in european countries",
                           tabPanel("Time series",
                                    
                                    titlePanel("Countries"),
                                    
                                    # Sidebar with a slider input for number of bins 
                                    sidebarLayout(
                                      
                                      sidebarPanel(
                                        selectInput("COUNTRY", label = h3("Select a country"),
                                                    c("Österrich" = "AT", 
                                                      "Belgique/België" = "BE", 
                                                      "Bulgaria" = "BG", 
                                                      "Schweiz/Suisse/Svizzera" = "CH", 
                                                      "Kýpros" = "CY", 
                                                      "Česko" = "CZ",
                                                      "Deutschland"="DE",
                                                      "Danmark"="DK",
                                                      "Eesti"="EE",
                                                      "Elláda"="EL",
                                                      "España"="ES",
                                                      "Suomi/Finland"="FI",
                                                      "France"="FR",
                                                      "Hrvatska"="HR",	
                                                      "Magyarország"="HU",
                                                      "Éire/Ireland"="IE",
                                                      "Ísland"="IS",
                                                      "Italia"="IT",
                                                      "Liechtenstein"="LI",
                                                      "Lietuva"="LT",
                                                      "Luxembourg"="LU",
                                                      "Latvija"="LV",
                                                      "Crna Gora"="ME",
                                                      "Severna Makedonija"="MK",
                                                      "Malta"="MT",
                                                      "Nederland"="NL",
                                                      "Norge"="NO",
                                                      "Polska"="PL",
                                                      "Portugal"="PT",
                                                      "România"="RO",
                                                      "Sverige"="SE",
                                                      "Slovenija"="SI",
                                                      "Slovensko"="SK",
                                                      "United Kingdom"="UK"), 
                                                    selected = "AT")),
                                      
                                      
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        p("Here you can choose to inspect the inmigration data through the years, with the data
                                        available from 1990 until 2020, selecting the country of interest to see the changes."),
                                        plotOutput("timePlot")
                                      )
                                    ),
                           ),
                           
                           
                           # Here comes the second page
                           tabPanel(title = "Maps", 
                                    titlePanel(title = "Maps"),
                                    
                                    # Sidebar with input:
                                    sidebarPanel(
                                      radioButtons("type", label = h3("Select the type of map"), 
                                                   choices = list("Total number of inmigrants"="1",
                                                                  "Inmigrants per capita"="2"), 
                                                   selected = "1"),
                                      
                                      radioButtons("year", label = h3("Select a year"), 
                                                   choices = list("2010"="1",                                                  
                                                                  "2011"="2",
                                                                  "2012"="3",
                                                                  "2013"="4",
                                                                  "2014"="5",
                                                                  "2015"="6",
                                                                  "2016"="7",
                                                                  "2017"="8",
                                                                  "2018"="9",
                                                                  "2019"="10"), 
                                                   selected = "1"),
                                      
                                      
                                      
                                    ),
                                    mainPanel(
                                      p("Here you can choose a type of map and a year to inspect either the distribution of the
                                      total number of inmigrants or the rate of number of inmigrants by country population 
                                        (inmigration per capita)."),
                                      plotOutput("maptotal",width = "100%", height = "500px"),
                                      plotOutput("maprate",width = "100%", height = "500px")
                                    )
                                    
                           )
                           
                           
                ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$timePlot <- renderPlot({
    
    if (input$COUNTRY=="AT") {
      mig1<-mig[which(mig$geo=="AT"),]
      x1<-ggplot(mig1,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Österrich",
             x="Year",
             y="")+ theme_bw()
      print(x1)
    }  
    
    if (input$COUNTRY=='BE') {
      mig2<-mig[which(mig$geo=="BE"),]
      x<-ggplot(mig2,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants -  Belgique/België",
             x="Year",
             y="")+ theme_bw()
      print(x)
    } 
    
    if (input$COUNTRY=="BG") {
      mig3<-mig[which(mig$geo=="BG"),]
      x<-ggplot(mig3,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Bulgaria",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="CH") {
      mig4<-mig[which(mig$geo=="CH"),]
      x<-ggplot(mig4,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Schweiz/Suisse/Svizzer",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="CY") {
      mig5<-mig[which(mig$geo=="CY"),]
      x<-ggplot(mig5,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Kýpros",
             x="Year",
             y="")+ theme_bw()
     print(x)
    }
    
    if (input$COUNTRY=="CZ") {
      mig6<-mig[which(mig$geo=="CZ"),]
      x<-ggplot(mig6,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Česko",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="DE") {
      mig7<-mig[which(mig$geo=="DE"),]
      x<-ggplot(mig7,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Deutschland",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="DK") {
      mig8<-mig[which(mig$geo=="DK"),]
      x<-ggplot(mig8,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Danmark",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="EE") {
      mig9<-mig[which(mig$geo=="EE"),]
      x<-ggplot(mig9,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Eesti",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="EL") {
      mig10<-mig[which(mig$geo=="EL"),]
      x<-ggplot(mig10,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Elláda",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if(input$COUNTRY=="ES") {
      mig11<-mig[which(mig$geo=="ES"),]
      x<-ggplot(mig11,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - España",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="FI") {
      mig12<-mig[which(mig$geo=="FI"),]
      x<-ggplot(mig12,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Suomi/Finland",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="FR") {
      mig13<-mig[which(mig$geo=="FR"),]
      x<-ggplot(mig13,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - France",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if(input$COUNTRY=="HR") {
      mig14<-mig[which(mig$geo=="HR"),]
      x<-ggplot(mig14,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Hrvatska",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="HU") {
      mig15<-mig[which(mig$geo=="HU"),]
      x<-ggplot(mig15,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Magyarország",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if(input$COUNTRY=="IE") {
      mig16<-mig[which(mig$geo=="IE"),]
      x<-ggplot(mig16,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Éire/Ireland",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="IS") {
      mig17<-mig[which(mig$geo=="IS"),]
      x<-ggplot(mig17,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Ísland",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="IT") {
      mig18<-mig[which(mig$geo=="IT"),]
      x<-ggplot(mig18,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Italia",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="LI") {
      mig19<-mig[which(mig$geo=="LI"),]
      x<-ggplot(mig19,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Liechtenstein",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if(input$COUNTRY=="LT") {
      mig20<-mig[which(mig$geo=="LT"),]
      x<-ggplot(mig20,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Lietuva",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="LU") {
      mig21<-mig[which(mig$geo=="LU"),]
      x<-ggplot(mig21,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Luxemburg",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if(input$COUNTRY=="LV") {
      mig22<-mig[which(mig$geo=="LV"),]
      x<-ggplot(mig22,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Latvija",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="ME") {
      mig23<-mig[which(mig$geo=="ME"),]
      x<-ggplot(mig23,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants -Crna Gora",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="MK") {
      mig24<-mig[which(mig$geo=="MK"),]
      x<-ggplot(mig24,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Severna Makedonija",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="MT") {
      mig25<-mig[which(mig$geo=="MT"),]
      x<-ggplot(mig25,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Malta",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="NL") {
      mig26<-mig[which(mig$geo=="NL"),]
      x<-ggplot(mig26,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Nederland",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="NO") {
      mig27<-mig[which(mig$geo=="NO"),]
      x<-ggplot(mig27,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Norge",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="PL") {
      mig28<-mig[which(mig$geo=="PL"),]
      x<-ggplot(mig28,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Polska",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="PT") {
      mig29<-mig[which(mig$geo=="PT"),]
      x<-ggplot(mig29,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Portugal",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="RO") {
      mig30<-mig[which(mig$geo=="RO"),]
      x<-ggplot(mig30,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - România",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if(input$COUNTRY=="SE") {
      mig31<-mig[which(mig$geo=="SE"),]
      x<-ggplot(mig31,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Sverige",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="SI") {
      mig32<-mig[which(mig$geo=="SI"),]
      x<-ggplot(mig32,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Slovenija",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="SK") {
      mig33<-mig[which(mig$geo=="SK"),]
      x<-ggplot(mig33,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - Slovensko",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
    if (input$COUNTRY=="UK") {
      mig34<-mig[which(mig$geo=="UK"),]
      x<-ggplot(mig34,aes(x=time,y=values))+
        geom_line()+
        geom_point()+
        theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_x_date(date_breaks="1 year", date_labels = "%Y")+
        labs(title="Total number of inmigrants - United Kingdom",
             x="Year",
             y="")+ theme_bw()
      print(x)
    }
    
  })
    
    output$maptotal<- renderPlot ({
      
    if ((input$type=="1") & (input$year=="1")){ 
      
      x<-mig[which(mig$time=="2010-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2010-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      
    }
    
    
    if ((input$type=="1") & (input$year=="2")){  
      
      x<-mig[which(mig$time=="2011-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2011-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
    }
      
      
    if ((input$type=="1") & (input$year=="3")){  
      
      x<-mig[which(mig$time=="2012-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2012-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
    if ((input$type=="1") & (input$year=="4")){  
      
      x<-mig[which(mig$time=="2013-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2013-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
    if ((input$type=="1") & (input$year=="5")){  
      
      x<-mig[which(mig$time=="2014-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2014-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
    if ((input$type=="1") & (input$year=="6")){ 
      
      x<-mig[which(mig$time=="2015-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2015-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
    if ((input$type=="1") & (input$year=="7")){  
      
      x<-mig[which(mig$time=="2016-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2016-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
    if ((input$type=="1") & (input$year=="8")){  
      
      x<-mig[which(mig$time=="2017-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2017-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
    if ((input$type=="1") & (input$year=="9")){ 
      
      x<-mig[which(mig$time=="2018-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2018-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
    }
      
    if ((input$type=="1") & (input$year=="10")){ 
      
      x<-mig[which(mig$time=="2019-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2019-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      x1<-tm_shape(limits)+
        tm_polygons(col="values.x",breaks=c(0,1000,10000,100000,300000,500000,900000),palette="-viridis",
                    title="Number of inmigrants")+
        tm_borders(alpha=0.2)+
        tm_layout(legend.outside=TRUE,legend.width=0.35)
      
      print(x1)
      
      }
   
    if ((input$type=="2") & (input$year=="1")){ 
      
    x<-mig[which(mig$time=="2010-01-01"),]
    names(x)[5]<-"id"
    limits<-merge(radata,x,by="id")
    pop.total<-get_eurostat("tps00001")
    pop.y<-pop.total[which(pop.total$time=="2010-01-01"),]
    names(pop.y)[2]<-"id"
    limits<-merge(limits,pop.y,by="id")
    limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
    x1<-ggplot(limits)+
      geom_sf(aes(fill=mig.per.cap))+
      coord_sf()+
      scale_fill_viridis_b(name="Inmigration per capita")
    print(x1)
      
     }
    
    
    if ((input$type=="2") & (input$year=="2")){  
      
      x<-mig[which(mig$time=="2011-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2011-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
    }
      
    if ((input$type=="2") & (input$year=="3")){  
      
      x<-mig[which(mig$time=="2012-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2012-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
    }
      
    if ((input$type=="2") & (input$year=="4")){  
      
      x<-mig[which(mig$time=="2013-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2013-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
    }
      
    if ((input$type=="2") & (input$year=="5")){  
      
      x<-mig[which(mig$time=="2014-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2014-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
    }
      
    if ((input$type=="2") & (input$year=="6")){ 
      x<-mig[which(mig$time=="2015-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2015-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
    }
      
    if ((input$type=="2") & (input$year=="7")){ 
      
      
      x<-mig[which(mig$time=="2016-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2016-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
      }
    if ((input$type=="2") & (input$year=="8")){ 
      x<-mig[which(mig$time=="2017-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2017-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
      }
    if ((input$type=="2") & (input$year=="9")){  
      x<-mig[which(mig$time=="2018-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2018-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      
      }
    if ((input$type=="2") & (input$year=="10")){ 
      
      x<-mig[which(mig$time=="2019-01-01"),]
      names(x)[5]<-"id"
      limits<-merge(radata,x,by="id")
      pop.total<-get_eurostat("tps00001")
      pop.y<-pop.total[which(pop.total$time=="2019-01-01"),]
      names(pop.y)[2]<-"id"
      limits<-merge(limits,pop.y,by="id")
      limits$mig.per.cap<-limits$values.x/limits$values.y #mig/pop
      x1<-ggplot(limits)+
        geom_sf(aes(fill=mig.per.cap))+
        coord_sf()+
        scale_fill_viridis_b(name="Inmigration per capita")
      print(x1)
      }
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
