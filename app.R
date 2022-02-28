

data1<-data1
data2<-data2
data3<-data3

library(shinythemes)
library(sampling)
library(survey)
# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(title = "Gender Pay Gap in the UK",
                           
                           # welcoming tab panel
                           tabPanel("Welcome",
                             
                                    titlePanel(h1("Welcome to this Shiny App about the Gender Pay Gap in the UK!")), # p(h3("Anaïs Fopma, Evelien Rodenburg, Sofía Villalobos, & Thijs Carrière"))),
                             
                                    # Sidebar with explanation about the tabs
                                    sidebarLayout(position = "left",
                                      sidebarPanel(width = 3,
                                        p(h3(style="text-align: center;","In this Shiny App you can inspect the data of the UK Gender Pay Gap. ")),
                                        p(h4(style="text-align: center;", "__________")),
                                                   p(h5(style="text-align: center;","The different panels in the top menu lead you to different graphs with information over the data set.")),
                                                   p(h5(style="text-align: center;","- The panel ‘Summary per Size  of Company’ leads you to a page where you can explore the summary statistics sorted by the category of size (number of employees) of the companies.")),
                                                   p(h5(style="text-align: center;","- The panel ‘Summary per Section’ leads you to a page where you can explore the summary statistics sorted by the sector (type of industry in which the employer operates) of the companies.")),
                                                   p(h5(style="text-align: center;","- The panel 'Summary of the pay gap' leads to a graph where you can see how the pay gap changes over the years for different sizes of companies.")),
                                                   p(h5(style="text-align: center;","- In the 'Sample designs' tab, it is summarised what the effects would be if the data would be collected from a sample of companies, instead of the census it is now.")),
                                                   
                                      p(h5(style="text-align: center;","_____________________")),
                                      p(h6(style="text-align: justify;","This Shiny App is made for the course 'Survey Data Analysis', by Anaïs Fopma, Evelien Rodenburg, Sofía Villalobos & Thijs Carrière"))),
                                  
                                    
                                    
                                    # Main panel with welcoming text and explanation about the app
                                    mainPanel(width = 6,
                                      p(""),
                                      p(h3("__________________________________________")),
                                      p(h3("")),
                                      p(h4(style="text-align: justify;","Since 2017, the UK is running a yearly census on the gender pay gap. The gender pay gap is a measure for the difference in earnings between men and women. It is often expressed as either the difference between mean or median earnings relative the earnings of men.")),
                                      p(style="text-align: justify;","The UK made it mandatory for companies with more than 250 employees to provide their data related to gender pay gap every year. Companies with less than 250 employees can submit the data voluntarily."),
                                      p(style="text-align: justify;", "However, the collection of data at the population level is costly and time-consuming for both the government and the companies that need to report it. To say something about the gender pay gap, it might be easier and cheaper to estimate it by taking a sample. Therefore, our main goal is to use the population data that has already been collected to devise an efficient sampling design. In addition, our goal is to summarise, visualise, and intuitively present the census data on the gender pay gap in the UK in a format that is easily accessible to the public."),
                                      p(style="text-align: justify;", "This Shiny app was developed with these goals in mind. It presents the following:"),
                                      p(""),
                                      p(style="text-align: justify;", "•   An overview of the census data collected in the fiscal years of 2017-2018, 2018-2019, and 2019-2020."),
                                      p(style="text-align: justify;", "•   How the estimates of the gender pay gap are affected by taking a sample as opposed to using the census data, for the fiscal year of 2017-2018."),
                                      p(h3(style="text-align: center;", "~~~~~")),
                                      p(h5(style="text-align: justify;","For exploring these features, it is important to note that the data of the companies related to the gender pay gap is summarized in the following variables:")),
                                      p(style="text-align: justify;","•   Mean Hourly Percent"),
                                      p(style="text-align: justify;","•   Median Hourly Percent"),
                                      p(style="text-align: justify;","•	  The proportion of employees in each pay quartile (25% band) who are men and women, for the lower, lower-middle, upper-middle, and top pay quartiles."),
                                      p(h5(style="text-align: justify;", "In addition, the data contain classification variables such as Size of the company, and Section (field of work). The main sampling design chosen is a stratified sampling design using the variable 'Section', since it is the variable that presents the greatest variance between strata, and therefore is useful to increase precision. Unfortunately, this variable is only available for the first wave (2017-2018). Therefore, the effect of taking a sample can only be explored for this fiscal year. Also, there is four more designs using variables Size and Company size in different combinations of stratification.")),
                                      p(h3("__________________________________________"))
                                    ))
                           ),
                           
                           tabPanel("Summary per Size of Company",
                                    
                                    titlePanel(""),
                                    
                                    # Sidebar with a slider input for number of bins 
                                    sidebarLayout(
                                      
                                      sidebarPanel(
                                        p(""),
                                        radioButtons("size", label = h3("Size of Company"),
                                                     choices = list("< 250" = 1, "250 - 499" = 2, "500 - 999" = 3, 
                                                                    "1000 - 4999" = 4, "5000 - 20.000" = 5, "> 20.000" = 6), 
                                                     selected = 1),
                                        
                                        radioButtons("var", label=h3("Variable"),
                                                     
                                                     choices=list("Mean hourly difference"=1,
                                                                  "Median hourly difference"=2,
                                                                  "Lowest Quartile of employees"=3,
                                                                  "Lowest Middle Quartile of employees"=4,
                                                                  "Upper Middle Quartile of employees"=5,
                                                                  "Top Quartile of employees"=6),
                                                     selected=1)
                                        
                                                          ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        plotOutput("meandist1Plot", width = "70%", height = "1000px")
                                      )
                                    )
                           ),
                           
                           
                           
                           
                           # Here comes the second page with sector
                           tabPanel(title = "Summary per Section",
                                    sidebarLayout(
                                      sidebarPanel(radioButtons("section",label=h3("Section"),
                                        choices=list("Manufacturing "=1,
                                                                "Electricity, gas, steam and air conditioning supply "=2,
                                                                "Water supply; Sewerage, waste management and remediation activities"=3,
                                                                "Construction"=4,
                                                                "Wholesale and retail trade; Repair of motor vehicles and motorcycles"=5,
                                                                "Transportation and storage"=6,
                                                                "Accommodation and food service activities"=7,
                                                                "Information and communication"=8,
                                                                "Financial and insurance activities"=9,
                                                                "Real estate activities"=10,
                                                                "Professional, scientific and technical activities"=11,
                                                                "Administrative and support service activities"=12,
                                                                "Public administracion and defence; Compulsory social security"=13,
                                                                "Education"=14,
                                                                "Human health and social work activities"=15,
                                                                "Arts, entertainment and recreation"=16,
                                                                "Other service activities"=17,
                                                                "Activities of households as employers"=18,
                                                                "Activities of extraterritorial organisations"=19),
                                                   selected=1)
                                        
                                      ),
                                      
                                      mainPanel(plotOutput("sections",width = "100%", height = "1000px"))
                                    )),
                           
                          
                           #Third page with summary of the gap
                           tabPanel(title = "Summary of the Gap", 
                                    titlePanel(title = "The Mean Gender Pay Gap per Company Size and Year or per Section"),
                                    
                                    # Sidebar with input:
                                    sidebarLayout(
                                    sidebarPanel(
                                      
                                      p(h4("This first graph summarises the Gender Pay Gap per company size over the years.")),
                                      
                                      # input in radiobuttons
                                      radioButtons("radio.3", label = h3("Size of Company"),
                                                   choices = list("< 250" = 1, "250 - 499" = 2, "500 - 999" = 3, 
                                                                  "1000 - 4999" = 4, "5000 - 20.000" = 5, "> 20.000" = 6), 
                                                   selected = 1),
                                      checkboxGroupInput("checkGroup.3", label = h3("Year"), 
                                                         choices = list("2017-2018" = 1, "2018-2019" = 2, "2019-2020" = 3), 
                                                         selected = c("1","2","3")),
                                      p("__________________"),
                                      p(h3("Legend of the sections")),
                                      p("1 - Manufacturing"),
                                      p("2 - Electricity, gas, steam and air conditioning supply"),
                                      p("3 - Water supply; Sewerage, waste management and remediation activities"),
                                      p("4 - Construction"),
                                      p("5 - Wholesale and retail trade; Repair of motor vehicles and motorcycles"),
                                      p("6 - Transportation and storage"),
                                      p("7 - Accomodation and food service activities"),
                                      p("8 - Information and communication"),
                                      p("9 - Financial and insurance activities"),
                                      p("10 - Real estate activities"),
                                      p("11 - Professional, scientific and technical activities"),
                                      p("12 - Administrative and support service activities"),
                                      p("13 - Public administration and defence; Compulsory social security"),
                                      p("14 - Education"),
                                      p("15 - Human health and social work activities"),
                                      p("16 - Arts, entertainment and recreation"),
                                      p("17 - Other service activities"),
                                      p("18 - Activities of households as employers"),
                                      p("19 - Activities of extraterritorial organisations")
                                      
                                    ),
                                    mainPanel(
                                              
                                   plotOutput("meanperyear", width = "50%", height = "500px"),
                                   plotOutput("sector2017", height = "600px")
                                              
                                              )
                                    
                                    
                           )),
                           
                           # Here starts the fourth page with 'precision'
                           tabPanel(title = "Sample Designs",
                           sidebarLayout(
                           sidebarPanel(
                             p(h2("First Sample Design")),
                             p(h3("For this sampling, stratification on 'Section' is used with Neymann allocation to determine the sizes of the strata.")),
                             
                             sliderInput("slider",h4("Sample size"),
                                   min=250,max=1600,value=1000, step = 50),
                             p("The graph shows the distribution of the bias for that particular sample size. The red line shows the mean bias, which is the avarage overestimation of the gender pay gap when taking that sample instead of the census."),
                             p(h3("______________________")),
                             radioButtons("sampled", label=h3("Other sampling designs."),
                                                                                                     
                                          choices = list("Stratification per Company Size uncollapsed" = 1, 
                                                         "Stratification per cross strata: Section and Company Size" = 2, 
                                                         "Stratification per Section collapsed " = 3, 
                                                         "Stratification per Company Size collapsed" = 4), 
                                          selected = 1),
                             p("This graph shows the distribution of the bias for each of the sample designs with sample sizes 1000. "),
                             
                           ),
                           mainPanel(
                             
                             p(h1("")),
                             
                             plotOutput("precision"),
                             plotOutput("precision2")
                           )
                           
                ))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$meandist1Plot <- renderPlot({
    ###### MEAN HOURLY DIFFERENCE #####
    if( (input$size=='1') & (input$var=='1')) {
      mean_hourly_1_1=data1[data1$EmployerSize=="Less than 250","DiffMeanHourlyPercent"]
      mean_hourly_1_2=data2[data2$EmployerSize=="Less than 250","DiffMeanHourlyPercent"]
      mean_hourly_1_3=data3[data3$EmployerSize=="Less than 250","DiffMeanHourlyPercent"]
      par(mfrow=c(3,1))
      hist(mean_hourly_1_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2017-2018",xlab="") 
      hist(mean_hourly_1_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2018-2019",xlab="") 
      hist(mean_hourly_1_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='2') & (input$var=='1')) {
      mean_hourly_2_1=data1[data1$EmployerSize=="250 to 499","DiffMeanHourlyPercent"]
      mean_hourly_2_2=data2[data2$EmployerSize=="250 to 499","DiffMeanHourlyPercent"]
      mean_hourly_2_3=data3[data3$EmployerSize=="250 to 499","DiffMeanHourlyPercent"]
      par(mfrow=c(3,1))
      hist(mean_hourly_2_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2017-2018",xlab="") 
      hist(mean_hourly_2_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2018-2019",xlab="") 
      hist(mean_hourly_2_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='3') & (input$var=='1')) {
      mean_hourly_3_1=data1[data1$EmployerSize=="500 to 999","DiffMeanHourlyPercent"]
      mean_hourly_3_2=data2[data2$EmployerSize=="500 to 999","DiffMeanHourlyPercent"]
      mean_hourly_3_3=data3[data3$EmployerSize=="500 to 999","DiffMeanHourlyPercent"]
      par(mfrow=c(3,1))
      hist(mean_hourly_3_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2017-2018",xlab="") 
      hist(mean_hourly_3_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2018-2019",xlab="") 
      hist(mean_hourly_3_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='4') & (input$var=='1')) {
      mean_hourly_4_1=data1[data1$EmployerSize=="1000 to 4999","DiffMeanHourlyPercent"]
      mean_hourly_4_2=data2[data2$EmployerSize=="1000 to 4999","DiffMeanHourlyPercent"]
      mean_hourly_4_3=data3[data3$EmployerSize=="1000 to 4999","DiffMeanHourlyPercent"]
      par(mfrow=c(3,1))
      hist(mean_hourly_4_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2017-2018",xlab="") 
      hist(mean_hourly_4_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2018-2019",xlab="") 
      hist(mean_hourly_4_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='5') & (input$var=='1')) {
      mean_hourly_5_1=data1[data1$EmployerSize=="5000 to 19,999","DiffMeanHourlyPercent"]
      mean_hourly_5_2=data2[data2$EmployerSize=="5000 to 19,999","DiffMeanHourlyPercent"]
      mean_hourly_5_3=data3[data3$EmployerSize=="5000 to 19,999","DiffMeanHourlyPercent"]
      par(mfrow=c(3,1))
      hist(mean_hourly_5_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2017-2018",xlab="") 
      hist(mean_hourly_5_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2018-2019",xlab="") 
      hist(mean_hourly_5_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='6') & (input$var=='1')) {
      mean_hourly_6_1=data1[data1$EmployerSize=="20,000 or more","DiffMeanHourlyPercent"]
      mean_hourly_6_2=data2[data2$EmployerSize=="20,000 or more","DiffMeanHourlyPercent"]
      mean_hourly_6_3=data3[data3$EmployerSize=="20,000 or more","DiffMeanHourlyPercent"]
      par(mfrow=c(3,1))
      hist(mean_hourly_6_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2017-2018",xlab="") 
      hist(mean_hourly_6_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2018-2019",xlab="") 
      hist(mean_hourly_6_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Mean Hourly Difference years 2019-2020",xlab="")}
    
    ###### MEDIAN HOURLY DIFFERENCE##########
    if( (input$size=='1') & (input$var=='2')) {
      median_hourly_1_1=data1[data1$EmployerSize=="Less than 250","DiffMedianHourlyPercent"]
      median_hourly_1_2=data2[data2$EmployerSize=="Less than 250","DiffMedianHourlyPercent"]
      median_hourly_1_3=data3[data3$EmployerSize=="Less than 250","DiffMedianHourlyPercent"]
      par(mfrow=c(3,1))
      hist(median_hourly_1_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2017-2018",xlab="") 
      hist(median_hourly_1_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2018-2019",xlab="") 
      hist(median_hourly_1_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='2') & (input$var=='2')) {
      median_hourly_2_1=data1[data1$EmployerSize=="250 to 499","DiffMedianHourlyPercent"]
      median_hourly_2_2=data2[data2$EmployerSize=="250 to 499","DiffMedianHourlyPercent"]
      median_hourly_2_3=data3[data3$EmployerSize=="250 to 499","DiffMedianHourlyPercent"]
      par(mfrow=c(3,1))
      hist(median_hourly_2_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2017-2018",xlab="") 
      hist(median_hourly_2_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2018-2019",xlab="") 
      hist(median_hourly_2_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='3') & (input$var=='2')) {
      median_hourly_3_1=data1[data1$EmployerSize=="500 to 999","DiffMedianHourlyPercent"]
      median_hourly_3_2=data2[data2$EmployerSize=="500 to 999","DiffMedianHourlyPercent"]
      median_hourly_3_3=data3[data3$EmployerSize=="500 to 999","DiffMedianHourlyPercent"]
      par(mfrow=c(3,1))
      hist(median_hourly_3_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2017-2018",xlab="") 
      hist(median_hourly_3_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2018-2019",xlab="") 
      hist(median_hourly_3_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='4') & (input$var=='2')) {
      median_hourly_4_1=data1[data1$EmployerSize=="1000 to 4999","DiffMedianHourlyPercent"]
      median_hourly_4_2=data2[data2$EmployerSize=="1000 to 4999","DiffMedianHourlyPercent"]
      median_hourly_4_3=data3[data3$EmployerSize=="1000 to 4999","DiffMedianHourlyPercent"]
      par(mfrow=c(3,1))
      hist(median_hourly_4_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2017-2018",xlab="") 
      hist(median_hourly_4_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2018-2019",xlab="") 
      hist(median_hourly_4_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='5') & (input$var=='2')) {
      median_hourly_5_1=data1[data1$EmployerSize=="5000 to 19,999","DiffMedianHourlyPercent"]
      median_hourly_5_2=data2[data2$EmployerSize=="5000 to 19,999","DiffMedianHourlyPercent"]
      median_hourly_5_3=data3[data3$EmployerSize=="5000 to 19,999","DiffMedianHourlyPercent"]
      par(mfrow=c(3,1))
      hist(median_hourly_5_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2017-2018",xlab="") 
      hist(median_hourly_5_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2018-2019",xlab="") 
      hist(median_hourly_5_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2019-2020",xlab="")}
    
    if( (input$size=='6') & (input$var=='2')) {
      median_hourly_6_1=data1[data1$EmployerSize=="20,000 or more","DiffMedianHourlyPercent"]
      median_hourly_6_2=data2[data2$EmployerSize=="20,000 or more","DiffMedianHourlyPercent"]
      median_hourly_6_3=data3[data3$EmployerSize=="20,000 or more","DiffMedianHourlyPercent"]
      par(mfrow=c(3,1))
      hist(median_hourly_6_1, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2017-2018",xlab="") 
      hist(median_hourly_6_2, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2018-2019",xlab="") 
      hist(median_hourly_6_3, col = 'cornflowerblue',border = 'white',
           breaks=20,cex.main=2,cex.axis=2,
           main="Distribution of Median Hourly Difference years 2019-2020",xlab="")}
    
    
    ###### LOWEST QUANTILE BOXPLOT #########
    #VAR/YEAR_SIZE
    if( (input$size=='1') & (input$var=='3')) {
      par(mfrow=c(3,1))
      low_fem1_1=data1[data1$EmployerSize=="Less than 250","FemaleLowerQuartile"]
      low_male1_1=data1[data1$EmployerSize=="Less than 250","MaleLowerQuartile"]
      
      low_fem2_1=data2[data2$EmployerSize=="Less than 250","FemaleLowerQuartile"]
      low_male2_1=data2[data2$EmployerSize=="Less than 250","MaleLowerQuartile"]
      
      low_fem3_1=data3[data3$EmployerSize=="Less than 250","FemaleLowerQuartile"]
      low_male3_1=data3[data3$EmployerSize=="Less than 250","MaleLowerQuartile"]
      
      boxplot(low_fem1_1,low_male1_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem2_1,low_male2_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem3_1,low_male3_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='2') & (input$var=='3')) {
      par(mfrow=c(3,1))
      low_fem1_2=data1[data1$EmployerSize=="250 to 499","FemaleLowerQuartile"]
      low_male1_2=data1[data1$EmployerSize=="250 to 499","MaleLowerQuartile"]
      
      low_fem2_2=data2[data2$EmployerSize=="250 to 499","FemaleLowerQuartile"]
      low_male2_2=data2[data2$EmployerSize=="250 to 499","MaleLowerQuartile"]
      
      low_fem3_2=data3[data3$EmployerSize=="250 to 499","FemaleLowerQuartile"]
      low_male3_2=data3[data3$EmployerSize=="250 to 499","MaleLowerQuartile"]
      
      boxplot(low_fem1_2,low_male1_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem2_2,low_male2_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem3_2,low_male3_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='3') & (input$var=='3')) {
      par(mfrow=c(3,1))
      low_fem1_3=data1[data1$EmployerSize=="500 to 999","FemaleLowerQuartile"]
      low_male1_3=data1[data1$EmployerSize=="500 to 999","MaleLowerQuartile"]
      
      low_fem2_3=data2[data2$EmployerSize=="500 to 999","FemaleLowerQuartile"]
      low_male2_3=data2[data2$EmployerSize=="500 to 999","MaleLowerQuartile"]
      
      low_fem3_3=data3[data3$EmployerSize=="500 to 999","FemaleLowerQuartile"]
      low_male3_3=data3[data3$EmployerSize=="500 to 999","MaleLowerQuartile"]
      
      boxplot(low_fem1_3,low_male1_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem2_3,low_male2_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem3_3,low_male3_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='4') & (input$var=='3')) {
      par(mfrow=c(3,1))
      low_fem1_4=data1[data1$EmployerSize=="1000 to 4999","FemaleLowerQuartile"]
      low_male1_4=data1[data1$EmployerSize=="1000 to 4999","MaleLowerQuartile"]
      
      low_fem2_4=data2[data2$EmployerSize=="1000 to 4999","FemaleLowerQuartile"]
      low_male2_4=data2[data2$EmployerSize=="1000 to 4999","MaleLowerQuartile"]
      
      low_fem3_4=data3[data3$EmployerSize=="1000 to 4999","FemaleLowerQuartile"]
      low_male3_4=data3[data3$EmployerSize=="1000 to 4999","MaleLowerQuartile"]
      
      boxplot(low_fem1_4,low_male1_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem2_4,low_male2_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem3_4,low_male3_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='5') & (input$var=='3')) {
      par(mfrow=c(3,1))
      low_fem1_5=data1[data1$EmployerSize=="5000 to 19,999","FemaleLowerQuartile"]
      low_male1_5=data1[data1$EmployerSize=="5000 to 19,999","MaleLowerQuartile"]
      
      low_fem2_5=data2[data2$EmployerSize=="5000 to 19,999","FemaleLowerQuartile"]
      low_male2_5=data2[data2$EmployerSize=="5000 to 19,999","MaleLowerQuartile"]
      
      low_fem3_5=data3[data3$EmployerSize=="5000 to 19,999","FemaleLowerQuartile"]
      low_male3_5=data3[data3$EmployerSize=="5000 to 19,999","MaleLowerQuartile"]
      
      boxplot(low_fem1_5,low_male1_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem2_5,low_male2_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem3_5,low_male3_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='6') & (input$var=='3')) {
      par(mfrow=c(3,1))
      low_fem1_6=data1[data1$EmployerSize=="20,000 or more","FemaleLowerQuartile"]
      low_male1_6=data1[data1$EmployerSize=="20,000 or more","MaleLowerQuartile"]
      
      low_fem2_6=data2[data2$EmployerSize=="20,000 or more","FemaleLowerQuartile"]
      low_male2_6=data2[data2$EmployerSize=="20,000 or more","MaleLowerQuartile"]
      
      low_fem3_6=data3[data3$EmployerSize=="20,000 or more","FemaleLowerQuartile"]
      low_male3_6=data3[data3$EmployerSize=="20,000 or more","MaleLowerQuartile"]
      
      boxplot(low_fem1_6,low_male1_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem2_6,low_male2_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(low_fem3_6,low_male3_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    
    
    ###### LOWEST MIDDLE QUANTILE BOXPLOT #####
    if( (input$size=='1') & (input$var=='4')) {
      par(mfrow=c(3,1))
      lowupp_fem1_1=data1[data1$EmployerSize=="Less than 250","FemaleLowerMiddleQuartile"]
      lowupp_male1_1=data1[data1$EmployerSize=="Less than 250","MaleLowerMiddleQuartile"]
      
      lowupp_fem2_1=data2[data2$EmployerSize=="Less than 250","FemaleLowerMiddleQuartile"]
      lowupp_male2_1=data2[data2$EmployerSize=="Less than 250","MaleLowerMiddleQuartile"]
      
      lowupp_fem3_1=data3[data3$EmployerSize=="Less than 250","FemaleLowerMiddleQuartile"]
      lowupp_male3_1=data3[data3$EmployerSize=="Less than 250","MaleLowerMiddleQuartile"]
      
      boxplot(lowupp_fem1_1,lowupp_male1_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)

      boxplot(lowupp_fem2_1,lowupp_male2_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem3_1,lowupp_male3_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='2') & (input$var=='4')) {
      par(mfrow=c(3,1))
      lowupp_fem1_2=data1[data1$EmployerSize=="250 to 499","FemaleLowerMiddleQuartile"]
      lowupp_male1_2=data1[data1$EmployerSize=="250 to 499","MaleLowerMiddleQuartile"]
      
      lowupp_fem2_2=data2[data2$EmployerSize=="250 to 499","FemaleLowerMiddleQuartile"]
      lowupp_male2_2=data2[data2$EmployerSize=="250 to 499","MaleLowerMiddleQuartile"]
      
      lowupp_fem3_2=data3[data3$EmployerSize=="250 to 499","FemaleLowerMiddleQuartile"]
      lowupp_male3_2=data3[data3$EmployerSize=="250 to 499","MaleLowerMiddleQuartile"]
      
      boxplot(lowupp_fem1_2,lowupp_male1_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem2_2,lowupp_male2_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem3_2,lowupp_male3_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='3') & (input$var=='4')) {
      par(mfrow=c(3,1))
      lowupp_fem1_3=data1[data1$EmployerSize=="500 to 999","FemaleLowerMiddleQuartile"]
      lowupp_male1_3=data1[data1$EmployerSize=="500 to 999","MaleLowerMiddleQuartile"]
      
      lowupp_fem2_3=data2[data2$EmployerSize=="500 to 999","FemaleLowerMiddleQuartile"]
      lowupp_male2_3=data2[data2$EmployerSize=="500 to 999","MaleLowerMiddleQuartile"]
      
      lowupp_fem3_3=data3[data3$EmployerSize=="500 to 999","FemaleLowerMiddleQuartile"]
      lowupp_male3_3=data3[data3$EmployerSize=="500 to 999","MaleLowerMiddleQuartile"]
      
      boxplot(lowupp_fem1_3,lowupp_male1_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem2_3,lowupp_male2_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem3_3,lowupp_male3_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='4') & (input$var=='4')) {
      par(mfrow=c(3,1))
      lowupp_fem1_4=data1[data1$EmployerSize=="1000 to 4999","FemaleLowerMiddleQuartile"]
      lowupp_male1_4=data1[data1$EmployerSize=="1000 to 4999","MaleLowerMiddleQuartile"]
      
      lowupp_fem2_4=data2[data2$EmployerSize=="1000 to 4999","FemaleLowerMiddleQuartile"]
      lowupp_male2_4=data2[data2$EmployerSize=="1000 to 4999","MaleLowerMiddleQuartile"]
      
      lowupp_fem3_4=data3[data3$EmployerSize=="1000 to 4999","FemaleLowerMiddleQuartile"]
      lowupp_male3_4=data3[data3$EmployerSize=="1000 to 4999","MaleLowerMiddleQuartile"]
      
      boxplot(lowupp_fem1_4,lowupp_male1_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem2_4,lowupp_male2_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem3_4,lowupp_male3_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='5') & (input$var=='4')) {
      par(mfrow=c(3,1))
      lowupp_fem1_5=data1[data1$EmployerSize=="5000 to 19,999","FemaleLowerMiddleQuartile"]
      lowupp_male1_5=data1[data1$EmployerSize=="5000 to 19,999","MaleLowerMiddleQuartile"]
      
      lowupp_fem2_5=data2[data2$EmployerSize=="5000 to 19,999","FemaleLowerMiddleQuartile"]
      lowupp_male2_5=data2[data2$EmployerSize=="5000 to 19,999","MaleLowerMiddleQuartile"]
      
      lowupp_fem3_5=data3[data3$EmployerSize=="5000 to 19,999","FemaleLowerMiddleQuartile"]
      lowupp_male3_5=data3[data3$EmployerSize=="5000 to 19,999","MaleLowerMiddleQuartile"]
      
      boxplot(lowupp_fem1_5,lowupp_male1_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem2_5,lowupp_male2_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem3_5,lowupp_male3_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='6') & (input$var=='4')) {
      par(mfrow=c(3,1))
      lowupp_fem1_6=data1[data1$EmployerSize=="20,000 or more","FemaleLowerMiddleQuartile"]
      lowupp_male1_6=data1[data1$EmployerSize=="20,000 or more","MaleLowerMiddleQuartile"]
      
      lowupp_fem2_6=data2[data2$EmployerSize=="20,000 or more","FemaleLowerMiddleQuartile"]
      lowupp_male2_6=data2[data2$EmployerSize=="20,000 or more","MaleLowerMiddleQuartile"]
      
      lowupp_fem3_6=data3[data3$EmployerSize=="20,000 or more","FemaleLowerMiddleQuartile"]
      lowupp_male3_6=data3[data3$EmployerSize=="20,000 or more","MaleLowerMiddleQuartile"]
      
      boxplot(lowupp_fem1_6,lowupp_male1_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem2_6,lowupp_male2_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(lowupp_fem3_6,lowupp_male3_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    
    
    
    
    ###### UPPER MIDDLE QUANTILE BOXPLOT #####
    if( (input$size=='1') & (input$var=='5')) {
      par(mfrow=c(3,1))
      uppmidd_fem1_1=data1[data1$EmployerSize=="Less than 250","FemaleUpperMiddleQuartile"]
      uppmidd_male1_1=data1[data1$EmployerSize=="Less than 250","MaleUpperMiddleQuartile"]
      
      uppmidd_fem2_1=data2[data2$EmployerSize=="Less than 250","FemaleUpperMiddleQuartile"]
      uppmidd_male2_1=data2[data2$EmployerSize=="Less than 250","MaleUpperMiddleQuartile"]
      
      uppmidd_fem3_1=data3[data3$EmployerSize=="Less than 250","FemaleUpperMiddleQuartile"]
      uppmidd_male3_1=data3[data3$EmployerSize=="Less than 250","MaleUpperMiddleQuartile"]
      
      boxplot(uppmidd_fem1_1,uppmidd_male1_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem2_1,uppmidd_male2_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem3_1,uppmidd_male3_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='2') & (input$var=='5')) {
      par(mfrow=c(3,1))
      uppmidd_fem1_2=data1[data1$EmployerSize=="250 to 499","FemaleUpperMiddleQuartile"]
      uppmidd_male1_2=data1[data1$EmployerSize=="250 to 499","MaleUpperMiddleQuartile"]
      
      uppmidd_fem2_2=data2[data2$EmployerSize=="250 to 499","FemaleUpperMiddleQuartile"]
      uppmidd_male2_2=data2[data2$EmployerSize=="250 to 499","MaleUpperMiddleQuartile"]
      
      uppmidd_fem3_2=data3[data3$EmployerSize=="250 to 499","FemaleUpperMiddleQuartile"]
      uppmidd_male3_2=data3[data3$EmployerSize=="250 to 499","MaleUpperMiddleQuartile"]
      
      boxplot(uppmidd_fem1_2,uppmidd_male1_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem2_2,uppmidd_male2_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem3_2,uppmidd_male3_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='3') & (input$var=='5')) {
      par(mfrow=c(3,1))
      uppmidd_fem1_3=data1[data1$EmployerSize=="500 to 999","FemaleUpperMiddleQuartile"]
      uppmidd_male1_3=data1[data1$EmployerSize=="500 to 999","MaleUpperMiddleQuartile"]
      
      uppmidd_fem2_3=data2[data2$EmployerSize=="500 to 999","FemaleUpperMiddleQuartile"]
      uppmidd_male2_3=data2[data2$EmployerSize=="500 to 999","MaleUpperMiddleQuartile"]
      
      uppmidd_fem3_3=data3[data3$EmployerSize=="500 to 999","FemaleUpperMiddleQuartile"]
      uppmidd_male3_3=data3[data3$EmployerSize=="500 to 999","MaleUpperMiddleQuartile"]
      
      boxplot(uppmidd_fem1_3,uppmidd_male1_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem2_3,uppmidd_male2_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem3_3,uppmidd_male3_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='4') & (input$var=='5')) {
      par(mfrow=c(3,1))
      uppmidd_fem1_4=data1[data1$EmployerSize=="1000 to 4999","FemaleUpperMiddleQuartile"]
      uppmidd_male1_4=data1[data1$EmployerSize=="1000 to 4999","MaleUpperMiddleQuartile"]
      
      uppmidd_fem2_4=data2[data2$EmployerSize=="1000 to 4999","FemaleUpperMiddleQuartile"]
      uppmidd_male2_4=data2[data2$EmployerSize=="1000 to 4999","MaleUpperMiddleQuartile"]
      
      uppmidd_fem3_4=data3[data3$EmployerSize=="1000 to 4999","FemaleUpperMiddleQuartile"]
      uppmidd_male3_4=data3[data3$EmployerSize=="1000 to 4999","MaleUpperMiddleQuartile"]
      
      boxplot(uppmidd_fem1_4,uppmidd_male1_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem2_4,uppmidd_male2_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem3_4,uppmidd_male3_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='5') & (input$var=='5')) {
      par(mfrow=c(3,1))
      uppmidd_fem1_5=data1[data1$EmployerSize=="5000 to 19,999","FemaleUpperMiddleQuartile"]
      uppmidd_male1_5=data1[data1$EmployerSize=="5000 to 19,999","MaleUpperMiddleQuartile"]
      
      uppmidd_fem2_5=data2[data2$EmployerSize=="5000 to 19,999","FemaleUpperMiddleQuartile"]
      uppmidd_male2_5=data2[data2$EmployerSize=="5000 to 19,999","MaleUpperMiddleQuartile"]
      
      uppmidd_fem3_5=data3[data3$EmployerSize=="5000 to 19,999","FemaleUpperMiddleQuartile"]
      uppmidd_male3_5=data3[data3$EmployerSize=="5000 to 19,999","MaleUpperMiddleQuartile"]
      
      boxplot(uppmidd_fem1_5,uppmidd_male1_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem2_5,uppmidd_male2_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem3_5,uppmidd_male3_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='6') & (input$var=='5')) {
      par(mfrow=c(3,1))
      uppmidd_fem1_6=data1[data1$EmployerSize=="20,000 or more","FemaleUpperMiddleQuartile"]
      uppmidd_male1_6=data1[data1$EmployerSize=="20,000 or more","MaleUpperMiddleQuartile"]
      
      uppmidd_fem2_6=data2[data2$EmployerSize=="20,000 or more","FemaleUpperMiddleQuartile"]
      uppmidd_male2_6=data2[data2$EmployerSize=="20,000 or more","MaleUpperMiddleQuartile"]
      
      uppmidd_fem3_6=data3[data3$EmployerSize=="20,000 or more","FemaleUpperMiddleQuartile"]
      uppmidd_male3_6=data3[data3$EmployerSize=="20,000 or more","MaleUpperMiddleQuartile"]
      
      boxplot(uppmidd_fem1_6,uppmidd_male1_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem2_6,uppmidd_male2_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(uppmidd_fem3_6,uppmidd_male3_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    ####### TOP QUARTILE BOXPLOT #####
    
    if( (input$size=='1') & (input$var=='6')) {
      par(mfrow=c(3,1))
      top_fem1_1=data1[data1$EmployerSize=="Less than 250","FemaleTopQuartile"]
      top_male1_1=data1[data1$EmployerSize=="Less than 250","MaleTopQuartile"]
      
      top_fem2_1=data2[data2$EmployerSize=="Less than 250","FemaleTopQuartile"]
      top_male2_1=data2[data2$EmployerSize=="Less than 250","MaleTopQuartile"]
      
      top_fem3_1=data3[data3$EmployerSize=="Less than 250","FemaleTopQuartile"]
      top_male3_1=data3[data3$EmployerSize=="Less than 250","MaleTopQuartile"]
      
      boxplot(top_fem1_1,top_male1_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem2_1,top_male2_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem3_1,top_male3_1,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='2') & (input$var=='6')) {
      par(mfrow=c(3,1))
      top_fem1_2=data1[data1$EmployerSize=="250 to 499","FemaleTopQuartile"]
      top_male1_2=data1[data1$EmployerSize=="250 to 499","MaleTopQuartile"]
      
      top_fem2_2=data2[data2$EmployerSize=="250 to 499","FemaleTopQuartile"]
      top_male2_2=data2[data2$EmployerSize=="250 to 499","MaleTopQuartile"]
      
      top_fem3_2=data3[data3$EmployerSize=="250 to 499","FemaleTopQuartile"]
      top_male3_2=data3[data3$EmployerSize=="250 to 499","MaleTopQuartile"]
      
      boxplot(top_fem1_2,top_male1_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem2_2,top_male2_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem3_2,top_male3_2,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='3') & (input$var=='6')) {
      par(mfrow=c(3,1))
      top_fem1_3=data1[data1$EmployerSize=="500 to 999","FemaleTopQuartile"]
      top_male1_3=data1[data1$EmployerSize=="500 to 999","MaleTopQuartile"]
      
      top_fem2_3=data2[data2$EmployerSize=="500 to 999","FemaleTopQuartile"]
      top_male2_3=data2[data2$EmployerSize=="500 to 999","MaleTopQuartile"]
      
      top_fem3_3=data3[data3$EmployerSize=="500 to 999","FemaleTopQuartile"]
      top_male3_3=data3[data3$EmployerSize=="500 to 999","MaleTopQuartile"]
      
      boxplot(top_fem1_3,top_male1_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem2_3,top_male2_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem3_3,top_male3_3,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='4') & (input$var=='6')) {
      par(mfrow=c(3,1))
      top_fem1_4=data1[data1$EmployerSize=="1000 to 4999","FemaleTopQuartile"]
      top_male1_4=data1[data1$EmployerSize=="1000 to 4999","MaleTopQuartile"]
      
      top_fem2_4=data2[data2$EmployerSize=="1000 to 4999","FemaleTopQuartile"]
      top_male2_4=data2[data2$EmployerSize=="1000 to 4999","MaleTopQuartile"]
      
      top_fem3_4=data3[data3$EmployerSize=="1000 to 4999","FemaleTopQuartile"]
      top_male3_4=data3[data3$EmployerSize=="1000 to 4999","MaleTopQuartile"]
      
      boxplot(top_fem1_4,top_male1_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem2_4,top_male2_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem3_4,top_male3_4,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='5') & (input$var=='6')) {
      par(mfrow=c(3,1))
      top_fem1_5=data1[data1$EmployerSize=="5000 to 19,999","FemaleTopQuartile"]
      top_male1_5=data1[data1$EmployerSize=="5000 to 19,999","MaleTopQuartile"]
      
      top_fem2_5=data2[data2$EmployerSize=="5000 to 19,999","FemaleTopQuartile"]
      top_male2_5=data2[data2$EmployerSize=="5000 to 19,999","MaleTopQuartile"]
      
      top_fem3_5=data3[data3$EmployerSize=="5000 to 19,999","FemaleTopQuartile"]
      top_male3_5=data3[data3$EmployerSize=="5000 to 19,999","MaleTopQuartile"]
      
      boxplot(top_fem1_5,top_male1_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem2_5,top_male2_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem3_5,top_male3_5,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    if( (input$size=='6') & (input$var=='6')) {
      par(mfrow=c(3,1))
      top_fem1_6=data1[data1$EmployerSize=="20,000 or more","FemaleTopQuartile"]
      top_male1_6=data1[data1$EmployerSize=="20,000 or more","MaleTopQuartile"]
      
      top_fem2_6=data2[data2$EmployerSize=="20,000 or more","FemaleTopQuartile"]
      top_male2_6=data2[data2$EmployerSize=="20,000 or more","MaleTopQuartile"]
      
      top_fem3_6=data3[data3$EmployerSize=="20,000 or more","FemaleTopQuartile"]
      top_male3_6=data3[data3$EmployerSize=="20,000 or more","MaleTopQuartile"]
      
      boxplot(top_fem1_6,top_male1_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2017-2018",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem2_6,top_male2_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2018-2019",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
      
      boxplot(top_fem3_6,top_male3_6,col = 'cornflowerblue',border = 'black',
              main="Percentage of female and male employees years 2019-2020",
              names=c("Females","Males"),cex.main=2,cex.axis=2)
    }
    
    
    
  })
  
  # Output for 'summary page'.
  output$meanperyear <- renderPlot({
    
    names.summ <- c("2017/2018", "2018/2019", "2019/2020")
    # Size cat 1
    if(input$radio.3 == 1) {
      
      if (is.null(input$checkGroup.3) ){
        value <- c(0.1, 0.1, 0.1)
        barplot(value, width = .1, main = "Companies with less than 250 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'), border = 'black', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))}
      
      y.1718      <- "1"       %in% input$checkGroup.3
      y.1819    <- "2"     %in% input$checkGroup.3
      y.1920 <- "3"  %in% input$checkGroup.3
      
      val.1 <- val.1 <- mean(data1[data1$EmployerSize=="Less than 250", "DiffMeanHourlyPercent"])
      val.2 <- 13.63021
      val.3 <- 13.36738
      
      if (y.1718 & y.1819 & y.1920){
        value <- c(val.1, val.2, val.3)
      } else if (y.1718 & y.1819){
        value <- c(val.1, val.2, 0.1)
      } else if (y.1718 & y.1920){
        value <- c(val.1, 0.1, val.3)
      } else if (y.1819 & y.1920){
        value <- c(0.1, val.2, val.3)
      } else if (y.1718) {
        value <- c(val.1, 0.1, 0.1)
      } else if (y.1819) {
        value <- c(0.1, val.2,0.1)
      } else if (y.1920) {
        value <- c(0.1, 0.1, val.3)
      }
      
      barplot(value, width = .1, main = "Companies with less than 250 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'),border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))
      
    }
    
    # size cat 250 to 499
    if(input$radio.3 == 2) {
      
      if (is.null(input$checkGroup.3) ){
        value <- c(0.1, 0.1, 0.1)
        barplot(barplot(value, main = "Companies with 250 to 499 employees", width = .1, col = c('cornflowerblue', 'deepskyblue', 'slateblue3'), border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20)))}
      
      
      y.1718      <- "1"       %in% input$checkGroup.3
      y.1819    <- "2"     %in% input$checkGroup.3
      y.1920 <- "3"  %in% input$checkGroup.3
      
      val.1 <- val.1 <- mean(data1[data1$EmployerSize=="250 to 499", "DiffMeanHourlyPercent"])
      val.2 <- 14.33241
      val.3 <- 14.37046
      
      if (y.1718 & y.1819 & y.1920){
        value <- c(val.1, val.2, val.3)
      } else if (y.1718 & y.1819){
        value <- c(val.1, val.2, 0.1)
      } else if (y.1718 & y.1920){
        value <- c(val.1, 0.1, val.3)
      } else if (y.1819 & y.1920){
        value <- c(0.1, val.2, val.3)
      } else if (y.1718) {
        value <- c(val.1, 0.1, 0.1)
      } else if (y.1819) {
        value <- c(0.1, val.2,0.1)
      } else if (y.1920) {
        value <- c(0.1, 0.1, val.3)
      }
      
      barplot(value, width = .1, main = "Companies with 250 to 499 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'),border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))
      
    }
    
    
    # for size cat 3
    if(input$radio.3 == 3) {
      
      if (is.null(input$checkGroup.3) ){
        value <- c(0.1, 0.1, 0.1)
        barplot(barplot(value, main = "Companies with 500 to 999 employees", width = .1, col = c('cornflowerblue', 'deepskyblue', 'slateblue3'), border = 'white', axes = TRUE, 
                                cex.names = 1,  names.arg = names.summ, ylim = c(0, 20)))}
      
      y.1718      <- "1"       %in% input$checkGroup.3
      y.1819    <- "2"     %in% input$checkGroup.3
      y.1920 <- "3"  %in% input$checkGroup.3
      
      val.1 <- val.1 <- mean(data1[data1$EmployerSize=="500 to 999", "DiffMeanHourlyPercent"])
      val.2 <- 14.17492
      val.3 <- 13.68475
      
      if (y.1718 & y.1819 & y.1920){
        value <- c(val.1, val.2, val.3)
      } else if (y.1718 & y.1819){
        value <- c(val.1, val.2, 0.1)
      } else if (y.1718 & y.1920){
        value <- c(val.1, 0.1, val.3)
      } else if (y.1819 & y.1920){
        value <- c(0.1, val.2, val.3)
      } else if (y.1718) {
        value <- c(val.1, 0.1, 0.1)
      } else if (y.1819) {
        value <- c(0.1, val.2,0.1)
      } else if (y.1920) {
        value <- c(0.1, 0.1, val.3)
      }
      
      barplot(value, width = .1, main = "Companies with 500 to 999 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'),border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))
      
    }
    
    
    # for size cat 4
    if(input$radio.3 == 4) {
      
      if (is.null(input$checkGroup.3) ){
        value <- c(0.1, 0.1, 0.1)
        barplot(barplot(value, width = .1, main = "Companies with 1000 to 4999 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'), border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20)))}
      
      y.1718      <- "1"       %in% input$checkGroup.3
      y.1819    <- "2"     %in% input$checkGroup.3
      y.1920 <- "3"  %in% input$checkGroup.3
      
      val.1 <- val.1 <- mean(data1[data1$EmployerSize=="1000 to 4999", "DiffMeanHourlyPercent"])
      val.2 <- 13.8742
      val.3 <- 14.06967
      
      if (y.1718 & y.1819 & y.1920){
        value <- c(val.1, val.2, val.3)
      } else if (y.1718 & y.1819){
        value <- c(val.1, val.2, 0.1)
      } else if (y.1718 & y.1920){
        value <- c(val.1, 0.1, val.3)
      } else if (y.1819 & y.1920){
        value <- c(0.1, val.2, val.3)
      } else if (y.1718) {
        value <- c(val.1, 0.1, 0.1)
      } else if (y.1819) {
        value <- c(0.1, val.2,0.1)
      } else if (y.1920) {
        value <- c(0.1, 0.1, val.3)
      }
      
      barplot(value, width = .1, main = "Companies with 1000 to 4999 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'),border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))
      
    }
    
    
    # For size cat 5
    if(input$radio.3 == 5) {
      
      if (is.null(input$checkGroup.3) ){
        value <- c(0.1, 0.1, 0.1)
        barplot(barplot(value, main = "Companies with 5000 to 19,999 employees", width = .1, col = c('cornflowerblue', 'deepskyblue', 'slateblue3'), border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20)))}
      
      y.1718      <- "1"       %in% input$checkGroup.3
      y.1819    <- "2"     %in% input$checkGroup.3
      y.1920 <- "3"  %in% input$checkGroup.3
      
      val.1 <- val.1 <- mean(data1[data1$EmployerSize=="5000 to 19,999", "DiffMeanHourlyPercent"])
      val.2 <- 14.9662
      val.3 <- 15.25474
      
      if (y.1718 & y.1819 & y.1920){
        value <- c(val.1, val.2, val.3)
      } else if (y.1718 & y.1819){
        value <- c(val.1, val.2, 0.1)
      } else if (y.1718 & y.1920){
        value <- c(val.1, 0.1, val.3)
      } else if (y.1819 & y.1920){
        value <- c(0.1, val.2, val.3)
      } else if (y.1718) {
        value <- c(val.1, 0.1, 0.1)
      } else if (y.1819) {
        value <- c(0.1, val.2,0.1)
      } else if (y.1920) {
        value <- c(0.1, 0.1, val.3)
      }
      
      barplot(value, width = .1, main = "Companies with 5000 to 19,999 employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'),border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))
      
    }
    
    
    # For size cat 6
    if(input$radio.3 == 6) {
      
      if (is.null(input$checkGroup.3) ){
        value <- c(0.1, 0.1, 0.1)
        barplot(barplot(value, width = .1, main = "Companies with 20,000 or more employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'), border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20)))}
      
      y.1718      <- "1"       %in% input$checkGroup.3
      y.1819    <- "2"     %in% input$checkGroup.3
      y.1920 <- "3"  %in% input$checkGroup.3
      
      val.1 <- val.1 <- mean(data1[data1$EmployerSize=="20,000 or more", "DiffMeanHourlyPercent"])
      val.2 <- 13.85902
      val.3 <- 12.875
      
      if (y.1718 & y.1819 & y.1920){
        value <- c(val.1, val.2, val.3)
      } else if (y.1718 & y.1819){
        value <- c(val.1, val.2, 0.1)
      } else if (y.1718 & y.1920){
        value <- c(val.1, 0.1, val.3)
      } else if (y.1819 & y.1920){
        value <- c(0.1, val.2, val.3)
      } else if (y.1718) {
        value <- c(val.1, 0.1, 0.1)
      } else if (y.1819) {
        value <- c(0.1, val.2,0.1)
      } else if (y.1920) {
        value <- c(0.1, 0.1, val.3)
      }
      
      barplot(value, width = .1, main = "Companies with 20,000 or more employees", col = c('cornflowerblue', 'deepskyblue', 'slateblue3'),border = 'white', axes = TRUE, xlab = " ", cex.names = 1,  names.arg = names.summ, ylim = c(0, 20))
      
    }
    
  })
  
  # Output for precision page
  output$precision <-   renderPlot({
     
    if (input$slider == "250") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(29, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(1, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(1, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(25, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(23, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(11, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(6, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(7, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(11, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(3, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(20, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(28, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(5, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(25, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(20, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(28, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(5, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
    }
    
    if (input$slider == "300"){obtained <- c()
    
    for(i in 1:1000) {
      data1$strat_sic <- 0
      data1$strat_sic[data1$section=="C"] <- srswor(35, 960)
      data1$strat_sic[data1$section=="D"] <- srswor(2, 49)
      data1$strat_sic[data1$section=="E"] <- srswor(1, 46)
      data1$strat_sic[data1$section=="F"] <- srswor(30, 198)
      data1$strat_sic[data1$section=="G"] <- srswor(28, 653)
      data1$strat_sic[data1$section=="H"] <- srswor(13, 278)
      data1$strat_sic[data1$section=="I"] <- srswor(7, 334)
      data1$strat_sic[data1$section=="J"] <- srswor(8, 284)
      data1$strat_sic[data1$section=="K"] <- srswor(13, 308)
      data1$strat_sic[data1$section=="L"] <- srswor(4, 77)
      data1$strat_sic[data1$section=="M"] <- srswor(12, 489)
      data1$strat_sic[data1$section=="N"] <- srswor(34, 708)
      data1$strat_sic[data1$section=="O"] <- srswor(6, 399)
      data1$strat_sic[data1$section=="P"] <- srswor(30, 986)
      data1$strat_sic[data1$section=="Q"] <- srswor(24, 609)
      data1$strat_sic[data1$section=="R"] <- srswor(40, 190)
      data1$strat_sic[data1$section=="S"] <- srswor(7, 139)
      data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
      data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
      
      strat_sic <- subset(data1, strat_sic==1)
      
      obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
      
      obtained <- c(obtained, obtained.mean)
    }
    bias <- (obtained - 14.40973)
    hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
    abline(v = mean(bias), lwd = 3, col = "red")
    mean(bias)
    
    }
    
    if (input$slider == "350") {obtained <- c()
    
    for(i in 1:1000) {
      data1$strat_sic <- 0
      data1$strat_sic[data1$section=="C"] <- srswor(41, 960)
      data1$strat_sic[data1$section=="D"] <- srswor(2, 49)
      data1$strat_sic[data1$section=="E"] <- srswor(1, 46)
      data1$strat_sic[data1$section=="F"] <- srswor(35, 198)
      data1$strat_sic[data1$section=="G"] <- srswor(33, 653)
      data1$strat_sic[data1$section=="H"] <- srswor(16, 278)
      data1$strat_sic[data1$section=="I"] <- srswor(8, 334)
      data1$strat_sic[data1$section=="J"] <- srswor(9, 284)
      data1$strat_sic[data1$section=="K"] <- srswor(15, 308)
      data1$strat_sic[data1$section=="L"] <- srswor(5, 77)
      data1$strat_sic[data1$section=="M"] <- srswor(27, 489)
      data1$strat_sic[data1$section=="N"] <- srswor(39, 708)
      data1$strat_sic[data1$section=="O"] <- srswor(7, 399)
      data1$strat_sic[data1$section=="P"] <- srswor(35, 986)
      data1$strat_sic[data1$section=="Q"] <- srswor(28, 609)
      data1$strat_sic[data1$section=="R"] <- srswor(40, 190)
      data1$strat_sic[data1$section=="S"] <- srswor(7, 139)
      data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
      data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
      
      strat_sic <- subset(data1, strat_sic==1)
      
      obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
      
      obtained <- c(obtained, obtained.mean)
    }
    bias <- (obtained - 14.40973)
    hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
    abline(v = mean(bias), lwd = 3, col = "red")
    mean(bias)
    
    }
    
    if (input$slider == "400") {obtained <- c()
    
    for(i in 1:1000) {
      data1$strat_sic <- 0
      data1$strat_sic[data1$section=="C"] <- srswor(47, 960)
      data1$strat_sic[data1$section=="D"] <- srswor(2, 49)
      data1$strat_sic[data1$section=="E"] <- srswor(2, 46)
      data1$strat_sic[data1$section=="F"] <- srswor(40, 198)
      data1$strat_sic[data1$section=="G"] <- srswor(37, 653)
      data1$strat_sic[data1$section=="H"] <- srswor(18, 278)
      data1$strat_sic[data1$section=="I"] <- srswor(10, 334)
      data1$strat_sic[data1$section=="J"] <- srswor(11, 284)
      data1$strat_sic[data1$section=="K"] <- srswor(18, 308)
      data1$strat_sic[data1$section=="L"] <- srswor(5, 77)
      data1$strat_sic[data1$section=="M"] <- srswor(31, 489)
      data1$strat_sic[data1$section=="N"] <- srswor(45, 708)
      data1$strat_sic[data1$section=="O"] <- srswor(8, 399)
      data1$strat_sic[data1$section=="P"] <- srswor(40, 986)
      data1$strat_sic[data1$section=="Q"] <- srswor(31, 609)
      data1$strat_sic[data1$section=="R"] <- srswor(45, 190)
      data1$strat_sic[data1$section=="S"] <- srswor(8, 139)
      data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
      data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
      
      strat_sic <- subset(data1, strat_sic==1)
      
      obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
      
      obtained <- c(obtained, obtained.mean)
    }
    bias <- (obtained - 14.40973)
    hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
    abline(v = mean(bias), lwd = 3, col = "red")
    mean(bias)
    
    }
    
    if (input$slider == "450") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(53, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(2, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(2, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(45, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(42, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(20, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(11, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(12, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(20, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(6, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(35, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(51, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(8, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(45, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(36, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(51, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(9, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
    }
    
    if (input$slider == "500") {obtained <- c()
    
    for(i in 1:1000) {
      data1$strat_sic <- 0
      data1$strat_sic[data1$section=="C"] <- srswor(59, 960)
      data1$strat_sic[data1$section=="D"] <- srswor(3, 49)
      data1$strat_sic[data1$section=="E"] <- srswor(2, 46)
      data1$strat_sic[data1$section=="F"] <- srswor(50, 198)
      data1$strat_sic[data1$section=="G"] <- srswor(46, 653)
      data1$strat_sic[data1$section=="H"] <- srswor(22, 278)
      data1$strat_sic[data1$section=="I"] <- srswor(12, 334)
      data1$strat_sic[data1$section=="J"] <- srswor(13, 284)
      data1$strat_sic[data1$section=="K"] <- srswor(22, 308)
      data1$strat_sic[data1$section=="L"] <- srswor(7, 77)
      data1$strat_sic[data1$section=="M"] <- srswor(39, 489)
      data1$strat_sic[data1$section=="N"] <- srswor(56, 708)
      data1$strat_sic[data1$section=="O"] <- srswor(10, 399)
      data1$strat_sic[data1$section=="P"] <- srswor(50, 986)
      data1$strat_sic[data1$section=="Q"] <- srswor(40, 609)
      data1$strat_sic[data1$section=="R"] <- srswor(57, 190)
      data1$strat_sic[data1$section=="S"] <- srswor(10, 139)
      data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
      data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
      
      strat_sic <- subset(data1, strat_sic==1)
      
      obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
      
      obtained <- c(obtained, obtained.mean)
    }
    bias <- (obtained - 14.40973)
    hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
    abline(v = mean(bias), lwd = 3, col = "red")
    mean(bias)
    
    }
    
    if (input$slider == "550") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(65, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(3, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(2, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(55, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(51, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(25, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(13, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(15, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(25, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(7, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(43, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(62, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(10, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(55, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(44, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(62, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(11, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
    }
    
    if (input$slider == "600") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(70, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(3, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(3, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(60, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(56, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(27, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(15, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(16, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(27, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(8, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(47, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(68, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(11, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(60, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(48, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(68, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(11, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "650") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(76, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(4, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(3, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(65, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(61, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(29, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(16, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(17, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(29, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(9, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(51, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(73, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(12, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(65, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(52, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(74, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(12, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "700") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(82, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(4, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(3, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(70, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(65, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(31, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(17, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(19, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(31, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(10, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(55, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(79, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(13, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(70, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(56, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(80, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(13, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)   
    }
    
    if (input$slider == "750") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(88, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(4, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(3, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(75, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(70, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(34, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(18, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(20, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(33, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(10, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(59, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(85, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(14, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(75, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(60, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(85, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(15, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)    
    }
    
    if (input$slider == "800") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(94, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(4, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(3, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(81, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(75, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(36, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(19, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(21, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(36, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(11, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(63, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(90, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(15, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(80, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(64, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(91, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(15, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)   
    }
    
    if (input$slider == "850") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(100, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(5, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(3, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(86, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(79, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(38, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(21, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(23, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(38, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(12, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(67, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(96, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(16, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(84, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(68, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(96, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(16, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "900") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(106, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(5, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(4, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(91, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(84, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(40, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(22, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(24, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(40, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(12, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(71, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(102, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(17, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(89, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(72, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(102, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(17, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "950") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(112, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(5, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(4, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(96, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(89, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(43, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(23, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(25, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(42, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(13, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(75, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(107, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(18, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(94, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(76, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(108, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(18, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)   
    }
    
    if (input$slider == "1000") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(118, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(6, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(4, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(101, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(93, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(45, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(24, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(27, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(45, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(14, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(79, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(113, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(19, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(99, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(79, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(113, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(19, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)   
    }
    
    if (input$slider == "1050") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(123, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(6, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(4, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(106, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(98, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(47, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(26, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(28, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(47, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(15, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(83, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(119, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(20, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(104, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(83, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(119, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(20, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "1100") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(129, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(6, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(5, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(111, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(103, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(49, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(27, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(29, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(49, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(15, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(88, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(124, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(21, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(109, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(87, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(125, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(21, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias) 
    }
    
    if (input$slider == "1150") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(135, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(7, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(5, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(116, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(107, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(52, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(28, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(31, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(51, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(16, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(91, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(130, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(22, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(114, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(91, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(130, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(22, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)    
    }
    
    if (input$slider == "1200") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(141, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(7, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(5, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(121, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(112, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(54, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(29, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(32, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(54, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(17, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(94, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(136, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(23, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(119, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(95, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(136, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(23, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)    
    }
    
    if (input$slider == "1250") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(147, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(7, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(5, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(126, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(117, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(56, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(31, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(33, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(56, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(17, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(98, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(142, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(24, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(124, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(100, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(141, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(24, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "1300") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(153, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(7, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(5, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(131, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(121, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(59, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(32, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(35, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(58, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(18, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(102, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(147, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(25, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(129, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(103, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(148, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(25, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
    }
    
    if (input$slider == "1350") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(159, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(7, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(6, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(136, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(126, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(60, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(33, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(36, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(61, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(19, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(106, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(153, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(26, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(134, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(107, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(153, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(26, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)  
    }
    
    if (input$slider == "1400") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(165, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(8, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(6, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(141, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(131, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(63, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(34, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(37, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(63, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(19, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(110, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(158, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(27, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(139, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(111, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(159, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(27, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)    
    }
    
    if (input$slider == "1450") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(170, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(8, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(6, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(146, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(135, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(65, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(36, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(39, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(65, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(20, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(114, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(164, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(28, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(144, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(115, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(165, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(28, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)    
    }
    
    if (input$slider == "1500") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(176, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(8, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(6, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(151, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(140, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(68, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(37, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(40, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(67, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(21, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(118, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(170, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(29, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(149, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(119, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(170, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(29, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)    
    }
    
    if (input$slider == "1550") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(182, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(9, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(6, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(156, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(145, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(70, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(38, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(41, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(69, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(22, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(122, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(175, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(30, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(154, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(123, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(176, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(30, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)   
    }
    
    if (input$slider == "1600") {
      obtained <- c()
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section=="C"] <- srswor(188, 960)
        data1$strat_sic[data1$section=="D"] <- srswor(9, 49)
        data1$strat_sic[data1$section=="E"] <- srswor(6, 46)
        data1$strat_sic[data1$section=="F"] <- srswor(161, 198)
        data1$strat_sic[data1$section=="G"] <- srswor(149, 653)
        data1$strat_sic[data1$section=="H"] <- srswor(72, 278)
        data1$strat_sic[data1$section=="I"] <- srswor(39, 334)
        data1$strat_sic[data1$section=="J"] <- srswor(43, 284)
        data1$strat_sic[data1$section=="K"] <- srswor(72, 308)
        data1$strat_sic[data1$section=="L"] <- srswor(22, 77)
        data1$strat_sic[data1$section=="M"] <- srswor(126, 489)
        data1$strat_sic[data1$section=="N"] <- srswor(181, 708)
        data1$strat_sic[data1$section=="O"] <- srswor(31, 399)
        data1$strat_sic[data1$section=="P"] <- srswor(159, 986)
        data1$strat_sic[data1$section=="Q"] <- srswor(127, 609)
        data1$strat_sic[data1$section=="R"] <- srswor(182, 190)
        data1$strat_sic[data1$section=="S"] <- srswor(31, 139)
        data1$strat_sic[data1$section=="T"] <- srswor(1, 3)
        data1$strat_sic[data1$section=="U"] <- srswor(1, 3)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)   
    }
  })
  
  output$precision2 <- renderPlot({
    if (input$sampled == "1"){
    obtained <- c()
    set.seed(865)
    
    for(i in 1:1000) {
      data1$strat_size <- 0
      data1$strat_size[data1$EmployerSize=="Not Provided"] <- srswor(26, 201)
      data1$strat_size[data1$EmployerSize=="Less than 250"] <- srswor(25, 144)
      data1$strat_size[data1$EmployerSize=="250 to 499"] <- srswor(469, 2866)
      data1$strat_size[data1$EmployerSize=="500 to 999"] <- srswor(248, 1633)
      data1$strat_size[data1$EmployerSize=="1000 to 4999"] <- srswor(196, 1478)
      data1$strat_size[data1$EmployerSize=="5000 to 19,999"] <- srswor(30, 339)
      data1$strat_size[data1$EmployerSize=="20,000 or more"] <- srswor(6, 52)
      
      strat_sic <- subset(data1, strat_size==1)
      
      obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
      
      obtained <- c(obtained, obtained.mean)
    }
    bias <- (obtained - 14.40973)
    hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate using company size strata", ylab = "")
    abline(v = mean(bias), lwd = 3, col = "red")
    mean(bias) 
  }
    if(input$sampled=="2"){
      obtained <- c()
      set.seed(865)
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$cross_strata=="large_1"] <- srswor(3, 71)
        data1$strat_sic[data1$cross_strata=="large_2"] <- srswor(19, 185)
        data1$strat_sic[data1$cross_strata=="large_3"] <- srswor(3, 60)
        data1$strat_sic[data1$cross_strata=="large_4"] <- srswor(7, 70)
        data1$strat_sic[data1$cross_strata=="large_5"] <- srswor(4, 17)
        data1$strat_sic[data1$cross_strata=="medium_1"] <- srswor(44, 696)
        data1$strat_sic[data1$cross_strata=="medium_2"] <- srswor(364, 2589)
        data1$strat_sic[data1$cross_strata=="medium_3"] <- srswor(87, 583)
        data1$strat_sic[data1$cross_strata=="medium_4"] <- srswor(403, 2014)
        data1$strat_sic[data1$cross_strata=="medium_5"] <- srswor(39, 281)
        data1$strat_sic[data1$cross_strata=="small_1"] <- srswor(1, 18)
        data1$strat_sic[data1$cross_strata=="small_2"] <- srswor(7, 46)
        data1$strat_sic[data1$cross_strata=="small_3"] <- srswor(3, 10)
        data1$strat_sic[data1$cross_strata=="small_4"] <- srswor(13, 63)
        data1$strat_sic[data1$cross_strata=="small_5"] <- srswor(2, 10)
        
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate using cross strata", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
    }
    if(input$sampled=="3"){
      obtained <- c()
      set.seed(865)
      
      for(i in 1:1000) {
        data1$strat_sic <- 0
        data1$strat_sic[data1$section.col=="C"] <- srswor(118, 960)
        data1$strat_sic[data1$section.col=="D"] <- srswor(6, 49)
        data1$strat_sic[data1$section.col=="E"] <- srswor(4, 49)
        data1$strat_sic[data1$section.col=="F"] <- srswor(101, 198)
        data1$strat_sic[data1$section.col=="G"] <- srswor(93, 653)
        data1$strat_sic[data1$section.col=="H"] <- srswor(45, 278)
        data1$strat_sic[data1$section.col=="I"] <- srswor(24, 334)
        data1$strat_sic[data1$section.col=="J"] <- srswor(27, 284)
        data1$strat_sic[data1$section.col=="K"] <- srswor(45, 308)
        data1$strat_sic[data1$section.col=="L"] <- srswor(14, 77)
        data1$strat_sic[data1$section.col=="M"] <- srswor(79, 489)
        data1$strat_sic[data1$section.col=="N"] <- srswor(113, 708)
        data1$strat_sic[data1$section.col=="O"] <- srswor(19, 402)
        data1$strat_sic[data1$section.col=="P"] <- srswor(99, 986)
        data1$strat_sic[data1$section.col=="Q"] <- srswor(80, 609)
        data1$strat_sic[data1$section.col=="R"] <- srswor(114, 190)
        data1$strat_sic[data1$section.col=="S"] <- srswor(19, 139)
        strat_sic <- subset(data1, strat_sic==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate using collapsed section strata", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
    }    
    
    if(input$sampled=="4"){
      obtained <- c()
      set.seed(865)
      
      for(i in 1:1000) {
        data1$strat_size_c <- 0
        data1$strat_size_c[data1$size=="ESNP"] <- srswor(26, 201)
        data1$strat_size_c[data1$size=="ES1"] <- srswor(25, 144)
        data1$strat_size_c[data1$size=="ES2"] <- srswor(913, 5977)
        data1$strat_size_c[data1$size=="ES3"] <- srswor(36, 391)  
        strat_sic <- subset(data1, strat_size_c==1)
        
        obtained.mean <- mean(strat_sic$DiffMeanHourlyPercent)
        
        obtained <- c(obtained, obtained.mean)
      }
      bias <- (obtained - 14.40973)
      hist(bias, col = "cornflowerblue", xlim = c(-2, 4), xlab = "Distribution of the bias estimate using collapsed company size strata", ylab = "")
      abline(v = mean(bias), lwd = 3, col = "red")
      mean(bias)
      
    }
    })
  

  # Output for summary sections.
  output$sector2017 <- renderPlot({
    sec.1 <- mean(data1[data1$section=="C", "DiffMeanHourlyPercent"])
    sec.2 <- mean(data1[data1$section=="D", "DiffMeanHourlyPercent"])
    sec.3 <- mean(data1[data1$section=="E", "DiffMeanHourlyPercent"])
    sec.4 <- mean(data1[data1$section=="F", "DiffMeanHourlyPercent"])
    sec.5 <- mean(data1[data1$section=="G", "DiffMeanHourlyPercent"])
    sec.6 <- mean(data1[data1$section=="H", "DiffMeanHourlyPercent"])
    sec.7 <- mean(data1[data1$section=="I", "DiffMeanHourlyPercent"])
    sec.8 <- mean(data1[data1$section=="J", "DiffMeanHourlyPercent"])
    sec.9 <- mean(data1[data1$section=="K", "DiffMeanHourlyPercent"])
    sec.10 <- mean(data1[data1$section=="L", "DiffMeanHourlyPercent"])
    sec.11 <- mean(data1[data1$section=="M", "DiffMeanHourlyPercent"])
    sec.12 <- mean(data1[data1$section=="N", "DiffMeanHourlyPercent"])
    sec.13 <- mean(data1[data1$section=="O", "DiffMeanHourlyPercent"])
    sec.14 <- mean(data1[data1$section=="P", "DiffMeanHourlyPercent"])
    sec.15 <- mean(data1[data1$section=="Q", "DiffMeanHourlyPercent"])
    sec.16 <- mean(data1[data1$section=="R", "DiffMeanHourlyPercent"])
    sec.17 <- mean(data1[data1$section=="S", "DiffMeanHourlyPercent"])
    sec.18 <- mean(data1[data1$section=="T", "DiffMeanHourlyPercent"])
    sec.19 <- mean(data1[data1$section=="U", "DiffMeanHourlyPercent"])
    val.sec <- c(sec.1, sec.2, sec.3, sec.4, sec.5, sec.6, sec.7, sec.8, sec.9, sec.10, sec.11,
                 sec.12, sec.13, sec.14, sec.15, sec.16, sec.17, sec.18, sec.19)
    names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
               "11", "12", "13", "14", "15", "16", "17", "18", "19")
    barplot(val.sec, width = .1, main = "The Gender Pay Gap per Section in 2017/2018", col = c('cornflowerblue', 'deepskyblue', 'slateblue3', "blue2", "dodgerblue1"),
            border = 'white', axes = TRUE, xlab = " ", cex.names = 1, ylim = c(0, 30),
            names.arg = names)
  })
  
  # Output for section
  output$sections <- renderPlot ({
    if((input$section=='1')) {
      
      par(mfrow=c(3,2))
      mean_sec_C_1=data1[data1$section=="C","DiffMeanHourlyPercent"]
      median_sec_C_1=data1[data1$section=="C","DiffMedianHourlyPercent"]
      low_C_fem=data1[data1$section=="C","FemaleLowerQuartile"]
      low_C_male=data1[data1$section=="C","MaleLowerQuartile"]
      lowmid_C_fem=data1[data1$section=="C","FemaleLowerMiddleQuartile"]
      lowmid_C_male=data1[data1$section=="C","MaleLowerMiddleQuartile"]
      upp_C_fem=data1[data1$section=="C","FemaleUpperMiddleQuartile"]
      upp_C_male=data1[data1$section=="C","MaleUpperMiddleQuartile"]
      top_C_fem=data1[data1$section=="C","FemaleTopQuartile"]
      top_C_male=data1[data1$section=="C","MaleleTopQuartile"]
      
      hist(mean_sec_C_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_C_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_C_fem,low_C_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_C_fem,low_C_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_C_fem,low_C_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_C_fem,low_C_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='2')) {
      
      par(mfrow=c(3,2))
      mean_sec_D_1=data1[data1$section=="D","DiffMeanHourlyPercent"]
      median_sec_D_1=data1[data1$section=="D","DiffMedianHourlyPercent"]
      low_D_fem=data1[data1$section=="D","FemaleLowerQuartile"]
      low_D_male=data1[data1$section=="D","MaleLowerQuartile"]
      lowmid_D_fem=data1[data1$section=="D","FemaleLowerMiddleQuartile"]
      lowmid_D_male=data1[data1$section=="D","MaleLowerMiddleQuartile"]
      upp_D_fem=data1[data1$section=="D","FemaleUpperMiddleQuartile"]
      upp_D_male=data1[data1$section=="D","MaleUpperMiddleQuartile"]
      top_D_fem=data1[data1$section=="D","FemaleTopQuartile"]
      top_D_male=data1[data1$section=="D","MaleleTopQuartile"]
      
      hist(mean_sec_D_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_D_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_D_fem,low_D_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_D_fem,low_D_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_D_fem,low_D_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_D_fem,low_D_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='3')) {
      
      par(mfrow=c(3,2))
      mean_sec_E_1=data1[data1$section=="E","DiffMeanHourlyPercent"]
      median_sec_E_1=data1[data1$section=="E","DiffMedianHourlyPercent"]
      low_E_fem=data1[data1$section=="E","FemaleLowerQuartile"]
      low_E_male=data1[data1$section=="E","MaleLowerQuartile"]
      lowmid_E_fem=data1[data1$section=="E","FemaleLowerMiddleQuartile"]
      lowmid_E_male=data1[data1$section=="E","MaleLowerMiddleQuartile"]
      upp_E_fem=data1[data1$section=="E","FemaleUpperMiddleQuartile"]
      upp_E_male=data1[data1$section=="E","MaleUpperMiddleQuartile"]
      top_E_fem=data1[data1$section=="E","FemaleTopQuartile"]
      top_E_male=data1[data1$section=="E","MaleleTopQuartile"]
      
      hist(mean_sec_E_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_E_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_E_fem,low_E_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_E_fem,low_E_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_E_fem,low_E_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_E_fem,low_E_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='4')) {
      
      par(mfrow=c(3,2))
      mean_sec_F_1=data1[data1$section=="F","DiffMeanHourlyPercent"]
      median_sec_F_1=data1[data1$section=="F","DiffMedianHourlyPercent"]
      low_F_fem=data1[data1$section=="F","FemaleLowerQuartile"]
      low_F_male=data1[data1$section=="F","MaleLowerQuartile"]
      lowmid_F_fem=data1[data1$section=="F","FemaleLowerMiddleQuartile"]
      lowmid_F_male=data1[data1$section=="F","MaleLowerMiddleQuartile"]
      upp_F_fem=data1[data1$section=="F","FemaleUpperMiddleQuartile"]
      upp_F_male=data1[data1$section=="F","MaleUpperMiddleQuartile"]
      top_F_fem=data1[data1$section=="F","FemaleTopQuartile"]
      top_F_male=data1[data1$section=="F","MaleleTopQuartile"]
      
      hist(mean_sec_F_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_F_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_F_fem,low_F_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_F_fem,low_F_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_F_fem,low_F_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_F_fem,low_F_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='5')) {
      
      par(mfrow=c(3,2))
      mean_sec_G_1=data1[data1$section=="G","DiffMeanHourlyPercent"]
      median_sec_G_1=data1[data1$section=="G","DiffMedianHourlyPercent"]
      low_G_fem=data1[data1$section=="G","FemaleLowerQuartile"]
      low_G_male=data1[data1$section=="G","MaleLowerQuartile"]
      lowmid_G_fem=data1[data1$section=="G","FemaleLowerMiddleQuartile"]
      lowmid_G_male=data1[data1$section=="G","MaleLowerMiddleQuartile"]
      upp_G_fem=data1[data1$section=="G","FemaleUpperMiddleQuartile"]
      upp_G_male=data1[data1$section=="G","MaleUpperMiddleQuartile"]
      top_G_fem=data1[data1$section=="G","FemaleTopQuartile"]
      top_G_male=data1[data1$section=="G","MaleleTopQuartile"]
      
      hist(mean_sec_G_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_G_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_G_fem,low_G_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_G_fem,low_G_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_G_fem,low_G_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_G_fem,low_G_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='6')) {
      
      par(mfrow=c(3,2))
      mean_sec_H_1=data1[data1$section=="H","DiffMeanHourlyPercent"]
      median_sec_H_1=data1[data1$section=="H","DiffMedianHourlyPercent"]
      low_H_fem=data1[data1$section=="H","FemaleLowerQuartile"]
      low_H_male=data1[data1$section=="H","MaleLowerQuartile"]
      lowmid_H_fem=data1[data1$section=="H","FemaleLowerMiddleQuartile"]
      lowmid_H_male=data1[data1$section=="H","MaleLowerMiddleQuartile"]
      upp_H_fem=data1[data1$section=="H","FemaleUpperMiddleQuartile"]
      upp_H_male=data1[data1$section=="H","MaleUpperMiddleQuartile"]
      top_H_fem=data1[data1$section=="H","FemaleTopQuartile"]
      top_H_male=data1[data1$section=="H","MaleleTopQuartile"]
      
      hist(mean_sec_H_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_H_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_H_fem,low_H_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_H_fem,low_H_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_H_fem,low_H_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_H_fem,low_H_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='7')) {
      
      par(mfrow=c(3,2))
      mean_sec_I_1=data1[data1$section=="I","DiffMeanHourlyPercent"]
      median_sec_I_1=data1[data1$section=="I","DiffMedianHourlyPercent"]
      low_I_fem=data1[data1$section=="I","FemaleLowerQuartile"]
      low_I_male=data1[data1$section=="I","MaleLowerQuartile"]
      lowmid_I_fem=data1[data1$section=="I","FemaleLowerMiddleQuartile"]
      lowmid_I_male=data1[data1$section=="I","MaleLowerMiddleQuartile"]
      upp_I_fem=data1[data1$section=="I","FemaleUpperMiddleQuartile"]
      upp_I_male=data1[data1$section=="I","MaleUpperMiddleQuartile"]
      top_I_fem=data1[data1$section=="I","FemaleTopQuartile"]
      top_I_male=data1[data1$section=="I","MaleleTopQuartile"]
      
      hist(mean_sec_I_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_I_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_I_fem,low_I_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_I_fem,low_I_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_I_fem,low_I_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_I_fem,low_I_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='8')) {
      
      par(mfrow=c(3,2))
      mean_sec_J_1=data1[data1$section=="J","DiffMeanHourlyPercent"]
      median_sec_J_1=data1[data1$section=="J","DiffMedianHourlyPercent"]
      low_J_fem=data1[data1$section=="J","FemaleLowerQuartile"]
      low_J_male=data1[data1$section=="J","MaleLowerQuartile"]
      lowmid_J_fem=data1[data1$section=="J","FemaleLowerMiddleQuartile"]
      lowmid_J_male=data1[data1$section=="J","MaleLowerMiddleQuartile"]
      upp_J_fem=data1[data1$section=="J","FemaleUpperMiddleQuartile"]
      upp_J_male=data1[data1$section=="J","MaleUpperMiddleQuartile"]
      top_J_fem=data1[data1$section=="J","FemaleTopQuartile"]
      top_J_male=data1[data1$section=="J","MaleleTopQuartile"]
      
      hist(mean_sec_J_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_J_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_J_fem,low_J_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_J_fem,low_J_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_J_fem,low_J_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_J_fem,low_J_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='9')) {
      
      par(mfrow=c(3,2))
      mean_sec_K_1=data1[data1$section=="K","DiffMeanHourlyPercent"]
      median_sec_K_1=data1[data1$section=="K","DiffMedianHourlyPercent"]
      low_K_fem=data1[data1$section=="K","FemaleLowerQuartile"]
      low_K_male=data1[data1$section=="K","MaleLowerQuartile"]
      lowmid_K_fem=data1[data1$section=="K","FemaleLowerMiddleQuartile"]
      lowmid_K_male=data1[data1$section=="K","MaleLowerMiddleQuartile"]
      upp_K_fem=data1[data1$section=="K","FemaleUpperMiddleQuartile"]
      upp_K_male=data1[data1$section=="K","MaleUpperMiddleQuartile"]
      top_K_fem=data1[data1$section=="K","FemaleTopQuartile"]
      top_K_male=data1[data1$section=="K","MaleleTopQuartile"]
      
      hist(mean_sec_K_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_K_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_K_fem,low_K_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_K_fem,low_K_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_K_fem,low_K_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_K_fem,low_K_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='10')) {
      
      par(mfrow=c(3,2))
      mean_sec_L_1=data1[data1$section=="L","DiffMeanHourlyPercent"]
      median_sec_L_1=data1[data1$section=="L","DiffMedianHourlyPercent"]
      low_L_fem=data1[data1$section=="L","FemaleLowerQuartile"]
      low_L_male=data1[data1$section=="L","MaleLowerQuartile"]
      lowmid_L_fem=data1[data1$section=="L","FemaleLowerMiddleQuartile"]
      lowmid_L_male=data1[data1$section=="L","MaleLowerMiddleQuartile"]
      upp_L_fem=data1[data1$section=="L","FemaleUpperMiddleQuartile"]
      upp_L_male=data1[data1$section=="L","MaleUpperMiddleQuartile"]
      top_L_fem=data1[data1$section=="L","FemaleTopQuartile"]
      top_L_male=data1[data1$section=="L","MaleleTopQuartile"]
      
      hist(mean_sec_L_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_L_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_L_fem,low_L_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_L_fem,low_L_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_L_fem,low_L_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_L_fem,low_L_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    
    if((input$section=='11')) {
      
      par(mfrow=c(3,2))
      mean_sec_M_1=data1[data1$section=="M","DiffMeanHourlyPercent"]
      median_sec_M_1=data1[data1$section=="M","DiffMedianHourlyPercent"]
      low_M_fem=data1[data1$section=="M","FemaleLowerQuartile"]
      low_M_male=data1[data1$section=="M","MaleLowerQuartile"]
      lowmid_M_fem=data1[data1$section=="M","FemaleLowerMiddleQuartile"]
      lowmid_M_male=data1[data1$section=="M","MaleLowerMiddleQuartile"]
      upp_M_fem=data1[data1$section=="M","FemaleUpperMiddleQuartile"]
      upp_M_male=data1[data1$section=="M","MaleUpperMiddleQuartile"]
      top_M_fem=data1[data1$section=="M","FemaleTopQuartile"]
      top_M_male=data1[data1$section=="M","MaleleTopQuartile"]
      
      hist(mean_sec_M_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_M_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_M_fem,low_M_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_M_fem,low_M_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_M_fem,low_M_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_M_fem,low_M_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='12')) {
      
      par(mfrow=c(3,2))
      mean_sec_N_1=data1[data1$section=="N","DiffMeanHourlyPercent"]
      median_sec_N_1=data1[data1$section=="N","DiffMedianHourlyPercent"]
      low_N_fem=data1[data1$section=="N","FemaleLowerQuartile"]
      low_N_male=data1[data1$section=="N","MaleLowerQuartile"]
      lowmid_N_fem=data1[data1$section=="N","FemaleLowerMiddleQuartile"]
      lowmid_N_male=data1[data1$section=="N","MaleLowerMiddleQuartile"]
      upp_N_fem=data1[data1$section=="N","FemaleUpperMiddleQuartile"]
      upp_N_male=data1[data1$section=="N","MaleUpperMiddleQuartile"]
      top_N_fem=data1[data1$section=="N","FemaleTopQuartile"]
      top_N_male=data1[data1$section=="N","MaleleTopQuartile"]
      
      hist(mean_sec_N_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_N_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_N_fem,low_N_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_N_fem,low_N_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_N_fem,low_N_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_N_fem,low_N_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='13')) {
      
      par(mfrow=c(3,2))
      mean_sec_O_1=data1[data1$section=="O","DiffMeanHourlyPercent"]
      median_sec_O_1=data1[data1$section=="O","DiffMedianHourlyPercent"]
      low_O_fem=data1[data1$section=="O","FemaleLowerQuartile"]
      low_O_male=data1[data1$section=="O","MaleLowerQuartile"]
      lowmid_O_fem=data1[data1$section=="O","FemaleLowerMiddleQuartile"]
      lowmid_O_male=data1[data1$section=="O","MaleLowerMiddleQuartile"]
      upp_O_fem=data1[data1$section=="O","FemaleUpperMiddleQuartile"]
      upp_O_male=data1[data1$section=="O","MaleUpperMiddleQuartile"]
      top_O_fem=data1[data1$section=="O","FemaleTopQuartile"]
      top_O_male=data1[data1$section=="O","MaleleTopQuartile"]
      
      hist(mean_sec_O_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_O_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_O_fem,low_O_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_O_fem,low_O_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_O_fem,low_O_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_O_fem,low_O_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='14')) {
      
      par(mfrow=c(3,2))
      mean_sec_P_1=data1[data1$section=="P","DiffMeanHourlyPercent"]
      median_sec_P_1=data1[data1$section=="P","DiffMedianHourlyPercent"]
      low_P_fem=data1[data1$section=="P","FemaleLowerQuartile"]
      low_P_male=data1[data1$section=="P","MaleLowerQuartile"]
      lowmid_P_fem=data1[data1$section=="P","FemaleLowerMiddleQuartile"]
      lowmid_P_male=data1[data1$section=="P","MaleLowerMiddleQuartile"]
      upp_P_fem=data1[data1$section=="P","FemaleUpperMiddleQuartile"]
      upp_P_male=data1[data1$section=="P","MaleUpperMiddleQuartile"]
      top_P_fem=data1[data1$section=="P","FemaleTopQuartile"]
      top_P_male=data1[data1$section=="P","MaleleTopQuartile"]
      
      hist(mean_sec_P_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_P_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_P_fem,low_P_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_P_fem,low_P_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_P_fem,low_P_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_P_fem,low_P_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='15')) {
      
      par(mfrow=c(3,2))
      mean_sec_Q_1=data1[data1$section=="Q","DiffMeanHourlyPercent"]
      median_sec_Q_1=data1[data1$section=="Q","DiffMedianHourlyPercent"]
      low_Q_fem=data1[data1$section=="Q","FemaleLowerQuartile"]
      low_Q_male=data1[data1$section=="Q","MaleLowerQuartile"]
      lowmid_Q_fem=data1[data1$section=="Q","FemaleLowerMiddleQuartile"]
      lowmid_Q_male=data1[data1$section=="Q","MaleLowerMiddleQuartile"]
      upp_Q_fem=data1[data1$section=="Q","FemaleUpperMiddleQuartile"]
      upp_Q_male=data1[data1$section=="Q","MaleUpperMiddleQuartile"]
      top_Q_fem=data1[data1$section=="Q","FemaleTopQuartile"]
      top_Q_male=data1[data1$section=="Q","MaleleTopQuartile"]
      
      hist(mean_sec_Q_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_Q_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_Q_fem,low_Q_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_Q_fem,low_Q_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_Q_fem,low_Q_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_Q_fem,low_Q_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='16')) {
      
      par(mfrow=c(3,2))
      mean_sec_R_1=data1[data1$section=="R","DiffMeanHourlyPercent"]
      median_sec_R_1=data1[data1$section=="R","DiffMedianHourlyPercent"]
      low_R_fem=data1[data1$section=="R","FemaleLowerQuartile"]
      low_R_male=data1[data1$section=="R","MaleLowerQuartile"]
      lowmid_R_fem=data1[data1$section=="R","FemaleLowerMiddleQuartile"]
      lowmid_R_male=data1[data1$section=="R","MaleLowerMiddleQuartile"]
      upp_R_fem=data1[data1$section=="R","FemaleUpperMiddleQuartile"]
      upp_R_male=data1[data1$section=="R","MaleUpperMiddleQuartile"]
      top_R_fem=data1[data1$section=="R","FemaleTopQuartile"]
      top_R_male=data1[data1$section=="R","MaleleTopQuartile"]
      
      hist(mean_sec_R_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_R_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_R_fem,low_R_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_R_fem,low_R_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_R_fem,low_R_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_R_fem,low_R_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='17')) {
      
      par(mfrow=c(3,2))
      mean_sec_S_1=data1[data1$section=="S","DiffMeanHourlyPercent"]
      median_sec_S_1=data1[data1$section=="S","DiffMedianHourlyPercent"]
      low_S_fem=data1[data1$section=="S","FemaleLowerQuartile"]
      low_S_male=data1[data1$section=="S","MaleLowerQuartile"]
      lowmid_S_fem=data1[data1$section=="S","FemaleLowerMiddleQuartile"]
      lowmid_S_male=data1[data1$section=="S","MaleLowerMiddleQuartile"]
      upp_S_fem=data1[data1$section=="S","FemaleUpperMiddleQuartile"]
      upp_S_male=data1[data1$section=="S","MaleUpperMiddleQuartile"]
      top_S_fem=data1[data1$section=="S","FemaleTopQuartile"]
      top_S_male=data1[data1$section=="S","MaleleTopQuartile"]
      
      hist(mean_sec_S_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_S_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_S_fem,low_S_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_S_fem,low_S_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_S_fem,low_S_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_S_fem,low_S_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='18')) {
      
      par(mfrow=c(3,2))
      mean_sec_T_1=data1[data1$section=="T","DiffMeanHourlyPercent"]
      median_sec_T_1=data1[data1$section=="T","DiffMedianHourlyPercent"]
      low_T_fem=data1[data1$section=="T","FemaleLowerQuartile"]
      low_T_male=data1[data1$section=="T","MaleLowerQuartile"]
      lowmid_T_fem=data1[data1$section=="T","FemaleLowerMiddleQuartile"]
      lowmid_T_male=data1[data1$section=="T","MaleLowerMiddleQuartile"]
      upp_T_fem=data1[data1$section=="T","FemaleUpperMiddleQuartile"]
      upp_T_male=data1[data1$section=="T","MaleUpperMiddleQuartile"]
      top_T_fem=data1[data1$section=="T","FemaleTopQuartile"]
      top_T_male=data1[data1$section=="T","MaleleTopQuartile"]
      
      hist(mean_sec_T_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_T_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_T_fem,low_T_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_T_fem,low_T_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_T_fem,low_T_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_T_fem,low_T_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    if((input$section=='19')) {
      
      par(mfrow=c(3,2))
      mean_sec_U_1=data1[data1$section=="U","DiffMeanHourlyPercent"]
      median_sec_U_1=data1[data1$section=="U","DiffMedianHourlyPercent"]
      low_U_fem=data1[data1$section=="U","FemaleLowerQuartile"]
      low_U_male=data1[data1$section=="U","MaleLowerQuartile"]
      lowmid_U_fem=data1[data1$section=="U","FemaleLowerMiddleQuartile"]
      lowmid_U_male=data1[data1$section=="U","MaleLowerMiddleQuartile"]
      upp_U_fem=data1[data1$section=="U","FemaleUpperMiddleQuartile"]
      upp_U_male=data1[data1$section=="U","MaleUpperMiddleQuartile"]
      top_U_fem=data1[data1$section=="U","FemaleTopQuartile"]
      top_U_male=data1[data1$section=="U","MaleleTopQuartile"]
      
      hist(mean_sec_U_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Mean Hourly difference",xlab="")
      hist(median_sec_U_1,col='cornflowerblue',border='white',
           breaks=20,cex.axis=1.5,cex.main=1.5,
           main="Distribution of Median Hourly difference",xlab="")
      boxplot(low_U_fem,low_U_male,col='cornflowerblue',border='black',
              main="Lowest Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_U_fem,low_U_male,col='cornflowerblue',border='black',
              main="Lower Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_U_fem,low_U_male,col='cornflowerblue',border='black',
              main="Upper Middle Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      boxplot(low_U_fem,low_U_male,col='cornflowerblue',border='black',
              main="Top Quartile of female and male employees",
              names=c("Females","Males"),cex.main=1.5,cex.axis=1.5)
      
    }
    
    
    
  })
  }



# Run the application 
shinyApp(ui = ui, server = server)



