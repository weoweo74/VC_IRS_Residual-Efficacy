# Create shiny app in one single script togrther for both UI and SERVER
########################HERE IS UI.R ######################################################
library(shiny)
#install.packages("shinyGlobe")
#library(shinyGlobe)
library(ggplot2)
install.packages("dplyr")
#library(dplyr)
#install.packages("tidyverse")
# Alternatively, install just dplyr:
install.packages("dplyr")
install.packages("ggmap")
library(ggmap)
library(maptools)
library(maps)
install.packages("plotly")
library(plotly)
library(shinydashboard)
install.packages("shinythemes")
library(shinythemes)
install.packages("DT")
library(DT)
library(leaflet)
library(shinyjs)
library(V8)
install.packages("reshape")
library(reshape)
###################################################
#library(devtools)                                 #
#install.packages("devtools")                      #
#install_github("shinyGlobe", "trestletech")       #
###################################################
install.packages("remotes")
remotes::install_github("trestletech/shinyGlobe", force = TRUE)

#if(!require("devtools")) install.packages("devtools")
#devtools::install_github("bwlewis/rthreejs")

#install ggmap via devtools
#@if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap")

############WORK VIA GITHUB is below##############
#library(devtools)
#install.packages("plotly")
#devtools::install_github("ropensci/plotly")




dashboardPage(skin = "yellow",
              dashboardHeader(title = " Vector Control Malaria",
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://www.facebook.com/USAIDGH/", 
                                             target = "_blank", 
                                             tags$img(height = "20px", 
                                                      src = "facebook.png")
                                      )
                              ),
                              
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://twitter.com/PMIgov/", 
                                             target = "_blank", 
                                             tags$img(height = "20px", 
                                                      src = "twitter.png")
                                      )
                              )),
              
              
              dashboardSidebar(
                sidebarMenu(id = "sbm",
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Data Overview", tabName = "globalattack1", icon = icon("area-chart"),
                                     menuSubItem("IRS Residual Efficacy-I", icon = icon("check-circle"),tabName = "globalattack1"),
                                     menuSubItem("IRS Residual Efficacy-II", icon = icon("check-circle"), tabName = "globalattack2"),
                                     menuSubItem("IRS Residual Efficacy-III", icon = icon("check-circle"), tabName = "globalattack3")),
                            #menuSubItem("Globe Visulization", icon = icon("check-circle"), tabName = "globegl")),
                            menuItem("Intervention by Country", tabName = "bycountry", icon = icon("gears"),badgeLabel = "new", badgeColor = "green"),
                            conditionalPanel(
                              condition = "input.sbm == 'valueAnalysis' || input.sbm == 'trainModels' || input.sbm == 'compareModels' || input.sbm == 'forecast'",
                              uiOutput("stateUi"),
                              uiOutput("countyUi"),
                              uiOutput("cityUi"),
                              uiOutput("zipUi")
                            ),
                            menuItem("Forecast Analysis", tabName = "predict", icon = icon("dashboard"))
                            #menuItem("Help", tabName = "help", icon = icon("question-circle"))
                )),
              dashboardBody(
                useShinyjs(), 
                extendShinyjs(text = "shinyjs.activateTab = function(name){
                              setTimeout(function(){
                              $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').closest('li').addClass('active')
                              }, 200);
                              }"
                ),
                tabItems(
                  tabItem(tabName = "predict",
                          fluidPage(
                            title = "Studying Bomb / Explotion Attacks",
                            column(
                              width = 12,
                              
                              box(
                                title = "Studying Bomb / Explotion Attacks",
                                status = "primary",
                                width = 12,
                                #height = 680,
                                collapsible = TRUE,
                                solidHeader = TRUE,
                                box(
                                  width = 6,
                                  title = "Logistic Regression Model",
                                  status = "primary",
                                  style = "font-size: 120%;",
                                  style = "color: #444",
                                  tags$ul(
                                    tags$li("Data: The data set consists of 167 features describing over 150,000 different terrorist attacks
                                            between 1970 - 2015. In this analysis, I study patterns in bomb or explosion related attacks. 
                                            The features used to develop the logistic regression model are Success (1/0), Multiple (1/0),
                                            Suicide (1/0), Target Type, Fatalities, and Property damage."),
                                    p(""),
                                    tags$li("The final model was developed by including features that are more likely to influence the outcome and by 
                                            performing various statistical tests to check its significance."),
                                    p(""),
                                    tags$li("A comparison of models is shown here:"),
                                    p(""),
                                    img(src = "model.png",height = 200),
                                    p(""),
                                    tags$li("For a Chi-Square statistics, the P-value is almost 0 suggesting that there is almost no evidence to
                                            support null hypothesis. In short, the model seems to better support the hypothesis."),
                                    p(""),
                                    tags$li("The 95th percentile of the Chi-Squared distribution with 89444 degrees of freedom is 90120.75 and the residual
                                            deviance is 113194 which is >> 90120.75. Thus, the null hypothesis is rejected.")
                                    
                                  )
                                  
                                  
                                  
                                ),
                                box(
                                  width = 6,
                                  status = "warning",
                                  title = "Model:",
                                  img(src = "2.png",height = 560, width = 550)
                                  
                                )
                              ),
                              
                              box(
                                width = 12,
                                title = "Predicting Terrorist Groups",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                box(
                                  width = 6,
                                  style = "font-size: 120%;",
                                  style = "color: #444",
                                  status = "primary",
                                  withMathJax(),
                                  
                                  tags$ul(
                                    p(""),
                                    tags$li("Goal: Terrorist attacks are biggest and leading issue in the world. To predict the terrorist
                                  group responsible for attacks is essential in order to prevent further attacks."),
                                    p(""),
                                    tags$li("In this section, I develop an accurate classification technique to predict 40 most deadly and frequent terrorist 
                                  groups in the world. The following features were used to build the classification model: attack location, date, attack type,
                                        weapon type, suicide attack (Yes / No), Multiple Attacks (Yes / No)."),
                                    p(""),
                                    tags$li("Random Forest technique was used. The developed model is ~95% accurate on the test data. The plot showing the
                                        feature importance other than the date and location is shown. The importance was measured by the overall reduction in 
                                        the Gini impurity index."),
                                    p(""),
                                    tags$li("Gini impurity index is defined as:
                                       
                                        $$ G = \\sum_{i=1}^n p_i (1 - p_i),$$
                                        where \\(\\ n\\) is the number of classes in the target feature and \\(\\ p_i\\) is the
                                        ratio of \\(\\ i^{ th}\\)class.")
                                    
                                  )
                                ),
                                box(
                                  width = 6,
                                  status = "warning",
                                  solidHeader = F,
                                  p(""),
                                  
                                  plotlyOutput("plot12",height = 407)
                                  
                                )
                              )
                            )
                            
                          )
                          
                  ),
                  tabItem(tabName = "bycountry",
                          fluidPage(
                            title = "Intervention by Country",
                            column(width = 2,
                                   box(
                                     title = "Query Builder",
                                     status = "primary",
                                     width = 12,
                                     solidHeader = TRUE,
                                     background = "navy",
                                     box(
                                       width = 12,
                                       status = "primary",
                                       solidHeader = FALSE,
                                       background = "navy",
                                       uiOutput("levelQueryUi")
                                     ), # query builder box closing
                                     conditionalPanel(
                                       condition = "input.analysisLevel == 1",
                                       box(
                                         status = "primary",
                                         solidHeader = FALSE,
                                         width = 12,
                                         background = "navy",
                                         uiOutput("regionlist")
                                       )# end of box
                                     ),# end of conditional panel 
                                     conditionalPanel(
                                       condition = "input.analysisLevel == 2",
                                       box(
                                         status = "primary",
                                         solidHeader = FALSE,
                                         width = 12,
                                         background = "navy",
                                         uiOutput("countrylist")
                                       )# end of box
                                     ), # enf of conditional box
                                     box(
                                       status = "primary",
                                       solidHeader = FALSE,
                                       width = 12,
                                       background = "navy",
                                       sliderInput("hviQuery", label = "Year Range", min = 2010, max = 2019, value = c(2010,2019))
                                       #checkboxInput("maxValue", label = "Excluded uncertain attacks", value = FALSE)
                                     ),
                                     box(
                                       width = 12,
                                       status = "primary",
                                       solidHeader = FALSE,
                                       background = "navy",
                                       uiOutput("timeplot")
                                     ),
                                     actionButton("query", label = "Go")
                                   )
                            ),
                            conditionalPanel(
                              condition = "input.query",
                              column(width = 10,
                                     box(
                                       title = textOutput("Design"), 
                                       status = "primary",
                                       width = 12,
                                       height = 1500,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       fluidRow(
                                         box(
                                           status = "primary",
                                           width = 12,
                                           solidHeader = FALSE,
                                           collapsible = TRUE,
                                           valueBoxOutput("totcity_country", width = 3),
                                           valueBoxOutput("totAttacks_country", width = 3),
                                           valueBoxOutput("totlife_country", width = 3),
                                           valueBoxOutput("totloss_country", width = 3)
                                         )# end of box
                                       ), #end of fluid row
                                       fluidRow(
                                         column(width = 12,
                                                box(
                                                  title = "Attacks",
                                                  status = "primary",
                                                  width = 4,
                                                  solidHeader = FALSE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("plot7",height = 250)
                                                ),
                                                box(
                                                  title = "Targets",
                                                  status = "primary",
                                                  width = 4,
                                                  solidHeader = FALSE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("plot8",height = 250)
                                                ),
                                                box(
                                                  title = "Weapons",
                                                  status = "primary",
                                                  width = 4,
                                                  solidHeader = FALSE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("plot9",height = 250)
                                                )
                                                
                                         )
                                       ), # end of fluid row
                                       fluidRow(
                                         column(width = 12,
                                                
                                                box(
                                                  title = "Major Attacks (Click on points for more information)",
                                                  status = "primary",
                                                  width = 6,
                                                  height = 475,
                                                  solidHeader = FALSE,
                                                  collapsible = TRUE,
                                                  style = "color: #444",
                                                  leafletOutput("country_map")
                                                ),
                                                box(
                                                  title = "Major Terrorist Groups",
                                                  status = "primary",
                                                  width = 6,
                                                  solidHeader = FALSE,
                                                  collapsible = TRUE,
                                                  style = "color: #444",
                                                  DT::dataTableOutput("groupnameTbl")
                                                )
                                         )
                                         
                                       ),
                                       fluidRow(
                                         column(width = 12,
                                                box(
                                                  title = textOutput("tseries"),
                                                  status = "primary",
                                                  width = 12,
                                                  solidHeader = FALSE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("plot10",height = 350)
                                                  
                                                )
                                                
                                         )
                                       )
                                     )
                              )
                            )
                            
                          ) # Closing fluidpage
                  ), # Closing tabName = bycountry
                  
                  tabItem(tabName = "globegl",
                          fluidPage(
                            title = "Globe",
                            fluidRow(
                              column(width = 12,
                                     box(
                                       width = 12,
                                       height = 1030,
                                       status = "primary",
                                       solidHeader = TRUE,
                                       style = "font-size: 120%;",
                                       style = "color: #444",
                                       
                                       h1("Global Terrorist Attacks",style = "text-align: center"),
                                       
                                       h4("Global terrorist attacks with more than 10 fatalities are represented as bars rising from a 3D globe.
                                           There are more than 6400 such incidents. The length of the bar represents the 
                                          number of fatalities in the attack. ", style = "text-align: center"),
                                       globeOutput("globe")
                                       
                                       
                                     )
                                     
                              )
                            )
                          )
                  ),
                  tabItem(tabName = "dashboard",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                              column(width = 4,
                                     box(
                                       style = "font-size: 110%;",
                                       width = 17,
                                       height = 490,
                                       background = "light-blue",
                                       solidHeader = FALSE,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       h2("Vector-Control Intervention"),
                                       p("The Malaria dashboard is an interactivie visualization tools to monitor, manage and guide informed decision making for 
                                       both previous and future malaria interventions ranging from Indoor Residual Spraying (IRS), Insecticides Treated Nets (ITN), Case Management and Entomological data.
                                       This visualization tool can be used continously to track the future goal of Global Malaria Strategy."),
                                       tags$ul(
                                         #tags$li("Time period: Year 2017 - Upward"),
                                         #p(""),
                                         # tags$li("Unit of analysis: Attack"),
                                         #p(""),
                                         #tags$li("Variables: >100 variables on location, tactics, perpetrators, targets, and outcomes"),
                                         #p(""),
                                         # tags$li("Sources: Unclassified media articles (Note: Please interpret changes over time with caution. 
                                         #         Global patterns are driven by diverse trends in particular regions, and data collection is influenced by 
                                         #        fluctuations in access to media coverage over both time and place.)")
                                         
                                         
                                       )
                                     ),
                                     box(
                                       h2("Data Source"),
                                       style = "font-size: 120%; background-color : #e77e7e;",
                                       #background = "red",
                                       
                                       width = 17,
                                       solidHeader = TRUE,
                                       p("Vector Link(VL) Collect DHIS2 Database, PMI-Malaria Quarterly Report,National Malaria Control Program (NMCP) and 
                                       Subnational Level NHMIS/DHIS2 Database"),
                                       #  "The clean data set used in this application is also available to download."),
                                       #p(""),
                                       #p("Cleaned dataset:-"),
                                       #tags$ul(
                                       # tags$li("The dataset consists of 42 features and 156,771 records "),
                                       # p(""),
                                       #   tags$li("Feature names are modified to friendlier ones and Na / NaN / -9999 are replaced by NA"),
                                       #   p(""),
                                       #   tags$li("Year, Month, and Days are combined to form a single feature \"date\""),
                                       #   p("")
                                       #),
                                       
                                       downloadButton('downloadData', 'Download Malaria Data'),
                                       
                                       downloadButton('downloadData2', 'Download Indicator Definition')
                                     )
                              ),
                              column(width = 8,
                                     height = 530,
                                     fluidRow(
                                       box(
                                         title = "Indicators definition",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         width = 12,
                                         #height = 490,
                                         fluidRow(
                                           box(
                                             style = "font-size: 120%;",
                                             title = "1. Vector Control Indicators",
                                             status = "primary",
                                             width = 7,
                                             style = "color: #444",
                                             tags$ul(
                                               tags$li("IRS Coverage: Spray- Structures sprayed (Details) / Spray- Structures found (Details)
                                                               . This analysis looks at the number of household structures sprayed out of the 
                                                               total number of household structures found during the IRS campaign.
                                                            "),
                                               tags$li("IRS Population Protected: Population Protected / Total population of the district.This
                                                               analysis looks at the number of population protected by IRS out of the 
                                                               total number of populalation in the district.
                                                            "),
                                               tags$li("Household struture sprayed by number of nets distributed:Number of sprayed household / structure/Number of LLIN net distributed.
                                                            "),
                                               
                                               tags$li("Children <5 yrs sleeping under nets protected : Spray-Children <5 years old sleeping under nets (Totals)/ Spray- Children <5 years old protected (Totals).
                                                            "),
                                               
                                               
                                               p(""),
                                               tags$li(""),
                                               p(""),
                                               tags$ol(
                                                 tags$li(""),
                                                 p(""),
                                                 tags$li(""),
                                                 p(""),
                                                 tags$li("")
                                               )
                                             )        
                                             
                                           ),
                                           box(
                                             title = "Measurement Parameters",
                                             style = "font-size: 120%;",
                                             status = "warning",
                                             width = 5,
                                             height = 435,
                                             solidHeader = FALSE,
                                             collapsible = FALSE,
                                             #  DT::dataTableOutput("logisticTbl")
                                             
                                           )
                                           
                                           
                                         ),
                                         fluidRow(
                                           box(
                                             style = "font-size: 120%;",
                                             
                                             status = "primary",
                                             width = 12,
                                             style = "color: #444",
                                             box(
                                               width = 6,
                                               title = "2. Williams to use this space for another graph or summary table",
                                               solidHeader=TRUE,
                                               tags$ul(
                                                 tags$li(" "),
                                                 
                                                 #Terrorist attacks are the biggest challenging problem for the mankind across the world, which need the
                                                 #wholly attention of the researchers, practitioners to cope up deliberately."),
                                                 p(""),
                                                 tags$li("")
                                                 
                                                 
                                                 
                                                 
                                                 
                                               )
                                             ),
                                             box(
                                               width = 6,
                                               #status = "warning",
                                               solidHeader=T,
                                               p(""),
                                               plotlyOutput("plot11",height = 270)
                                               
                                             )
                                           )
                                           
                                         )
                                         
                                       )
                                     )
                              )
                            )
                            
                          )
                  ),
                  ##THIS CODE IS USED FOR GLOBAL ITN TRACKER
                  tabItem(tabName = "globalattack1",
                          fluidPage(
                            title = "Data Overview",
                            fluidRow(
                              column(width = 12,
                                     valueBoxOutput("totCountry", width = 3),
                                     valueBoxOutput("totAttacks", width = 3),
                                     valueBoxOutput("totDeaths", width = 3),
                                     valueBoxOutput("totLoss", width = 3))),
                            fluidRow(
                              plotlyOutput("plot1",height='auto', width = 'auto'))
                          )
                  ),
                  
                  
                  
                  tabItem(tabName = "globalattack2",
                          fluidPage(
                            title = "Market Explorer",
                            fluidPage(
                              
                              column(width = 12,
                                     height = 300,
                                     box(
                                       title = "Top 10 Location By",
                                       status = "primary",
                                       width = 12,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       fluidRow(
                                         box(
                                           title = "Case_Mgt",
                                           status = "primary",
                                           width = 4,
                                           solidHeader = FALSE,
                                           collapsible = FALSE,
                                           plotlyOutput("plot3",height = 250)
                                         ),
                                         box(
                                           title = "ITN",
                                           status = "primary",
                                           width = 4,
                                           solidHeader = FALSE,
                                           collapsible = FALSE,
                                           plotlyOutput("plot4",height = 250)
                                         ),
                                         box(
                                           title = "IRS",
                                           status = "primary",
                                           width = 4,
                                           solidHeader = FALSE,
                                           collapsible = FALSE,
                                           plotlyOutput("plot2",height = 250)
                                         )
                                       )
                                     )
                              ),
                              column(width = 12,
                                     box(
                                       title = "Time-series of ITN",
                                       status = "primary",
                                       width = 6,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot5")
                                     ),
                                     box(
                                       title = "Time-series of IRS(Indoor Residual Spraying",
                                       status = "primary",
                                       width = 6,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot6")
                                     )
                              )
                            )
                          )
                  )
                )
                
              )
)





########################HERE IS SERVER.R ######################################################

#library(shiny)
#library(shinyGlobe)
#library(ggplot2)
#library(dplyr)
#library(ggmap)
#library(maptools)
#library(maps)
#library(plotly)
#library(shinydashboard)
#library(DT)
#library(leaflet)
#library(shinyjs)
#library(V8)
#library(reshape)



attack_freq_country <- readRDS("attack_freq_country.rds")
shiny_logistic <- readRDS("shiny_logistic.rds")
data1 <- readRDS("data1.rds")

convert_to_word <- function(x) {
  if (x >= 1.0E+9) {
    return(paste(round(x / 1.0E+9,0),"Billion",sep = " "))
  }
  if (x >= 1.0E+6) {
    return(paste(round(x / 1.0E+6,0),"Million",sep = " "))
  }}



shinyServer(function(input, output, session) {
  
  country_data <- reactive({
    if (input$analysisLevel == 1) {
      r_name = input$byregion
      minValue = input$hviQuery[1]
      maxValue = input$hviQuery[2]
      country_data <- data1[data1$region == r_name,]
      country_data <- country_data[country_data$iyear >= minValue & country_data$iyear <= maxValue,]
    } else {
      c_name = input$bycountry
      minValue = input$hviQuery[1]
      maxValue = input$hviQuery[2]
      country_data <- data1[data1$country == c_name,]
      country_data <- country_data[country_data$iyear >= minValue & country_data$iyear <= maxValue,]
    }
    country_data
  })
  
  
  #output$totCountry <- renderValueBox({
  #                   count <- nrow(attack_freq_country)
  #                   valueBox(count,"Countries Affected",icon = icon("flag-o"), color = 'green') })
  
  
  # output$totAttacks <- renderValueBox({
  #                    attacks <- sum(attack_freq_country$FREQ)
  #                    valueBox(attacks,"Total IRS Population",icon = icon("globe"), color = 'red') })
  
  # output$totDeaths <- renderValueBox({
  #                    deaths <- round(sum(attack_freq_country$DEATH),2)
  #                    valueBox(deaths,"Total ITN Distributed",icon = icon("user"), color = 'blue') })
  
  output$totLoss <- renderValueBox({
    valueBox("16 Countries", " Tested with Pirimiphos-methyl",icon = icon("bank"), color = 'purple') })
  
  
  ##########GOBAL VC QUATERLY# World Map by Attack # World Map by Attack # World Map by Attack # World Map by Attack # World Map by Attack
  output$plot1 <- renderPlotly({
    
    l <- list(color = toRGB("green"), width = 0.6)
    
    g1 <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator'))
    
    m <- list(l = 0,r = 0,b = 0,t = 100, pad = 0, autoexpand = TRUE)
    
    p <- plot_geo(attack_freq_country,height = 800) %>%
      add_trace(
        z = ~FREQ, color = ~FREQ, colors = 'Reds',
        text = ~paste(paste("Country:",COUNTRY),
                      paste("Start Date:",Start),
                      paste("End Date:", End),
                      paste("Count of Province:", Province),
                      paste("Mosquito Origin:", Funder),sep = "<br />"), locations = ~CODE,
        marker = list(line = l), hoverinfo = "text"
      ) %>%
      colorbar(title = '', tickprefix = '',xanchor = "left",thickness = "20",len = 0.3,
               tickfont = list(size = 15), nticks = 5) %>%
      layout(
        title = 'Global IRS Insecticide Residual Efficacy-I 2019',
        titlefont = list( size=36),
        geo = g1, xaxis=list(fixedrange=TRUE), yaxis=list(fixedrange=TRUE), margin = m) %>%
      config(displayModeBar = F)
    
    p  })
  
  output$plot2 <- renderPlotly({
    
    a <- data.frame(table(data1$country))
    colnames(a) <- c("COUNTRY","FREQ")
    a <- a[order(a$FREQ, decreasing = TRUE),]
    attack_country <- a[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p1 <- plot_ly(attack_country, labels = ~COUNTRY, values = ~FREQ, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Malaria:', FREQ),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = F)
    
    p1  })
  
  output$plot3 <- renderPlotly({
    
    b <- aggregate(nkill ~ country, data = data1, FUN = sum)
    colnames(b) <- c("COUNTRY","DEATH")
    b <- b[order(b$DEATH, decreasing = TRUE),]
    death_country <- b[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p3 <- plot_ly(death_country, labels = ~COUNTRY, values = ~DEATH, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Fatalities:', DEATH),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p3  })
  
  output$plot4 <- renderPlotly({
    
    c <- aggregate(propvalue ~ country, data = data1, FUN = sum)
    colnames(c) <- c("COUNTRY","LOSS")
    c <- c[order(c$LOSS, decreasing = TRUE),]
    loss_country <- c[1:10,]
    loss_country$TEXT <- lapply(loss_country$LOSS,convert_to_word)
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p4 <- plot_ly(loss_country, labels = ~COUNTRY, values = ~LOSS, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'),hoverinfo = 'text',
                  text = ~paste('$', TEXT),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p4  })
  
  output$plot5 <- renderPlotly({
    
    attack_category <- aggregate(ones ~ iyear + attack_type, data = data1, FUN = sum)
    attack_category$onessqrt <- sqrt(attack_category$ones)
    
    p5 <- plot_ly(attack_category) %>%
      add_trace(x = ~iyear, y = ~onessqrt, color = ~attack_type, type = 'scatter', 
                mode = 'lines+markers',line = list(width = 2), hoverinfo = "text", 
                text = ~paste(paste("Total Malaria:", ones), attack_type,
                              sep = "<br />"), colors = c("red","blue","green","orange")) %>%
      layout(
        xaxis = list(range = c(2010, 2019),zeroline = TRUE, title = ""),
        yaxis = list(side = 'left', rangemode = "tozero", overlaying = "y", 
                     title = 'SQRT(Number of Malaria)',showgrid = TRUE, 
                     zeroline = TRUE,range = c(0, 100),showticklabels = TRUE),
        legend = list(x = 0.06, y = 0.98)) %>%
      config(displayModeBar = F)
    p5  })
  
  output$plot6 <- renderPlotly({
    
    death_year <- aggregate(nkill ~ iyear + attack_type, data = data1, FUN = sum)
    death_year$nkill <- sqrt(death_year$nkill)
    levels(death_year$attack_type) <- c("IRS_pop_protected","Sprayed_Structures", "IRS_Coverage", "Children_U5yrs_protected",
                                        "IRS_pop_Not_protected", "PregWomen_U5yrs_sleeping_underNet_IRS_protected", "Spray_Structures_found", "UnIRS_pop_protected", "Unknown")
    attack_category2 <- cast(death_year,iyear ~ attack_type, sum)
    
    p6 <- plot_ly(attack_category2, x = ~iyear, y = ~IRS_pop_protected, type = 'bar', name = 'IRS_pop_protected',
                  text = ~paste(IRS_pop_protected*IRS_pop_protected,":IRS_pop_protected"),hoverinfo = 'text') %>%
      add_trace(y = ~Sprayed_Structures, name = 'Sprayed_Structures',text = ~paste(Sprayed_Structures*Sprayed_Structures,":Sprayed_Structures")) %>%
      add_trace(y = ~IRS_Coverage, name = 'IRS_Coverage',text = ~paste(IRS_Coverage*IRS_Coverage,":IRS_Coverage")) %>%
      add_trace(y = ~Children_U5yrs_protected, name = 'Children_U5yrs_protected',text = ~paste(Children_U5yrs_protected*Children_U5yrs_protected,":Children_U5yrs_protected")) %>%
      add_trace(y = ~IRS_pop_Not_protected, name = 'IRS_pop_Not_protected',text = ~paste(IRS_pop_Not_protected*IRS_pop_Not_protected,":IRS_pop_Not_protected")) %>%
      add_trace(y = ~PregWomen_U5yrs_sleeping_underNet_IRS_protected, name = 'PregWomen_U5yrs_sleeping_underNet_IRS_protected',text = ~paste(PregWomen_U5yrs_sleeping_underNet_IRS_protected*PregWomen_U5yrs_sleeping_underNet_IRS_protected,":PregWomen_U5yrs_sleeping_underNet_IRS_protected")) %>%
      add_trace(y = ~Spray_Structures_found, name = 'Spray_Structures_found',text = ~paste(Spray_Structures_found*Spray_Structures_found,":Spray_Structures_found")) %>%
      add_trace(y = ~UnIRS_pop_protected, name = 'UnIRS_pop_protected',text = ~paste(UnIRS_pop_protected*UnIRS_pop_protected,":UnIRS_pop_protected")) %>%
      add_trace(y = ~Unknown, name = 'Unknown',text = ~paste(Unknown*Unknown,":Unknown") ) %>%
      layout(yaxis = list(title = ' SQRT(Case Mgt)',range = c(0, 450)), xaxis = list(title=""), barmode = 'stack',legend = list(x = 0.06, y = 0.98))  %>% 
      config(displayModeBar = F)
    p6  })
  
  output$plot7 <- renderPlotly({
    
    country_data <- country_data()
    country_type_attack <- data.frame(aggregate(ones ~ attack_type, data = country_data, sum))
    country_type_attack <- country_type_attack[order(country_type_attack$ones, decreasing  = TRUE),]
    country_type_attack <- country_type_attack[1:5,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p7 <- plot_ly(country_type_attack, labels = ~attack_type, values = ~ones, type = 'pie',hole = 0.8,
                  textinfo = 'none' ,
                  hoverinfo = 'text',text = ~paste('Total Malaria:', ones),showlegend = T) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.3, y = 0.5)) %>%
      config(displayModeBar = F)
    p7  })
  
  output$plot8 <- renderPlotly({
    
    country_data <- country_data()
    country_type_target <- data.frame(aggregate(ones ~ target_type, data = country_data, sum))
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p8 <- plot_ly(country_type_target, labels = ~target_type, values = ~ones, type = 'pie',hole = 0.8,
                  textinfo = 'none',insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total Malaria:', ones),showlegend = T) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.25, y = 0.5)) %>%
      config(displayModeBar = F)
    p8  })
  
  
  output$plot9 <- renderPlotly({
    
    country_data <- country_data()
    country_type_weapon <- data.frame(aggregate(ones ~ weapon_type, data = country_data, sum))
    country_type_weapon <- country_type_weapon[order(country_type_weapon$ones, decreasing  = TRUE),]
    country_type_weapon <- country_type_weapon[1:5,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p9 <- plot_ly(country_type_weapon, labels = ~weapon_type, values = ~ones, type = 'pie',hole = 0.8,
                  textinfo = 'none',insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total Malaria:', ones),showlegend = T) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.3, y = 0.5)) %>%
      config(displayModeBar = F)
    p9  })
  
  
  output$plot10 <- renderPlotly({
    
    country_data <- country_data()
    
    if (input$plotby == 1) {
      attack_category <- aggregate(ones ~ iyear + attack_type, data = country_data, FUN = sum)
      colnames(attack_category) <- c("iyear", "type", "ones")
    }
    if (input$plotby == 2) {
      attack_category <- aggregate(ones ~ iyear + weapon_type, data = country_data, FUN = sum)
      colnames(attack_category) <- c("iyear", "type", "ones")
    }
    if (input$plotby == 3) {
      attack_category <- aggregate(ones ~ iyear + target_type, data = country_data, FUN = sum)
      colnames(attack_category) <- c("iyear", "type", "ones")
    }
    
    attack_category$onessqrt <- sqrt(attack_category$ones)
    
    p10 <- plot_ly(attack_category) %>%
      add_trace(x = ~iyear, y = ~onessqrt, color = ~type, type = 'scatter', 
                mode = 'lines+markers',line = list(width = 2), hoverinfo = "text", 
                text = ~paste(paste("Total Malaria:", ones), type,
                              sep = "<br />"), colors = c("red","blue","green","orange")) %>%
      layout(
        xaxis = list(range = c(2010, 2019),zeroline = TRUE, title = ""),
        yaxis = list(side = 'left', rangemode = "tozero", overlaying = "y", 
                     title = 'SQRT(Number of Malaria)',showgrid = TRUE, 
                     zeroline = TRUE,showticklabels = TRUE),
        legend = list(orientation = 'h')) %>%
      config(displayModeBar = F)
    p10  })
  
  
  output$plot11 <- renderPlotly({
    
    #m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p11 <- plot_ly(
      x = c("IRS","ITN","CaseMgt", "Pop_Protected"),
      y = c(28,56,65,94),
      type = "bar",
      color = c("DT: 28%","KNN: 56%","NB: 65","RF: 94"),
      showlegend = FALSE) %>%
      layout(title = "Malaria Intervention", xaxis = list(title = "Method"), yaxis = list(title = "Coverage (%)")) %>%
      config(displayModeBar = F)
    p11  })
  
  output$plot12 <- renderPlotly({
    
    p12 <- plot_ly(
      x = c("Suicide", "Success","Multiple","Weapon","Attack","Fatalities","Target"),
      y = c(78.9,94.7,231.3,424.1,599.2,634.8,1121.0),
      type = "bar",
      color = c("Suicide","Success","Multiple","Weapon Type", "Attack Type","Fatalities",
                "Target Type"),
      showlegend = FALSE
    ) %>%
      layout(title = "Importance of Paramters",xaxis = list(categoryarray = c("Suicide", "Success","Multiple","Weapon","Attack","Fatalities","Target"), categoryorder = "array"),
             yaxis = list(title = "Mean Decrease in Gini Impurity Index")) %>%
      config(displayModeBar = F)
    p12 })                   
  
  
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(data1, file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = "cookbook.pdf",
    content = function(file) {
      file.copy("cookbook.pdf", file)
    }
  )
  
  
  
  
  
  
  
  output$logisticTbl <- DT::renderDataTable({
    
    dat <- datatable(shiny_logistic, options = list( initComplete = JS("
                                                        function(settings, json) {
                                                        $(this.api().table().header()).css({
                                                        'background-color': '#fff',
                                                        'color': '#444'
                                                        });
                                                        }"),pageLength = 7, pagingType = "simple", bFilter = FALSE,bInfo = FALSE, bPaginate = FALSE),
                     rownames= FALSE) %>% 
      formatStyle('Parameter', color = '#444') %>%
      formatStyle('Error', color = '#444',textAlign = 'center') %>%
      formatStyle('Coefficient', color = '#444',textAlign = 'center')
    return(dat) })
  
  
  
  
  output$groupnameTbl <- DT::renderDataTable({
    
    country_data <- country_data()
    country_group_name <- aggregate( nkill ~ group_name, data = country_data, FUN = sum)
    country_group_name <- country_group_name[order(country_group_name$nkill, decreasing = T), ]
    country_group_name$group_name <- as.character(country_group_name$group_name)
    country_group_name <- country_group_name[!country_group_name$group_name == "Unknown",]
    country_group_name <- country_group_name[0:10,]
    colnames(country_group_name) <- c("Group", "Fatalities")
    
    dat1 <- datatable(country_group_name, options = list( initComplete = JS("
                                                                                                                       function(settings, json) {
                                                                                                                       $(this.api().table().header()).css({
                                                                                                                       'background-color': '#fff',
                                                                                                                       'color': '#444'
                                                                                                                       });
                                                                                                                       }"),pageLength = 7, pagingType = "simple", bFilter = FALSE,bInfo = FALSE, bPaginate = FALSE),
                      rownames= FALSE) #%>% 
    #formatStyle('Parameter', color = '#444') %>%
    #formatStyle('Error', color = '#444',textAlign = 'center') %>%
    #formatStyle('Coefficient', color = '#444',textAlign = 'center')
    return(dat1) })
  
  
  
  
  output$globe <- renderGlobe({
    terror <- readRDS("terror.rds")
    
    terror})
  
  
  output$levelQueryUi <- renderUI({
    radioButtons("analysisLevel", label = "Level of Analysis",
                 choices = list("Region" = 1, "Country" = 2), 
                 selected = 1)
  })
  
  output$timeplot <- renderUI({
    radioButtons("plotby", label = "Time-series By",
                 choices = list("IRS" = 1, "ITN" = 2, "Case_Mgt" = 3), 
                 selected = 1)
  })
  
  output$regionlist <- renderUI({
    region_name <- readRDS("region_name.rds")
    selectInput("byregion", label = "Region:", choices = c(Choose='', as.character(region_name)), selected = "North America", selectize = FALSE)
  })
  
  output$countrylist <- renderUI({
    country_name <- readRDS("country_name.rds")
    selectInput("bycountry", label = "Country:", choices = c(Choose='', as.character(country_name)), selected = "United States", selectize = FALSE)
  })
  
  
  
  
  
  output$totAttacks_country <- renderValueBox({
    
    country_data <- country_data()
    
    country_no_attack <- nrow(country_data)
    valueBox(country_no_attack,"Total IRS Population",icon = icon("globe"), color = 'red') })
  
  output$totloss_country <- renderValueBox({
    
    
    country_data <- country_data()
    country_finance_loss <- convert_to_word(sum(country_data$propvalue, na.rm = TRUE))
    #country_life_loss <- sum(country_data$nkill, na.rm = TRUE)
    valueBox(paste("$",country_finance_loss,sep = ""),"Total Intervention Cost",icon = icon("bank"), color = 'purple') })
  
  output$totlife_country <- renderValueBox({
    
    country_data <- country_data()
    country_life_loss <- sum(country_data$nkill, na.rm = TRUE)
    valueBox(country_life_loss,"Total ITN Distributed",icon = icon("user"), color = 'blue') })
  
  
  output$totcity_country <- renderValueBox({
    
    country_data <- country_data()
    country_city <- length(unique(country_data$city, na.rm = TRUE))
    valueBox(country_city,"Cities Covered",icon = icon("flag-o"), color = 'green') })
  
  
  output$Design <- renderText({
    if (input$analysisLevel == 1) {
      name = paste(input$byregion)
    } else {
      name = paste(input$bycountry)
    }
    name
  })
  
  output$tseries <- renderText({
    if (input$plotby == 1) {
      name = paste("Time-series by IRS")
    } 
    if (input$plotby == 2) {
      name = paste("Time-series by ITN")
    }
    if (input$plotby == 3) {
      name = paste("Time-series by Case_Mgt")
    }
    name
  })
  
  
  output$country_map <- renderLeaflet({
    
    
    country_data <- country_data()
    country_data <- country_data[complete.cases(country_data$latitude),]
    country_data <- country_data[complete.cases(country_data$nkill),]
    country_data <- country_data[country_data$nkill > 0, ]
    country_data$sqrt <- log(country_data$nkill)
    
    
    
    m1 = leaflet(country_data) %>% addProviderTiles("CartoDB.DarkMatterNoLabels", options= providerTileOptions(opacity = 0.7)) 
    
    pal <- colorNumeric(palette = "YlOrRd", domain = country_data$sqrt)
    m1 %>% addCircleMarkers(radius = 2, color = ~pal(sqrt),popup = ~paste(paste0("Fatalities: ", nkill),paste0("City: ", city),
                                                                          paste0("Date: ", date), sep = '<br />')
                            , opacity = 0.9)
    
    
    
  })
})

shinyApp(ui, server)
