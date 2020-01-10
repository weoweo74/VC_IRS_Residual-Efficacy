library(shiny)
library(shinyGlobe)
library(ggplot2)
library(dplyr)
library(ggmap)
library(maptools)
library(maps)
library(plotly)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinyjs)
library(V8)
library(reshape)

#Install devtools
install.packages("devtools")
library(devtools)
install_github("shinyGlobe", "trestletech")

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
