rm(list=ls())
library(darksky) # powered by dark sky
library(data.table)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(lubridate)
library(scales)
library(shiny)
library(zipcode)

source("R/weather_functions.R")
source("C:/Users/set-darksky-api.R") # need key from darksky.com to work

data(zipcode)
setDT(zipcode)


# Shiny App ---------------------------------------------------------------

ui <- fluidPage(

  fluidRow(
    column(6, 
           inputPanel(textInput(inputId = 'zip', label = 'Enter a Zipcode', value = '66210'),
                        fluidPage(
                         fluidRow(uiOutput(outputId = 'citystate')),
                         fluidRow(uiOutput(outputId = 'conditions')))
                         )
           )

  ),
  fluidRow(
    column(6,
           plotOutput(outputId = "hourly_graph")),
    column(6, 
           plotOutput(outputId = "weekly_graph"))
  ),
  fluidRow(
    column(6, 
           fluidPage(
             fluidRow(),
             fluidRow(plotOutput(outputId = "windspeed_graph", click = 'plot_click'))
             )),
    column(3, 
           fluidPage(
             fluidRow(),
             fluidRow(plotOutput(outputId = 'windbearing_graph'))
             )),
    column(3, 
           verbatimTextOutput(outputId = 'table_info'))
          
  )
)


server <- function(input, output) {

  zip_weather_data <- reactive({
    if(length(zipcode[zip == input$zip]$city) > 0){
    print('getting new forecast...')
    get_current_forecast(latitude = as.character(zipcode[zip == input$zip,]$latitude),
                         longitude = as.character(zipcode[zip == input$zip,]$longitude))
    } else {
      print('not yet')
    }
})
  
  
  city_state <- reactive({
    my_city <- zipcode[zip == input$zip,]$city
    my_state <- zipcode[zip== input$zip,]$state
    paste0("<H4><b>",my_city, ", ", my_state,"</H4></b>")
  })
  
  conditions <- reactive({
    paste0("<H5><b>It is ", tolower(zip_weather_data()$currently$summary), " and ", 
           round(zip_weather_data()$currently$temperature), "°F</H5>")
    
  })
  

  click_time <- reactive({
    print(input$plot_click$x)
    if(is.null(input$plot_click$x)) {
      min(zip_weather_data()$hourly$time)
    } else {
      as.POSIXct(input$plot_click$x, origin ='1970-01-01 00:00.00 UTC') %>% round(units = "hours")
    }
    
    #print(input$plot_click)
    #as.POSIXct(input$plot_click$x)
  })
  


    output$weekly_graph <- renderPlot({
      if(length(zipcode[zip == input$zip]$city) > 0){
        weekly_graph(zip_weather_data()$daily)
      }
    })


    output$hourly_graph <- renderPlot({
      if(length(zipcode[zip == input$zip]$city) > 0) {
        hourly_graph(zip_weather_data()$hourly)
      }
    })

    output$conditions <- renderText({
      if(length(zipcode[zip == input$zip]$city) > 0) {
        HTML(conditions())
      }
    })


    output$citystate <- renderText({
      if(length(zipcode[zip == input$zip]$city) > 0) {
        HTML(city_state())
      } else {
        HTML('<H4>Zip code not found...</H4>')
      }

    })

    output$windspeed_graph <- renderPlot({
      if(length(zipcode[zip == input$zip]$city) > 0) {
        windspeed_graph(zip_weather_data()$hourly)
      }
      
    })

    output$windbearing_graph <- renderPlot({
      if(length(zipcode[zip == input$zip]$city) > 0) {
        windbearing_graph(zip_weather_data()$hourly, click_time())
        }
    })

    output$table_info <- renderPrint(zip_weather_data()$hourly[time==click_time()]$temperature)
  }

    
  

shinyApp(ui = ui, server = server)

