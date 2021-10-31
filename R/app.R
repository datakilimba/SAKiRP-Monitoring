library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)

source("Price Data.R")
source("Weather Data.R")
source("./WAEO Tasks/Market Survey Completion.R")

## Load global data yeah
load_data()

sidebar = dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Home", icon = icon("home"), tabName = "home"),
              menuItem("Market Data", tabName = "market", icon = icon("chart-line")),
              menuItem("Weather Forecast", tabName = "weather", icon = icon("cloud-sun-rain")),
              menuItem("Value Chains",tabName = "chains", icon = icon("link"),
                       menuSubItem("Sunflower", icon = icon("sun"), tabName = "sunflower"),
                       menuSubItem("Beans", icon = icon("seedling"), tabName = "beans"),
                       menuSubItem("Cassava", icon = icon("leaf"), tabName = "cassava"),
                       menuSubItem("Soya", icon = icon("pagelines"), tabName = "soya")
                       ),
              menuItem("Extension", icon = icon("tractor"), tabName = "extension",
                       menuSubItem(icon=icon("calendar"), 
                                   radioButtons("task", "Tracked Tasks",
                                                c("Market Survey" = "ms",
                                                  "Land Prep & Planting" = "lpp"))),
                       menuSubItem("Task Completion Rate", tabName = "completion",
                                   icon = icon("chart-line")),
                       menuSubItem("Task Error Rate", tabName = "error",
                                   icon = icon("chart-bar")))
  )
  ,
  conditionalPanel(
    condition = "input.menu == 'market'",
    selectInput("district", label = "District",
                choices = c("",unique(market_dat_long$district)), 
                selected = "Kigoma"),
    uiOutput("ward_choice"),
    selectInput("product", label = "Product",
                choices = c("",unique(market_dat_long$item)),
                selected = "Local Yellow Market Price")
  ),
  conditionalPanel(
    condition = "input.menu == 'weather'",
    selectInput("weather_district", label = "District",
                choices = c("",unique(market_dat_long$district)), 
                selected = "Kigoma"),
    uiOutput("weather_ward")
  )
  
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("Infoboxs on aggregate (all task) completion Rate per district and error rate per district")
    ),
    tabItem(tabName = "completion",
            tabBox(
              title = "Monthly Extension Tasks",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tasks", height = "490px",
              tabPanel("Completion Rate",
                       dataTableOutput("completion_rate")),
              tabPanel("Error Rate", "Error Rate"),
              tabPanel("Error Rows","Error Rows"),
              tabPanel("Task Score","Task Score"),width = 12
              
            )
    ),
    
    tabItem(tabName = "market",
            fluidRow(
              column(10, dygraphOutput("regionPlot"))
            )
    ),
    
    tabItem(tabName = "sunflower",
            h2("Sunflower tab content")
    ),
    tabItem(tabName = "weather",
            fluidRow(
              dygraphOutput("weather_plot", height = "400")
            )
    ),
    tabItem(tabName = "beans",
            h2("Beans tab content")
    ),
    tabItem(tabName = "cassava",
            h2("Cassava tab content")
    ),
    tabItem(tabName = "soya",
            h2("Soya tab content")
    )
  )
)

ui = dashboardPage(
  dashboardHeader(title = "SAKiRP Dashboard"),
  sidebar,
  body
)

server = function(input, output, session) { 
  
  output$ward_choice <- renderUI({
    selectInput(inputId="ward",
                label="Ward", 
                choices = unique(ward_dat 
                                 [ward_dat$district==input$district, 
                                   "ward"]))
  })
  
  output$weather_ward <- renderUI({
    selectInput(inputId="ward",
                label="Ward", 
                choices = unique(ward_dat 
                                 [ward_dat$district==input$weather_district, 
                                   "ward"]))
  })
  
  output$completion_rate = renderDT({
    datatable(monthly_market_outcome %>% 
                select(district,ward,waeo,completion_rate)) 
  })
  
  
  ward = eventReactive(input$product,{
    if(is.null(input$ward)){
      return("Bitale")
    }else{
      input$ward
    }
  })
  
  district = eventReactive(input$product,{
    input$district
  })
  
  output$regionPlot = renderDygraph({
    get_region_dygraph(product = input$product ,dstrct =  district(),
                       wrd = ward())
  })
  
  output$weather_plot = renderDygraph({
    get_weather_dygraph()
  })
  
}

shinyApp(ui, server)
