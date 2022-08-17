
# https://jaroslavkotrba.shinyapps.io/realestate

# Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

library(googlesheets4)
gs4_deauth() # non restricted
data <- read_sheet("https://docs.google.com/spreadsheets/d/1BYDc2Fy1rNi3btrMVwSSj9FzDqBYUw-tl-Z0TNE9D9c/edit#gid=1727888783")

# ui
ui <- dashboardPage(
  skin='red',
  
  dashboardHeader(
    title = "Real Estate in Brno",
    titleWidth = 600
  ),
  
  dashboardSidebar(
    width = 300,
    
    br(),
    h4(strong("AI Price Prediction"), style="padding-left: 15px"),
    
    selectInput(
      inputId = "rooms",
      label = strong("Select type:", style = "font-family: 'arial', font-size: 14px"),
      choices = c("1+1", "1+kk", "2+1", "2+kk", "3+1", "3+kk", "4+1", "4+kk", "5+1", "5+kk"),
      selected = "2+kk"
    ),
    
    selectInput(
      inputId = "location",
      label = strong("Select location:", style = "font-family: 'arial', font-size: 14px"),
      choices = dput(sort(unique(data$location))),
      selected = "BrnoVenkov"
    ),
    
    numericInput(
      inputId = "m2",
      label = "Select size of an appartment in m2: ",
      value = 65,
      min = 0, max = 300, step = 1
    ),
    
    numericInput(
      inputId = "savings",
      label = "My savings: ",
      value = 200000,
      min = 0, max = 10000000, step = 100000
    ),
    
    br(),
    h4(strong("Mortgage Calculation"), style="padding-left: 15px"),
    
    numericInput(
      inputId = "property",
      label = "Predicted price:",
      value = 5000000,
      min = 0, max = 10000000, step = 100000
    ),
    
    numericInput(
      inputId = "mortgage",
      label = "Need to borrow:",
      value = 3000000,
      min = 0, max = 10000000, step = 100000
    ),
    
    numericInput(
      inputId = "i.m",
      label = "Select mortgage rate (we start at 3%): ",
      value = 0.03,
      min = 0.01, max = 0.99, step = 0.001
    ),
    
    selectInput(
      inputId = "year",
      label = "Select how many years: ",
      choices = c(1:100),
      selected = 15
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      type = "pills", # tabs look
      id = "tab_selected",
      # tab panel number 1
      tabPanel(
        title = "AI & Mortgage",
        style="padding-top: 15px",
        p(strong("Make your own price prediction of an appartment, calculate a mortgage for a payment and compare with other appartments in Brno region."), style="padding-left: 15px; padding-right: 15px"),
        br(),
        h4(strong("Model accuracy:"), style="padding-left: 15px"),
        verbatimTextOutput("accuracy"),
        p(em("The calculation of the model will take cca 20 sec..."), style="padding-left: 15px"),
        br(),
        h4(strong("Model prediction:"), style="padding-left: 15px"),
        verbatimTextOutput("prediction"),
        verbatimTextOutput("need_to_borrow"),
        br(),
        h4(strong("Mortgage calculation:"), style="padding-left: 15px"),
        verbatimTextOutput("payment_per_month"),
        verbatimTextOutput("payment_total"),
        verbatimTextOutput("payment_difference"),
        verbatimTextOutput("payment_more"),
        verbatimTextOutput("payment_salary"),
        verbatimTextOutput("payment_part"),
        br(),
        # icon("glyphicon glyphicon-eye-open", lib = "glyphicon", style="padding-left: 20px"),
        uiOutput("link1", style="padding-left: 20px"),
        
      ),
      # tab panel number 2
      tabPanel(
        title = "Visualisation",
        br(),
        h4(strong("Real estate visualisation rooms:"), style="padding-left: 15px"),
        plotlyOutput("rom"),
        h4(strong("Real estate visualisation location:"), style="padding-left: 15px"),
        plotlyOutput("loc"),
        h4(strong("Real estate visualisation with regression:"), style="padding-left: 15px"),
        plotlyOutput("reg"),
        h4(strong("Real estate visualisation of box plots:"), style="padding-left: 15px"),
        plotlyOutput("box"),
        h4(strong("Real estate visualisation of average price per m2:"), style="padding-left: 15px"),
        plotlyOutput("aver"),
        h4(strong("Real estate visualisation of average price per m2:"), style="padding-left: 15px"),
        plotlyOutput("avel"),
        h4(strong("Real estate visualisation of count of properties:"), style="padding-left: 15px"),
        plotlyOutput("cou"),
        br(),
        uiOutput("link2", style="padding-left: 20px")
      ),
      # tab panel number 3
      tabPanel(
        title = "CNB - Rates",
        br(),
        style="padding-top: 15px",
        h5(strong("Here is the development of the three main rates given by the CNB, the output is directly taken from the main web page of the CNB."), style="padding-left: 15px"),
        h4(strong("Repo rates:"), style="padding-left: 15px"),
        plotlyOutput("repo"),
        h4(strong("Discount rates:"), style="padding-left: 15px"),
        plotlyOutput("disco"),
        h4(strong("Lombard rates:"), style="padding-left: 15px"),
        plotlyOutput("lombard"),
        br(),
        uiOutput("link3", style="padding-left: 20px")
      )
    )
  )
)