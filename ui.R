# https://jaroslavkotrba.shinyapps.io/realestate

# Libraries
library(shiny)
library(ggplot2)
library(plotly)
library(shinymanager)
library(shinythemes)

library(googlesheets4)
gs4_deauth() # non restricted
data <- read_sheet("https://docs.google.com/spreadsheets/d/1BYDc2Fy1rNi3btrMVwSSj9FzDqBYUw-tl-Z0TNE9D9c/edit#gid=1727888783")

# ui
ui <- navbarPage(
  tags$head(tags$link(rel="shortcut icon", href="https://freeiconshop.com/wp-content/uploads/edd/home-solid.png")),
  title = div(icon("glyphicon glyphicon-home", lib = "glyphicon", style="padding-left: 0px;padding-right: 5px;"), "Real Estates of BRNO"), 
  theme = shinytheme("superhero"),
  collapsible = TRUE,
  
  tabPanel("AI & Mortgage",
           sidebarLayout(
             sidebarPanel(
               h5(strong("AI Price Prediction"), style="padding-left: 15px"),
               
               selectInput(
                 inputId = "rooms",
                 label = strong("Select type:", style = "font-family: 'arial', font-size: 14px"),
                 choices = c("1+1", "1+kk", "2+1", "2+kk", "3+1", "3+kk", "4+1", "4+kk", "5+1", "5+kk", "aty"),
                 selected = "3+kk"
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
                 value = 70,
                 min = 0, max = 300, step = 10
               ),
               
               numericInput(
                 inputId = "savings",
                 label = "My savings: ",
                 value = 200000,
                 min = 0, max = 10000000, step = 100000
               ),
               
               br(),
               h5(strong("Mortgage Calculation"), style="padding-left: 15px"),
               
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
             mainPanel(
               style="padding-top: 30px",
               p(em(strong("Make your own price prediction of an appartment, calculate a monthly mortgage payment and compare with other appartments in the Brno region.")), style="padding-left: 15px; padding-right: 15px"),
               br(),
               h4(strong("Model accuracy:"), style="padding-left: 15px"),
               verbatimTextOutput("accuracy"),
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
               uiOutput("link1", style="padding-left: 20px")
             )
           )
  ),
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               h5(strong("Map of Brno"), style="padding-left: 15px"),
               
               numericInput(
                 inputId = "price_min",
                 label = "Min price:",
                 value = 2500000,
                 min = 0, max = 100000000, step = 100000
               ),
               
               numericInput(
                 inputId = "price_max",
                 label = "Max price:",
                 value = 8500000,
                 min = 0, max = 100000000, step = 100000
               ),
               
               selectInput(
                 inputId = "m2_min",
                 label = "Min m2:",
                 choices = c(1:350),
                 selected = 40
               ),
               
               selectInput(
                 inputId = "m2_max",
                 label = "Max m2:",
                 choices = c(1:350),
                 selected = 120
               ),
               
               selectInput(
                 inputId = "rooms_searched",
                 label = strong("Select type:", style = "font-family: 'arial', font-size: 14px"),
                 choices = c("1+1", "1+kk", "2+1", "2+kk", "3+1", "3+kk", "4+1", "4+kk", "5+1", "5+kk", "aty"),
                 selected = c("1+1", "1+kk", "2+1", "2+kk", "3+1", "3+kk", "4+1", "4+kk", "5+1", "5+kk", "aty"),
                 multiple = TRUE
               ),
               
               selectInput(
                 inputId = "file",
                 label = strong("Save filtered data as:", style = "font-family: 'arial', font-size: 14px"),
                 choices = c(".xlsx", ".csv"),
                 selected = ".xlsx"
               ),
               
               downloadButton(
                 "downloadData", "Download", class = "btn btn-secondary", icon = shiny::icon("download")
               )
             ),
             mainPanel(
               style="padding-top: 15px",
               h4(strong("Real estates in Brno and surroundings:"), style="padding-left: 15px"),
               verbatimTextOutput("plot_error"),
               plotlyOutput("map"),
               br(),
               uiOutput("link2", style="padding-left: 20px")
             )
           )
  ),
  tabPanel("Visualisation",
           style="padding-top: 15px",
           h4(strong("Real estate visualisation rooms:"), style="padding-left: 15px"),
           
           selectInput(
             inputId = "rooms_plot",
             label = strong("Select type:", style = "font-family: 'arial', font-size: 14px"),
             choices = c("1+1", "1+kk", "2+1", "2+kk", "3+1", "3+kk", "4+1", "4+kk", "5+1", "5+kk", "aty"),
             selected = "2+kk"
           ),
           
           plotlyOutput("rom"),
           h4(strong("Real estate visualisation location:"), style="padding-left: 15px"),
           
           selectInput(
             inputId = "location_plot",
             label = strong("Select location:", style = "font-family: 'arial', font-size: 14px"),
             choices = dput(sort(unique(data$location))),
             selected = "BrnoVenkov"
           ),
           
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
           uiOutput("link3", style="padding-left: 20px")
  ),
  tabPanel("CNB-Rates",
           style="padding-top: 15px",
           h5(em(strong("Here is the development of the three main rates given by the Czech National Bank, the output is directly taken from the main web page of the CNB.")), style="padding-left: 15px"),
           h4(strong("Repo rates:"), style="padding-left: 15px"),
           plotlyOutput("repo"),
           h4(strong("Discount rates:"), style="padding-left: 15px"),
           plotlyOutput("disco"),
           h4(strong("Lombard rates:"), style="padding-left: 15px"),
           plotlyOutput("lombard"),
           br(),
           uiOutput("link4", style="padding-left: 20px")
  ),
  tabPanel("Inflation",
           style="padding-top: 15px",
           h5(em(strong("Here is the inflation in the Czech Republic that is directly taken from the Czech Statistical Office.")), style="padding-left: 15px"),
           h4(strong("Inflation rates:"), style="padding-left: 15px"),
           plotlyOutput("inf"),
           br(),
           uiOutput("link5", style="padding-left: 20px")
  )
)

# ui login
ui <- secure_app(ui, theme = shinythemes::shinytheme("flatly"), fab_position = "bottom-right", enable_admin = TRUE,
  tag_img = tags$img(src = "https://www.forbes.com/advisor/wp-content/uploads/2022/06/Real-estate1.png", width = 100)
)
