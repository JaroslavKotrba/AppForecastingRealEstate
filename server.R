# server
server <- function(input, output){
  
  library(googlesheets4)
  gs4_deauth() # non restricted
  data <- read_sheet("https://docs.google.com/spreadsheets/d/1BYDc2Fy1rNi3btrMVwSSj9FzDqBYUw-tl-Z0TNE9D9c/edit#gid=1727888783")
  
  # Outliers
  library(tidyverse)
  outlier_limits <- data %>% 
    group_by(rooms) %>% 
    summarise (Q1 = quantile(price, probs = 0.25),
               Q3 = quantile(price, probs = 0.75),
               IQR = Q3 - Q1,
               upper_inner_limit = Q3 + 3 * IQR, # rule of thump - extreme outliers
               lower_inner_limit = Q1 - 3 * IQR) # rule of thump - extreme outliers
  
  data <- left_join(data, outlier_limits, by = "rooms") %>% select (-Q1, -Q3, -IQR)
  data <- data %>% mutate(outlier = ifelse(price > lower_inner_limit & 
                                             price < upper_inner_limit, "no outlier", "outlier"))
  
  data <- subset(data, outlier %in% c("no outlier"))
  data[c("upper_inner_limit", "lower_inner_limit", "outlier")] <- NULL
  
  # Get all numeric data
  library(dplyr)
  dataset_numeric <- data %>% select_if(is.numeric); dataset_numeric
  
  # Get all categorical variables
  library("fastDummies")
  dataset_categorical <- data %>% select_if(is.character); dataset_categorical
  
  # Transform into dummies
  dataset_categorical <- dummy_cols(dataset_categorical, remove_first_dummy = T); dataset_categorical
  
  # Joining numerical and character datasets
  dataset <- cbind(dataset_numeric, dataset_categorical)
  dataset[ ,c('rooms', 'location')] <- list(NULL)
  
  # Split
  library(caTools)
  split <- sample.split(dataset$price, SplitRatio = 0.8)
  train <- subset(dataset, split==T)
  test <- subset(dataset, split==F)
  
  X_train <- data.matrix(train[, -1])
  y_train <- train[ ,1]
  
  X_test <- data.matrix(test[, -1])
  y_test <- test[ ,1]
  
  library(xgboost)
  xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_test <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Model
  # model <- xgboost(data = xgb_train, max.depth = 20, nrounds = 1)
  model <- readRDS("model.rds") # model ONLY
  
  y_pred <- predict(model, xgb_test); y_pred
  
  library(caret)
  proc <- paste0(round(mean((abs(y_test - y_pred)/y_test)*100), 2), "%")
  mae <- caret::MAE(y_test, y_pred)
  rmse <- caret::RMSE(y_test, y_pred)
  
  output$accuracy <- renderText({
    paste0("Percentage: ", proc,", ", "MAE: ", round(mae,2), ", ", "RMSE: ", round(rmse,2))
  })
  
  sample <- tail(train,1)
  sample[dput(colnames(sample))] <- 0
  
  # Prediction
  output$prediction <- renderText({
    sample[paste0('rooms_',input$rooms)] <- 1
    sample$m2 <- as.numeric(input$m2)
    sample[paste0('location_',input$location)] <- 1  
    sample$price <- NULL
    
    X_sample <- data.matrix(sample)
    xgb_sample <- xgb.DMatrix(data = X_sample)
    
    sample_pred <- predict(model, xgb_sample)
    
    paste0("Predicted price: ", round(sample_pred,2), " CZK")
  })
  
  output$need_to_borrow <- renderText({
    sample[paste0('rooms_',input$rooms)] <- 1
    sample$m2 <- as.numeric(input$m2)
    sample[paste0('location_',input$location)] <- 1  
    sample$price <- NULL
    
    X_sample <- data.matrix(sample)
    xgb_sample <- xgb.DMatrix(data = X_sample)
    
    sample_pred <- predict(model, xgb_sample)
    paste0("Need to borrow: ", round(sample_pred,2) - input$savings, " CZK")
  })
  
  # Mortgage
  make_instalment <- function(H, i.m, year){(H*(i.m/12)*(1+(i.m/12))^(12*year))/((1+(i.m/12))^(12*year)-1)}
  
  output$payment_per_month <- renderText({
    paste0("Mortgage payment per month: ", round(make_instalment(input$mortgage, input$i.m, as.numeric(input$year)),2), " CZK")
  })
  
  output$payment_total <- renderText({
    paste0("Mortgage payment in total: ", round(make_instalment(input$mortgage, input$i.m, as.numeric(input$year))*(as.numeric(input$year)*12),2), " CZK")
  })
  
  output$payment_difference <- renderText({
    paste0("Will pay more in CZK: ", round(make_instalment(input$mortgage, input$i.m, as.numeric(input$year))*(as.numeric(input$year)*12) - input$mortgage,2), " CZK")
  })
  
  output$payment_more <- renderText({
    paste0("Will pay more in %: ", round(((make_instalment(input$mortgage, input$i.m, as.numeric(input$year))*(as.numeric(input$year)*12)/input$mortgage)-1)*100,2), " %")
  })
  
  output$payment_salary <- renderText({
    paste0("Should earn netto per month: ", round(((make_instalment(input$mortgage, input$i.m, as.numeric(input$year))/45)*55) + make_instalment(input$mortgage, input$i.m, as.numeric(input$year)), 2), " CZK")
  })
  
  output$payment_part <- renderText({
    paste0("Should have own 10%: ", round((input$mortgage/90)*10,2), " CZK, ", "or better 20%: ", round((input$mortgage/80)*20,2), " CZK")
  })
  
  output$reg <- renderPlotly({
    # Geom point
    X <- data$m2
    y <- data$price
    model <- lm(y ~ X, data=data) # linear regression
    summary(model)
    `predicted price` <- predict(model, newdata=data)
    
    library(ggplot2)
    library(plotly)
    library(scales)
    g <- ggplot(data, aes(m2, price)) +
      geom_point(stat='identity',aes(color = location, fill = rooms), size=2.4) +
      geom_line(aes(m2, `predicted price`), color='red', size=0.8) +
      scale_y_continuous(labels = comma, limits = c(1000000, 30000000), breaks = seq(0,30000000, by=3000000)) +
      scale_x_continuous(labels = comma, limits = c(0, 400), breaks = seq(0,400, by=40)) +
      theme(legend.position="right") +
      ggtitle("Regression Plot of Real Estate in Brno") +
      theme_bw() +
      theme(legend.title = element_blank()) +
      theme(  
        # Title format
        plot.title = element_text(colour="Black",size=16, face = "bold", hjust = 0.5)) +
      theme(
        # Legend elements format
        legend.text = element_text(colour="Black", size=8, face = "bold")) +
      theme(
        # Axis text format
        axis.text.x = element_text(face="bold", color="Black", size=8),
        axis.text.y = element_text(face="bold", color="Black", size=8)) +
      theme(
        # Axis title format
        axis.title.x = element_text(colour="Black", size=10, face="bold"),
        axis.title.y = element_text(colour="Black", size=10, face="bold"))
    g + scale_fill_hue(l=1, c=50)
  })
  
  output$box <- renderPlotly({
    # Geom box plot
    library(ggplot2)
    library(plotly)
    library(scales)
    g <- ggplot(data, aes(rooms, price)) +
      geom_boxplot(fill = "red", color = 'black') +
      scale_y_continuous(labels = comma, limits = c(1000000, 30000000), breaks = seq(0,30000000, by=3000000)) +
      theme(legend.position="right") +
      ggtitle("Box Plot of Real Estate in Brno") +
      theme_bw() +
      theme(legend.title = element_blank()) +
      # Mean
      geom_point(as.data.frame(data %>% group_by(rooms) %>% summarise(mean = mean(price))), mapping = aes(rooms, mean), color='white', size=1.2) +
      theme(
        # Title format
        plot.title = element_text(colour="Black",size=16, face = "bold", hjust = 0.5)) +
      theme(
        # Legend elements format
        legend.text = element_text(colour="Black", size=8, face = "bold")) +
      theme(
        # Axis text format
        axis.text.x = element_text(face="bold", color="Black", size=8),
        axis.text.y = element_text(face="bold", color="Black", size=8)) +
      theme(
        # Axis title format
        axis.title.x = element_text(colour="Black", size=10, face="bold"),
        axis.title.y = element_text(colour="Black", size=10, face="bold")) +
      theme(
        # Legend
        legend.position = "none")
  })
  
  output$repo <- renderPlotly({
    # Repo rate
    library(rvest)
    library(dplyr)
    
    link <- "https://www.cnb.cz/cs/casto-kladene-dotazy/Jak-se-vyvijela-dvoutydenni-repo-sazba-CNB/"
    page <- read_html(link)
    
    date <- page %>% html_nodes('td:nth-child(1)') %>% html_text()
    rate <- page %>% html_nodes('td+ td') %>% html_text()
    
    date <- stringi::stri_escape_unicode(date)
    
    Sazba <- data.frame(cbind(date, rate))
    Sazba$rate <- as.numeric(gsub(",", ".", Sazba$rate))
    Sazba$date <- gsub("\\\\u00a0", "", Sazba$date)
    Sazba$date <- as.Date(Sazba$date, "%d.%m.%Y")
    
    library(ggplot2)
    options(scipen=999)
    ggplot(data = Sazba, aes(x=date,y=rate)) +
      geom_line(color='red', size=1) +
      geom_point(color='blue', size=2) +
      ylab("Repo rate") +
      xlab("Years") +
      theme_bw()
  })
  
  output$disco <- renderPlotly({
    # Discount rate
    library(rvest)
    library(dplyr)
    
    link <- "https://www.cnb.cz/cs/casto-kladene-dotazy/Jak-se-vyvijela-diskontni-sazba-CNB/"
    page <- read_html(link)
    
    date <- page %>% html_nodes('td:nth-child(1)') %>% html_text()
    rate <- page %>% html_nodes('td+ td') %>% html_text()
    
    date <- stringi::stri_escape_unicode(date)
    
    Sazba <- data.frame(cbind(date, rate))
    Sazba$rate <- as.numeric(gsub(",", ".", Sazba$rate))
    Sazba$date <- gsub("\\\\u00a0", "", Sazba$date)
    Sazba$date <- as.Date(Sazba$date, "%d.%m.%Y")
    
    library(ggplot2)
    options(scipen=999)
    ggplot(data = Sazba, aes(x=date,y=rate)) +
      geom_line(color='red', size=1) +
      geom_point(color='blue', size=2) +
      ylab("Discount rate") +
      xlab("Years") +
      theme_bw()
  })
  
  output$lombard <- renderPlotly({
    # Lombard rate
    library(rvest)
    library(dplyr)
    
    link <- "https://www.cnb.cz/cs/casto-kladene-dotazy/Jak-se-vyvijela-lombardni-sazba-CNB/"
    page <- read_html(link)
    
    date <- page %>% html_nodes('td:nth-child(1)') %>% html_text()
    rate <- page %>% html_nodes('td+ td') %>% html_text()
    
    date <- stringi::stri_escape_unicode(date)
    
    Sazba <- data.frame(cbind(date, rate))
    Sazba$rate <- as.numeric(gsub(",", ".", Sazba$rate))
    Sazba$date <- gsub("\\\\u00a0", "", Sazba$date)
    Sazba$date <- as.Date(Sazba$date, "%d.%m.%Y")
    
    library(ggplot2)
    options(scipen=999)
    ggplot(data = Sazba, aes(x=date,y=rate)) +
      geom_line(color='red', size=1) +
      geom_point(color='blue', size=2) +
      ylab("Lombard rate") +
      xlab("Years") +
      theme_bw()
  })
  
  url <- a("https://jaroslavkotrba.com", href="https://jaroslavkotrba.com/index.html")
  
  output$link1 <- renderUI({
    tagList("To see other author's projects:", url)
  })
  
  output$link2 <- renderUI({
    tagList("To see other author's projects:", url)
  })
  
  output$link3 <- renderUI({
    tagList("To see other author's projects:", url)
  })
}
