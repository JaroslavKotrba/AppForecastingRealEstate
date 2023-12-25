# server

library(shinymanager)
password <- read.table("password.txt", header = TRUE)
credentials <- data.frame(
  user = c("user", "admin"), # mandatory
  password = dput(password$password), # mandatory
  start = c("2022-01-01"), # optinal (all others)
  expire = c("9999-12-31", NA),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# Init the database
create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite",
  passphrase = "passphrase_wihtout_keyring"
)

server <- function(input, output){
  
  # Login
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "database.sqlite",
      passphrase = "passphrase_wihtout_keyring"
    )  
  )
  
  # Data
  library(googlesheets4)
  gs4_deauth() # non restricted
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1mgYyObGN0yVBuLClF0Cki0Y-bZ-Fqog6HTS6Hd-cxMQ/edit#gid=245974009")
  
  data <- subset(df, select = c(price, rooms, m2, location))
  
  # Outliers
  library(tidyverse)
  outlier_limits <- data %>%
    group_by(rooms) %>%
    summarise (Q1 = quantile(price, probs = 0.25),
               Q3 = quantile(price, probs = 0.75),
               IQR = Q3 - Q1,
               upper_inner_limit = Q3 + 2 * IQR, # rule of thump - extreme outliers
               lower_inner_limit = Q1 - 2 * IQR) # rule of thump - extreme outliers
  
  data <- left_join(data, outlier_limits, by = "rooms") %>% select (-Q1, -Q3, -IQR)
  data <- data %>% mutate(outlier = ifelse(price > lower_inner_limit &
                                             price < upper_inner_limit, "no outlier", "outlier"))
  
  data <- subset(data, outlier == "no outlier")
  data[c("upper_inner_limit", "lower_inner_limit", "outlier")] <- NULL
  
  # Get all numeric data
  library(dplyr)
  dataset_numeric <- data %>% select_if(is.numeric); dataset_numeric
  
  # Get all categorical variables
  library("fastDummies")
  dataset_categorical <- data %>% select_if(is.character); dataset_categorical
  
  # Transform into dummies
  dataset_categorical <- dummy_cols(dataset_categorical, remove_first_dummy = F); dataset_categorical #CHECK
  
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
  
  # Scaling
  mean_train <- mean(y_train)
  sd_train   <- sd(y_train)
  y_train <- scale(y_train, center = mean_train, scale = sd_train)
  
  library(xgboost)
  xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_test <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Model ### PATH ###
  # model <- xgboost(data = xgb_train, max.depth = 20, nrounds = 1)
  model <- readRDS("model.rds")
  
  y_pred <- predict(model, xgb_test)
  
  # Scaling
  y_pred <- y_pred * sd_train + mean_train
  
  library(caret)
  proc <- paste0(round(mean((abs(y_test - y_pred)/y_test)*100), 2), "%")
  mae <- caret::MAE(y_test, y_pred)
  rmse <- caret::RMSE(y_test, y_pred)
  
  output$accuracy <- renderText({
    paste0("Percentage: ", proc,", ", "MAE: ", round(mae,2), ", ", "RMSE: ", round(rmse,2))
  })
  
  sample <- tail(train,1)
  sample[dput(colnames(sample))] <- 0
  
  # Predicted price:
  output$prediction <- renderText({
    sample[paste0('rooms_',input$rooms)] <- 1
    sample$m2 <- as.numeric(input$m2)
    sample[paste0('location_',input$location)] <- 1  
    sample$price <- NULL
    
    X_sample <- data.matrix(sample)
    xgb_sample <- xgb.DMatrix(data = X_sample)
    
    sample_pred <- predict(model, xgb_sample)
    
    # Scaling
    sample_pred <- sample_pred * sd_train + mean_train
    
    paste0("Predicted price: ", round(sample_pred,2), " CZK")
  })
  
  # Need to borrow:
  output$need_to_borrow <- renderText({
    sample[paste0('rooms_',input$rooms)] <- 1
    sample$m2 <- as.numeric(input$m2)
    sample[paste0('location_',input$location)] <- 1  
    sample$price <- NULL
    
    X_sample <- data.matrix(sample)
    xgb_sample <- xgb.DMatrix(data = X_sample)
    
    sample_pred <- predict(model, xgb_sample)
    
    # Scaling
    sample_pred <- sample_pred * sd_train + mean_train
    
    paste0("Need to borrow: ", round(sample_pred,2) - input$savings, " CZK")
  })
  
  # Mortgage
  options(scipen=999)
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
    paste0("Should have own 10%: ", round((input$property/90)*10,2), " CZK, ", "or better 20%: ", round((input$property/80)*20,2), " CZK")
  })
  
  output$map <- renderPlotly({
    # MAP - href
    lonlat <- subset(df, lon >= 16 & lon <= 17.1 & lat >= 48.8 & lat <= 50)
    test <- subset(lonlat, price >= input$price_min & price <= input$price_max & m2 >= as.integer(input$m2_min) & m2 <= as.integer(input$m2_max) & rooms %in% input$rooms_searched)
    
    # Check for correct inputs
    output$plot_error <- renderText({
      if (dim(test)[1] == 0) {
        paste0("No such combinations found, please change inputs!")
      }
    })
    
    library(plotly)
    library(htmlwidgets)
    
    plotly <- as.data.frame(subset(test, select = c("price", "rooms", "m2", "location", "lat", "lon", "link")))
    plotly$hover_text <- paste("<br>",
                               "Click to VIEW more info:", "<br>",
                               "Type: ", plotly$rooms, "<br>",
                               "Price: ", plotly$price, " CZK", "<br>",
                               "M2: ", plotly$m2, " m2", "<br>")
    
    # Create plotly figure
    fig <- plot_ly(
      data = plotly,
      lat = ~lat,
      lon = ~lon,
      marker = list(size = 7, color = "blue"),
      type = 'scattermapbox',
      hovertext = ~hover_text,
      customdata = ~link, # stores links as custom data
      hovertemplate = paste(# renders custom data (link)
        '<br><extra>%{hovertext}</extra>'), # includes hovertext
      mode = 'markers+text', # enables clicking on markers
      textposition = 'middle center', # centers text on markers
      textfont = list(color = 'black', size = 10) # sets font color and size of text
    ) %>%
      # Add custom JavaScript code to open link in new window when marker is clicked
      onRender("
                function(el) {
                  el.on('plotly_click', function(d) {
                    var url = d.points[0].customdata;
                    window.open(url);
                  });
                }")
    
    # Customize layout
    fig <- fig %>%
      layout(
        mapbox = list(
          style = 'open-street-map',
          zoom = 10,
          center = list(lon = 16.6068, lat = 49.1951)),
        margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  output$rom <- renderPlotly({
    # SCATTER PLOT - rooms
    
    test <- subset(df, rooms == input$rooms_plot)
    
    X <- test$m2
    y <- test$price
    model <- lm(y ~ X, data=test) # linear regression
    summary(model)
    `predicted price` <- predict(model, newdata=test)
    
    library(ggplot2)
    library(plotly)
    library(scales)
    library(ggthemes)
    library(htmlwidgets)
    
    g <- ggplot(test, aes(m2, price)) +
      geom_point(stat='identity',aes(color = rooms, fill = rooms, text = paste0("Price: ", price, " CZK", "\n",
                                                                                "Area: ", m2, " m2", "\n",
                                                                                "Rooms: ", rooms, "\n",
                                                                                "Location: ", location, "\n",
                                                                                "Click to view more details"), customdata = link), size=2.4) +
      geom_line(aes(m2, `predicted price`), color='red', size=0.8) +
      scale_y_continuous(labels = comma, limits = c(1000000, 30000000), breaks = seq(0,30000000, by=3000000)) +
      scale_x_continuous(labels = comma, limits = c(0, 400), breaks = seq(0,400, by=50)) +
      theme(legend.position="right") +
      ggtitle(paste0("Real Estate ", input$rooms_plot)) +
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
    g <- ggplotly(g + scale_fill_hue(l=1, c=50), tooltip = c("text"))
    
    onRender(g, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }")
  })
  
  output$loc <- renderPlotly({
    # SCATTER PLOT - location
    
    test <- subset(df, location == input$location_plot)
    
    X <- test$m2
    y <- test$price
    model <- lm(y ~ X, data=test) # linear regression
    summary(model)
    `predicted price` <- predict(model, newdata=test)
    
    library(ggplot2)
    library(plotly)
    library(scales)
    library(ggthemes)
    library(htmlwidgets)
    
    g <- ggplot(test, aes(m2, price)) +
      geom_point(stat='identity',aes(color = rooms, fill = rooms, text = paste0("Price: ", price, " CZK", "\n",
                                                                                "Area: ", m2, " m2", "\n",
                                                                                "Rooms: ", rooms, "\n",
                                                                                "Location: ", location, "\n",
                                                                                "Click to view more details"), customdata = link), size=2.4) +
      geom_line(aes(m2, `predicted price`), color='red', size=0.8) +
      scale_y_continuous(labels = comma, limits = c(1000000, 30000000), breaks = seq(0,30000000, by=3000000)) +
      scale_x_continuous(labels = comma, limits = c(0, 400), breaks = seq(0,400, by=50)) +
      theme(legend.position="right") +
      ggtitle(paste0("Real Estate ", input$location_plot)) +
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
    g <- ggplotly(g + scale_fill_hue(l=1, c=50), tooltip = c("text"))
    
    onRender(g, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }")
  })
  
  output$reg <- renderPlotly({
    # SCATTER PLOT - regression
    
    X <- df$m2
    y <- df$price
    model <- lm(y ~ X, data=df) # linear regression
    summary(model)
    `predicted price` <- predict(model, newdata=df)
    
    library(ggplot2)
    library(plotly)
    library(scales)
    library(ggthemes)
    library(htmlwidgets)
    
    g <- ggplot(df, aes(m2, price)) +
      geom_point(stat='identity',aes(color = rooms, fill = rooms, text = paste0("Price: ", price, " CZK", "\n",
                                                                                "Area: ", m2, " m2", "\n",
                                                                                "Rooms: ", rooms, "\n",
                                                                                "Location: ", location, "\n",
                                                                                "Click to view more details"), customdata = link), size=2.4) +
      geom_line(aes(m2, `predicted price`), color='red', size=0.8) +
      scale_y_continuous(labels = comma, limits = c(1000000, 30000000), breaks = seq(0,30000000, by=3000000)) +
      scale_x_continuous(labels = comma, limits = c(0, 400), breaks = seq(0,400, by=50)) +
      theme(legend.position="right") +
      ggtitle("Real Estate in Brno and Surroundings") +
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
    g <- ggplotly(g + scale_fill_hue(l=1, c=50), tooltip = c("text"))
    
    onRender(g, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }")
  })
  
  output$box <- renderPlotly({
    # BOX PLOT
    
    library(ggplot2)
    library(plotly)
    library(scales)
    g <- ggplot(data, aes(rooms, price)) +
      geom_boxplot(fill = "red", color = 'black') +
      scale_y_continuous(labels = comma, limits = c(1000000, 30000000), breaks = seq(0,30000000, by=3000000)) +
      theme(legend.position="right") +
      ggtitle("Box Plot of Real Estate in Brno and Surroundings") +
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
  
  output$aver <- renderPlotly({
    # BAR PLOT - price/m2 rooms
    
    library(ggplot2)
    library(plotly)
    library(tidyverse)
    
    data$price_m2 <- round(data$price/data$m2, 2)
    test <- data %>% group_by(rooms) %>% 
      summarise(
        mean_price_m2 = round(mean(price_m2),2)
      )
    
    options(scipen=999)
    g <- ggplot(data=test, aes(reorder(rooms, mean_price_m2), mean_price_m2, 
                               text = paste0("Rooms: ", rooms, "\n",
                                             "Average price: ", mean_price_m2, " CZK", "\n"))) +
      geom_col(aes(fill = mean_price_m2), color='black', size=0.2) +
      xlab("rooms") +
      ylab("average price per m2") +
      ggtitle("Average Price per m2 Based on Rooms") +
      theme_bw() +
      scale_fill_gradient2(
        # Color
        low = "blue", high = "red", midpoint = median(test$mean_price_m2)) +
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
        # Position vertical
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(
        # Legend
        legend.position = "none")
    g <- ggplotly(g, tooltip = c("text"))
  })
  
  output$avel <- renderPlotly({
    # BAR PLOT - price/m2 location
    
    library(ggplot2)
    library(plotly)
    
    data$price_m2 <- round(data$price/data$m2, 2)
    test <- data %>% group_by(location) %>% 
      summarise(
        mean_price_m2 = round(mean(price_m2),2)
      )
    
    g <- ggplot(data=test, aes(reorder(location, mean_price_m2), mean_price_m2, 
                               text = paste0("Location: ", location, "\n",
                                             "Average price: ", mean_price_m2, " CZK", "\n"))) +
      geom_col(aes(fill = mean_price_m2), color='black', size=0.2) +
      xlab("location") +
      ylab("average price per m2") +
      ggtitle("Average Price per m2 Based on Location") +
      theme_bw() +
      scale_fill_gradient2(
        # Color
        low = "blue", high = "red", midpoint = median(test$mean_price_m2)) +
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
        # Position vertical
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(
        # Legend
        legend.position = "none")
    g <- ggplotly(g, tooltip = c("text"))
  })
  
  output$cou <- renderPlotly({
    # BAR PLOT - count
    
    library(ggplot2)
    library(plotly)
    
    test <- data %>% group_by(rooms) %>% 
      summarise(
        count = n()
      )
    
    g <- ggplot(data=test, aes(reorder(rooms, count), count, 
                               text = paste0("Rooms: ", rooms, "\n",
                                             "Count: ", count, "\n"))) +
      geom_col(aes(fill = count), color='black', size=0.2) +
      xlab("rooms") +
      ylab("count") +
      ggtitle("Count of Properties") +
      theme_bw() +
      scale_fill_gradient2(
        # Color
        low = "blue", high = "red", midpoint = median(test$count)) +
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
    g <- ggplotly(g, tooltip = c("text"))
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
  
  output$link4 <- renderUI({
    tagList("To see other author's projects:", url)
  })
}
