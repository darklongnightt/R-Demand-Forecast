library(shiny)
library(zoo)
library(DT)
library(ggplot2)
library(DBI)
library(forecast)
library(datasets)
library(lubridate)
library(reshape2)
library(dplyr)
library(TSA)
library(tseries)
library(MASS)
library(scales)
library(prophet)
options(stringsAsFactors = FALSE)

sqlQuery <- function (query) {
  # Connection to db
  conn <- dbConnect(RMySQL::MySQL(), 
                    dbname="super_data", 
                    host="34.87.121.216", 
                    user="root", 
                    password="superdata123")
  # Close db connection after function call exits
  on.exit(dbDisconnect(conn))
  
  rs <- dbGetQuery(conn, query)
  return(rs)
}

shinyServer(function(input, output) {
  # connect to database
  orders <- sqlQuery("select PDTID, ORDERQTY, PCHASEDATE from orders")
  products <- sqlQuery("select PDTID, PDTNAME from product")
  
  # merge price with the order data
  df <- orders[!format(as_date(orders$PCHASEDATE), "%Y") == "-001",]
  df <- merge(df, products, by = "PDTID", all.x = TRUE)
  df$Product <- paste(df$PDTID, df$PDTNAME)
  
  # convert to monthly
  monthly_df <<- df %>%
    group_by(Time = floor_date(as_date(df$PCHASEDATE), "month"), Product) %>%
    summarize(Demand = n())

  #========================Selectize List ============================
  output$productSelector <- renderUI({
    selectizeInput(
      'select_product', "Search Product", choices = unique(monthly_df$Product),
      multiple = TRUE, options = list(maxItems = 1, maxOptions = 3, icon = c("glyphicon-search"))
    )
    
  })
  
  #=============================FB Prophet=============================
  output$prophet_plot <- renderPlot({
    if (!is.null(input$select_product)) {
      
      sub_df_prophet <- monthly_df %>%
        filter(Product == input$select_product)
      
      # convert to time series
      sub_df_prophet$Time <- as_date(sub_df_prophet$Time)
      df2_prophet <-
        data.frame(Time = seq(min(sub_df_prophet$Time), max(sub_df_prophet$Time), by = "month"))
      df1_prophet <- data.frame(sub_df_prophet[, c("Time", "Demand")])
      
      df_final_prophet <- dplyr::right_join(df1_prophet, df2_prophet)
      if (sum(is.na(df_final_prophet$Demand)) > 0) {
        df_final_prophet$Demand[is.na(df_final_prophet$Demand)] <- 0
      }
      
      df_prophet <- df_final_prophet[, c("Time", "Demand")]
      colnames(df_prophet) <-   c("ds", "y")
      df_prophet$ds <-
        as.character(df_prophet$ds)
      df_prophet <- data.frame(df_prophet)
      
      # fit prophet model
      m <- prophet(df_prophet, seasonality.mode = 'multiplicative')
      
      # make predictions
      forecast_data_prophet <-
        make_future_dataframe(m,
                              freq = "month",
                              periods  = as.numeric(input$month_forecast))
      fcst <- predict(m, forecast_data_prophet)
      
      # plot the model
      forecast_df_prophet <- data.frame(Time = fcst$ds, Demand = fcst$yhat)
      forecast_df_prophet <- tail(forecast_df_prophet, n = input$month_forecast)
      forecast_df_prophet$Time <- as_date(forecast_df_prophet$Time)
      render_df_prophet <- rbind(df_final_prophet, forecast_df_prophet)
      render_df_prophet$Time <- format(render_df_prophet$Time, format = "%b %Y")
      
      numeric_columns <- sapply(render_df_prophet, mode) == 'numeric'
      render_df_prophet[numeric_columns] <-  round(render_df_prophet[numeric_columns], 0)
      prophet_set <<- render_df_prophet

      ggplot() +
        geom_line(data = df_final_prophet, aes(x = Time, y = Demand)) +
        geom_line(data = forecast_df_prophet, aes(x = Time, y = Demand, color = "Prophet Forecast")) +
        xlab("Time") +
        ylab("Demands") +
        ggtitle("Product Sales") +
        guides(colour = guide_legend(title = "Monthly Demand Forecast")) +
        theme_classic() + theme(legend.position = "bottom")
      
    }
    
  })
  
  output$sales_table_prophet <- DT::renderDT({
    if (!is.null(input$select_product)) {
      prophet_set
    }
  }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
  #=============================HW Addictive=============================
  output$arima_plot <- renderPlot({
    if (!is.null(input$select_product)) {
      sub_df_arima <- monthly_df %>%
        filter(Product == input$select_product)
      
      # convert to time series
      sub_df_arima$Time <- as_date(sub_df_arima$Time)
      df2_arima <-
        data.frame(Time = seq(min(sub_df_arima$Time), max(sub_df_arima$Time), by = "month"))
      df1_arima <- data.frame(sub_df_arima[, c("Time", "Demand")])
      
      df_final_arima <- dplyr::right_join(df1_arima, df2_arima)
      if (sum(is.na(df_final_arima$Demand)) > 0) {
        df_final_arima$Demand[is.na(df_final_arima$Demand)] <- 0
      }
      
      tsData <-
        ts(
          df_final_arima$Demand,
          start = c(year(df_final_arima$Time[1]), month(df_final_arima$Time[1])),
          end =  c(year(df_final_arima$Time[length(df_final_arima$Time)]),
                   month(df_final_arima$Time[length(df_final_arima$Time)])),
          frequency = 12
        )
      t <- time(tsData)

      # fit model
      fit2_model_arima <-  auto.arima(tsData)
      
      # do forecast
      fit2_arima <-
        forecast(fit2_model_arima, h = as.numeric(input$month_forecast))
      forecast_data_arima <- fit2_arima
      
      # forecast table
      forecast_df_arima <- data.frame(forecast_data_arima)
      forecast_df_arima$Time <- rownames(forecast_df_arima)
      forecast_df_arima <- forecast_df_arima[, c("Time", "Point.Forecast")]
      
      colnames(forecast_df_arima)[colnames(forecast_df_arima)=="Point.Forecast"] <- "Demand"
      df_final_arima$Time <-  format(df_final_arima$Time, format = "%b %Y")
      render_df_arima <- rbind(df_final_arima, forecast_df_arima)
      rownames(render_df_arima) <- NULL
      
      numeric_columns <- sapply(render_df_arima, mode) == 'numeric'
      render_df_arima[numeric_columns] <-  round(render_df_arima[numeric_columns], 0)
      arima_set <<- render_df_arima
      
      # plot the forecast
      autoplot(tsData) +
        autolayer(fit2_arima, series = "ARIMA forecasts") +
        xlab("Time") +
        ylab("Demands") +
        ggtitle("Product Sales") +
        guides(colour = guide_legend(title = "Monthly Demand Forecast")) +
        theme_classic() + theme(legend.position = "bottom")
      
    }
  })
  
  output$sales_table_arima <- DT::renderDT({
    if (!is.null(input$select_product)) {
      arima_set
    }
  }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
  #=============================HW Addictive=============================
  output$hw_additive_plot <- renderPlot({
    if (!is.null(input$select_product)) {
      sub_df <- monthly_df %>%
        filter(Product == input$select_product)
      
      # convert to time series
      tsData <-
        ts(sub_df$Demand,
           start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
           frequency = 12)
      
      # fit model
      fit1_model <- hw(tsData, seasonal = "additive")
      
      # do forecasting
      fit1 <- forecast(fit1_model, h = input$month_forecast)
      forecast_data <- fit1
      
      # plot the forecast data
      originial_df <- data.frame(Time = as.Date(as.yearmon(time(tsData))), Demand = as.matrix(tsData))
      df <- data.frame(forecast_data)
      df$Time <- rownames(df)
      df <- df[, c("Time", "Point.Forecast")]
      df$Time <- paste(df$Time, "01")
      df$Time <- as.Date(df$Time, "%b %Y %d")
      colnames(df)[2] <- "Demand"
      render_df <- rbind(originial_df, df)
      render_df$Time <-  format(render_df$Time, format = "%b-%Y")
      rownames(render_df) <- NULL
      
      numeric_columns <- sapply(render_df, mode) == 'numeric'
      render_df[numeric_columns] <-  round(render_df[numeric_columns], 0)
      hwa_set <<- render_df
      
      # plot the model
      colMax <- max(render_df$Demand, na.rm = TRUE)+100
      colMin <- min(render_df$Demand, na.rm = TRUE)-100
      
      autoplot(tsData) +
        autolayer(fit1, series = "Holt-Winters Forecast (Additive Method)") +
        xlab("Time") +
        ylab("Demands") +
        ggtitle("Product Sales (Quantity)") +
        guides(colour = guide_legend(title = "Monthly Demand Forecast")) +
        theme_classic() + theme(legend.position = "bottom") + ylim(colMin, colMax)
    }
  })
  
  output$sales_table_hw_additive <- renderDataTable({
    if (!is.null(input$select_product)) {
      hwa_set
    }
    
  }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
  #=============================HW Multiplicative=============================
  output$hw_multiplcative_plot <- renderPlot({
    if (!is.null(input$select_product)) {
      
      sub_df <- monthly_df %>%
        filter(Product == input$select_product)
      
      # convert to time series
      tsData <-
        ts(sub_df$Demand,
           start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
           frequency = 12)
      
      # fit the model
      fit2_model <- hw(tsData, seasonal = "multiplicative")
      # do forecast
      fit2 <- forecast(fit2_model, h = input$month_forecast)
      forecast_data <- fit2 
      
      # forecast table
      originial_df <-
        data.frame(Time = as.Date(as.yearmon(time(tsData))),
                   Demand = as.matrix(tsData))
      df <- data.frame(forecast_data)
      df$Time <- rownames(df)
      df <- df[, c("Time", "Point.Forecast")]
      df$Time <- paste(df$Time, "01")
      df$Time <- as.Date(df$Time, "%b %Y %d")
      
      colnames(df)[2] <- "Demand"
      render_df <- rbind(originial_df, df)
      render_df$Time <-  format(render_df$Time, format = "%b-%Y")
      rownames(render_df) <- NULL
      
      numeric_columns <- sapply(render_df, mode) == 'numeric'
      render_df[numeric_columns] <-  round(render_df[numeric_columns], 0)
      hwm_set <<- render_df
      
      # plot the forecast
      colMax <- max(render_df$Demand, na.rm = TRUE)+100
      colMin <- min(render_df$Demand, na.rm = TRUE)-100
      
      autoplot(tsData) +
        autolayer(fit2, series = "Holt-Winters Forecast (Multiplicative Method)") +
        xlab("Time") +
        ylab("Demands") +
        ggtitle("Product Sales (Quantity)") +
        guides(colour = guide_legend(title = "Monthly Demand Forecast")) +
        theme_classic() + theme(legend.position = "bottom") + ylim(colMin, colMax)
      
    }
  })
  
  output$sales_table_hw_multiplcative <- renderDataTable({
    if (!is.null(input$select_product)) {
      hwm_set
    }
  }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
  #=============================HW Damped=============================
  output$hw_dumped_plot <-
    renderPlot({
      if (!is.null(input$select_product)) {
        sub_df <- monthly_df %>%
          filter(Product == input$select_product)
        
        # convert to time series
        tsData <-
          ts(sub_df$Demand,
             start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
             frequency = 12)
        # fit model
        fc_model <- hw(tsData, damped = TRUE, seasonal = "multiplicative")
        
        # do forecasting
        fc <- forecast(fc_model, h = input$month_forecast)
        forecast_data <- fc
      
        # forecast table
        originial_df <-
          data.frame(Time = as.Date(as.yearmon(time(tsData))),
                     Demand = as.matrix(tsData))
        df <- data.frame(forecast_data)
        df$Time <- rownames(df)
        df <- df[, c("Time", "Point.Forecast")]
        df$Time <- paste(df$Time, "01")
        df$Time <- as.Date(df$Time, "%b %Y %d")
        colnames(df)[2] <- "Demand"
        render_df <- rbind(originial_df, df)
        render_df$Time <-  format(render_df$Time, format = "%b-%Y")
        rownames(render_df) <- NULL
        
        numeric_columns <- sapply(render_df, mode) == 'numeric'
        render_df[numeric_columns] <-  round(render_df[numeric_columns], 0)
        hwd_set <<- render_df
        
        colMax <- max(render_df$Demand, na.rm = TRUE)+100
        colMin <- min(render_df$Demand, na.rm = TRUE)-100

        # plot forecasting
        autoplot(tsData) +
          autolayer(fc, series = "Holt-Winters Forecast (Damped Multiplicative Method)") +
          guides(colour = guide_legend(title = "Monthly Demand Forecast")) +
          theme_classic() + theme(legend.position = "bottom") + xlab("Time") +
          ggtitle("Product Sales (Quantity)") +
          ylab("Demands") + ylim(colMin, colMax)
      
      }
    })  
  
  output$sales_table_hw_dumped <- renderDataTable({
    if (!is.null(input$select_product)) {
      hwd_set
    }
  }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
  # =================== Export Single Product Demand =====================
  observeEvent(input$exportButton, {
    # Check which tab is currently active
    method <- input$tab
    months_predicted <- input$month_forecast
    productid <- strsplit(input$select_product, ' ')[[1]][1]
    
    if (input$tab == "FB Prophet Automatic Forecast") {
      currentTable <- prophet_set
      
    } else if (input$tab == "Holt-Winters Forecast (Additive)") {
      currentTable <- hwa_set
      
    } else if (input$tab == "Holt-Winters Forecast (Multiplicative)"){
      currentTable <- hwm_set
      
    } else if (input$tab == "ARIMA Forecast"){
      currentTable <- arima_set
      
    } else {
      currentTable <- hwd_set
    }
    
    print(method)
    
    # Format data table timesteps and demand columns for exporting to db
    timestep <- ""
    demand <- ""
    
    withProgress(message = 'Exporting: ', value = 0, {
      n <- nrow(currentTable)
      for (row in 1:n) {
        # Increment the progress bar
        incProgress(1/n, detail = paste(row, "/", n))
        
        timestep <- paste(timestep, currentTable[row, "Time"], sep="|")
        demand <- paste(demand, currentTable[row, "Demand"], sep="|")
      }
    })
    
    # Delete any duplicate entries first
    query <- "DELETE FROM demand_forecast WHERE PDTID="
    query <- paste(query, productid, sep="'")
    query <- paste(query, "';", sep="")
    sqlQuery(query)
  
    # Prepare sql statement for insert db
    sql <- "INSERT INTO demand_forecast(PDTID, METHOD, MONTHS_PREDICTED, TIMESTEP, DEMAND) VALUES("
    sql <- paste(sql, productid, sep="'")
    sql <- paste(sql, method, sep="', '")
    sql <- paste(sql, months_predicted, sep="', '")
    sql <- paste(sql, timestep, sep="', '")
    sql <- paste(sql, demand, sep="', '")
    sql <- paste(sql, "');", sep="")
    print(sql)
    sqlQuery(sql)
    
    showModal(modalDialog(
      title = input$tab,
      paste0("Successfully exported product demand report to the database!"),
      easyClose = TRUE,
      footer = NULL
    ))
    
    
    
  })
  
  # =================== Export ALL product demands to the database =====================
  observeEvent(input$exportAll, {
    # Check which tab is currently active
    method <- input$tab
    months_predicted <- input$month_forecast
    print(method)
    
    # Connection to db
    conn <- dbConnect(RMySQL::MySQL(), 
                      dbname="super_data", 
                      host="34.87.121.216", 
                      user="root", 
                      password="superdata123")
    
    # Close db connection after function call exits
    on.exit(dbDisconnect(conn))
    
    # Truncate the table first
    query <- "TRUNCATE TABLE demand_forecast;"
    dbGetQuery(conn, query)
    
    # Get all unique products
    products <- data.frame(unique(monthly_df$Product))
    
    withProgress(message = 'Exporting: ', value = 0, {
    n <- nrow(products)
    for (row in 1:n) {
      
      product <- products[row, 'unique.monthly_df.Product.']
      productid <- strsplit(product, ' ')[[1]][1]
      print(productid)
      
      # Increment the progress bar
      incProgress(1/n, detail = paste(row, "/", n))
      
      # Get table from respect model depending on active tab
      tryCatch({
        if (method == "FB Prophet Automatic Forecast") {
          table <- getProphet(product)
          
        } else if (method == "Holt-Winters Forecast (Additive)") {
          table <- getHWA(product)
          
        } else if (method == "Holt-Winters Forecast (Multiplicative)"){
          table <- getHWM(product)
          
        } else if (input$tab == "ARIMA Forecast"){
          table <- getARIMA(product)
          
        } else {
          table <- getHWD(product)
        }
        
        # Format data table timesteps and demand columns for each product
        timestep <- ""
        demand <- ""
        
        for (row in 1:nrow(table)) {
          timestep <- paste(timestep, table[row, "Time"], sep="|")
          demand <- paste(demand, table[row, "Demand"], sep="|")
        }
        
        # Prepare sql statement for insert db
        sql <- "INSERT INTO demand_forecast(PDTID, METHOD, MONTHS_PREDICTED, TIMESTEP, DEMAND) VALUES("
        sql <- paste(sql, productid, sep="'")
        sql <- paste(sql, method, sep="', '")
        sql <- paste(sql, months_predicted, sep="', '")
        sql <- paste(sql, timestep, sep="', '")
        sql <- paste(sql, demand, sep="', '")
        sql <- paste(sql, "');", sep="")
        
        dbGetQuery(conn, sql)

      }, error=function(e){cat("ERROR : Insufficient observations for product: ", product, "\n")})
      
    }
    
    })
    
    showModal(modalDialog(
      title = input$tab,
      paste0("Successfully exported all product demands to the database!"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  
  #===========Respective Functions To Return Forecast from Respective Models ==============
  
  getProphet <- function(product) {
    print(product)
    
    # Filter out a product and convert to time series
    sub_df <- monthly_df %>%
      filter(Product == product)
    
    tsData <-
      ts(sub_df$Demand,
         start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
         frequency = 12)
    df <- sub_df[, c("Time", "Demand")]
    colnames(df) <-   c("ds", "y")
    df$ds <-
      as.character(as.Date(sub_df$Time, format = "%Y-%m-%d"))
    df <- data.frame(df)
    
    # fit prophet model
    m <- prophet(df, seasonality.mode = 'multiplicative')
    
    # make predictions
    forecast_data <-
      make_future_dataframe(m,
                            freq = "month",
                            periods  = as.numeric(input$month_forecast))
    fcst <- predict(m, forecast_data)
    
    # plot the model table
    originial_df <- sub_df
    df <- data.frame(Time = fcst$ds, Demand = fcst$yhat)
    df <- df[(nrow(df) - as.numeric(input$month_forecast)):nrow(df),]
    
    originial_df <- data.frame(originial_df)
    
    originial_df <- originial_df[,c("Time","Demand")]
    render_df <- rbind(originial_df, df)
    render_df$Time <-  format(render_df$Time, format = "%b-%Y")
    rownames(render_df) <- NULL
    
    render_df$Demand <- format(round(render_df$Demand, 0), nsmall = 0)
    return(render_df)
  }
  
  getHWA <- function(product) {
    sub_df <- monthly_df %>%
      filter(Product == product)
    
    # convert to time series
    tsData <-
      ts(sub_df$Demand,
         start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
         frequency = 12)
    
    # fit model
    fit1_model <- hw(tsData, seasonal = "additive")
    
    # do forecasting
    fit1 <- forecast(fit1_model, h = input$month_forecast)
    forecast_data <- fit1
    
    # plot the forecast data
    originial_df <- data.frame(Time = as.Date(as.yearmon(time(tsData))), Demand = as.matrix(tsData))
    df <- data.frame(forecast_data)
    df$Time <- rownames(df)
    df <- df[, c("Time", "Point.Forecast")]
    df$Time <- paste(df$Time, "01")
    df$Time <- as.Date(df$Time, "%b %Y %d")
    colnames(df)[2] <- "Demand"
    render_df <- rbind(originial_df, df)
    render_df$Time <-  format(render_df$Time, format = "%b-%Y")
    rownames(render_df) <- NULL
    
    render_df$Demand <- format(round(render_df$Demand, 0), nsmall = 0)
    return(render_df)
  }
  
  getHWM <- function(product) {
    sub_df <- monthly_df %>%
      filter(Product == product)
    
    # convert to time series
    tsData <-
      ts(sub_df$Demand,
         start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
         frequency = 12)
    
    # fit the model
    fit2_model <- hw(tsData, seasonal = "multiplicative")
    # do forecast
    fit2 <- forecast(fit2_model, h = input$month_forecast)
    forecast_data <- fit2 
    
    # forecast table
    originial_df <-
      data.frame(Time = as.Date(as.yearmon(time(tsData))),
                 Demand = as.matrix(tsData))
    df <- data.frame(forecast_data)
    df$Time <- rownames(df)
    df <- df[, c("Time", "Point.Forecast")]
    df$Time <- paste(df$Time, "01")
    df$Time <- as.Date(df$Time, "%b %Y %d")
    
    colnames(df)[2] <- "Demand"
    render_df <- rbind(originial_df, df)
    render_df$Time <-  format(render_df$Time, format = "%b-%Y")
    rownames(render_df) <- NULL
    
    render_df$Demand <- format(round(render_df$Demand, 0), nsmall = 0)
    return (render_df)
  }
  
  getHWD <- function(product) {
    sub_df <- monthly_df %>%
      filter(Product == product)
    
    # convert to time series
    tsData <-
      ts(sub_df$Demand,
         start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
         frequency = 12)
    # fit model
    fc_model <- hw(tsData, damped = TRUE, seasonal = "multiplicative")
    
    # do forecasting
    fc <- forecast(fc_model, h = input$month_forecast)
    forecast_data <- fc
    
    # forecast table
    originial_df <-
      data.frame(Time = as.Date(as.yearmon(time(tsData))),
                 Demand = as.matrix(tsData))
    df <- data.frame(forecast_data)
    df$Time <- rownames(df)
    df <- df[, c("Time", "Point.Forecast")]
    df$Time <- paste(df$Time, "01")
    df$Time <- as.Date(df$Time, "%b %Y %d")
    colnames(df)[2] <- "Demand"
    render_df <- rbind(originial_df, df)
    render_df$Time <-  format(render_df$Time, format = "%b-%Y")
    rownames(render_df) <- NULL
    
    render_df$Demand <- format(round(render_df$Demand, 0), nsmall = 0)
    return (render_df)
  }
  
  getHWM <- function(product) {
    sub_df <- monthly_df %>%
      filter(Product == product)
    
    # convert to time series
    tsData <-
      ts(sub_df$Demand,
         start = c(year(sub_df$Time[1]), month(sub_df$Time[1])),
         frequency = 12)
    
    # fit the model
    fit2_model <- hw(tsData, seasonal = "multiplicative")
    # do forecast
    fit2 <- forecast(fit2_model, h = input$month_forecast)
    forecast_data <- fit2 
    
    # forecast table
    originial_df <-
      data.frame(Time = as.Date(as.yearmon(time(tsData))),
                 Demand = as.matrix(tsData))
    df <- data.frame(forecast_data)
    df$Time <- rownames(df)
    df <- df[, c("Time", "Point.Forecast")]
    df$Time <- paste(df$Time, "01")
    df$Time <- as.Date(df$Time, "%b %Y %d")
    
    colnames(df)[2] <- "Demand"
    render_df <- rbind(originial_df, df)
    render_df$Time <-  format(render_df$Time, format = "%b-%Y")
    rownames(render_df) <- NULL
    
    render_df$Demand <- format(round(render_df$Demand, 0), nsmall = 0)
    return (render_df)
  }
  
  getARIMA <- function(product) {
    sub_df_arima <- monthly_df %>%
      filter(Product == product)
    
    # convert to time series
    sub_df_arima$Time <- as_date(sub_df_arima$Time)
    df2_arima <-
      data.frame(Time = seq(min(sub_df_arima$Time), max(sub_df_arima$Time), by = "month"))
    df1_arima <- data.frame(sub_df_arima[, c("Time", "Demand")])
    
    df_final_arima <- dplyr::right_join(df1_arima, df2_arima)
    if (sum(is.na(df_final_arima$Demand)) > 0) {
      df_final_arima$Demand[is.na(df_final_arima$Demand)] <- 0
    }
    
    tsData <-
      ts(
        df_final_arima$Demand,
        start = c(year(df_final_arima$Time[1]), month(df_final_arima$Time[1])),
        end =  c(year(df_final_arima$Time[length(df_final_arima$Time)]),
                 month(df_final_arima$Time[length(df_final_arima$Time)])),
        frequency = 12
      )
    t <- time(tsData)
    
    # fit model
    fit2_model_arima <-  auto.arima(tsData)
    
    # do forecast
    fit2_arima <-
      forecast(fit2_model_arima, h = as.numeric(input$month_forecast))
    forecast_data_arima <- fit2_arima
    
    # forecast table
    forecast_df_arima <- data.frame(forecast_data_arima)
    forecast_df_arima$Time <- rownames(forecast_df_arima)
    forecast_df_arima <- forecast_df_arima[, c("Time", "Point.Forecast")]
    
    colnames(forecast_df_arima)[colnames(forecast_df_arima)=="Point.Forecast"] <- "Demand"
    df_final_arima$Time <-  format(df_final_arima$Time, format = "%b %Y")
    render_df_arima <- rbind(df_final_arima, forecast_df_arima)
    rownames(render_df_arima) <- NULL
    
    numeric_columns <- sapply(render_df_arima, mode) == 'numeric'
    render_df_arima[numeric_columns] <-  round(render_df_arima[numeric_columns], 0)
    return (render_df_arima)
  }
  
})