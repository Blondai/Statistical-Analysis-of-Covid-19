library(shiny)
library(DT)
library(ggplot2)
library(tidyquant)

source("functions.R")


# Daten vorbereiten
rawdata = read.csv('COVID19.csv', header = TRUE)
rawdata$date = as.Date(rawdata$date)
rawdata$new_cases <-
  replace(rawdata$new_cases, is.na(rawdata$new_cases), 0)

world_data = read.csv('Welt.csv')
world_data$date = as.Date(world_data$date)


shinyServer(function(input, output) {
  # Startwerte für Länder
  unique_location <- unique(rawdata$location)
  updateSelectInput(inputId = "select_land1", choices = unique_location)
  updateSelectInput(inputId = "select_land1", selected = "United States")
  updateSelectInput(inputId = "select_land2", choices = unique_location)
  updateSelectInput(inputId = "select_land2", selected = "Germany")
  updateSelectInput(inputId = "select_land3", choices = unique_location)
  updateSelectInput(inputId = "select_land3", selected = "France")
  
  default_start = min(rawdata$date)
  default_end = max(rawdata$date)
  
  # Zeitraum zu Beginn passend einstellen
  updateDateRangeInput(inputId = "daterange",
                       start = default_start,
                       end = default_end)
  updateDateRangeInput(inputId = "daterange_welt",
                       start = default_start,
                       end = default_end)
  
  # Text neben Checkbox anpassen wenn Land ausgewählt wird
  observeEvent(input$select_land1, {
    updateCheckboxInput(inputId = "checkboxland1",
                        label = paste(input$select_land1))
  })
  observeEvent(input$select_land2, {
    updateCheckboxInput(inputId = "checkboxland2",
                        label = paste(input$select_land2))
  })
  observeEvent(input$select_land3, {
    updateCheckboxInput(inputId = "checkboxland3",
                        label = paste(input$select_land3))
  })
  
  location_data_reactive <- reactive({
    countries <- c()
    if (input$checkboxland1) {
      countries <- append(countries, input$select_land1)
    }
    if (input$checkboxland2) {
      countries <- append(countries, input$select_land2)
    }
    if (input$checkboxland3) {
      countries <- append(countries, input$select_land3)
    }
    # Filter nach Ländern und Zeitraum
    new_data = subset(rawdata, location == countries)
    new_data = subset(new_data, date >= input$daterange[1])
    new_data = subset(new_data, date <= input$daterange[2])
    
    # Sortiere Daten in ausgewählter Reihenfolge
    new_data$location <-
      factor(new_data$location, levels = countries)
    new_data <- new_data[order(new_data$location),]
    new_data
  })
  
  world_data_reactive <- reactive({
    new_data = world_data
    new_data = subset(new_data, date >= input$daterange_welt[1])
    new_data = subset(new_data, date <= input$daterange_welt[2])
    new_data
  })
  
  
  observeEvent(input$reset_button, {
    updateDateRangeInput(inputId = "daterange",
                         start = default_start,
                         end = default_end)
  })
  observeEvent(input$reset_button_welt, {
    updateDateRangeInput(inputId = "daterange_welt",
                         start = default_start,
                         end = default_end)
  })
  
  # Zeige Mittelwert, Median, Varianz und Standardabweichung
  output$text_mean1 <-
    renderText(mean(
      subset(location_data_reactive(), location == input$select_land1)$new_cases,
      na.rm = TRUE
    ))
  output$text_mean2 <-
    renderText(mean(
      subset(location_data_reactive(), location == input$select_land2)$new_cases,
      na.rm = TRUE
    ))
  output$text_mean3 <-
    renderText(mean(
      subset(location_data_reactive(), location == input$select_land3)$new_cases,
      na.rm = TRUE
    ))
  
  output$text_median1 <-
    renderText(median(
      subset(location_data_reactive(), location == input$select_land1)$new_cases,
      na.rm = TRUE
    ))
  output$text_median2 <-
    renderText(median(
      subset(location_data_reactive(), location == input$select_land2)$new_cases,
      na.rm = TRUE
    ))
  output$text_median3 <-
    renderText(median(
      subset(location_data_reactive(), location == input$select_land3)$new_cases,
      na.rm = TRUE
    ))
  
  output$text_variance1 <-
    renderText(var(
      subset(location_data_reactive(), location == input$select_land1)$new_cases,
      na.rm = TRUE
    ))
  output$text_variance2 <-
    renderText(var(
      subset(location_data_reactive(), location == input$select_land2)$new_cases,
      na.rm = TRUE
    ))
  output$text_variance3 <-
    renderText(var(
      subset(location_data_reactive(), location == input$select_land3)$new_cases,
      na.rm = TRUE
    ))
  
  output$text_stdev1 <-
    renderText(sd(
      subset(location_data_reactive(), location == input$select_land1)$new_cases,
      na.rm = TRUE
    ))
  output$text_stdev2 <-
    renderText(sd(
      subset(location_data_reactive(), location == input$select_land2)$new_cases,
      na.rm = TRUE
    ))
  output$text_stdev3 <-
    renderText(sd(
      subset(location_data_reactive(), location == input$select_land3)$new_cases,
      na.rm = TRUE
    ))
  
  select_ma <- reactive({
    n <- 0
    if (input$select_ma == "5 Tage") {
      n <- 5
    }
    if (input$select_ma == "7 Tage") {
      n <- 7
    }
    if (input$select_ma == "20 Tage") {
      n <- 20
    }
    n
  })
  
  
  # Render Plots Land
  output$plotNew <-
    renderPlot(plot_new_cases(location_data_reactive(), select_ma(), input$checkMA))
  output$plotReproduction <-
    renderPlot(plot_reproduction(location_data_reactive(), select_ma(), input$checkMA))
  
  output$plotEin1 <-
    renderPlot(plot_stringency(location_data_reactive(), select_ma(), input$checkMA))
  output$plotEin2 <-
    renderPlot(plot_new_cases(location_data_reactive(), select_ma(), input$checkMA))
  
  output$plotGes1 <-
    renderPlot(plot_total_cases(location_data_reactive(), select_ma(), input$checkMA))
  output$plotGes2 <-
    renderPlot(plot_total_death(location_data_reactive(), select_ma(), input$checkMA))
  
  output$plotTest1 <-
    renderPlot(plot_test_positiverate(location_data_reactive(), select_ma(), input$checkMA))
  output$plotTest2 <-
    renderPlot(plot_new_tests(location_data_reactive(), select_ma(), input$checkMA))
  output$plotTest3 <-
    renderPlot(plot_new_cases(location_data_reactive(), select_ma(), input$checkMA))
  
  
  output$plotHospICU1 <-
    renderPlot(plot_hosp(location_data_reactive(), select_ma(), input$checkMA))
  output$plotHospICU2 <-
    renderPlot(plot_bettenbelegung(location_data_reactive(), select_ma(), input$checkMA))
  output$plotHospICU3 <-
    renderPlot(plot_ICU(location_data_reactive(), select_ma(), input$checkMA))
  output$plotHospICU4 <-
    renderPlot(plot_new_cases(location_data_reactive(), select_ma(), input$checkMA))
  
  output$plotImpf1 <-
    renderPlot(plot_vac(location_data_reactive(), select_ma(), input$checkMA))
  output$plotImpf2 <-
    renderPlot(plot_new_cases(location_data_reactive(), select_ma(), input$checkMA))
  
  output$plotSterblichkeit1 <-
    renderPlot(plot_sterblichkeit(location_data_reactive(), select_ma(), input$checkMA))
  output$plotSterblichkeit2 <-
    renderPlot(plot_new_cases(location_data_reactive(), select_ma(), input$checkMA))
  
  # Render Plots Welt
  
  
  # Plot in reactive zurückgeben?
  observeEvent(
    c(
      input$select_welt_plot1,
      input$select_welt_ma,
      input$checkMA_welt,
      input$daterange_welt
    ),
    {
      p <- ggplot()
      if (input$select_welt_plot1 == "Neuinfektionen") {
        p <-
          plot_world_newcases(world_data_reactive(),
                              input$select_welt_ma,
                              input$checkMA_welt)
      }
      if (input$select_welt_plot1 == "Gesamte Infektionen") {
        p <-
          plot_world_totalcases(world_data_reactive(),
                                input$select_welt_ma,
                                input$checkMA_welt)
      }
      if (input$select_welt_plot1 == "Neue Todesfaelle") {
        p <-
          plot_world_newdeaths(world_data_reactive(),
                               input$select_welt_ma,
                               input$checkMA_welt)
      }
      if (input$select_welt_plot1 == "Gesamte Todesfaelle") {
        p <-
          plot_world_totaldeaths(world_data_reactive(),
                                 input$select_welt_ma,
                                 input$checkMA_welt)
      }
      if (input$select_welt_plot1 == "Reproduktionszahl") {
        p <-
          plot_world_r(world_data_reactive(),
                       input$select_welt_ma,
                       input$checkMA_welt)
      }
      if (input$select_welt_plot1 == "Impfungen") {
        p <-
          plot_world_vac(world_data_reactive(),
                         input$select_welt_ma,
                         input$checkMA_welt)
      }
      output$plotWelt1 <- renderPlot(p)
    }
  )
  
  observeEvent(
    c(
      input$select_welt_plot2,
      input$select_welt_ma,
      input$checkMA_welt,
      input$daterange_welt
    ),
    {
      p <- ggplot()
      if (input$select_welt_plot2 == "Neuinfektionen") {
        p <-
          plot_world_newcases(world_data_reactive(),
                              input$select_welt_ma,
                              input$checkMA_welt)
      }
      if (input$select_welt_plot2 == "Gesamte Infektionen") {
        p <-
          plot_world_totalcases(world_data_reactive(),
                                input$select_welt_ma,
                                input$checkMA_welt)
      }
      if (input$select_welt_plot2 == "Neue Todesfaelle") {
        p <-
          plot_world_newdeaths(world_data_reactive(),
                               input$select_welt_ma,
                               input$checkMA_welt)
      }
      if (input$select_welt_plot2 == "Gesamte Todesfaelle") {
        p <-
          plot_world_totaldeaths(world_data_reactive(),
                                 input$select_welt_ma,
                                 input$checkMA_welt)
      }
      if (input$select_welt_plot2 == "Reproduktionszahl") {
        p <-
          plot_world_r(world_data_reactive(),
                       input$select_welt_ma,
                       input$checkMA_welt)
      }
      if (input$select_welt_plot2 == "Impfungen") {
        p <-
          plot_world_vac(world_data_reactive(),
                         input$select_welt_ma,
                         input$checkMA_welt)
      }
      output$plotWelt2 <- renderPlot(p)
    }
  )
  
  
  observeEvent(
    c(
      input$select_prognose,
      input$selectize_prognosedauer,
      input$numeric_herdenimmunitaet
    ),
    {
      p1 <- ggplot() # Wachstumsfaktoren
      p2 <- ggplot() # Prognose
      if (input$select_prognose == "Linear") {
        p1 <-
          wachstumsfaktor_linear(subset(rawdata, location == input$select_land1))
        p2 <-
          prognose_linear(
            subset(rawdata, location == input$select_land1),
            input$selectize_prognosedauer,
            input$numeric_herdenimmunitaet
          )
      }
      if (input$select_prognose == "Exponentiell (Wachstumsfaktoren)") {
        p1 <-
          wachstumsfaktor_exp(subset(rawdata, location == input$select_land1))
        p2 <-
          prognose_exp(
            subset(rawdata, location == input$select_land1),
            input$selectize_prognosedauer,
            input$numeric_herdenimmunitaet
          )
      }
      
      output$plotWachstumsfaktor <- renderPlot(p1)
      output$plotPrognose <- renderPlot(p2)
    }
  )
  
  
  # Tabellen
  
  output$table <-
    renderDT(
      rawdata,
      options = list(
        autoWidth = TRUE,
        pagelength = 15,
        lengthMenu = c(10, 15, 20, 25),
        searchHighlight = TRUE
      ),
      filter = 'top'
    )
  output$table_welt <-
    renderDT(
      world_data,
      options = list(
        autoWidth = TRUE,
        pagelength = 15,
        lengthMenu = c(10, 15, 20, 25),
        searchHighlight = TRUE
      ),
      filter = 'top'
    )
  
})
