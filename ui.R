library(shiny)
library(DT)

navbarPage(
  "Covidanalyse",
  tabPanel("Daten", fluidPage(
    titlePanel("Daten"),
    fluidRow(
      column(
        4,
        selectInput(
          "select_land1",
          "Land/Kontinent 1",
          c("United States"),
          selected = "United States"
        )
      ),
      column(4,
             selectInput("select_land2", "Land/Kontinent 2", c())),
      column(4,
             selectInput("select_land3", "Land/Kontinent 3", c()))
    ),
    tabsetPanel(tabPanel("Gesamt", DTOutput("table")),
                tabPanel("Welt", DTOutput("table_welt")))
    
    
  )),
  tabPanel("Land", fluidPage(
    titlePanel("Land"),
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("daterange", "Zeitraum"),
        actionButton("reset_button", "Reset"),
        checkboxInput("checkboxland1", "Land/Kontinent 1", value = TRUE),
        checkboxInput("checkboxland2", "Land/Kontinent 2", value = FALSE),
        checkboxInput("checkboxland3", "Land/Kontinent 3", value = FALSE),
        selectInput(
          "select_ma",
          "Moving Average",
          c("Keiner", "5 Tage", "7 Tage", "20 Tage")
        ),
        checkboxInput("checkMA", "Nur Moving Average", value =
                        FALSE),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Neuinfektionen",
            plotOutput("plotNew"),
            fluidRow(
              column(
                3,
                h4("Mittelwert"),
                textOutput("text_mean1"),
                textOutput("text_mean2"),
                textOutput("text_mean3")
              ),
              
              column(
                3,
                h4("Median"),
                textOutput("text_median1"),
                textOutput("text_median2"),
                textOutput("text_median3")
              ),
              
              column(
                3,
                h4("Varianz"),
                textOutput("text_variance1"),
                textOutput("text_variance2"),
                textOutput("text_variance3")
              ),
              
              column(
                3,
                h4("Standardabweichung"),
                textOutput("text_stdev1"),
                textOutput("text_stdev2"),
                textOutput("text_stdev3")
              )
            ),
            fluidRow(plotOutput("plotReproduction"))
          ),
          tabPanel("Einschränkungen",
                   fluidRow(
                     plotOutput("plotEin1"),
                     plotOutput("plotEin2")
                   )),
          tabPanel("Gesamte Infektionen",
                   fluidRow(
                     plotOutput("plotGes1"),
                     plotOutput("plotGes2")
                   )),
          tabPanel(
            "Tests",
            fluidRow(plotOutput("plotTest1")),
            fluidRow(plotOutput("plotTest2")),
            fluidRow(plotOutput("plotTest3"))
          ),
          tabPanel(
            "Hospitalisierung/ ICU",
            fluidRow(
              plotOutput("plotHospICU1"),
              plotOutput("plotHospICU2"),
              plotOutput("plotHospICU3"),
              plotOutput("plotHospICU4"),
            )
          ),
          tabPanel("Impfungen", fluidRow(
            plotOutput("plotImpf1"),
            plotOutput("plotImpf2")
          )),
          tabPanel(
            "Sterblichkeit",
            fluidRow(plotOutput("plotSterblichkeit1")),
            fluidRow(plotOutput("plotSterblichkeit2"))
          )
        )
        
      )
      
    )
  )),
  tabPanel(
    "Wachstumsprognose",
    fluidPage(
      titlePanel("Wachtumsprognose (Land 1)"),
      fluidRow(
        column(4,
               selectInput(
                 "select_prognose",
                 "Modell",
                 c("Linear", "Exponentiell (Wachstumsfaktoren)")
               )),
        column(
          4,
          selectizeInput(
            "selectize_prognosedauer",
            "Prognose",
            choices = c("10 Tage", "100 Tage", "1000 Tage", "Bis Herdenimmunitaet")
          )
        ),
        column(
          4,
          numericInput(
            "numeric_herdenimmunitaet",
            "Herdenimmunität (ab % der Bevölkerung)",
            min = 0,
            step = 1,
            value = 60
          )
        )
      ),
      fluidRow(plotOutput("plotWachstumsfaktor")),
      fluidRow(plotOutput("plotPrognose"))
    )
  ),
  tabPanel("Welt", fluidPage(
    titlePanel("Welt"),
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("daterange_welt", "Zeitraum"),
        actionButton("reset_button_welt", "Reset"),
        selectInput(
          "select_welt_plot1",
          "Plot 1",
          choices = c(
            "Neuinfektionen",
            "Gesamte Infektionen",
            "Neue Todesfaelle",
            "Gesamte Todesfaelle",
            "Reproduktionszahl",
            "Impfungen"
          ),
          selected = "Neuinfektionen"
        ),
        selectInput(
          "select_welt_plot2",
          "Plot 2",
          choices = c(
            "Neuinfektionen",
            "Gesamte Infektionen",
            "Neue Todesfaelle",
            "Gesamte Todesfaelle",
            "Reproduktionszahl",
            "Impfungen"
          ),
          selected = "Gesamte Infektionen"
        ),
        selectInput(
          "select_welt_ma",
          "Moving Average",
          c("Keiner", "5 Tage", "7 Tage", "20 Tage")
        ),
        checkboxInput("checkMA_welt", "Nur Moving Average", value =
                        FALSE)
      ),
      mainPanel(fluidRow(plotOutput("plotWelt1")),
                fluidRow(plotOutput("plotWelt2")))
    )
  ))
  
)
