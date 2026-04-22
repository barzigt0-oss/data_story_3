library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(DT)
getwd()
# The average home in the east south central region of the united states uses 481 therms annually
# The average home in the east south central region of the united states uses 13,900 kWh annually
# Data prep – utilities data
url <- 'https://ourworldindata.org/grapher/long-run-air-pollution.csv?v=1&csvType=full&useColumnShortNames=true'
air <- read_csv(url)
air %>% head
names(air) <- gsub('emissions__pollutant_','', names(air))
names(air) <- gsub('__sector_all_sectors','', names(air))
air <- air %>% mutate(total = nox + so2 + co + bc + nh3 + nmvoc)
air %>% head

###################################################################
###################################################################
# User Interface

ui <- fluidPage(
  titlePanel('The story air'),
  p('Here is a nice little tagline about my dashboard'),
  helpText('this is helpText, a little more subtle maybe?'),
  tabsetPanel(
    tabPanel(h5('Time series'),
             fluidRow(column(4, sliderInput(inputId = 'year',
                                            label = 'Select years',
                                            min = min(air$year),
                                            max = max(air$year),
                                            value = range(air$year))),
                      column(4, selectInput(inputId = 'entity',
                                            label = 'Select countries',
                                            multiple = TRUE,
                                            choices = unique(air$entity),
                                            selected = 'United Kingdom')),
                      radioButtons(inputId = 'rank',
                                   label = 'Rank countries ...',
                                   choices = c('Alphabetically', 'By total emissions'),
                                   selected = 'Alphabetically',
                                   inline = TRUE)),
             column(4, radioButtons(inputId = 'yvar',
                                    label = 'Select variable',
                                    choices = names(air)[-1:-3],
                                    selected = 'total',
                                    inline = TRUE))),
    br(),
    br(),
    fluidRow(column(1),
             column(10, plotOutput("airplot")),
             column(1))
  ),
  tabPanel(h5('Data viewer'),
           fluidRow(column(12, DTOutput('dt1'))))
)


###################################################################
###################################################################
# Server

server <- function(input, output) {

  rv <- reactiveValues()
  rv$air <- air

  observe({
    if(! is.null(input$entity)){
      rv$air <- air %>% filter(entity %in% input$entity)
    }else{
      rv$air <- air
    }
  })

  output$entity <- renderUI({

    (countries <- air %>% pull(entity) %>% unique %>% sort)

    if(input$rank == 'By total emissions'){
      air %>% head
      countries <- air %>%
        group_by(entity) %>%
        summarize(total = sum(total)) %>%
        arrange(desc(total)) %>%
        pull(entity)

    }

    selectInput(inputId = 'entity',
                label = 'select countries',
                choices = countries,
                selected = 'United Kingdom')

  })


  output$airplot <- renderPlot({
    print(input$entity)


    if(input$entity %>% is.null){
      ggplot(rv$air,
             aes_string(x='year', y=input$yvar, group='entity')) +
        geom_path() +
        xlim(input$year)
    }else{
      ggplot(rv$air,
             #air %>% filter(entity %in% input$entity)
             aes_string(x='year', y=input$yvar, colour='entity')) +
        geom_path() +
        xlim(input$year)
    }
  })

  output$dt1 <- renderDT({ rv$air })
}

# Run the application
shinyApp(ui = ui, server = server)


