library(shiny)

shinyUI(fluidPage(

    sidebarLayout(
        sidebarPanel(
            numericInput('S', 'Price of Underlying', 100),
            numericInput('X', 'Strike Price', 100),
            numericInput('r', 'Risk free rate (%)', 0),
            numericInput('VorP', 'Volatility of Underlying (%)', 20),
            numericInput('t', 'Maturity of Option', 1),
            numericInput('q', 'Dividend Yield', 0),
            selectInput('Freq', 'Compounding Frequency',
                        list('Annual' = 1,
                             'Semi-Annual' = 2,
                             'Quarterly' = 4,
                             'Monthly' = 12,
                             'Continuous' = Inf)),
            radioButtons('Implied', NULL,
                         list('Find Option Price' = FALSE,
                              'Find Implied Volatility' = TRUE),
                         selected = FALSE, inline = TRUE)          
            ) ,
        
        mainPanel(
            h1('Black Scholes Values and Greeks'),
            tableOutput("gbsout")
            )
        ),
    title ="Generalized Black Scholes"
    ))
