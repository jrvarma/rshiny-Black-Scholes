library(shiny)

fluidPage(
    titlePanel("Generalized Black Scholes App: Value and Greeks for Call and Put Options"),
    sidebarLayout(
        sidebarPanel(
            numericInput('S', 'Price of Underlying', 100),
            numericInput('q', 'Dividend Yield', 0),            
            numericInput('X', 'Strike Price', 100),
            numericInput('r', 'Risk free rate (%)', 0),
            selectInput('Freq', 'Compounding Frequency',
                        list('Annual' = 1,
                             'Semi-Annual' = 2,
                             'Quarterly' = 4,
                             'Monthly' = 12,
                             'Continuous' = Inf)),            
#            numericInput('VorP', 'Volatility of Underlying (%)', 20),
            numericInput('t', 'Maturity of Option', 1),
            sliderInput("sigma", "Volatility (%)", value = 20,
                        min = 2, max = 60),
            sliderInput("t", "Time to Maturity (Years)", value = 1,
                        min = 0, max = 5, step = 0.1),            
            radioButtons('Implied', NULL,
                         list('Find Option Price' = FALSE,
                              'Find Implied Volatility' = TRUE),
                         selected = FALSE, inline = TRUE),
            br(),
            radioButtons("greek", "Select Greek to Plot",
                   c("Delta" = "Delta",
                     "Gamma" = "Gamma",
                     "Rho" = "Rho",
                     "Theta" = "Theta",
                     "Vega" = "Vega"))
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Black Scholes Values",
                                 h3('Option Values for selected Inputs'), tableOutput("gbsout"), hr(),
                                 h3('Plot of Call and Put Options'),
                                 plotOutput("callPlot"), br(), 
                                 plotOutput("putPlot")),
                        tabPanel("Black Scholes Greeks",
                                 h3('Option Greeks for selected Inputs'), tableOutput("greekout"), hr(),
                                 h3('Plot of selected Greek'), 
                                 plotOutput("plot"), br(),  
                                 h5("Note: Gamma and Vega are same for both Call and Put options (because of Put-Call parity)")
                                 )
                        )
        )
    ),
    title ="Generalized Black Scholes"
)
