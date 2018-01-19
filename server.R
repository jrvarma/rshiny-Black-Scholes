library(shiny)
source('compute.R')

shinyServer(function(input, output, session) {
                
  observe({
      if(input$Implied){
          updateNumericInput(session, "VorP", label = "Option Price",
                             value = isolate(input$X))
      }else{
          updateNumericInput(session, "VorP", label = "Volatility (%)",
                             value = 20)
      }
   })

  output$gbsout <- renderTable({
      
      rho <- equiv.rate(input$r/100, as.integer(input$Freq), Inf)
      if(input$Implied){
          Csigma <- GenBSImplied(input$S, input$X, rho,
                                 input$VorP, input$t, input$q/100, F)
          Psigma <- GenBSImplied(input$S, input$X, rho,
                                 input$VorP, input$t, input$q/100, T)
          df <- data.frame(Call = Csigma * 100, Put = Psigma * 100,
                           row.names = c('Implied Volatility (%)'))
      }else{
          sigma <- input$VorP / 100
          res <- GenBS(input$S, input$X, rho,
                       sigma, input$t, input$q/100)
          df <- data.frame(Call = c(res$call, res$Greeks$callDelta,
                               res$Greeks$callTheta,
                               res$Greeks$Gamma, res$Greeks$Vega,
                               res$Greeks$callRho, res$extra$callProb),
                           Put = c(res$put, res$Greeks$putDelta,
                               res$Greeks$putTheta,
                               res$Greeks$Gamma, res$Greeks$Vega,
                               res$Greeks$putRho, res$extra$putProb),
                           row.names = c('Value', 'Delta', 'Theta',
                               'Gamma', 'Vega', 'Rho',
                               'Risk Neutral Probability'))
      }
      df
  }, rownames = TRUE, align = 'lrr')
                
})

