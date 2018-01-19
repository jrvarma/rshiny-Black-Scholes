library(shiny)
library(ggplot2)
source('compute.R')

function(input, output, session) {
                
  observe({
      if(input$Implied){
          updateNumericInput(session, "sigma", label = "Option Price",
                             value = isolate(input$X))
      }else{
          updateNumericInput(session, "sigma", label = "Volatility (%)",
                             value = 20)
      }      
  })

    rho <- reactive(equiv.rate(input$r/100, as.integer(input$Freq), Inf))

    output$gbsout <- renderTable({
      if(input$Implied){
          ## Csigma <- GenBSImplied(input$S, input$X, rho(),
          ##                        input$VorP, input$t, input$q/100, F)
          ## Psigma <- GenBSImplied(input$S, input$X, rho(),
          ##                        input$VorP, input$t, input$q/100, T)
          Csigma <- GenBSImplied(input$S, input$X, rho(),
                                 input$sigma, input$t, input$q/100, F)
          Psigma <- GenBSImplied(input$S, input$X, rho(),
                                 input$sigma, input$t, input$q/100, T)          
          dfV <- data.frame(Call = Csigma * 100, Put = Psigma * 100,
                           row.names = c('Implied Volatility (%)'))
      }else{
          sig <- input$sigma / 100
          res <- GenBS(input$S, input$X, rho(),
                       sig, input$t, input$q/100)
          dfV <- data.frame(Call = c(res$call, res$extra$callProb), Put = c(res$put, res$extra$putProb), row.names = c('Value', 'Risk Neutral Probability'))
      }
     t(dfV)
  }, rownames = TRUE)

    output$greekout <- renderTable({
          sig <- input$sigma / 100
          res <- GenBS(input$S, input$X, rho(),
                       sig, input$t, input$q/100)
          dfG <- data.frame(Call = c(res$Greeks$callDelta,
                               res$Greeks$callTheta,
                               res$Greeks$Gamma, res$Greeks$Vega,
                               res$Greeks$callRho),
                           Put = c(res$Greeks$putDelta,
                               res$Greeks$putTheta,
                               res$Greeks$Gamma, res$Greeks$Vega,
                               res$Greeks$putRho),
                           row.names = c('Delta', 'Theta',
                               'Gamma', 'Vega', 'Rho'))
     t(dfG)
  }, rownames = TRUE)
    
    spots <- reactive(seq(0.2*input$S, 1.8*input$S, 0.01 * input$S))
    results <- reactive(GenBS(spots(), input$X, rho(), input$sigma/100, input$t, input$q/100))

    output$callPlot <- renderPlot({
        valueC <- results()$call
        payoffC <- pmax(spots() - input$X, 0)
        dfC <- data.frame(spots(), valueC, payoffC)
        ggplot(dfC, aes(spots())) +
            geom_line(aes(y = valueC, color = "Call Value")) +
            geom_line(aes(y = payoffC, color = "Call Payoff")) +
            xlab(expression(paste("Underlying Price (", S[0], ")"))) +
            ylab("Call Option Payoff and Value") +
            theme_bw() + theme(legend.title = element_blank(), text = element_text(size=15))
        })

    output$putPlot <- renderPlot({
        valueP <- results()$put
        payoffP <- pmax(input$X - spots(), 0)
        dfP <- data.frame(spots(), valueP, payoffP)
        ggplot(dfP, aes(spots())) +
            geom_line(aes(y = valueP, color = "Put Value")) +
            geom_line(aes(y = payoffP, color = "Put Payoff")) +
            xlab(expression(paste("Underlying Price (", S[0], ")"))) +
            ylab("Put Option Payoff and Value") +
            theme_bw() + theme(legend.title = element_blank(), text = element_text(size=15))
        })
    
    callData <- reactive({
        switch(input$greek,
               Delta = results()$Greeks$callDelta,
               Gamma = results()$Greeks$Gamma,
               Rho = results()$Greeks$callRho,                        
               Theta = results()$Greeks$callTheta,
               Vega = results()$Greeks$Vega,
               results()$Greeks$callDelta
               )
    })

    putData <- reactive({
        switch(input$greek,
               Delta = results()$Greeks$putDelta,
               Gamma = results()$Greeks$Gamma,
               Rho = results()$Greeks$putRho,                        
               Theta = results()$Greeks$putTheta,
               Vega = results()$Greeks$Vega,
               results()$Greeks$putDelta
               )
    })
    
    output$plot <- renderPlot({
        greek <- input$greek
        dfG <- data.frame(spots(), callData(), putData())
        ggplot(dfG, aes(spots())) +
            geom_line(aes(y = callData(), color = paste("Call", greek))) +
            geom_line(aes(y = putData(), color = paste("Put", greek))) +
            xlab(expression(paste("Underlying Price (", S[0], ")"))) +
            ylab(paste("Call and Put ", greek)) +
            theme_bw() + theme(legend.title = element_blank(), text = element_text(size=15))
  })
   
}
