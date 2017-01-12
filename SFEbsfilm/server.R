library(plotly)
library(shiny)
library(fOptions)


function(input, output) {
  
  ##  Black-Scholes formula for European call option ##
  
  bs_call <-function(S, K, r, tau, sigma) {
    # Black-Scholes formula for European call price with b = r (costs of carry =
    # risk free interest rate -> the underlying pays no continuous dividend)
    d1 = (log(S/K) + (r + (sigma^2/2)) * tau)/(sigma * sqrt(tau))
    d2 = d1 - (sigma * sqrt(tau))
    C  = S * pnorm(d1) - K * exp(-r * tau) * pnorm(d2)
    return(C)
  }
  
  
  ## Computation of the stock value  ##
  
  T   = 1001
  t   = (1:T)/T
  dt  = t[2] - t[1]
  Wt1 = rnorm(length(t), mean = 0, sd = 1)
  Wt  = cumsum(Wt1)  # cumulative sum
  
  
  ## Reactive Stock value  St with respect to input values ##
  
  
  S_t<-reactive({
    data = input_values()
    
     St  = data$S0 * exp((data$r - 0.5 * data$sigma) * dt + data$sigma * sqrt(dt) * Wt)
    
  })

  ## Reactive Input values ##
 
  input_values <- reactive({
    
    values = data.frame(S0=input$S0,K=input$K,r=input$r,t=input$t,sigma=input$sigma)
    
   return(values)
   
    })
  
  
  
  ##  Graph BS / Payoff at maturity T ##
  
 
  
  output$Plot_BS_Call <- renderPlotly({
    
    data = input_values()
    St = S_t()
    
    x = c(0:200)
    
    p <- plot_ly(x=x,y = pmax(0,x-data$K), name = "Payoff at maturity", type = 'scatter',mode = 'lines')%>%
      
      add_lines(y = bs_call(x, data$K, data$r,1- data$t, data$sigma), name = "BSE call price",type = 'scatter',mode = 'lines' ) %>%
      
      add_lines(x=St[data$t*1000+1],y = bs_call(St[data$t*1000+1], data$K, data$r,1- data$t, data$sigma), name = "St",marker = list(size = 10,
                                                                                                                                       color = 'rgba(255, 252, 252, .9)',
                                                                                                                                       line = list(color = 'rgba(0, 0, 0, .8)',
                                                                                                                                                   width = 2))) %>%
      add_annotations(x = St[data$t*1000+1],y = bs_call(St[data$t*1000+1], data$K, data$r,1- data$t, data$sigma),text = "St",showarrow = T,arrowhead = 5,
                      arrowsize = 0.3,ax = 0,ay = -30,font = list(color = '#264E86',
                                                                  family = 'sans serif',
                                                                  size = 18)) %>%
      
      
      
    
      layout(yaxis = list(title = 'C(S,t)'),xaxis = list (title = 'S'))
    
    
  })
  
  
 
 
  ##  Graph Stock value ##
  
  
  output$Plot_St <- renderPlotly({
    
    data = input_values()
    St = S_t()
    
    p <- plot_ly(x =t,y=St,name = "S" ,type = 'scatter',mode = 'lines') %>%
        add_lines(x = c(0,1), y = c(data$K, data$K),name = "Strike Price" ,type = 'scatter',mode = 'lines') %>%
        add_lines(x = data$t, y = St[data$t*1000+1],name = "St" ,type = 'scatter',mode = 'lines',marker = list(size = 10,
                                                                                                                    color = 'rgba(255, 252, 252, .9)',
                                                                                                                  line = list(color = 'rgba(0, 0, 0, .8)',
                                                                                                                               width = 2))) %>%
        add_annotations(x = data$t,y = St[data$t*1000+1],text = "St",showarrow = TRUE,arrowhead = 4,
                     arrowsize = .3,ax = 0,ay = -30,font = list(color = '#264E86',
                                                                  family = 'sans serif',
                                                                   size = 18)) %>%
         layout(yaxis = list(title = 'St'),xaxis = list (title = 't'),autosize = F, width = 650, height = 300)
    
    
  })
  
  
  ##  Graph Call value ##
  
  
  output$Plot_Ct <- renderPlotly({
    
    data = input_values()
    St = S_t()
    
    
    Call    = GBSOption(TypeFlag = "c", S = St, X = data$K, Time = 1-data$t, r = data$r, b = data$r, sigma = data$sigma)  
    Callp   = attr(Call, "price")
    
    p <- plot_ly(x =t,y=Callp,name = "S" ,type = 'scatter',mode = 'lines') %>%
      
      add_lines(x = data$t, y = Callp[data$t*1000+1],name = "t" ,type = 'scatter',mode = 'lines',marker = list(size = 10,
                                                                                                             color = 'rgba(255, 252, 252, .9)',
                                                                                                             line = list(color = 'rgba(0, 0, 0, .8)',
                                                                                                                         width = 2))) %>%
      add_annotations(x = data$t,y = Callp[data$t*1000+1],text = "t",showarrow = TRUE,arrowhead = 4,
                      arrowsize = .3,ax = 0,ay = -30,font = list(color = '#264E86',
                                                                 family = 'sans serif',
                                                                 size = 18)) %>%
      
      layout(yaxis = list(title = 'C(S,t)'),xaxis = list (title = 't'),autosize = F, width = 650, height = 300)
    
    
  })
  

  

  

  
  
  
}