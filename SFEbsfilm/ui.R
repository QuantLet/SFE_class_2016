library(plotly)
library(shiny)


fluidPage( 
  
 
titlePanel("Black-Scholes formula for European call price"),
  
     
 fluidRow(
  
   column(12,
    
           column(3,numericInput("S0", "Stock price at t=0:",100),
                    numericInput("K", "Strike price:",100),
                    sliderInput("r","Risk free interest rate:",min = 0,  max = 1, value = 0.1,step = 0.1),
                    sliderInput("sigma", "Volatility:",min = 0,  max = 1, value = 0.1,step = 0.05),
                    sliderInput("t","Time t (t=1-τ):",min = 0,  max = 1,value=0.05,animate=TRUE,step=0.05)
                  ),
          
          column(width = 9,
         
          plotlyOutput("Plot_BS_Call")
           
           
                )
      
      
      
        ),

    
    column(width = 12,
      
        fluidRow(
          
          column(6,plotlyOutput("Plot_St")),
           
          column(6,plotlyOutput("Plot_Ct"))
                  
                )
          )
        
         
  
  
     )

)






