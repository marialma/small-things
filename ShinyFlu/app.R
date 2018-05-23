rm(list=ls())
library(shiny)
library(dplyr)
library(ggplot2)
library(deSolve)
SIR_ode <- function(time, state, theta) {
  beta <- theta["R0"] / theta["D"]
  rec <- 1 / theta["D"]
  
  ## States:
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  N <- S + I + R
  
  ## ODEs:
  dS <- -(beta * S * I/N)
  dI <- (beta * S * I/N) - (rec * I)
  dR <- rec * I
  
  return(list(c(dS, dI, dR)))
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("SIR Model"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ---
      sliderInput("totalpop", 
                  label = "Total Population",
                  min= 0, max = 10000, value = 1000),
      sliderInput("R0", 
                  label = "R0",
                  min= 1, max = 16, value = 1.4, step = 0.1),
      sliderInput("resistant", 
                  label = "Fraction of the population that has been vaccinated",
                  min= 0, max = 1, value = 0.3),
      sliderInput("VE", 
                  label = "Vaccine Efficacy",
                  min= 0, max = 1, value = 0.3)
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("sirplot"),
      textOutput("numinfected")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  output$sirplot <- renderPlot({
    theta <- c(R0 = input$R0[1], D = 7, N = input$totalpop[1])

    initState <- c(S=input$totalpop[1] - input$resistant[1]*input$VE[1] * input$totalpop[1], I=1, R=input$resistant[1]*input$VE[1]*input$totalpop[1])
    times <- seq(0, 365, by = 1) 
    trajModel <- data.frame(ode(y=initState, times=times, func=SIR_ode, 
                                parms=theta, method = "ode45"))
    plot(trajModel$time, trajModel$S, type="l", frame=FALSE, col = "black", ylim = c(0,theta["N"]))
    lines(trajModel$time, trajModel$R, col = "blue", type = "l")
    lines(trajModel$time, trajModel$I, col = "red", type = "l")
    legend("topright", legend=c("Susceptible", "Infected", "Resistant/Recovered"),
           col=c("black", "red", "blue"), bty = "n", lty=1:1, cex=0.8)
  })
  
  output$numinfected <- renderText({ 
    theta <- c(R0 = input$R0[1], D = 7, N = input$totalpop[1])
    
    initState <- c(S=input$totalpop[1] - input$resistant[1]*input$VE[1] * input$totalpop[1], I=1, R=input$resistant[1]*input$VE[1]*input$totalpop[1])
    times <- seq(0, 365, by = 1) 
    trajModel <- data.frame(ode(y=initState, times=times, func=SIR_ode, 
                                parms=theta, method = "ode45"))
    infectednumbers <- input$totalpop[1] - initState["R"] - tail(trajModel$S, n=1)
    paste("By the end of the outbreak, the number of total infected patients is", round(infectednumbers, digits = 0))
  })  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
