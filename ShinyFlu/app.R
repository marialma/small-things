rm(list=ls())
library(shiny)
library(dplyr)
library(ggplot2)
library(deSolve)
#ODE code ----
SIR_ode <- function(time, state, theta) {
  beta <- theta["R0"] / theta["D"]
  rec <- 1 / theta["D"]
  
  ## States:
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  N <- S + I + R
  V <- N - (S+I+R)
  
  ## ODEs:
  dS <- -(beta * S * I/N)
  dI <- (beta * S * I/N) - (rec * I)
  dR <- rec * I
  
  return(list(c(dS, dI, dR)))
}

# Define UI ----
ui <- fluidPage(
  
  titlePanel("A Simple Flu Model"),

  
  # Sidebar ----
  sidebarLayout(

    sidebarPanel(
      sliderInput("totalpop", 
                  label = "Total Population",
                  min= 0, max = 10000, value = 1000),
      sliderInput("resistant", 
                  label = "Vaccine Coverage",
                  min= 0, max = 1, value = 0.4),
      sliderInput("VE", 
                  label = "Vaccine Effectiveness",
                  min= 0, max = 1, value = 0.3),
      sliderInput("R0", 
                  label = "R0 (how infectious the disease is)",
                  min= 1, max = 16, value = 1.4, step = 0.1)
      ),
    

    mainPanel(
      tabsetPanel(
        tabPanel("Graphs",
                 p("Adjust the sliders to see what happens during this simple simulation of a flu outbreak. Note that, even if a vaccine is not very effective, if the coverage is still high, many other people can still be protected from disease."),
                 textOutput("numinfected", container = pre),
                 plotOutput("sirplot"),
                 p("This graph shows the progression through an outbreak. The blue line shows the number of people who have recovered from an infection - it's a tally of the total number of people who got sick. The higher this number is, the bigger the outbreak.")
                 ),
        tabPanel("Description",
                 p("This simple flu model is based on what is known as an", strong("SIR model."), 
                   "This model is extremely pared down and is intended to serve as an illustration of concepts. In reality, the dynamics are much more complex, and complicated."),
                 p("The R0 is known as the", strong("basic reproductive number."), "It is the average number of new infections that one infection will generate. So, an R0 of 2 means that each infection will lead to two more infections.
                   The default R0 here is 1.4, meant to simulate flu. You can change the R0 to get an idea what an outbreak might look like "), 
                 p(strong("Model Details:"), "This is the basic SIR model, assuming that the population mixes evenly. People start off either susceptible or vaccinated (resistant), with a single person who is infected.",
                   "No new vaccinations happen throughout the simulation. Susceptible people who become infected recover 7 days later. People who recover or are vaccinated can't be infected again. This model does not include deaths, or take age structures into consideration.")
                 )
      )
    )
  )
)
  

server <- function(input, output) {
  
  output$echo <- renderText({
    paste("Currently, the estimates are for what an outbreak might look like in a population with", input$totalpop[1], "people, where", input$resistant*100, "% of the population is vaccinated with a vaccine that is", input$VE *100, "% effective,")
  })
  output$sirplot <- renderPlot({
    theta <- c(R0 = input$R0[1], D = 7, N = input$totalpop[1])

    initState <- c(S=input$totalpop[1] - input$resistant[1]*input$VE[1] * input$totalpop[1], I=1, R = 0)
    times <- seq(0, 365, by = 1) 
    trajModel <- data.frame(ode(y=initState, times=times, func=SIR_ode, 
                                parms=theta, method = "ode45"))
    #plot(trajModel$time, trajModel$S, type="l", frame=FALSE, col = "black", ylim = c(0,theta["N"]))
    #lines(trajModel$time, trajModel$R, col = "blue", type = "l")
    #lines(trajModel$time, trajModel$I, col = "red", type = "l")
    #legend("topright", legend=c("Susceptible", "Infected", "Resistant/Recovered"),
    #       col=c("black", "red", "blue"), bty = "n", lty=1:1, cex=0.8)
    vacc <-  input$resistant[1]*input$VE[1]*input$totalpop[1]
    newdat <- reshape2::melt(trajModel,id="time")
    ggplot(newdat) + geom_line(aes(x=time,y=value,color=variable)) + 
      labs(x="Time (days)", y="Population") + 
      theme_bw() + theme(legend.title=element_blank()) + geom_hline(yintercept=vacc, color = "green") +geom_text(x=350, y=vacc, label="Vaccinated")+ ylim(0,input$totalpop[1]) +
      scale_color_manual(values = c("black", "maroon", "blue"), labels = c("Susceptible", "Infected", "Recovered"))
  })
  
  output$numinfected <- renderText({ 
    theta <- c(R0 = input$R0[1], D = 7, N = input$totalpop[1])
    
    initState <- c(S=input$totalpop[1] - input$resistant[1]*input$VE[1] * input$totalpop[1], I=1, R=0)
    times <- seq(0, 365, by = 1) 
    trajModel <- data.frame(ode(y=initState, times=times, func=SIR_ode, 
                                parms=theta, method = "ode45"))
    infectednumbers <- (input$totalpop[1] - input$resistant[1]*input$VE[1] * input$totalpop[1] - tail(trajModel$S, n=1))
    percent_sick <- ((infectednumbers / input$totalpop[1]) *100)
    print(c(round(infectednumbers, digits = 0), "people will get sick, which accounts for", round(percent_sick, digits = 1), "% of the population"))
    
    #df <- data.frame(infected = c("Infected"), total=input$totalpop[1],inf=c(infectednumbers))
    #df<- mutate(df, pct = inf/total)
    #ggplot(df, aes(x=infected, y=inf, label = paste(round(inf),"infected individuals"))) + geom_col(width = .5, fill = "maroon") + ylim(0,input$totalpop[1]) +
    #  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + xlab("Infected") +
    #  ylab("Number of Infected Patients") + geom_label(nudge_y=-30) + theme_bw() 
    })  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
