library(shiny)
library(data.table)
library(janitor)

# Layout
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("numRounds", "Number of rounds", 10, min = 6, max = 15, step = 1),
      numericInput("riskParam", "Theta", 2, min = 0, max = 10, step = 1),
      numericInput("pValue", "p(Poor rain) = 1 /", 3, min = 1, max = 100, step = 1),
      numericInput("normalCP", "Normal: CP", 100, min=0, max=200, step=10),
      numericInput("normalMSD", "Normal: MSD",100, min=0, max=200, step=10),
      numericInput("poorCP", "Poor (CP)", 30, min = 0, max = 200, step = 10),
      numericInput("poorMSD", "Poor (MSD)", 50, min = 0, max = 200, step = 10),
      numericInput("costCP", "Input cost (CP)", 10, min = 0, max = 100, step = 10),
      numericInput("costMSD", "Input cost (MSD)", 30, min = 0, max = 100, step = 10),
      numericInput("gainNormal", "Gain (Normal)", 20, min = 0, max = 100, step = 10),
      numericInput("gainPoor", "Gain (Poor)", 10, min = 0, max = 100, step = 10),
      numericInput("treatBonus", "Bonus value", 20, min = 0, max = 100, step = 10),
      width=3
    ), 
    mainPanel(
      tableOutput("tableOut")
      
    )
  )
)
