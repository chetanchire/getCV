library(shiny)
library(dplyr)

fluidPage(
  titlePanel("Proteowise"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("analysis", label="Select analysis file", 
                multiple = FALSE,accept = NULL, 
                width = NULL, buttonLabel = "Browse...", 
                placeholder = "No file selected", capture = NULL),
      selectInput("cycleAB", label = "Select antibody cycle", 
                  choices = NULL, 
                  selected = NULL,
                  multiple = FALSE),
      checkboxGroupInput("lanes", label = "Check unwanted lanes",
                         choices = NULL,
                         selected = NULL)
      ),
  
  mainPanel(
    textOutput("selectedFolder"),
    textOutput("excludedLanes"),
    fluidRow(
      column(6, tableOutput("table1")),
      ),
    fluidRow(
      column(6, plotOutput("Hist1")),
      column(6, plotOutput("Hist2"))
    ),
    fluidRow(
      column(6, plotOutput("Hist3")),
      column(6, plotOutput("Hist4"))
    )
    )
  )
  )