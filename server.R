library(shiny)
library(dplyr)
library(ggplot2)

function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  rData <- reactive({
    req(input$analysis$datapath)
    df <- read.csv2(input$analysis$datapath, sep=',')
    df$S <- as.numeric(df$S)
    df$bandMW <- as.numeric(df$bandMW) # new line 10/22/24
    df <- df %>% filter(!(df$band == 0))
    df$cycleAB <- paste(df$cycle, df$Ab1_name)
    df
  })
  
  observeEvent(rData(), {
    choices1 <- unique(rData()$cycleAB)
    updateSelectInput(inputId = "cycleAB", choices = choices1)
  })
  
  aData <- eventReactive(input$cycleAB, {
    rData() %>% filter(rData()$cycleAB == input$cycleAB)
  })
  
  observeEvent(aData(),{
    req(input$cycleAB)
    choices2 <- unique(aData()$lane)
    updateCheckboxGroupInput(inputId = "lanes", choices = choices2)
  })
  
  bData <- reactive({
    lanes <- as.numeric(input$lanes)
    aData() %>% filter(!(lane %in% lanes))
  })
  
  # New reactive function added on 10/22/24
  excludedLanes <- reactive({
    # aLanes <- unique(aData()$lane)
    # bLanes <- unique(bData()$lane)
    setdiff(unique(aData()$lane), unique(bData()$lane))
  })
  
  output$selectedFolder <- renderText({
    paste("Data is from: ", unique(aData()$run_directory))
  })
  
  # Added excluded lanes output 10/22/24
  output$excludedLanes <- renderText({
    paste("Excluded lanes: ", toString(excludedLanes()))
  })
  
  output$table1 <- renderTable({
    # added Mean MW and order by MW on 10/22/24
    DF <- bData() %>% group_by(band) %>% summarise(CV = ((sd(S)*100)/mean(S)), MW = mean(bandMW)) 
    arrange(DF, MW)
  })
  
 " output$table2 <- renderTable({
    unique(aData()$lane)[order(unique(aData()$lane))]
  })"
  
  output$Hist1 <- renderPlot({
    ggplot(bData(), aes(x=lane)) +
      geom_histogram(binwidth=1, colour="red", fill="pink")+
      ggtitle("Lane distribution in the analysis log")+
      xlab("Lane") + ylab("Row Count")+
      scale_x_continuous(breaks = round(seq(min(bData()$lane), max(bData()$lane), by = 1), 1))+
      theme(
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$Hist2 <- renderPlot({
    ggplot(bData(), aes(x=band)) +
      geom_histogram(binwidth=1, colour="darkgreen", fill="darkolivegreen1")+
      ggtitle("Band distribution in the analysis log")+
      xlab("Band") + ylab("Row Count") +
      scale_x_continuous(breaks = round(seq(min(bData()$band), max(bData()$band), by = 1), 1))+
      theme(
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$Hist3 <- renderPlot({
    ggplot(bData(), aes(x=class)) +
      geom_histogram(binwidth=1, colour="blue", fill="skyblue1")+
      ggtitle("Class distribution in the analysis log")+
      xlab("Class") + ylab("Row Count") +
      scale_x_continuous(breaks = round(seq(min(bData()$class), max(bData()$class), by = 1), 1))+
      theme(
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5)
      )
  })
}