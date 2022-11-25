library(shiny)
library(data.table)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel) 
fs <- readRDS("fs.rds")
colnames(fs)[3] <- "il"
colnames(fs)[2] <- "UniTur"
fs<- fs %>% mutate(`K` = as.numeric(`K`))
fs<- fs %>% mutate(`E` = as.numeric(`E`))
fs<- fs %>% mutate(`T` = as.numeric(`T`))
fs <- fs[!fs$`Üniversite Adı` == "TOPLAM", ]
results <- fs
fs1_sum_E <- fs %>%group_by(`UniTur`)%>%
  summarize(sum =sum(`E`)) %>%
  arrange(desc(sum))
fs1_sum_K <- fs %>%group_by(`UniTur`)%>%
  summarize(sum =sum(`K`)) %>%
  arrange(desc(sum))
colnames(fs1_sum_E)[2] <- "Male"
colnames(fs1_sum_K)[2] <- "Female"

fs1_k_e <- fs1_sum_K %>% left_join(fs1_sum_E, by = "UniTur")
fs1_k_e <- pivot_longer(fs1_k_e,2:3,names_to = "GENDER",values_to ="SUM")
# UI
ui <- fluidPage(
  
  # Application title
  titlePanel(
    h1("Foreign Students in Turkey", align="center")
  ),
  sidebarLayout(position = "right",
    
    sidebarPanel(h4("This is dashboard"),
    
    pickerInput("ilInput", "İl(Data Table)", choices=sort(unique(results$il)), selected=NULL, multiple=TRUE),
    pickerInput("UniTurInput", "Üniversite Türü(Data Table)", choices=unique(results$UniTur),selected=NULL, multiple=TRUE),
    pickerInput("UyrukInput", "Uyruk(Data Table)", choices=sort(unique(results$Uyruk)),selected=NULL, multiple=TRUE),
    pickerInput("ColInput","Kategori(Statistics)", choices= c("Uyruk", "il","Üniversite Adı"),selected=NULL, multiple=FALSE),
    pickerInput("ColInput2","Sum(Statistics)", choices= c("E","K","T"),selected=NULL, multiple=FALSE)
    ),
    mainPanel(tabsetPanel(type = "tabs",
                          tabPanel("Data Table", DT::dataTableOutput('table')),
                          tabPanel("Visual Statistics", plotOutput("plot")),
                          tabPanel("Statistics", tableOutput("distPlot")))
    
)
))


  


# Server
server <- function(input, output) {
  
  output$distPlot <- renderTable({
    fs%>%group_by(!! rlang::sym(input$ColInput))%>%
      summarize(sum =sum(!! rlang::sym(input$ColInput2))) %>%
      arrange(desc(sum))
    })
  
  output$plot <- renderPlot({
    ggplot(data = fs1_k_e) + 
      geom_bar(aes(x = `UniTur`, y = `SUM`,fill = GENDER),stat = 'identity', position = "dodge") +
      geom_text_repel(aes(x = `UniTur`, y = `SUM`,label=`SUM`), position=position_dodge(width=0.9), vjust=-1)
    })
  
  mydata <- reactive({
    if (is.null(input$ilInput) & is.null(input$UniTurInput) & (is.null(input$UyrukInput))) {df <- results} 
    else if (!is.null(input$ilInput) & is.null(input$UniTurInput) & is.null(input$UyrukInput)) {df <- results[results$il %in% input$ilInput,]
    df}
    else if (is.null(input$ilInput) &!is.null(input$UniTurInput) & is.null(input$UyrukInput)) {df <- results[results$UniTur %in% input$UniTurInput,]
    df}
    else if (is.null(input$ilInput) & is.null(input$UniTurInput) & !is.null(input$UyrukInput)) {df <- results[results$Uyruk %in% input$UyrukInput,]
    df}
    else if (!is.null(input$ilInput) & !is.null(input$UniTurInput) & is.null(input$UyrukInput)) { df <- results[(results$il %in% input$ilInput) & (results$UniTur %in% input$UniTurInput),]
    df}
    else if (!is.null(input$ilInput) & is.null(input$UniTurInput) & !is.null(input$UyrukInput)) { df <- results[(results$il %in% input$ilInput) & (results$Uyruk %in% input$UyrukInput),]
    df}
    else if (is.null(input$ilInput) & !is.null(input$UniTurInput) & !is.null(input$UyrukInput)) { df <- results[(results$UniTur %in% input$UniTurInput) & (results$Uyruk %in% input$UyrukInput),]
    df}
    else if (!is.null(input$ilInput) & !is.null(input$UniTurInput) & !is.null(input$UyrukInput)) { df <- results[(results$il %in% input$ilInput) & (results$UniTur %in% input$UniTurInput) & (results$Uyruk %in% input$UyrukInput),]
    df}
    
    
  })

  output$table <- DT::renderDataTable(
    datatable(mydata())
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
