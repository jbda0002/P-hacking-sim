library(shiny)
library(ggplot2)
library(data.table)

falsepostive = fread("C:/Users/Jacob Dalgaard/Documents/GitHub/P-hacking-sim/R/shiny/Data/Results.csv",sep=";")
## Figures for paper ##

ui <- fluidPage(
  titlePanel("FPP and FPR"),
sidebarLayout(
    sidebarPanel(
      p("This application will let you play around with different types of flexibilities to see how this affect the FPP and FPR"),
      selectInput("choice1","Select if you want over a range of sample size or just one specefic sample"),  
      numericInput(inputId = "cov",label = "Number of covariates",value = 2,min=2,max=3),
          numericInput(inputId = "sample",label = "How big should the sample be",value = 200,min=150,max=300,step = 50),
      selectInput("outlier",label = "Choose to use outlier criteria or not",c("Use outlier criteira" = "TRUE","Don't use outlier criteria"="FALSE")),
      selectInput("type","Type of data that should be used",c("Normal"="h1=Normal, Co=Normal","Bin"="h1=Binary, Co=Binary","NormalBin"="h1=Normal, Co=Binary","BinNormal"="h1=Binary, Co=Normal"))),
mainPanel(
    plotOutput("plot"))
    
)
)


server <- function(input, output) {

  figuredata<-reactive({falsepostive[falsepostive$SampleSize==input$sample & falsepostive$OutlierExclusion==input$outlier & falsepostive$IndependentVariables==(input$cov-1) & falsepostive$Correlation==0.2 & falsepostive$DV==1
                                        & falsepostive$Type==input$type,]
  })
 
  
   output$plot <- renderPlot(
    ggplot(figuredata(),aes(x=Set))+
      geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
      geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
      scale_fill_manual(values=c("black","red"))+
      facet_grid(Type~Main)+
      xlab("Model set")+
      ylab("Probability")+
      ylim(0,1)+
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
            axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
    
  )

}

shinyApp(ui = ui, server = server)