library(shiny)
library(ggplot2)
library(data.table)
library(ggthemes)
library(dplyr)

falsepostive = fread("C:/Users/Jacob Dalgaard/Documents/GitHub/P-hacking-sim/R/shiny/Data/dataShiny.csv",sep=";")
## Figures for paper ##
names(falsepostive)[names(falsepostive) == "Pr"] <- "FPP"

ui <- fluidPage(
  titlePanel("FPP and FPR"),
sidebarLayout(
    sidebarPanel(
      p("This application will let you play around with different types of flexibilities to see how this affect the FPP and FPR"),
      #selectInput("choice1","Select if you want over a range of sample size or just one specefic sample"),  
      numericInput(inputId = "cov",label = "Number of covariates",value = 2,min=2,max=3),
          numericInput(inputId = "sample",label = "How big should the sample be",value = 200,min=150,max=300,step = 50),
      selectInput("outlier",label = "Choose to use outlier criteria or not",c("Use outlier criteira" = "TRUE","Don't use outlier criteria"="FALSE")),
      selectInput("cor",label = "Choose the correlation between the covariates and the dependent varialbe",c("r=0.1" = "0.1","r=0.2"="0.2","r=0.3"="0.3")),
      selectInput("dv",label = "Choose to use multiple dependent variables",c("Use three dependent variables plus the average of them" = "1","Use one dependent variable"="2")),
      selectInput("type","Type of data that should be used",c("h1=Normal, Co=Normal","h1=Binary, Co=Binary","h1=Normal, Co=Binary","h1=Binary, Co=Normal"))),
mainPanel(
    plotOutput("plot", click = "plot_click"),
    fluidRow(
      column(width = 7,
             h4("Click on the top of the black bar to get the information. \nPoints near click"),
             verbatimTextOutput("click_info")
      )
    )
  )

)
)


server <- function(input, output) {

  figuredata<-reactive({falsepostive[falsepostive$SampleSize==input$sample & falsepostive$OutlierExclusion==input$outlier & falsepostive$IndependentVariables==(input$cov-1) & falsepostive$Correlation==input$cor & falsepostive$DV==input$dv
                                        & falsepostive$Type==input$type,]
  })
 
  
   output$plot <- renderPlot(
    ggplot(figuredata(),aes(x=Set))+
      geom_bar(aes(x=Set,y=FPP, fill = "FPP"), stat = "identity",position="dodge")+
      geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
      scale_fill_manual(values=c("black","red"))+
      facet_grid(Type~Main)+
      xlab("Model set")+
      ylab("Probability")+
      ylim(0,1)+
      theme_fivethirtyeight()+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
            axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
    
  )
   output$click_info <- renderPrint({
     nearPoints(select(figuredata(),Set,FPR,FPP,Type,Main), input$plot_click,threshold = 30)
   })

}

shinyApp(ui = ui, server = server)