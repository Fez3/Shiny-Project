

shinyUI(fluidPage(
  
  titlePanel(textOutput("slidertime") ),
  sidebarLayout(
   
    sidebarPanel(
      selectizeInput(inputId = "disease",
                     label = "Contagion",
                     choices = Diseases),
     
    #  selectizeInput(inputId = "year",
     #                label = "Year",
      #               choices = NULL),
      sliderInput(inputId="Date", label="Date", as.Date(min(data$TIME)), as.Date(max(data$TIME)),value=as.Date(min(data$TIME)),
                  timeFormat="%Y-%m-%d" ,animate = animationOptions(interval = 100   ))
      
      #,
      
      #animationOptions(interval = 7, loop = FALSE, playButton = NULL,
       #                pauseButton = NULL)
    ),
    #mainPanel(plotOutput("count"))
  
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("tab1",
               fluidRow(
                 column(5, leafletOutput("count")),
                 column(5, 
                        fluidRow(plotOutput("peak")),
                        fluidRow(plotOutput("heat") )
                        )
               )
              
      )
    )
  )
  )
))
