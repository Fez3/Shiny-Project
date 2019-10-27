

shinyUI(fluidPage(
  
  titlePanel(textOutput("slidertime") ),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "origin",
                     label = "Departure airport",
                     choices = unique(flights$origin)),
      selectizeInput(inputId = "dest",
                     label = "Arrival airport",
                     choices = unique(flights$dest)),
      selectizeInput(inputId = "month",
                     label = "Date of Travel",
                     choices = unique(flights$month)),
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
        column(7, plotOutput("delay"))
      )
      ),
      tabPanel("tab2",
               tableOutput("dataf")
      )
               )
  )
))
)