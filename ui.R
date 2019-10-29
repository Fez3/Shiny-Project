
 
shinyUI(dashboardPage(skin = "green",
  dashboardHeader(title = "Contagion Spread"),
  
  dashboardSidebar(Title="Aaron Festinger", br(), 
                   img(src="A.jpg", width=100,height = 100, align="center"),
    
        sidebarMenu(
              
          menuItem("Map", tabName = "map", icon = icon("map")),
          menuItem("Data", tabName = "data", icon = icon("database")),
          menuItem("State by State", tabName = "statedata", icon = icon("database")),
          menuItem("Geographic Spread", tabName = "hmap", icon = icon("database"))                          
                                    )
                                    ),
 
   dashboardBody(
   
    tabItems(
      tabItem(tabName = "map",
              leafletOutput("map"),box(
              selectizeInput(inputId = "disease",label = "Contagion",choices = Diseases), 
              sliderInput(inputId="Date", label="Date", as.Date(min(data$TIME)), as.Date(max(data$TIME)),value=as.Date(min(data$TIME)),
                                                  timeFormat="%Y-%m-%d" ,animate = animationOptions(interval = 100   ))
            )  ),
      tabItem(tabName = "data",
              htmlOutput("peak"),selectizeInput(inputId = "disease",label = "Contagion",choices = Diseases),plotOutput ("heat")
              ),
      
      tabItem(tabName = "statedata", box(selectizeInput(inputId = "disease",label = "Contagion",choices = Diseases),selectizeInput(inputId = "stin",label = "State",choices = stt)),column(width = 4, htmlOutput("byState")
     ) ),
      
      
      tabItem(tabName = "hmap", box(tags$iframe(src="https://www.youtube.com/embed/dQw4w9WgXcQ",width="600",height="400") )
)
 
)
)
)
)