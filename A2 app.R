# install packages
if (!require(dplyr)) install.packages('dplyr')
if (!require(leaflet)) install.packages('leaflet')
if (!require(maps)) install.packages('maps')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(scales)) install.packages('scales')
if (!require(plotly)) install.packages('plotly')
if (!require(shiny)) install.packages('shiny')
if (!require(shinyWidgets)) install.packages('shinyWidgets')
if (!require(lubridate)) install.packages('lubridate')


# import libraties
library(dplyr)
library(leaflet)
library(maps)
library(ggplot2)
library(scales)
library(plotly)
library(shiny)
library(shinyWidgets)
library(lubridate)


# read data from the original file
shooting <- read.csv("school-shootings-data.csv", header =T)

# count the number of shootings in each state
state_shootings <- shooting %>% group_by(state) %>% summarise(Num=n())

# categorize shooting types and map the states
state_shooting_type <- shooting %>% group_by(state, shooting_type) %>% summarise(Num=n(),Lng=mean(long),Lat=mean(lat))
k_shooting <- shooting %>% group_by(state,killed) %>% summarise(Num=n(),Lng=mean(long),Lat=mean(lat))

# provides latitude and longitude
map_states <- map("state", fill = TRUE, plot = FALSE)

# match state name with map
map_name <- lapply(strsplit(map_states$names,":"), function(x) x[1])

# change the state into lower case in order to match
state_shootings$state <- tolower(state.name[match(state_shootings$state,state.name)])

# order for Num by state name in map library through match function
total_shooting <- state_shootings$Num[match(map_name,state_shootings$state)]
cpol <- colorNumeric("Oranges",na.color = NA,total_shooting)
v <- shooting %>% group_by(day_of_week) %>% summarise(Num=n())

mon=v$Num[v$day_of_week == "Monday"]
tue=v$Num[v$day_of_week == "Tuesday"]
wed=v$Num[v$day_of_week == "Wednesday"]
thu=v$Num[v$day_of_week == "Thursday"]
fri=v$Num[v$day_of_week == "Friday"]

x <- c(v$day_of_week)
y <- c(v$Num)
df <- data.frame(x=x,y=y)

# adjust the display order
df$x <- factor(df$x,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))

# years and shooting numbers
shooting_year <- shooting %>% group_by(year) %>% summarise(Num=n())

# shooting type
type <- shooting %>% group_by(shooting_type) %>% summarise(Num=n()) %>%
  filter(shooting_type %in% c("accidental", "indiscriminate","targeted","unclear","public suicide"))

# calculate the percentage of different types of shootings
per <- prop.table(type$Num)
per <- scales::percent(per, 1) 

# generate data frame
df_2 <- data.frame(
  t = type$shooting_type,
  n = type$Num,
  p = per
) 

# limit the range into the recent 5 years
shooting_recent_5 <- shooting %>%  filter(year >= 2018 & year <= 2022) 

# generate data frame of people killed
df_killed <- data.frame(
  x = shooting_recent_5$year,
  y = shooting_recent_5$killed
) 
df_killed <- aggregate(df_killed$y, by=list(year=df_killed$x),sum)

df_killed <- data.frame(
  df_killed,
  type = c("killed","killed","killed","killed","killed")
)

# generate data frame of people injured
df_injured <- data.frame(
  x = shooting_recent_5$year,
  y = shooting_recent_5$injured
) 
df_injured <- aggregate(df_injured$y, by=list(year=df_injured$x),sum)

df_injured <- data.frame(
  df_injured,
  type = c("injured","injured","injured","injured","injured")
)

# merge the number of people killed and injured
df_3 <- rbind(df_killed, df_injured)


## design ui
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  "School Shootings in America",
  tabPanel(
    "Background",
    br(),
    br(),
    span(h4("School shootings - terrifying to students, educators, parents, 
    and communities - always reignite polarizing debates about gun rights and school safety.")),
    span(h4("Every number you see stands for a person, often a child, with hopes, dreams, 
    and loved ones who cherished them. Let's investigate the data behind, and do something.")),     

    br(),
    "Danlan Chen - 1288528",
    br(),
    br(),
    ),
  navbarMenu(
    "Overview",
    tabPanel(
      "Map",
      leafletOutput("HeatMap",width='100%',height = 500),
      br(),
      span((h6("338 shootings happened in America from 1999 to 2022. Click each state to see in detail."))),
      
    ),
    tabPanel(
      "1999 - 2022",
      sliderInput("year", "Year:",min = 1999, max = 2022,step=1,value=c(2000,2022)),
      plotOutput("LineMap",height = 500)
    )
    ),
  
  tabPanel(
    "Shootings",
    
    fluidPage(
      titlePanel(
        "Shootings"
      ),
      sidebarLayout(
        sidebarPanel(
          "We analyze the number of shootings in weeks.",
          br(),
          "And five main types of shootings are chosen display."
          
        ),
        mainPanel(
          
          tabsetPanel(
            # two tabs
            tabPanel("day of week ", 
                     br(),
                     plotOutput("BarMap",height=300)),
            tabPanel("5 main types",
                     plotOutput("PieMap",height=300)
          )
        )
      ))
    ),
    
    
  
  ),
  
  tabPanel(
    "Casualties",
    fluidPage(
      titlePanel(
        "Casualties"),
      sidebarLayout(
        sidebarPanel(
          
          "Detailed information of casualties in the recent five years is shown. 
        2020 has the least injured / killed people, while 2018 has the most.
        People injured are more than people killed, except 2022.",
          style="color:grey"  
        ),
        mainPanel(plotOutput("HisMap",width = 300))
    )
    
  )
  ),
  navbarMenu(
    "About",
    
    tabPanel(
      "Data",
      br(),
      a("Original data",
        href="https://github.com/washingtonpost/data-school-shootings/blob/master/school-shootings-data.csv"),
      br(),
      dataTableOutput("dynamic")
    ),
    
    tabPanel(
      
      "Information",
      
      fluidPage(
        navlistPanel(
          id = "tabset",

          tabPanel("Author", br(),
                   "Danlan Chen - 1288528",
                   br(),
                   br(),
                   br(),
                   span((h6("@All rights reserved")), 
                        style="color:grey"),
                   br()
                   ),

          tabPanel("Reference",
                   "https://github.com/washingtonpost/data-school-shootings",br(),
                   "https://www.k12academics.com/school-shootings/history-school-shootings-united-states",br(),
                   "https://www.sandyhookpromise.org/blog/gun-violence/16-facts-about-gun-violence-and-school-shootings/",br(),
                   "https://stackoverflow.com/questions/32969659/shiny-reactive-ggplot-output",br(),
                   "https://lubridate.tidyverse.org/reference/lubridate-package.html",br(),
                   "https://cloud.tencent.com/developer/article/1711047?ivk_sa=1024320u",br(),
                   "https://www.statmethods.net/management/merging.html",br(),
                   "https://blog.csdn.net/m0_45047077/article/details/122093481",br(),
                   "https://blog.csdn.net/m0_52069102/article/details/125957340",br(),
                   "https://zhuanlan.zhihu.com/p/370223674",br(),
                   "https://mastering-shiny.org/action-layout.html#themes",br(),
                   "https://mastering-shiny.org/index.html",br(),
                   )
        )
      )
    )
  )
)


server <- function(input, output) {
  # figure 1
  output$HeatMap <- renderLeaflet({
    # generate the map
    leaflet(map_states) %>% 
      addTiles() %>% 
      
      addPolygons(
        stroke = F,
        color = "white",
        smoothFactor = 0.2,
        fillOpacity = 1,
        fillColor = cpol(total_shooting),
        popup = paste("<strong>State: </strong>", toupper(map_name),"<br>",
                      "<strong>Shootings: </strong>",total_shooting)) %>%
      
      addLegend(
        position = "bottomleft",
        pal = cpol,
        values = total_shooting,
        title = paste("Number of<br>school<br> shootings")) %>%
      
      addCircleMarkers(data=k_shooting[k_shooting$killed == 0,],~Lng,~Lat,radius=~Num^(1/2), group = 'Nonfatal', 
                       stroke = FALSE, fillOpacity = 1,color = "yellow") %>% 
      addCircleMarkers(data=k_shooting[k_shooting$killed >= 1,],~Lng,~Lat,radius=~Num^(1/2), group = 'Fatal', 
                       stroke = FALSE, fillOpacity = 1,color = "red") %>%
      
      addLayersControl(
        position = "bottomright",
        overlayGroups = c('Nonfatal','Fatal'),
        options = layersControlOptions(collapsed = F) #expand the layer
      )
  })  
  
  # figure 2
  output$BarMap <- renderPlot(
    # generate the bar chart
    ggplot(data=df,aes(x=x,y=y)) +
      geom_bar(stat = "identity", width=0.5, color='black',fill='orange') +
      labs(title='School Shootings in Day of Week',x = "day of week",y = "times")+
      theme(
        plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")
      )
  )

  dat <- reactive({
    test <- shooting_year[shooting_year$year %in% seq(from=min(input$year),to=max(input$year),by=1),]
    print(test)
    test
  })
  
  output$LineMap <- renderPlot(
    # generate the line chart
    ggplot(dat(), aes(x=year, y=Num)) +
      geom_line(color="orange",size=3) +
      geom_point() +
      labs(title ='Times of Shootings from 1999 to 2022', y = "times")+
      theme_minimal()+
      theme(
        plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")
      )
  )
  
  # figure 3
  output$PieMap <- renderPlot(
    # generate the pie chart
    ggplot(df_2, aes(x="",y=n, fill=t)) +
      geom_bar(stat="identity", width=10, color = "white") +
      coord_polar("y", start=0) +
      theme_void() + 
      theme(legend.position="left",
            plot.title = element_text(size=20, face="bold.italic")
            ) +
      scale_fill_brewer(palette="Pastel1") +
      labs(title ='5 Main Types of School Shootings') +
      guides(fill = guide_legend(title = "Types")) +
      geom_text(aes(label = p),
                position = position_stack(vjust = 0.5))
    
  )
  
  # figure 4
  output$HisMap <- renderPlot(
    # generate the histogram
    ggplot(df_3,aes(x=year,y=x,fill=type))+geom_col()+
      scale_fill_brewer(palette="Oranges")+
      labs(title ='People Injured / Killed from 2018 to 2022',x = "year",y = "number") +
      theme(
        plot.title = element_text(face="bold.italic"),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")
      )
    
  )
  
  # output data table
  output$dynamic <- renderDataTable(
    shooting, options = list(pageLength = 30))
  
  }
  
shinyApp(ui = ui, server = server)