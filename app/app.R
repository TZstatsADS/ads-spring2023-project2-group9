#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(readr)
library(sp)
library(rgdal)
library(leaflet)

df_counts <- read.csv('../output/df_counts.csv')
df_res <- read.csv('../output/df_res.csv')
df_vio <- read.csv('../output/Processed_Violence.csv', header=T)
################################################
################################################
grade <- read.csv("../output/grade.csv")
nyc <- read.csv("../output/nyc.csv")
coordinates(nyc) = c("lng","lat")
crs.geo1 <- CRS("+proj=longlat")
proj4string(nyc) <- crs.geo1
newyork <- readOGR('../data/nyu_2451_34509.shp')
proj4string(newyork) <- crs.geo1
nyc_agg <- aggregate(x=nyc["avg_rent"], by=newyork, FUN=function(x) round(mean(x), 2))
coordinates(grade) <- c("lng","lat")
proj4string(grade) <- crs.geo1 
nyc_agg2 <- aggregate(x=grade["tot_A"], by=newyork, FUN=function(x) sum(x))
bins <- c(0, 1500, 2500, 3000, 3500, 4100)
qpal = colorBin("YlGnBu", nyc_agg$avg_rent, bins=bins,na.color = "transparent")
labels <- sprintf(
  ifelse(!is.na(nyc_agg$avg_rent),
         "<strong>Price: $%s<br>Grade A Restaurants: %s</strong>",
         ""),
  nyc_agg$avg_rent,
  nyc_agg2$tot_A
) %>% lapply(htmltools::HTML)
##########################################################
##########################################################


colnames(df_vio) <- c("Boro", "Year", "Family Felony Assaults", "Domestic Violence Felony Assaults", "Domestic Violence Incident", "Rape")
###traffic
traffic <- read.csv("morningtraffic.csv")
colnames(traffic) <- c("time", "Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island")
traffic2 <- read.csv("eveningtraffic.csv")
colnames(traffic2) <- c("time", "Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island")

### colors
color <- c("cornflowerblue", 'firebrick2', "coral", "blue", "yellow", "pink","green", "white","black",
                           "grey", "orange", "violet","blueviolet","darkseagreen1","burlywood1")
                           

ui <- fluidPage(
  navbarPage(
    title = HTML("<b>UrbanScout</b>"),
    tabPanel(title = HTML("<b>Intro</b>"),
             fluidRow(
               tags$img(
                 src = "https://cdn.parklanenewyork.com/s3-uploads/20210824101616/centralparkview-copy-1920x1280.jpg",
                 style = "opacity: 0.85", class = "background", height ="70%", width="100%"
               )),
             absolutePanel(
               style = "background-color: white", top = "30%", left = "20%", right = "20%", height = 170,
               tags$p(style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 120%",
                      HTML("Nearly every NYC college student is faced with the task of finding the perfect place to live.<br>What is the cost?<br>Is the neighborhood busy?<br>Is it safe?<br>Does it have good places to eat and study?<br>...<br>There are bunches of questions a city student must answer to ensure a smooth and enjoyable living experience.<br>The purpose of this app is to help college students easily visualize rental prices in NYC, along with other lifestyle statistics (e.g. crime rates, traffic, restaurants, etc.) to help them make an informed decision about their accommodations.<br><br>After all, better homes means better learners.")
               ))
    ),
    navbarMenu(title = HTML("<b>City Walk</b>"),
               tabPanel("Rental Price And Grade A Restaurants Map", leafletOutput("map", height = "95vh")),
               tabPanel("Grade A Restaurants", plotOutput("plot", height = "80vh"))),
    tabPanel(title = HTML("<b>Traffic</b>"),
             titlePanel(title = "New York Traffic Information"),
             absolutePanel(
               top = 0, left = 0, bottom = 0, right = 0,
               tags$img(
                 src = "https://images.pexels.com/photos/290386/pexels-photo-290386.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2",
                 style = "opacity: 0.25", class = "background", height = "130%", width = "130%"
               )),          
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput(inputId = "loc", label = "Boro:", choices = colnames(traffic)[-1]),
                             selectInput(inputId = "df", label = "Time:", choices = c("Morning", "Evening")),
                             selectInput(inputId = "col", label = "Color:", choices = color)
                           ),
                           mainPanel(plotOutput("scatter_plot"))
             )),
    tabPanel(title = HTML("<b>Violence</b>"),
             titlePanel(title = "New York Violence Records"),
             absolutePanel(
               top = 0, left = 0, bottom = 0, right = 0,
               tags$img(
                 src = "https://images.pexels.com/photos/290386/pexels-photo-290386.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2",
                 style = "opacity: 0.25", class = "background", height = "130%", width = "130%"
               )),           
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput(inputId = "area", label = "Area:", choices = c("Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"))
                           ),
                           mainPanel(plotOutput("hist"))
             )),
    tabPanel(title = HTML("<b>Reference</b>"),
             fluidRow(
               tags$img(
                 src = "https://images.pexels.com/photos/290386/pexels-photo-290386.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2",
                 style = "opacity: 0.4", class = "background", height = "100%", width = "100%"
               )),
             absolutePanel(id = "text", class = "foreground-content",
                           top = "20%", left = "15%", right = "15%", width = "70%", height = 200, fixed=FALSE, draggable = FALSE,
                           fluidRow(
                             tags$h2("Data Sources"),
                             tags$p("For rental price information, please click on ",
                                    a("Rental in NYC",
                                      href = "https://data.cityofnewyork.us/City-Government/DOF-Condominium-Comparable-Rental-Income-in-NYC/9ck6-2jew",
                                      style = "font-weight:bold;color:black"),
                                    style="font-weight:bold;color:black"),
                             tags$p("For transportation volume information, please click on ",
                                    a("Transportation in NYC",
                                      href = "https://data.cityofnewyork.us/Transportation/Automated-Traffic-Volume-Counts/7ym2-wayt",
                                      style = "font-weight:bold;color:black"),
                                    style="font-weight:bold;color:black"),
                             tags$p("For restaurant inspection information, please click on ",
                                    a("Restaurants in NYC",
                                      href = "https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j",
                                      style = "font-weight:bold;color:black"),
                                    style="font-weight:bold;color:black"),
                             tags$p("For violence information, please click on ",
                                    a("Violence in NYC",
                                      href = "https://data.cityofnewyork.us/Public-Safety/Family-Violence-Related-Snapshots-New-York-City-Co/a35y-93e7",
                                      style = "font-weight:bold;color:black"),
                                    style="font-weight:bold;color:black"),
                             br(),   
                             tags$h2("Contributors"),
                             tags$p("Xinming Pan, Statistics Department, Columbia University", style = "font-weight:bold;color:black"),
                             tags$p("Ananya Tinaikar, Statistics Department, Columbia University", style = "font-weight:bold;color:black"),
                             tags$p("Hongyan Zhou, Statistics Department, Columbia University", style = "font-weight:bold;color:black"),
                             tags$p("Wenchang Zhu, Statistics Department, Columbia University", style = "font-weight:bold;color:black"),
                             tags$p("Yiming Zhu, Statistics Department, Columbia University", style = "font-weight:bold;color:black"),
                             br(),
                             tags$h2("GitHub Repository"),
                             tags$p("For more details, please visit our ", 
                                    a("GitHub Repository",
                                      href = "https://github.com/TZstatsADS/ads-spring2023-project2-group9",
                                      style = "font-weight:bold;color:black"),
                                    style="font-weight:bold;color:black")
                           ))
    )
  )
)


server = function(input, output) {
  
  output$map <- renderLeaflet({
    
    leaflet(nyc_agg) %>%
      setView(lng = -74, lat = 40.7128, zoom = 11) %>%
      addProviderTiles("CartoDB") %>% 
      addPolygons(stroke = TRUE, opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
                  color = "white", fillColor = ~qpal(avg_rent), weight = 1, label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(values = ~avg_rent, pal=qpal, title = "Rental price")
  })
  
  output$scatter_plot <- renderPlot({
    if(input$df == "Morning"){df = traffic}
    else{df = traffic2}
    plot(as.POSIXct(df$time), df[input$loc][,1], type = "o", col = input$col, cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.8,
         pch = "o", ylab = "Counts", xlab = 'Month', lwd = 3, lty = 1, main = paste0(input$df, " Daily Traffic Volume Counts in ", input$loc))
  })
  
  output$plot <- renderPlot({
    ggplot() + 
      geom_col(aes(x = as.factor(BORO), y = 1, fill = grade), data = df_res, group = 1, position = "stack", width = 0.5)+ 
      geom_line(aes(x = as.factor(df_counts$Borough), y = 10000*df_counts$Ratios, group = 1), color = "red", lwd = 1)+ 
      geom_point(aes(x = as.factor(df_counts$Borough), y = 10000*df_counts$Ratios), color = "black") +
      scale_fill_manual(values=c("OTHER" = '#E0A96D', "A" = '#DDC3A5', "B" = 'gray24', "C" = 'seashell1')) +
      labs(title = "Restaurants Grade Distribution", x = "Borough", y = "Counts") +
      guides(fill = guide_legend(title = "GRADE")) +
      scale_y_continuous(name = "Counts", limits = c(0, 8000)) +
      scale_y_continuous(sec.axis = sec_axis(~(.+2000)/10000, name = "Ratios of Grade A")) +
      geom_bar(width = 0.2) +
      theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 17), legend.text = element_text(size = 12))
    
  })
  
  output$hist <- renderPlot({
    df_filtered = df_vio %>% 
      filter(Boro == input$area) %>% 
      pivot_longer(cols = "Family Felony Assaults":"Rape", names_to = "crime_type", values_to = "count")
    
    ggplot(df_filtered, aes(x = Year, y = count, fill = crime_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("goldenrod1", "darkolivegreen3", "lightskyblue1", "sienna1")) +
      ggtitle(paste0("Violence in ", input$area)) +
      labs(x = "Year", y = "Count", fill = "Crime Type") +
      theme(plot.title = element_text(hjust  =0.5, size = 25, face = "bold"),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 17),
            legend.text = element_text(size = 12), legend.title = element_text(size = 14))
  })
  
  output$table = renderDataTable({
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)