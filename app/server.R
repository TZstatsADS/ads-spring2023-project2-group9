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


df_counts <- read_csv('df_counts.csv')
df_res <- read_csv('df_res.csv')
df_vio <- read.csv('Processed_Violence.csv', header=T)
################################################
################################################
grade_rent <- read.csv("grade_rent.csv")
coordinates(grade_rent) = c("lng","lat")
crs.geo1 = CRS("+proj=longlat")  
proj4string(grade_rent) = crs.geo1 
newyork <- readOGR('nyu_2451_34509.shp')
proj4string(newyork) = crs.geo1
nyc_agg = aggregate(x=grade_rent["avg_rent"],by=newyork,FUN=function(x) round(mean(x), 2))
nyc_agg2 = aggregate(x=grade_rent["tot_A"],by=newyork,FUN=function(x) sum(x))
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
                           

ui = navbarPage(
  title = "Project2_group9",
  tabPanel(title = "Intro", includeMarkdown("Introduction.Rmd")),
  tabPanel(title = "Violence",
           titlePanel(title = "New York Violence Information"),
           sidebarLayout(
             position = "right",
             sidebarPanel(
               selectInput(inputId = "area", label = "Area:", choices = c("Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"))
             ),
             mainPanel(plotOutput("hist"))
           )
           
  ),
  tabPanel(title = "Traffic",
           titlePanel(title = "New York Traffic Information"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "loc", label = "Boro:", choices = colnames(traffic)[-1]),
               selectInput(inputId = "df", label = "Time:", choices = c("Morning", "Evening")),
               selectInput(inputId = "col", label = "Color:", choices = color)
             ),
             mainPanel(plotOutput("scatter_plot"))
           )),
  tabPanel(
    title = "Restaurant",
    titlePanel(title = "New York Resturant infomation"),
    sidebarLayout(
      sidebarPanel(
        "Pick Any Color You Like.",
        selectInput(inputId = "col1", label = "Other:", choices = sample(color,length(color),replace = FALSE)),
        selectInput(inputId = "col2", label = "A:", choices = sample(color,length(color),replace = FALSE)),
        selectInput(inputId = "col3", label = "B:", choices = sample(color,length(color),replace = FALSE)),
        selectInput(inputId = "col4", label = "C:", choices = sample(color,length(color),replace = FALSE)),
      ),
      mainPanel(plotOutput("plot"))
    )),
  tabPanel(title = "Map",
           titlePanel(title = "Map Information"),
           mainPanel(leafletOutput("map"))),
  tabPanel(title = "Reference", includeMarkdown("Reference.Rmd"))
)


server = function(input, output) {
  
  output$map <- renderLeaflet({
    
    leaflet(nyc_agg) %>%
      addProviderTiles("CartoDB") %>% 
      addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
                  color="white",fillColor = ~qpal(avg_rent),weight = 1, label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(values=~avg_rent,pal=qpal,title="Rental price")
  })
  
  
  output$scatter_plot <- renderPlot({
    if(input$df== "Morning"){df = traffic}
    else{df = traffic2}
    plot(as.POSIXct(df$time), df[input$loc][,1], type="o", col=input$col, 
         pch="o", ylab="Counts",xlab='Month', lwd=3,lty=1,main= paste0(input$df, " Daily Traffic Volume Counts in ", input$loc))
  })
  
  output$plot <- renderPlot({
    ggplot() + 
      geom_col(aes(x = as.factor(BORO), y = 1, fill = grade), data = df_res, group = 1, position = "stack")+ 
      geom_line(aes(x = as.factor(df_counts$Borough), y = 39000*df_counts$Ratios, group = 1), color = "red",lwd=1)+ 
      geom_point(aes(x = as.factor(df_counts$Borough), y = 39000*df_counts$Ratios), color = "black") +
      scale_fill_manual(values=c("OTHER"=input$col1, "A"=input$col2, "B"=input$col3, "C"=input$col4)) +
      labs(title= "Restaurants Grade Distribution", x = "Borough", y = "Counts") +
      guides(fill=guide_legend(title="GRADE")) +
      scale_y_continuous(sec.axis=sec_axis(~.*1/400, name="Ratios of Grade A")) +
      theme(plot.title = element_text(hjust = 0.5))
    
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
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$table = renderDataTable({
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


