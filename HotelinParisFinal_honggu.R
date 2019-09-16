library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(geosphere)
library(DT)


fulldata<-read.csv('D:/HUFS/4-1/데이터시각화/과제4/fulldata.csv')
reviewfulldata<-read.csv('D:/HUFS/4-1/데이터시각화/과제4/hotelfulldata_paris.csv')

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Hotel in Paris"),
  dashboardSidebar(
    selectInput("landmark1",label = "Landmark:", c((fulldata %>%
                                                      filter(category=='landmark'))[1])),
    sliderInput("km", label= "Radius(km):", min = 0, max = 5, value = c(0, 3), sep = "", ticks = F, round=F),
    sliderInput("hotelscore", label= "Hotelscore:", min = 0, max = 10, value = c(0, 10), sep = "", ticks = F, round=F),
    selectInput("hotelname", label='Hotel name', (unique(reviewfulldata$Hotel_Name)))
  ),
  
  
  dashboardBody(
    
    fixedRow(
      column(width = 12, div(class="col-md-6"))),
    navbarPage("Map Type",
               tabPanel("Spot",
                        mainPanel(fluidRow(box(width = 12, leafletOutput(outputId = "mymap" ))))),
               tabPanel("Clustering", mainPanel(fluidRow(box(width = 12, leafletOutput(outputId = "clusteringmap" )))))
               
    ),
    fluidRow(
      valueBoxOutput("val1")
      ,valueBoxOutput("val2")
    ),
    navbarPage("Review Type",
               tabPanel("Positive Review",
                        mainPanel(fluidRow(box(width = 20, dataTableOutput(outputId = "positivereview" ))))),
               tabPanel("Negative Review",
                        mainPanel(fluidRow(box(width = 20, dataTableOutput(outputId = "negativereview" )))))
               
    )
  )
)


server <- function(input, output){
  
  data_landmark_input<-reactive({
    aa<-input$landmark1
    
    test <-
      if(aa != "all") {
        test<-
          fulldata%>%
          filter(category=='landmark') %>%
          filter(name==aa)
      } else {
        test<-
          fulldata%>%
          filter(category=='landmark')
      }
    test<-as.data.frame(test)
    
  })
  
  data_subwaydistance_input<-reactive({
    
    landmark_name<-input$landmark1
    dis1<-input$km[1]
    dis2<-input$km[2]
    
    subway_distance<-data.frame((fulldata%>%
                                   filter(category=='subway'))$name,t(distm((fulldata%>%
                                                                               filter(category=='landmark')%>%
                                                                               filter(name==landmark_name))[3:4],
                                                                            (fulldata%>%
                                                                               filter(category=='subway'))[3:4], fun=distVincentyEllipsoid)/1000),
                                (fulldata%>%
                                   filter(category=='subway'))$lat,
                                (fulldata%>%
                                   filter(category=='subway'))$lng)
    
    
    colnames(subway_distance)<-c('station', 'subwaydistance', 'sublat', 'sublng')
    subway_distance<-subway_distance %>%
      arrange(subwaydistance)
    
    subway_distance<-subway_distance %>%
      filter(subwaydistance>dis1) %>%
      filter(subwaydistance<dis2)
    subway_distance
    subway_distance<-as.data.frame(subway_distance)
    subway_distance
    
  })
  
  data_subwaydistance2_input<-reactive({
    
    landmark_name4<-input$landmark1
    dis7<-input$km[1]
    dis8<-input$km[2]
    
    subway_distance<-data.frame((fulldata%>%
                                   filter(category=='subway'))$name,t(distm((fulldata%>%
                                                                               filter(category=='landmark')%>%
                                                                               filter(name==landmark_name4))[3:4],
                                                                            (fulldata%>%
                                                                               filter(category=='subway'))[3:4], fun=distVincentyEllipsoid)/1000),
                                (fulldata%>%
                                   filter(category=='subway'))$lat,
                                (fulldata%>%
                                   filter(category=='subway'))$lng)
    
    
    colnames(subway_distance)<-c('station', 'subwaydistance', 'sublat', 'sublng')
    subway_distance<-subway_distance %>%
      arrange(subwaydistance)
    
    subway_distance<-subway_distance %>%
      filter(subwaydistance>dis7) %>%
      filter(subwaydistance<dis8)
    subway_distance
    subway_distance<-as.data.frame(subway_distance)
    subway_distance %>%
      summarise(val2=n())
    
  })
  
  data_hoteldistance_input<-reactive({
    
    landmark_name2<-input$landmark1
    dis3<-input$km[1]
    dis4<-input$km[2]
    
    avg1<-input$hotelscore[1]
    avg2<-input$hotelscore[2]
    
    hotel_distance<-data.frame((fulldata%>%
                                  filter(category=='hotel'))$name,t(distm((fulldata%>%
                                                                             filter(category=='landmark')%>%
                                                                             filter(name==landmark_name2))[3:4],
                                                                          (fulldata%>%
                                                                             filter(category=='hotel'))[3:4], fun=distVincentyEllipsoid)/1000),
                               (fulldata%>%
                                  filter(category=='hotel'))$lat,
                               (fulldata%>%
                                  filter(category=='hotel'))$lng,
                               (fulldata%>%
                                  filter(category=='hotel'))$score)
    
    
    colnames(hotel_distance)<-c('hotelname', 'hoteldistance', 'hotlat', 'hotlng', 'avgscore')
    hotel_distance<-hotel_distance %>%
      arrange(hoteldistance)
    
    hotel_distance<-hotel_distance %>%
      filter(hoteldistance>dis3) %>%
      filter(hoteldistance<dis4) %>%
      filter(avgscore>avg1) %>%
      filter(avgscore<avg2)
    hotel_distance
    hotel_distance<-as.data.frame(hotel_distance)
    
    
    
    
  })
  
  
  data_hoteldistance2_input<-reactive({
    
    landmark_name3<-input$landmark1
    dis5<-input$km[1]
    dis6<-input$km[2]
    
    avg3<-input$hotelscore[1]
    avg4<-input$hotelscore[2]
    
    hotel_distance<-data.frame((fulldata%>%
                                  filter(category=='hotel'))$name,t(distm((fulldata%>%
                                                                             filter(category=='landmark')%>%
                                                                             filter(name==landmark_name3))[3:4],
                                                                          (fulldata%>%
                                                                             filter(category=='hotel'))[3:4], fun=distVincentyEllipsoid)/1000),
                               (fulldata%>%
                                  filter(category=='hotel'))$lat,
                               (fulldata%>%
                                  filter(category=='hotel'))$lng,
                               (fulldata%>%
                                  filter(category=='hotel'))$score)
    
    
    colnames(hotel_distance)<-c('hotelname', 'hoteldistance', 'hotlat', 'hotlng', 'avgscore')
    hotel_distance<-hotel_distance %>%
      arrange(hoteldistance)
    
    hotel_distance<-hotel_distance %>%
      filter(hoteldistance>dis5) %>%
      filter(hoteldistance<dis6) %>%
      filter(avgscore>avg3) %>%
      filter(avgscore<avg4)
    hotel_distance
    hotel_distance<-as.data.frame(hotel_distance)
    
    hotel_distance %>%
      summarise(val1=n())
    
    
  })
  
  data_hotelposreview_input<-reactive({
    hotelname1<-input$hotelname
    positivereview<-select((reviewfulldata %>%
                              filter(Hotel_Name==hotelname1)), Reviewer_Nationality, Positive_Review)
    positivereview
  })
  
  data_hotelnegreview_input<-reactive({
    hotelname2<-input$hotelname
    negativereview<-select((reviewfulldata %>%
                              filter(Hotel_Name==hotelname2)), Reviewer_Nationality, Negative_Review)
    negativereview
  })
  
  
  
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      addMarkers(data=data_landmark_input(), lat=~lat, lng=~lng, popup=~name) %>%
      addCircleMarkers(data=data_subwaydistance_input(), lat=~sublat, lng=~sublng, radius=3, popup=~paste("<strong>Station Name :</strong>", station, "<br>",
                                                                                                          "<strong>Station Distance :</strong>", gsub(' ', '.', paste(round(subwaydistance,2))),'km', "<br>")) %>%
      addCircleMarkers(data=data_hoteldistance_input(), lat=~hotlat, lng=~hotlng, color='#FF0000',radius=5, popup=~paste("<strong>Hotel Name :</strong>", hotelname, "<br>",
                                                                                                                         "<strong>Hotel Distance :</strong>", gsub(' ', '.', paste(round(hoteldistance,2))),'km', "<br>",
                                                                                                                         "<strong>Average Score :</strong>", avgscore)) %>%
      addLegend("bottomright", opacity =2, colorFactor(c("#FF0000", "#0033FF"), domain = c("Subway Stations","Hotels")), value = c("Subway Stations","Hotels"), title='Category')
    
    
    
  )
  
  
  
  
  
  
  
  
  
  
  
  
  output$clusteringmap <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      addMarkers(data=data_landmark_input(), lat=~lat, lng=~lng, popup=~name) %>%
      addCircleMarkers(data=data_subwaydistance_input(), lat=~sublat, lng=~sublng, radius=3, popup=~paste("<strong>Station Name :</strong>", station, "<br>",
                                                                                                          "<strong>Station Distance :</strong>", gsub(' ', '.', paste(round(subwaydistance,2))),'km', "<br>")) %>%
      addCircleMarkers(data=data_hoteldistance_input(), lat=~hotlat, lng=~hotlng, color='#FF0000',radius=5, clusterOptions = markerClusterOptions(), popup=~paste("<strong>Hotel Name :</strong>", hotelname, "<br>",
                                                                                                                                                                  "<strong>Hotel Distance :</strong>", gsub(' ', '.', paste(round(hoteldistance,2))),'km', "<br>",
                                                                                                                                                                  "<strong>Average Score :</strong>", avgscore)) %>%
      addLegend("bottomright", opacity =2, colorFactor(c("#FF0000", "#0033FF"), domain = c("Subway Stations","Hotels")), value = c("Subway Stations","Hotels"), title='Category')
  )
  
  
  
  
  output$val1 <- renderValueBox({
    valueBox(
      formatC(data_hoteldistance2_input()$val1)
      ,'Nearby Hotels'
      ,icon = icon("hotel",lib='font-awesome')
      ,color = "red")
    
    
  })
  
  output$val2 <- renderValueBox({
    valueBox(
      formatC(data_subwaydistance2_input()$val2)
      ,'Nearby SubwayStations'
      ,icon = icon("subway",lib='font-awesome')
      ,color = "yellow")
  })
  
  
  
  output$positivereview<- renderDataTable({
    hotelname1<-input$hotelname
    positivereview<-select((reviewfulldata %>%
                              filter(Hotel_Name==hotelname1)), Reviewer_Nationality, Positive_Review)
    positivereview
    
  })
  
  
  output$negativereview<-renderDataTable({
    hotelname2<-input$hotelname
    negativereview<-select((reviewfulldata %>%
                              filter(Hotel_Name==hotelname2)), Reviewer_Nationality, Negative_Review)
    negativereview
  })
  
}



shinyApp(ui = ui, server = server)
