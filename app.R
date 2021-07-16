#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(bsplus)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(COVID19)
library(ggplot2)
library(shinybusy)
library(DT)
library(icon)
library(plotly)
library(ggpubr)

#Obtaining our Covid Values..
Nigeria <- COVID19::covid19("Nigeria")
Egypt <- COVID19::covid19("Egypt")
SA <- COVID19::covid19("South Africa")
Algeria <- COVID19::covid19("Algeria")
Morocco <- COVID19::covid19("Morocco")
Morocco <- Morocco %>% select(c(1:6,10))
Nigeria <- Nigeria %>% select(c(1:6,10))
Egypt <- Egypt %>% select(c(1:6,10))
Algeria <- Algeria %>% select(c(1:6,10))
SA <- SA %>% select(c(1:6,10))

countries <- c("Egypt", "Nigeria", "South Africa", "Algeria", "Morocco")

LatestDate <- format(Sys.Date() -2-2, '%Y-%m-%d')
DTCOVID19Latest <- function(id){id %>% filter(date == LatestDate) } 


flags <- c(
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/eg.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ng.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/za.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ma.svg"
)


ui <- dashboardPage(
    dashboardHeader(tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/nosa-ugowe-02010318/", icon("linkedin"))),
        title = "EFFECTS OF COVID19 ON AFRICAS TOP 5 ECONOMIES", titleWidth = 600),
    dashboardSidebar(width = 310,
                     sidebarMenu(
                         #img(src = "au.png", height = 150, width = 300)),
                         bs_carousel(id = "the_beatles", use_indicators = TRUE) %>%
                             bs_append(
                                 content = bs_carousel_image(src = "au.png"),
                                 
                             ) %>%
                             bs_append(
                                 content = bs_carousel_image(src = "au.png")
                             
                             
                             
                                 
                             ),
                         
                         id = "tabs",
                         menuItem("Guidotti E, Ardia D (2020). COVID-19 Data Hub", icon = icon("info"), tabName = "about")),
                        
                     
                     pickerInput("countries", multiple = F,
                                 choices = countries,
                                 
                                 choicesOpt = list(content =  
                                                       mapply(countries, flags, FUN = function(country, flagUrl) {
                                                           HTML(paste(
                                                               tags$img(src=flagUrl, width=40, height=40),
                                                               country
                                                           ))
                                                       }, SIMPLIFY = TRUE, USE.NAMES = TRUE)
                                                   
                                 ))),
    
    
         
       
        
    dashboardBody(
        tags$head(tags$style(HTML('
                               
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #420420;font-family:"Open Sans";font-weight: bold;font-size:22px;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #000000;
                              }
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #000000;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #000000;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #000000;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #000000;
                              }
                              
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #000000;
                              color: #FFFFFF;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #000000;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #000000;
                              }
                              '
        ))),
        
        fluidRow(
            
            infoBoxOutput("Info1", width = 3),
            infoBoxOutput("Info2", width = 3),
            infoBoxOutput("Info3", width = 3),
            infoBoxOutput("Info4", width = 3)
        ), 
        
        fluidRow(
        
        column(width = 12,
               box( 
                   add_busy_spinner(spin = "intersecting-circles"),
                   addSpinner(plotOutput('Data2'), spin = "bounce", color = "#000000"), height = 400, width = 13 
               ),
        
        
        column(width = 12,
               box( 
                   add_busy_spinner(spin = "intersecting-circles"),
                   addSpinner(dataTableOutput('Data1'), spin = "bounce", color = "#000000"), height = 500, width = 13
               )),
               
               
               
               
               
               
               
               
        ))))





       
    


server <- function(input, output) { 
    
    DTCOVID19 <- function(id){ DT::datatable(
        id, class = 'cell-border stripe', extensions = 'Buttons', options = list(dom = 'Bfrtip',pageLength = 10,buttons = c('excel','pdf','print'),
                                                                                 initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
        ))%>% formatStyle('date',  color = 'black', backgroundColor = '#888C46', fontWeight = 'bold')%>% formatStyle('tests',  color = 'black', backgroundColor = '#888C46', fontWeight = 'bold')%>% formatStyle('confirmed',  color = 'black', backgroundColor = '#888C46', fontWeight = 'bold')%>% formatStyle('recovered',  color = 'black', backgroundColor = '#888C46', fontWeight = 'bold')%>% formatStyle('deaths',  color = 'black', backgroundColor = '#888C46', fontWeight = 'bold')%>% formatStyle('population',  color = 'black', backgroundColor = '#888C46', fontWeight = 'bold')
        
    }
    
    #MAIN DATASOURCE DT
    datasourceinput1 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            DTCOVID19(Egypt)
           
            
        } else if(input$countries == "South Africa"){
            DTCOVID19(SA)
          
            
        }else if(input$countries == "Nigeria"){
            
            DTCOVID19(Nigeria)
            
        }else if(input$countries == "Morocco"){
            DTCOVID19(Morocco)
            
        }else {
            
           DTCOVID19(Algeria)
        }
        
    })
    

    output$Data1 <- renderDataTable({
        
        datasourceinput1()
    })
    
    
    #FIRST INFOBOX
    datasourceinput2 <- reactive({
    req (input$countries)
    if(input$countries == "Egypt"){
        
        m <- "% of Recovered Cases"
        return(m)
        
        
    } else if(input$countries == "South Africa"){
        m <- "% of Recovered Cases"
        return(m)
        
        
        
    }else if(input$countries == "Nigeria"){
        
        m <- "% of Recovered Cases"
        return(m)
        
        
    }else if(input$countries == "Morocco"){
        m <- "% of Recovered Cases"
        return(m)
        
        
    }else {
        
        m <- "% of Recovered Cases"
        return(m)
        
    }
    
})
   
    datasourceinput3 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
             EgyptLatest <- DTCOVID19Latest(Egypt)
             m <- (EgyptLatest$recovered/EgyptLatest$confirmed)*100
             m = signif(m, digits = 3)
             return(m)
            
        } else if(input$countries == "South Africa"){
            SALatest <- DTCOVID19Latest(SA)
            m <- (SALatest$recovered/SALatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Nigeria"){
            
            
            NigeriaLatest <- DTCOVID19Latest(Nigeria)
            m <- (NigeriaLatest$recovered/NigeriaLatest$confirmed)*100
            m = signif(m, digits = 3)
             return(m)
            
        }else if(input$countries == "Morocco"){
            MoroccoLatest <- DTCOVID19Latest(Morocco)
            m <- (MoroccoLatest$recovered/MoroccoLatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else {
            
            AlgeriaLatest <- DTCOVID19Latest(Algeria)
            m <- (AlgeriaLatest$recovered/AlgeriaLatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
        }
        
    })
    
    output$Info1 <- renderInfoBox({
        invalidateLater(7200000)
        infoBox(
            datasourceinput2(),paste0(datasourceinput3(), "%"), icon = icon ("hands-wash", class = "fa-spins"),
            color = "black",fill = TRUE
        )
        
        
    })
    
    #SECOND INFOBOX
    datasourceinput4 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            m <- "% of Deaths "
            return(m)
            
            
        } else if(input$countries == "South Africa"){
            m <- "% of Deaths "
            return(m)
            
            
            
        }else if(input$countries == "Nigeria"){
            
            m <- "% of Deaths "
            return(m)
            
            
        }else if(input$countries == "Morocco"){
            m <- "% of Deaths "
            return(m)
            
            
        }else {
            
            m <- "% of Deaths "
            return(m)
            
        }
        
    })
    
    
    datasourceinput5 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            EgyptLatest <- DTCOVID19Latest(Egypt)
            m <- (EgyptLatest$deaths/EgyptLatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
            
        } else if(input$countries == "South Africa"){
            SALatest <- DTCOVID19Latest(SA)
            m <- (SALatest$deaths/SALatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Nigeria"){
            
            
            NigeriaLatest <- DTCOVID19Latest(Nigeria)
            m <- (NigeriaLatest$deaths/NigeriaLatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Morocco"){
            MoroccoLatest <- DTCOVID19Latest(Morocco)
            m <- (MoroccoLatest$deaths/MoroccoLatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else {
            
            AlgeriaLatest <- DTCOVID19Latest(Algeria)
            m <- (AlgeriaLatest$deaths/AlgeriaLatest$confirmed)*100
            m = signif(m, digits = 3)
            return(m)
        }
        
    })
    
    
    output$Info2 <- renderInfoBox({
        invalidateLater(7200000)
        infoBox(
            datasourceinput4(),paste0(datasourceinput5(), "%"), icon = icon ("head-side-mask", class = "fa-spins"),
            color = "black",fill = TRUE
        )
        
        
    })
    
    
    
    #THIRD INFOBOX
    datasourceinput6 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            m <- "% OF  RECOVERIES Vs Population Size"
            return(m)
            
            
        } else if(input$countries == "South Africa"){
            m <- "% OF  RECOVERIES Vs Population Size"
            return(m)
            
            
            
        }else if(input$countries == "Nigeria"){
            
            m <- "% OF  RECOVERIES Vs Population Size"
            return(m)
            
            
        }else if(input$countries == "Morocco"){
            m <- "% OF  RECOVERIES Vs Population Size"
            return(m)
            
            
        }else {
            
            m <- "% OF  RECOVERIES Vs Population Size"
            return(m)
            
        }
        
    })
    
    
    datasourceinput7 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            EgyptLatest <- DTCOVID19Latest(Egypt)
            m <- (EgyptLatest$recovered/EgyptLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        } else if(input$countries == "South Africa"){
            SALatest <- DTCOVID19Latest(SA)
            m <- (SALatest$recovered/SALatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Nigeria"){
            
            
            NigeriaLatest <- DTCOVID19Latest(Nigeria)
            m <- (NigeriaLatest$recovered/NigeriaLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Morocco"){
            MoroccoLatest <- DTCOVID19Latest(Morocco)
            m <- (MoroccoLatest$recovered/MoroccoLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else {
            
            AlgeriaLatest <- DTCOVID19Latest(Algeria)
            m <- (AlgeriaLatest$recovered/AlgeriaLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
        }
        
    })
    
    
    output$Info3 <- renderInfoBox({
        invalidateLater(7200000)
        infoBox(
            datasourceinput6(),paste0(datasourceinput7(), "%"), icon = icon ("ambulance", class = "fa-spins"),
            color = "black",fill = TRUE
        )
        
        
    })
    
    
    #FOURTH INFOBOX
    datasourceinput8 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            m <- "% of Deaths  Vs Population Size"
            return(m)
            
            
        } else if(input$countries == "South Africa"){
            m <- "% of Deaths  Vs Population Size"
            return(m)
            
            
            
        }else if(input$countries == "Nigeria"){
            
            m <- "% of Deaths  Vs Population Size"
            return(m)
            
            
        }else if(input$countries == "Morocco"){
            m <- "% of Deaths  Vs Population Size"
            return(m)
            
            
        }else {
            
            m <- "% of Deaths  Vs Population Size"
            return(m)
            
        }
        
    })
    
    
    datasourceinput9 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            
            EgyptLatest <- DTCOVID19Latest(Egypt)
            m <- (EgyptLatest$deaths/EgyptLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        } else if(input$countries == "South Africa"){
            SALatest <- DTCOVID19Latest(SA)
            m <- (SALatest$deaths/SALatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Nigeria"){
            
            
            NigeriaLatest <- DTCOVID19Latest(Nigeria)
            m <- (NigeriaLatest$deaths/NigeriaLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else if(input$countries == "Morocco"){
            MoroccoLatest <- DTCOVID19Latest(Morocco)
            m <- (MoroccoLatest$deaths/MoroccoLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
            
        }else {
            
            AlgeriaLatest <- DTCOVID19Latest(Algeria)
            m <- (AlgeriaLatest$deaths/AlgeriaLatest$population)*100
            m = signif(m, digits = 3)
            return(m)
        }
        
    })
    
    
    output$Info4 <- renderInfoBox({
        invalidateLater(7200000)
        infoBox(
            datasourceinput8(),paste0(datasourceinput9(), "%"), icon = icon ("viruses", class = "fa-spin"),
            color = "black",fill = TRUE
        )
        
        
    })
    
    
    ##GGPLOTS
    
    datasourceinput10 <- reactive({
        req (input$countries)
        if(input$countries == "Egypt"){
            a <- ggplot(Egypt, aes(Egypt$date, Egypt$deaths)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED DEATHS IN EGYPT", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Death ")
            
            b <- ggplot(Egypt, aes(Egypt$date, Egypt$recovered)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED RECOVERIES IN EGYPT", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Recovered Cases")
            
            k <- ggarrange(a,b, nrow = 2)
            
            return(k)
           
            
            
        } else if(input$countries == "South Africa"){
            a <- ggplot(SA, aes(SA$date, SA$deaths)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED DEATHS IN SOUTH AFRICA", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Death ")
            
            b <- ggplot(SA, aes(SA$date, SA$recovered)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED RECOVERIES IN SOUTH AFRICA", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Recovered Cases")
            
            k <- ggarrange(a,b, nrow = 2)
            
            return(k)
            
            
        }else if(input$countries == "Nigeria"){
            
            a <- ggplot(Nigeria, aes(Nigeria$date, Nigeria$deaths)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED DEATHS IN NIGERIA", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Death ")
            
            b <- ggplot(Nigeria, aes(Nigeria$date, Nigeria$recovered)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED RECOVERIES IN NIGERIA", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Recovered Cases")
            
            k <- ggarrange(a,b, nrow = 2)
            
            return(k)
            
        }else if(input$countries == "Morocco"){
            a <- ggplot(Morocco, aes(Morocco$date, Morocco$deaths)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED DEATHS IN MOROCCO", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Death ")
            
            b <- ggplot(Morocco, aes(Morocco$date, Morocco$recovered)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED RECOVERIES IN MOROCCO", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Recovered Cases")
            
            k <- ggarrange(a,b, nrow = 2)
            
            return(k)
            
        }else {
            
            a <- ggplot(Algeria, aes(Algeria$date, Algeria$deaths)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED DEATHS IN ALGERIA", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Death ")
            
            b <- ggplot(Algeria, aes(Algeria$date, Algeria$recovered)) + geom_col(color = 'red') + theme_dark(base_size = 11) + xlab('dates') + scale_y_continuous(labels = scales::label_comma(big.mark = ",")) + ggtitle("COVID19 RELATED RECOVERIES IN ALGERIA", subtitle = Sys.Date() -2)+ theme(plot.title = element_text(color = "red", size = 11, face = "italic", hjust = 0.5),plot.subtitle = element_text(color = "blue",hjust = 0.5))+ ylab("Recovered Cases")
            
            k <- ggarrange(a,b, nrow = 2)
            
            return(k)
        }
        
    })
    
    
    output$Data2 <- renderPlot({
        
        print(datasourceinput10())
        
    })
    
}

shinyApp(ui, server)
