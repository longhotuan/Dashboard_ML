#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(usethis)
library(feather)

river_ml <- read_feather("river_ml.feather")

first_country <- which(colnames(river_ml) == 'Afghanistan')
last_country <- which(colnames(river_ml) == 'Zimbabwe')
first_lat <- which(colnames(river_ml) == 'lat_Afghanistan')
last_lat <- which(colnames(river_ml) == 'lat_Zimbabwe')
first_long <- which(colnames(river_ml) == 'long_Afghanistan')
last_long <- which(colnames(river_ml) == 'long_Zimbabwe')
period_column <- which(colnames(river_ml) == "Period")
first_theme <- which(colnames(river_ml) == "Water Quality/Pollution")
last_theme <- which(colnames(river_ml) == "Microbial")
first_tech <- which(colnames(river_ml) == "Model validation")
last_tech <- which(colnames(river_ml) == "Uncertainty analysis")

#### ui #####

ui <- dashboardPage(
    # Dashboard header ####
    dashboardHeader(title="ML applications in river management"),
    # Dashboard sidebar #### 
    dashboardSidebar(
        sidebarMenu(id="tab",
                    menuItem("About", 
                             tabName = "about",
                             icon = icon("info")),
                    menuItem("Overview", 
                             tabName = "info",
                             icon = icon("database")), 
                    menuItem("Journal info", 
                             tabName = "journal",
                             icon = icon("newspaper")),
                    menuItem("Publication info", 
                             tabName = "research",
                             icon = icon("microscope")),
                    selectInput(inputId = "year", label = "Select a year/period",
                                choices = c(All = "All", rev(levels(as.factor(river_ml$Period))),levels(as.factor(river_ml$Year))[-nlevels(as.factor(river_ml$Year))])),
                    selectInput(inputId = "id", label = "Select a machine learning", 
                                choices = c(All = "All", levels(as.factor(as.character(river_ml$id))))),
                    selectInput(inputId = "method", label = "Select a modeling method", 
                                choices = c(All = "All", colnames(river_ml)[first_tech:last_tech])),
                    selectInput(inputId = "theme", label = "Select a research theme", 
                                choices = c(All = "All", colnames(river_ml)[first_theme:last_theme]))
        )
    ),
    # Dashboard body #### 
    dashboardBody(
        tabItems(
            # About tab content ####
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12, 
                            h2("Abstract"),
                            br(),
                            h4("..."),
                            
                        )
                    ),
                    fluidRow(
                        box(width = 12, 
                            h2("Acknowledgment"),
                            br(),
                            h4("..."),
                            
                        )
                    ),
                    fluidRow(
                        box(width = 12, 
                            h2("Information"),
                            br(),
                            h4("If you find this tool useful, please cite the reference of our paper (submitted) and help spread the word. If you have questions related to the dataset, please  send us an email to ",
                               a("Long.TuanHo@UGent.com",
                                 href = "mailto: Long.TuanHo@UGent.com"))
                        )
                    ),
                    fluidRow(
                        column(6,
                               h1("Funded by"),
                               img(style = "max-width:30%",
                                   src = "Logo2.jpg")
                        ),
                        column(6, 
                               img(align = "left|bottom",
                                   style = "max-width:20%",
                                   src = "Logo.png") 
                        )
                    )
            ), # end of About tabItem
            # Info tab content ####
            tabItem(tabName = "info",
                    fluidRow(
                        valueBoxOutput("Publication"),
                        valueBoxOutput("TotalCountry"),
                        valueBoxOutput("Citation")
                    ),
                    fluidRow(
                        valueBoxOutput("DocumentType"),
                        valueBoxOutput("OpenAccess"),
                        valueBoxOutput("Totaljournal")
                    ),
                    fluidRow(
                        box(title = "Research info", width = 12, height = 700,
                            DT::dataTableOutput("table"
                                                ,  width = "100%", height = 700
                            )
                        )
                    ),
                    fluidRow(
                        box(title = "Collaboration map (circles' area proportional to the number of publications)", width = 12, 
                            leafletOutput("map", width = "100%", height = 400) #
                        )
                    )
            ), # end of info tabItem
            # Journal tab content ####
            tabItem(tabName = "journal",
                    fluidRow(
                        valueBoxOutput("Publication1"),
                        valueBoxOutput("TotalCountry1"),
                        valueBoxOutput("Citation1")
                    ),
                    fluidRow(
                        valueBoxOutput("DocumentType1"),
                        valueBoxOutput("OpenAccess1"),
                        valueBoxOutput("Totaljournal1")
                    ),
                    fluidRow(
                        box(width = 12,
                            title = "Top 20 most frequent publishers publishing publications in machine learning applications in river management",
                            plotlyOutput("topjournal")
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            title = "Proportion of open access pulications in machine learning applications in river management", 
                            plotlyOutput("Openaccess")
                        )
                    )
            ), # end of Journal tabItem
            # Research tab content ####
            tabItem(tabName = "research",
                    fluidRow(
                        valueBoxOutput("Publication2"),
                        valueBoxOutput("TotalCountry2"),
                        valueBoxOutput("Citation2")
                    ),
                    fluidRow(
                        valueBoxOutput("DocumentType2"),
                        valueBoxOutput("OpenAccess2"),
                        valueBoxOutput("Totaljournal2")
                    ),
                    fluidRow(
                        box(title = "Number of publications in machine learning applications in river management over time", width = 12,
                            plotlyOutput("Pubyear")
                        )
                    ),
                    fluidRow(
                        box(title = "Word cloud of the most common author keywords ", width = 12,
                            plotOutput("Topkw")
                        )
                    )
            ) # end tabItem of research tab
        ) # end tabItems
    ) # end dashboardbody
) # end dashboardpage

#### server ####
server <- function(input, output, session){
    # Setting reactivities #####
    df <- reactive({river_ml})
    
    #** Year ####
    df_year <- reactive({
        input$year
    })
    
    #** Machine learning ####
    choose_ml <- reactive({
        if(df_year() == "All"){
            df <- df()
            levels(as.factor(as.character(df$id)))
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            df <- df()
            ml <- df[df$Period == df_year(),]
           levels(as.factor(as.character(ml$id)))
        } else {
            df <- df()
            ml <- df[df$Year == df_year(),]
            levels(as.factor(as.character(ml$id)))
        }
    })
    
    observe({
        updateSelectInput(session, inputId = "id", label = "Select a machine learning", 
                          choices = c(All = "All", choose_ml()))
    })
    
    df_ml <- reactive({
        input$id
    })
    
    #** Method ###
    # SOMETHING WRONG HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
    choose_method <- reactive({
        if(df_year() == "All"){
            df1 <- df()
            df_year2 <- df1
            if(df_ml() == "All"){
                colnames(df_year2)[first_tech:last_tech]
            } else { 
                df_ml1 <- df_year[df_year2$id == df_ml(),]
                df_ml1 <- df_ml1[, colSums(is.na(df_ml1)) < nrow(df_ml1)]
                f_method <- which(colnames(df_ml1) == "Period") +1
                l_method <- str_which(colnames(df_ml1), "Model")[1]-1
                colnames(df_ml1)[f_method:l_method]
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            df1 <- df()
            df_year1 <- df1[df1$Period == df_year(),]
            if(df_ml() == "All"){
                df_ml1 <- df_year1
                df_ml1 <- df_ml1[, colSums(is.na(df_ml1)) < nrow(df_ml1)]
                f_method <- which(colnames(df_ml1) == "Period") +1
                l_method <- str_which(colnames(df_ml1), "Model")[1]-1
                colnames(df_ml1)[f_method:l_method]
            } else {
                df_ml1 <- df_year1[df_year1$id == df_ml(),]
                df_ml1 <- df_ml1[, colSums(is.na(df_ml1)) < nrow(df_ml1)]
                f_method2 <- which(colnames(df_ml1) == "Period") +1
                l_method2 <- str_which(colnames(df_ml1), "Model")[1]-1
                colnames(df_ml1)[f_method2:l_method2]
            }
        } else {
            df1 <- df()
            df_year2 <- df1[df1$Year == df_year(),]
            if(df_ml() == "All"){
                df_ml2 <- df_year2
                df_ml2 <- df_ml2[, colSums(is.na(df_ml2)) < nrow(df_ml2)]
                f_method2 <- which(colnames(df_ml2) == "Period") +1
                l_method2 <- str_which(colnames(df_ml2), "Model")[1]-1
                colnames(df_ml2)[f_method2:l_method2]
            } else {
                df_ml3 <- df_year2[df_year2$id == df_ml(),]
                df_ml3 <- df_ml3[, colSums(is.na(df_ml3)) < nrow(df_ml3)]
                f_method <- which(colnames(df_ml3) == "Period") +1
                l_method <- str_which(colnames(df_ml3), "Model")[1]-1
                colnames(df_ml3)[f_method:l_method]
            }
        }
        observe({
            updateSelectInput(session, inputId = "method", label = "Select a research method", 
                              choices = c(All = "All", choose_method()))
        })
        df_method <- reactive({
            input$method
        })
        
    })
    
    
}

#### Run the application ####
shinyApp(ui = ui, server = server)