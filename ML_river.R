#### Global R ####

library(scales)
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
library(rworldmap)
library(usethis)
library(ggwordcloud)
library(colorspace)
library(tableHTML)
library(feather)


river_ml <- read_feather("river_ml_new.feather")
river_ml <- river_ml %>% filter(Year < 2021 & Year > 1979)
first_country <- which(colnames(river_ml) == 'Afghanistan')
last_country <- which(colnames(river_ml) == 'Zimbabwe')
first_lat <- which(colnames(river_ml) == 'lat_Afghanistan')
last_lat <- which(colnames(river_ml) == 'lat_Zimbabwe')
first_long <- which(colnames(river_ml) == 'long_Afghanistan')
last_long <- which(colnames(river_ml) == 'long_Zimbabwe')

period_column <- which(colnames(river_ml) == "Period")

first_tech <- which(colnames(river_ml) == "Model validation")
last_tech <- which(colnames(river_ml) == "Other methods")

first_theme <- which(colnames(river_ml) == "Water Quality/Pollution")
last_theme <- which(colnames(river_ml) == "Misc")

#### ui #####

ui <- dashboardPage(
    # Dashboard header ####
    dashboardHeader(title="ML applications"),
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
                                choices = c(All = "All", rev(levels(as.factor(river_ml$Period))),rev(levels(as.factor(river_ml$Year)))[-1])),
                    selectInput(inputId = "id", label = "Select a machine learning", 
                                choices = c(All = "All", levels(as.factor(as.character(river_ml$id))))),
                    selectInput(inputId = "method", label = "Select a modeling method", 
                                choices = c(All = "All", colnames(river_ml)[first_tech:last_tech])),
                    selectInput(inputId = "theme", label = "Select a research topic", 
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
                        box(title = "Research info", width = 12, height = 700, DT::dataTableOutput("table", 
                                                                                                   width = "100%", height = 700)
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
    
    #** Method ####
    choose_method <- reactive({
        if(df_year() == "All"){
            df1 <- df()
            df_year2 <- df1
            if(df_ml() == "All"){
                colnames(df_year2)[first_tech:last_tech]
            } else { 
                df_ml1 <- df_year2[df_year2$id == df_ml(),]
                df_ml2 <- df_ml1[, colSums(is.na(df_ml1)) < nrow(df_ml1)]
                f_method <- str_which(colnames(df_ml2), "Model")
                l_method <- ncol(df_ml2)
                colnames(df_ml2)[f_method:l_method]
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            df1 <- df()
            df_year1 <- df1[df1$Period == df_year(),]
            if(df_ml() == "All"){
                df_ml1 <- df_year1
                df_ml1 <- df_ml1[, colSums(is.na(df_ml1)) < nrow(df_ml1)]
                f_method <- str_which(colnames(df_ml1), "Model")
                l_method <- ncol(df_ml1)
                colnames(df_ml1)[f_method:l_method]
            } else {
                df_ml1 <- df_year1[df_year1$id == df_ml(),]
                df_ml1 <- df_ml1[, colSums(is.na(df_ml1)) < nrow(df_ml1)]
                f_method <- str_which(colnames(df_ml1), "Model")
                l_method <- ncol(df_ml1)
                colnames(df_ml1)[f_method:l_method]
            }
        } else {
            df1 <- df()
            df_year2 <- df1[df1$Year == df_year(),]
            if(df_ml() == "All"){
                df_ml2 <- df_year2
                df_ml2 <- df_ml2[, colSums(is.na(df_ml2)) < nrow(df_ml2)]
                f_method <- str_which(colnames(df_ml2), "Model")
                l_method <- ncol(df_ml2)
                colnames(df_ml2)[f_method:l_method]
            } else {
                df_ml3 <- df_year2[df_year2$id == df_ml(),]
                df_ml3 <- df_ml3[, colSums(is.na(df_ml3)) < nrow(df_ml3)]
                f_method <- str_which(colnames(df_ml3), "Model")
                l_method <- ncol(df_ml3)
                colnames(df_ml3)[f_method:l_method]
            }
        }
    })
    observe({
        updateSelectInput(session, inputId = "method", label = "Select a modeling method", 
                          choices = c(All = "All", choose_method()))
    })
    df_method <- reactive({
        input$method
    })
    
    #** Theme ####
    
    choose_topic <- reactive({
        if(df_year() == "All"){
            df1 <- df()
            df_year2 <- df1 # Year level
            if(df_ml() == "All"){
                df_ml1 <- df_year2 # machine learning level 
                if(df_method() == "All"){
                    colnames(df_ml1)[first_theme:last_theme] # modeling method level
                } else {
                    df_method1 <- df_ml1 # research theme level
                    df_theme <- df_method1[as.vector(!is.na(df_method1[, colnames(df_method1) == df_method()])), ]
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                }
            } else { 
                df_ml1 <- df_year2[df_year2$id == df_ml(),]
                if(df_method() == "All"){
                    df_method1 <- df_ml1
                    df_theme <- df_method1
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                } else {
                    df_method1 <- df_ml1
                    df_theme <- df_method1[as.vector(!is.na(df_method1[, colnames(df_method1) == df_method()])), ]
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                }
                
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            df1 <- df()
            df_year2 <- df1[df1$Period == df_year(),]
            if(df_ml() == "All"){
                df_ml1 <- df_year2 # machine learning level 
                if(df_method() == "All"){
                    colnames(df_ml1)[first_theme:last_theme] # modeling method level
                } else {
                    df_method1 <- df_ml1 # research theme level
                    df_theme <- df_method1[as.vector(!is.na(df_method1[, colnames(df_method1) == df_method()])), ]
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                }
            } else { 
                df_ml1 <- df_year2[df_year2$id == df_ml(),]
                if(df_method() == "All"){
                    df_method1 <- df_ml1
                    df_theme <- df_method1
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                } else {
                    df_method1 <- df_ml1
                    df_theme <- df_method1[as.vector(!is.na(df_method1[, colnames(df_method1) == df_method()])), ]
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                }
                
            }
        } else {
            df1 <- df()
            df_year2 <- df1[df1$Year == df_year(),]
            if(df_ml() == "All"){
                df_ml1 <- df_year2 # machine learning level 
                if(df_method() == "All"){
                    colnames(df_ml1)[first_theme:last_theme] # modeling method level
                } else {
                    df_method1 <- df_ml1 # research theme level
                    df_theme <- df_method1[as.vector(!is.na(df_method1[, colnames(df_method1) == df_method()])), ]
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                }
            } else { 
                df_ml1 <- df_year2[df_year2$id == df_ml(),]
                if(df_method() == "All"){
                    df_method1 <- df_ml1
                    df_theme <- df_method1
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                } else {
                    df_method1 <- df_ml1
                    df_theme <- df_method1[as.vector(!is.na(df_method1[, colnames(df_method1) == df_method()])), ]
                    df_theme <- df_theme[, colSums(is.na(df_theme)) < nrow(df_theme)]
                    f_theme <- which(colnames(df_theme) == "Period") + 1
                    if(sum(str_count(colnames(df_theme), pattern = "Model")) > 0){
                        l_theme <- str_which(colnames(df_theme), "Model")[1] - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "Hyperparameter")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "Hyperparameter") - 1 
                    } else if(sum(str_count(colnames(df_theme), pattern = "analysis")) > 0) {
                        l_theme <- str_which(colnames(df_theme), "analysis")[1] - 1 
                    } else {
                        l_theme <- ncol(df_theme) - 1 
                    }
                    colnames(df_theme)[f_theme:l_theme]
                }
                
            }
        }
    })
    observe({
        updateSelectInput(session, inputId = "theme", label = "Select a research topic", 
                          choices = c(All = "All", choose_topic()))
    })
    df_theme <- reactive({
        input$theme
    })
    
    # Output valuebox in Info tab ####
    output$Publication <- renderValueBox({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selecteddata), big.mark = ","),
            subtitle = "Total number of publications",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    
    output$TotalCountry <- renderValueBox({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
       
        number_country <- str_which(colnames(selecteddata), "lat")[1] - which(colnames(selecteddata) == "EID") + 1 
        valueBox(
            value = number_country,
            subtitle = "Total number of countries",
            icon = icon("flag"),
            color = "yellow"
        )
        
    })
    output$Citation <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value =  prettyNum(sum(selecteddata$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citations",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = length(levels(as.factor(selecteddata$`Document Type`))),
            subtitle = "Document types",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = round(sum(!is.na(selecteddata$`Access Type`))*100/(sum(is.na(selecteddata$`Access Type`))+sum(!is.na(selecteddata$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publications",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = prettyNum(nlevels(as.factor(selecteddata$`Source title`)), big.mark = ","),
            subtitle = "Total number of journals",
            icon = icon("book"),
            color = "maroon"
        )
    })
    
    # Output valuebox in Journal tab ####
    output$Publication1 <- renderValueBox({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selecteddata), big.mark = ","),
            subtitle = "Total number of publications",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry1 <- renderValueBox({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        number_country <- str_which(colnames(selecteddata), "lat")[1] - which(colnames(selecteddata) == "EID") + 1 
        valueBox(
            value = number_country,
            subtitle = "Total number of countries",
            icon = icon("flag"),
            color = "yellow"
        )
        
    })
    output$Citation1 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value =  prettyNum(sum(selecteddata$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citations",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType1 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = length(levels(as.factor(selecteddata$`Document Type`))),
            subtitle = "Document types",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess1 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = round(sum(!is.na(selecteddata$`Access Type`))*100/(sum(is.na(selecteddata$`Access Type`))+sum(!is.na(selecteddata$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publications",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal1 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = prettyNum(nlevels(as.factor(selecteddata$`Source title`)), big.mark = ","),
            subtitle = "Total number of journals",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output valuebox in Research tab ####
    output$Publication2 <- renderValueBox({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selecteddata), big.mark = ","),
            subtitle = "Total number of publications",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry2 <- renderValueBox({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        number_country <- str_which(colnames(selecteddata), "lat")[1] - which(colnames(selecteddata) == "EID") + 1 
        valueBox(
            value = number_country,
            subtitle = "Total number of countries",
            icon = icon("flag"),
            color = "yellow"
        )
        
    })
    output$Citation2 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value =  prettyNum(sum(selecteddata$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citations",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType2 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = length(levels(as.factor(selecteddata$`Document Type`))),
            subtitle = "Document types",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess2 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = round(sum(!is.na(selecteddata$`Access Type`))*100/(sum(is.na(selecteddata$`Access Type`))+sum(!is.na(selecteddata$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publications",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal2 <- renderValueBox({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        valueBox(
            value = prettyNum(nlevels(as.factor(selecteddata$`Source title`)), big.mark = ","),
            subtitle = "Total number of journals",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output table in Info tab ####
    output$table <- DT::renderDataTable({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
            data_selected <- selecteddata[,c("Authors", "Title", "Year", "Source title", "Cited by", "Document Type"
                                             , "DOI", "Access Type", "Abstract")]
            # Adding 
            data_selected$DOI <- paste0("<a href='", "https://doi.org/", data_selected$DOI,"' target='_blank'>", "https://doi.org/", data_selected$DOI,"</a>")
            data_selected$Year <- as.factor(data_selected$Year)
            DT::datatable(data_selected, 
                          rownames = FALSE,
                          filter="top",
                          selection="multiple",
                          escape = FALSE,
                          extensions = c('Buttons'),
                          options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                                         pageLength = 5,
                                         # dom = 't',
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         scrollX = TRUE,
                                         fixedColumns = FALSE)
            )
        })
    # Output map in Info tab ####
    output$map <- renderLeaflet({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        first_country <- which(colnames(selecteddata) == "EID") + 1 
        last_country <- str_which(colnames(selecteddata), "lat_")[1] -1
        
        selecteddata_v2 <- selecteddata[,c(first_country:last_country, which(colnames(selecteddata) == "Document Type"))] %>%
            gather(key = "Country", value = "value", -`Document Type`, na.rm =TRUE)
        
        first_lat <- str_which(colnames(selecteddata), "lat_")[1]
        last_lat <-  str_which(colnames(selecteddata), "lat_")[length(str_which(colnames(selecteddata), "lat_"))]
        selecteddata_v3 <- selecteddata[,c(first_lat:last_lat, which(colnames(selecteddata) == "Document Type"))] %>%
            gather(key = "Lat", value = "value", -`Document Type`, na.rm =TRUE)
        
        first_long <- str_which(colnames(selecteddata), "long_")[1]
        last_long <-  str_which(colnames(selecteddata), "long_")[length(str_which(colnames(selecteddata), "long_"))]
        selecteddata_v4 <- selecteddata[,c(first_long:last_long, which(colnames(selecteddata) == "Document Type"))] %>%
            gather(key = "Long", value = "value", -`Document Type`, na.rm =TRUE)
        
        selecteddata_v2$lat <- selecteddata_v3$value
        selecteddata_v2$long <- selecteddata_v4$value
        
        selecteddata_v2 <- selecteddata_v2 %>%
            group_by(Country, lat, long, `Document Type`) %>%
            summarise(n=n()) %>%
            spread(key = `Document Type`, value = n)
        
        selecteddata_v2$Total <- rowSums(subset(selecteddata_v2, select = -c(Country, lat, long)), na.rm = TRUE)

        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 12, name = "Paired")
        colors <- colors[c(2:12,1)]
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selecteddata_v2$long, selecteddata_v2$lat,
                          type = "pie",
                          chartdata = subset(selecteddata_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selecteddata_v2$Total) / sqrt(max(selecteddata_v2$Total)),
                          transitionTime = 0)
    })
    
    # Output journal in Journal tab ####
    output$topjournal <- renderPlotly({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        WN_journal <- selecteddata %>% select(`Source title`) %>%
            dplyr::group_by(`Source title`) %>%
            dplyr::summarise(n=n()) %>%
            dplyr::arrange(desc(n)) %>%
            slice(1:20)
        
        ggplotly(ggplot(WN_journal, aes(x=reorder(`Source title`, n),y = n)) +
                     geom_bar(stat = "identity",
                              position = position_stack(reverse = TRUE),
                              fill = "tomato") +
                     coord_flip() +
                     theme_bw() +
                     xlab("Journals") +
                     ylab("Number of publications") +
                     theme(text=element_text(family = "Arial")) +
                     theme(axis.title.y = element_blank())
        )
    })
    # Output openaccess in Journal tab ####
    output$Openaccess <- renderPlotly({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        WN_OA <- selecteddata %>% select(`Access Type`, Year) %>%
            dplyr::group_by(`Access Type`,Year) %>%
            dplyr::filter(Year < 2021 & Year > 1949) %>% 
            dplyr::summarise(n=n())
        WN_OA$`Access Type` <- as.character(WN_OA$`Access Type`)
        WN_OA$`Access Type`[is.na(WN_OA$`Access Type`)] <- "Not OA"
        
        WN_OA <- WN_OA %>% group_by(Year) %>% 
            mutate_at(vars(n), funs("percent" = round(.*100/sum(.), digits = 2)))
        WN_OA <- WN_OA %>% filter(`Access Type` == "Not OA") %>% arrange(Year)
        WN_OA$percent2 <- 100- WN_OA$percent
        
        ggplotly(ggplot(WN_OA, aes(x = Year,  y = percent2, color = 'tomato')) +
                     geom_point(size = 2, color = 'tomato')+
                     geom_line(size = 1.1125, color = 'tomato')+
                     theme_bw() +
                     xlab("Year") +
                     ylab("Percent of publications (%)") +
                     scale_x_continuous(labels = scales::number_format(accuracy = 1))+
                     theme(text=element_text(family = "Arial")) +
                     theme(legend.position = "none"))
    })
    # Output publication year in Research tab ####
    output$Pubyear <- renderPlotly({
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        WN_PU_year <- selecteddata %>% select(Year,`Document Type`) %>%
            dplyr::group_by(Year, `Document Type`) %>%
            dplyr::summarise(`Number of publication`=n()) %>%
            dplyr::arrange(Year)
        
        ggplotly(ggplot(WN_PU_year, aes(x=Year, y=`Number of publication`, color = `Document Type`, group = `Document Type`))+
                     geom_point(size = 2)+
                     geom_line(size = 1.1125)+
                     theme_bw() +
                     xlab("Year") +
                     ylab("Number of Publication") +
                     theme(text=element_text(family = "Arial")) +
                     scale_x_continuous(labels = scales::number_format(accuracy = 1))+
                     theme(legend.title = element_blank())
        )
        
    })
    # Output top keywords in Research tab ####
    output$Topkw <- renderPlot({
        
        if(df_year() == "All"){
            year_chosen <- df()
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else if(str_detect(df_year(), pattern = "2020|2010s|2000s|1990s|1980s|< 1980s")){
            year_chosen <-  df()[df()$Period == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        } else {
            year_chosen <- df()[df()$Year == df_year(),]
            if(df_ml() == "All"){
                ml_chosen <- year_chosen
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            } else {
                ml_chosen <- year_chosen %>% filter(id == df_ml())
                if(df_method() == "All"){
                    method_chosen <- ml_chosen
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                } else {
                    method_chosen <- ml_chosen[as.vector(!is.na(ml_chosen[, colnames(ml_chosen) == df_method()])), ]
                    method_chosen <- method_chosen[, colSums(is.na(method_chosen)) < nrow(method_chosen)]
                    if(df_theme() == "All"){
                        selecteddata <- method_chosen
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    } else {
                        selecteddata <- method_chosen[as.vector(!is.na(method_chosen[, colnames(method_chosen) == df_theme()])), ]
                        selecteddata <- selecteddata[, colSums(is.na(selecteddata)) < nrow(selecteddata)]
                    }
                }
            }
        }
        
        selecteddata_KW <- selecteddata$`Author Keywords`[complete.cases(selecteddata$`Author Keywords`)]
        keyword <- strsplit(selecteddata_KW, "; ")
        for (i in 1:length(keyword)){
            keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
        }
        keyword2 <- rbindlist(keyword)
        colnames(keyword2)[1]<- "keyword"
        keyword2<- keyword2[complete.cases(keyword2),]
        keyword2$keyword <- str_to_title(keyword2$keyword)
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'modeling', "modelling")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Fishery', "Fisheries")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'algorithms', "algorithm")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Machines', "Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'machines', "machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Algorithms', "algorithm")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'methods', "method")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Methods', "method")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'rules', "rule")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Rules', "rule")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Networks', "Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Systems', "System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Pca', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'PCA', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Components Analysis (Pca)', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Components', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Components Analysis (Pca)', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component Analysis Method', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Analyses', "Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Cca', "Canonical Correspondence Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'CCA', "Canonical Correspondence Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Redundancy Analysis (Rda)', "Redundancy Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Neural Network', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Network \\(Ann\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Networks \\(Ann\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Networks \\(Anns\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ann', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'ANN', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'ANNs', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Anns', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'ANN', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Artificial Neural Network', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gis', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'GIS', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Geographic Information System \\(Gis\\)', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gis \\(Geographic Information System\\)', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Analysis Analysis', "Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Svm', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machine \\(Svm\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machines \\(Svms\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machines \\(Svm\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machine\\(Svm\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Generalized', "Generalised")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Network \\(Artificial Neural Network\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machine \\(Support Vector Machine\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component Analysiss Analysis', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component Analysis \\(Principal Component Analysis\\)', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Forest \\(Rf\\)', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'forests', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'forest', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Forests', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ground water', "Groundwater")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Estuaries', "Estuary")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Macroinvertebrates', "Macroinvertebrate")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gam', "Generalised Additive Model")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Generalised Additive Model \\(Gam\\)', "Generalised Additive Model")
        keyword2$keyword <- str_replace_all(keyword2$keyword, ' Generalised Additive Model \\(Gams\\)', "Generalised Additive Model")
        keyword3 <- keyword2 %>%
            dplyr::group_by(keyword) %>% 
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n)) 
                
        ggplot(keyword3[1:50,], aes(label = keyword, size =n,color = rainbow_hcl(50))) +
            geom_text_wordcloud_area(shape = "circle") +
            scale_size_area(max_size = 20) +
            theme_minimal()
    })
    
}

#### Run the application ####
shinyApp(ui = ui, server = server)