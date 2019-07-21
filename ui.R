library(DT)
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "PER what?"),
    dashboardSidebar(
        
        sidebarUserPanel("Daniel Avila",
                         image = photo_me),
        sidebarMenu(
            #menuItem("About", tabName = "about", icon = icon("map")),
            menuItem("Per vs DBPM", tabName = "tabPER_DBPM", icon = icon("database")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )#,
        #selectizeInput("selected",
        #               "Select Item to Display",
        #               choice)
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            #tabItem(tabName = "about",
            #        fluidRow(infoBoxOutput("maxBox"),
            #                 infoBoxOutput("minBox"),
            #                 infoBoxOutput("avgBox")),
            #        fluidRow(plotlyOutput("scatPERvsDBPM", height = "600px")),
            #tabItem(tabName = "data",
            #        fluidRow(box(DT::dataTableOutput("table"), width = 12))),
            tabItem(tabName = "tabPER_DBPM",
                    fluidRow(plotlyOutput("scatPERvsDBPM"))),
            tabItem(tabName = "data",
                    fluidRow(dataTableOutput("tbl30plusPER"), width = 12))
            
            )
        )
    )
)
