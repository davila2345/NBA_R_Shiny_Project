library(DT)
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    skin = "black",
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
                    fluidRow(box(plotlyOutput("scatPERvsDBPM"),width = 12))),
            tabItem(
              tagList(
                singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
              ),
              tabName = "data",
              fluidRow(box(dataTableOutput("tbl30plusPER"), width = 12)))
            
            )
        )
    )
)
