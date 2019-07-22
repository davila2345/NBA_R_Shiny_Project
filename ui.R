library(DT)
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "PER what?"),
    dashboardSidebar(
        
        sidebarUserPanel(HTML("<strong>Daniel Avila</strong><br><h6>NYCDSA Fellow</h6>"),
                         image = photo_me),
        sidebarMenu(
            menuItem("Intro", tabName = "about", icon = icon("book")),
            menuItem("Evolution of 3PTers", tabName = "tab3PA", icon = icon("calendar")),  
            menuItem("PER vs Height", tabName = "tabPervsHeight", icon = icon("bar-chart-o")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )#,
        #selectizeInput("selected",
        #               "Select Item to Display",
        #               choice)
    ),
    dashboardBody(
      
      ### changing theme
      shinyDashboardThemes(
        theme = "onenote"
      ),
      
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            
            tabItem(
              tabName = "about",
              fluidRow(
                box(HTML(intro_text), width = 12)
              )
              
            ),
            
            tabItem(
              tabName = "tab3PA",
              fluidRow(
                box(sliderInput("anim_Seasons", "Seasons",
                                min = 1978, max = 2017,
                                value = c(1978,2017),
                                sep = "",
                                animate = animationOptions(interval = 10, loop = TRUE))
                    , width = 12)
              ),
              fluidRow(
                box(plotlyOutput("line_3ptRate"),width = 6),
                box(plotlyOutput("rndr_scat_PERvs3PA"),width = 6)
              )
            ),
            
            tabItem(
              tabName = "tabPervsHeight",
              fluidRow(
                box(sliderInput("Seasons", "Seasons",
                                min = 1978, max = 2017,
                                value = c(1978,2017),
                                sep = ""), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("scatPERvsHeight"),width = 6),
                box(plotlyOutput("barPERvsHeight"),width = 6)
              )
            ),
            
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
