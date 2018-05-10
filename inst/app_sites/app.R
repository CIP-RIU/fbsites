library(shiny)
library(shinydashboard)
library(rhandsontable)
library(fbglobal)
library(fbsites)

tabNameS <- "resource_site"

server <- function(input, output, session) {
  values = shiny::reactiveValues()
  fbsites::server_site(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Demo Site"),
                    dashboardSidebar(width = 350,
                      menuItem("Resources",
                       sidebarMenu(id = "menu",
                         menuSubItem("Site", icon = icon("location-arrow"),
                                     tabName = tabNameS)
                                   )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        fbsites::ui_site(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)
