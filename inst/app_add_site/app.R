library(shinysky)
library(shinyTree)
library(dplyr)
library(magrittr)
library(stringr)
library(shinyFiles)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)
library(shinyBS)
library(openxlsx)
library(data.table)
library(countrycode)
library(fbglobal)
#library(shinysky)

#new library 6/3/2016


#tabNameS <- "resource_fieldbook_design"
tabNameS <- "add_trial_site"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbsites::server_addsite(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Trial Sites"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Add Trial Sites", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(

                      tabItems(
                        fbsites::addsite_ui(name = tabNameS)#$,

                      )
                    )
)

shinyApp(ui = ui, server = server)


