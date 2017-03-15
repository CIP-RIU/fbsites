#' UI Side for adding new trial site in HIDAP
#'
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

addsite_ui <- function(type = "tab", title = "New Trial Site", name = "createList"){

  shinydashboard::tabItem(tabName = name,

                          #begin tabItem
                          h2(title),
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),

                          div(
                            id = "geo-panel",

                          fluidRow( #begin fluidrow
                            box( #begin box
                              title = "Geographic Data", width = 12, status = "primary", height = "230px", solidHeader = TRUE,
                              #p("Seleccione un cultivo y una base de datos"),

                              fluidRow( #Geographic Data Location

                                column(6,
                                            uiOutput("continent_addloc")
                                                       ),

                                column(6,
                                            uiOutput("country_addloc")
                                           ),

                                column(6,
                                       shiny::selectizeInput(inputId = "fbsites_cipregion", label = "CIP Region", width="100%",
                                                             choices = c("SSA","SWCA","ESEAP","LAC", "Other"),
                                                             options = list(
                                                               placeholder = 'Please select the crop',
                                                               onInitialize = I('function() { this.setValue(""); }')
                                                             ))#,
                                )#,
                             )#,  #End Geographic Data Location

                            ), #end box

                            box(title = "New Locality", width = 12, status = "primary", height = "300px", solidHeader = TRUE,

                                fluidRow(

                                  column(6,  shiny::textInput("fbsites_admin1",   label =  "Admin1 (Region)")),
                                  column(6,  shiny::textInput("fbsites_admin2",   label =  "Admin2 (Province)")),
                                  column(6,  shiny::textInput("fbsites_admin3",   label =  "Admin3 (District)")),
                                  column(6,  shiny::textInput("fbsites_LocName",  label = "Locality")),
                                  column(6,  shiny::textInput("fbsites_LocFull",  label = "Locality Full Name")),
                                  column(6,  shiny::textInput("fbsites_LocShort", label = "Locality Short Name (Abbreviation)"))#,
                                )#,
                            ) ,

                         box(title = "Geographic Coordinates", width = 12, status = "primary", height =  NULL, solidHeader = TRUE, #begin box

                                column(12,
                                  radioButtons("select_geosystem", label = h4("Type of coordinates system",style = "font-family: 'Arial', cursive;
                                                font-weight: 500; line-height: 1.1; color: #4d3a7d;"),
                                                choices = c("Decimal", "Sexagesimal"),
                                                selected = "Decimal"),
                                fluidRow(

                                 conditionalPanel(
                                 condition = "input.select_geosystem == 'Decimal'",

                                 column(8,  div(style="display:inline-block",
                                             shiny::numericInput(inputId = "fbsites_latitude",  value = 0.0, label = "Latitude"),
                                             shiny::numericInput(inputId = "fbsites_longitude", value = 0.0, label = "Longitude")
                                     )
                                  ) #end column(6,
                                ),

                                conditionalPanel(
                                  condition = "input.select_geosystem == 'Sexagesimal'",


                                  column(4,  div(style="display:inline-block",
                                               h4("Longitude"),
                                               numericInput(inputId = "fbsites_grade_long", label="Grades (°)", value = 10),
                                               numericInput(inputId = "fbsites_minute_long", label="Minutes (')", value = 10),
                                               numericInput(inputId = "fbsites_second_long", label="Seconds ('')", value = 10),
                                               selectInput(inputId =  "fbsites_orientation_long", label="Orientation",
                                                           choices = c("E","W"), selected = "E" )#,
                                              ) #end div
                                 ),

                                  column(4,  div(style="display:inline-block",
                                                h4("Latitude"),
                                                numericInput(inputId = "fbsites_grade_lat", label="Grades (°)", value = 10),
                                                numericInput(inputId = "fbsites_minute_lat", label="Minutes (')", value = 10),
                                                numericInput(inputId = "fbsites_second_lat", label="Seconds ('')", value = 10),
                                                selectInput(inputId  =  "fbsites_orientation_lat", label="Orientation",
                                                            choices  = c("N","S"), selected = "N" )#,
                                              ) #end div
                                  )#,

                                ),
                                 column(8, shiny::numericInput(inputId = "fbsites_elevation", value = 2000, label = "Elevation (m.s.n.m)", width= "160px" ))

                               )# end fluidrow,

                            ) #  column(3


                           ), #end box

                         fluidRow(

                           HTML('<div style="float: right; margin: 0 31px 18px 0px;">'),
                           #div(style="display:inline-block",
                           shinysky::shinyalert("alert_fbsites_add", FALSE, auto.close.after = 10),
                           shiny::actionButton("fbsites_submit", "Submit",
                                               icon = icon("floppy-saved", lib = "glyphicon"),
                                               width = "100px",
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                               ),
                           shiny::actionButton("fbsites_refreshpage",
                                               "Refresh", icon = icon("refresh", lib = "glyphicon"),
                                               width = "100px",
                                               style="color: #fff; background-color: #51a351; border-color: #51a351"

                                               ),
                           #), #end div
                           HTML('</div>')#,
                         ),
                         br(),
                         br(),
                         br()






                          )#, #end fluidrow

  )      ,
  br(),
  br(),
  br()


                      ) #end tabItem


}

#' TextInputRow: Create a side-by-side input using shiny framework




