#' UI Side for adding new trial site in for HIDAP-AGROFIMS
#'
#' Returns user friendly ui for for HIDAP-AGROFIMS
#' @author Omar Benites
#' @param type type of UI element, default is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

addsite_ui_agrofims <- function(type = "tab", title = "New Trial Site", name = "createList"){

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
                                         uiOutput("continent_addloc_hdafims")
                                  ),

                                  column(6,
                                         uiOutput("country_addloc_hdafims")
                                  ),

                                  column(6,
                                         shiny::selectizeInput(inputId = "fbsites_cipregion_hdafims", label = "CIP Region", width="100%",
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

                                    column(6,  shiny::textInput("fbsites_admin1_hdafims",   label =  "Admin1 (Region)")),
                                    column(6,  shiny::textInput("fbsites_admin2_hdafims",   label =  "Admin2 (Province)")),
                                    column(6,  shiny::textInput("fbsites_admin3_hdafims",   label =  "Admin3 (District)")),
                                    column(6,  shiny::textInput("fbsites_LocName_hdafims",  label = "Locality")),
                                    column(6,  shiny::textInput("fbsites_LocFull_hdafims",  label = "Locality Full Name")),
                                    column(6,  shiny::textInput("fbsites_LocShort_hdafims", label = "Locality Short Name (Abbreviation)"))#,
                                  )#,
                              ) ,

                              box(title = "Geographic Coordinates", width = 12, status = "primary", height =  NULL, solidHeader = TRUE, #begin box

                                  column(12,
                                         radioButtons("select_geosystem_hdafims", label = h4("Type of coordinates system",style = "font-family: 'Arial', cursive;
                                                                                     font-weight: 500; line-height: 1.1; color: #4d3a7d;"),
                                                      choices = c("Decimal", "Sexagesimal"),
                                                      selected = "Sexagesimal"),
                                         fluidRow(

                                           conditionalPanel(
                                             condition = "input.select_geosystem_hdafims == 'Decimal'",

                                             column(8,  div(style="display:inline-block",
                                                            shiny::numericInput(inputId = "fbsites_latitude_hdafims",  value = 0.0, label = "Latitude"),
                                                            shiny::numericInput(inputId = "fbsites_longitude_hdafims", value = 0.0, label = "Longitude")
                                             )
                                             ) #end column(6,
                                           ),

                                           conditionalPanel(
                                             condition = "input.select_geosystem_hdafims == 'Sexagesimal'",


                                             column(4,  div(style="display:inline-block",
                                                            h4("Latitude"),
                                                            numericInput(inputId = "fbsites_grade_lat_hdafims", label="Grades (°)", value = 10),
                                                            numericInput(inputId = "fbsites_minute_lat_hdafims", label="Minutes (')", value = 10),
                                                            numericInput(inputId = "fbsites_second_lat_hdafims", label="Seconds ('')", value = 10),
                                                            selectInput(inputId  =  "fbsites_orientation_lat_hdafims", label="Orientation",
                                                                        choices  = c("N","S"), selected = "N" )#,
                                             ) #end div
                                             ),

                                             column(4,  div(style="display:inline-block",
                                                            h4("Longitude"),
                                                            numericInput(inputId = "fbsites_grade_long_hdafims", label="Grades (°)", value = 10),
                                                            numericInput(inputId = "fbsites_minute_long_hdafims", label="Minutes (')", value = 10),
                                                            numericInput(inputId = "fbsites_second_long_hdafims", label="Seconds ('')", value = 10),
                                                            selectInput(inputId =  "fbsites_orientation_long_hdafims", label="Orientation",
                                                                        choices = c("E","W"), selected = "E" )#,
                                             ) #end div
                                             )#,

                                           ),
                                           column(8, shiny::numericInput(inputId = "fbsites_elevation_hdafims", value = 2000, label = "Elevation (m.a.s.l)", width= "160px" ))

                                         )# end fluidrow,

                                         ) #  column(3


                              ), #end box

                              fluidRow(

                                HTML('<div style="float: right; margin: 0 31px 18px 0px;">'),
                                #div(style="display:inline-block",
                                shinysky::shinyalert("alert_fbsites_add_hdafims", FALSE, auto.close.after = 10),
                                shiny::actionButton("fbsites_submit_hdafims", "Submit",
                                                    icon = icon("floppy-saved", lib = "glyphicon"),
                                                    width = "100px",
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                ),
                                shiny::actionButton("fbsites_refreshpage_hdafims",
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




