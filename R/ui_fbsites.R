#' shiny UI element
#'
#' returns a re-usable user interface element
#'
#' @author Reinhard Simon
#' @param type of ui Element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @param output name of output element
#' @export
ui_site <- function(type = "tab", title = "List of trial sites", name = "resource_site",
                    output = "hot_sites"){
  # just only one type
  shinydashboard::tabItem(tabName = name,
            shiny::fluidRow(
            #box(plotOutput("plot1", height = 250)),

            shinydashboard::box(width = 12,
                title = title,
                #rhandsontable::rHandsontableOutput(output, height = 600)
                rhandsontable::rHandsontableOutput("tbl_sites", height = 600)
            )
          )
  )
}
