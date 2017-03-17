
#' Server side logic
#'
#' Constructs table
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param dom target dom element name
#' @param values reactive values
#' @author Reinhard Simon
#' @export
#'

server_site <- function(input, output, session, values){

  shiny::observe({
    path <- fbglobal::get_base_dir()
    #print(path)
    geodb_file <- "table_sites.rds"
    #path <- paste(path, geodb_file, sep = "\\")
    path <- file.path(path, geodb_file)
    values$geo_vistable <-  readRDS(file = path)
    #     values$geo_db <-  readRDS(file = "sites_table.rds")

  })


  output$tbl_sites = renderRHandsontable({

    rhandsontable(values$geo_vistable)

  })


}

#'
# server_site <- function(input, output, session, dom="hot_sites", values){
#
#   setHot_sites = function(x) values[[dom]] = x
#
#   data("iso_country")
#   #iso_country <- fbsites::iso_country
#   continents = sort(unique(iso_country$continent))
#   cipregions = sort(unique(iso_country$cipregion))
#   # dom_i <- paste0(dom,"_indices")
#   # dom_f <- paste0(dom,"_full")
#
#   shiny::observe({
#     input$saveBtn
#      if (!is.null(input[[dom]])) {
#        post_site_table(values[[dom]])
#      }
#
#   })
#
#   output[[dom]] = rhandsontable::renderRHandsontable({
#     if (!is.null(input[[dom]])) {
#       DF = rhandsontable::hot_to_r(input[[dom]])
#     } else {
#       DF = fbsites::get_site_table()
#     }
#
#     DF$cntry = factor(DF$cntry, levels = iso_country$country)
#     DF$cont = factor(DF$cont, levels = continents)
#     DF$creg = factor(DF$creg, levels = cipregions)
#
#     #print(input$cipregions)
#
#     # if(!is.null(input$cipregions)){
#     #   filtered <- with(DF, creg %in% input$cipregions)
#     #   values[[dom_i]] <- which(filtered)
#     #   values[[dom_f]] <- DF
#     #   DF = DF[filtered, ]
#     # }
#
#     setHot_sites(DF)
#     rh <- rhandsontable::rhandsontable(DF, height = 600)
#     rhandsontable::hot_table(rh, highlightCol = TRUE, highlightRow = TRUE)
#     rhandsontable::hot_cols(rh, columnSorting = TRUE)
#   })
# }
