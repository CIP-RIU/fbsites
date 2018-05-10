#' server_design for HIDAP-AGROFIMS
#'
#' Design a fieldbook for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites
#' @export
#'
#server_design <- function(input, output, session, dom="hot_fieldbook_design", values){

server_addsite_agrofims <- function(input, output, session, values){

  output$continent_addloc_agrofims <- renderUI({
    #req(input$fbmlist_select_new)
    #Use of countrycode dataset and package for extracting continent data
    continent_list <- unique(countrycode_data$continent)
    continent_list <- continent_list[!is.na(continent_list)]
    continent_list <- sort(continent_list)

    #selectInput("fbmlist_continent_new", label = h4("Continent"), choices = continent_list , placeholder = "Choose Continent")
    shiny::selectizeInput(inputId ="fbsites_continent_agrofims", label = "Select Continent",
                          multiple =  FALSE, width="100%", choices = continent_list,
                          options = list(
                            placeholder = 'Please select continent',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
  })

  output$country_addloc_agrofims <- renderUI({

    #create reactive values to store dataframe
    values <- reactiveValues(geo_db = NULL)


    continent_header <- input$fbsites_continent_agrofims

    if(continent_header == "" || is.null(continent_header)) {
      country_list <- ""

    } else {

      country_list <- filter(countrycode_data, continent == continent_header)
      country_list <- select(country_list, country.name.en)

    }

    shiny::selectizeInput(inputId ="fbsites_country_agrofims", label = "Select Country",
                          multiple =  FALSE, width="100%", choices = country_list,
                          options = list(
                            placeholder = 'Please select country',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
  })

  observe({

    #After all this conditions has been made, the submit button will appear to save the information
    toggleState("fbsites_submit_agrofims", !is.null(input$fbsites_continent_agrofims) && str_trim(input$fbsites_continent_agrofims, side = "both")!= "" &&
                  !is.null(input$fbsites_country_agrofims) && str_trim(input$fbsites_country_agrofims, side = "both")!= "" &&
                  !is.null(input$fbsites_LocName_agrofims) && str_trim(input$fbsites_LocName_agrofims, side = "both") != "" &&
                  !is.null(input$fbsites_LocFull_agrofims) && str_trim(input$fbsites_LocFull_agrofims, side = "both") != "" &&
                  !is.null(input$fbsites_LocShort_agrofims) && str_trim(input$fbsites_LocShort_agrofims, side = "both")!="" &&
                  !is.null(input$fbsites_admin1_agrofims) && str_trim(input$fbsites_admin1_agrofims, side = "both")!="" &&
                  !is.null(input$fbsites_admin2_agrofims) && str_trim(input$fbsites_admin2_agrofims, side = "both")!="" &&
                  !is.null(input$fbsites_admin3_agrofims) && str_trim(input$fbsites_admin3_agrofims, side = "both")!=""

                #                               !is.null(input$fbsites_grade_lat) && str_trim(input$fbsites_grade_lat, side = "both")!="" &&
                #                               !is.null(input$fbsites_grade_lon) && str_trim(input$fbsites_grade_lon, side = "both")!="" &&
                #
                #                               !is.null(input$fbsites_minute_lon) && str_trim(input$fbsites_minute_lon, side = "both")!="" &&
                #                               !is.null(input$fbsites_minute_lon) && str_trim(input$fbsites_minute_lon, side = "both")!="" &&
                #                               !is.null(input$fbsites_minute_lon) && str_trim(input$fbsites_second_lon, side = "both")!="" &&
                #                               !is.null(input$fbsites_minute_lon) && str_trim(input$fbsites_second_lat, side = "both")!=""
                # #
    )
  })
  #geo_db <- reactive({
  #db <- readRDS(file = "sites_table.rds")

  shiny::observe({
    path <- fbglobal::get_base_dir()
    #print(path)
    geodb_file <- "table_sites.rds"
    path <- paste(path, geodb_file, sep = "\\")
    values$geo_db <-  readRDS(file = path)
    #     values$geo_db <-  readRDS(file = "sites_table.rds")

  })



  shiny::observeEvent(input$fbsites_submit_agrofims, {
    #alert("Thank you!")


    # cntry <- fbsites::get_country_list(sites_data =  values$geo_db)
    # print(cntry)

    withProgress(message = 'Saving the new locality...', value = 0, {

      incProgress(3/15)
      # use the reactive geo_db data
      #geo_db_data <- geo_db()

      continent <- stringr::str_trim(input$fbsites_continent_agrofims,side = "both")
      country   <- stringr::str_trim(input$fbsites_country_agrofims, side = "both")
      cregion <- stringr::str_trim(input$fbsites_cipregion_agrofims, side = "both")

      # Trim all whitespaces for every geovariable ------------------------------

      admin1    <- stringr::str_trim(input$fbsites_admin1_agrofims, side = "both")
      admin2    <- stringr::str_trim(input$fbsites_admin2_agrofims, side = "both")
      admin3    <- stringr::str_trim(input$fbsites_admin3_agrofims, side = "both")
      locFull   <- stringr::str_trim(input$fbsites_LocFull_agrofims, side = "both")
      locName   <- stringr::str_trim(input$fbsites_LocName_agrofims, side = "both")
      locShort  <- stringr::str_trim(input$fbsites_LocShort_agrofims, side = "both")
      locShort <-  gsub("[[:space:]]","", locShort)

      if(input$select_geosystem_agrofims == "Decimal"){
        latitude <- stringr::str_trim(input$fbsites_latitude_agrofims, side = "both")
        longitude <- stringr::str_trim(input$fbsites_longitude_agrofims, side = "both")
      }

      if(input$select_geosystem_agrofims == "Sexagesimal"){ # for sexagesimal degress

        grade_long <-  input$fbsites_grade_long_agrofims
        minute_long <- input$fbsites_minute_long_agrofims
        second_long <- input$fbsites_second_long_agrofims
        orientation_long <- input$fbsites_orientation_long_agrofims

        grade_lat <- input$fbsites_grade_lat_agrofims
        minute_lat <- input$fbsites_minute_lat_agrofims
        second_lat <- input$fbsites_second_lat_agrofims
        orientation_lat <- input$fbsites_orientation_lat_agrofims

        if(orientation_long=="E"){
          longitude <- seg2dec(grade = grade_lat, minute = minute_lat, second = second_lat )
        } else {
          longitude <- seg2dec(grade = grade_lat, minute = minute_lat, second = second_lat)
          longitude <- -longitude
        }

        if(orientation_lat=="N"){
          latitude  <- seg2dec(grade = grade_long, minute = minute_long, second = second_long)
        } else {
          latitude  <- seg2dec(grade = grade_long, minute = minute_long, second = second_long)
          latitude <- -latitude
        }

      } # end for sexagesimal degress

      elevation <- stringr::str_trim(input$fbsites_elevation, side = "both")
      ###


      # Creation of list with geovariables

      lgeovar <- list(id    = nrow(values$geo_db)+1, #to add new row index
                      shortn =  toupper(locShort), #uppercase
                      altern =  NA,
                      fulln  =  locFull,
                      local  =  locName ,#local = adm4
                      latd   =  latitude,
                      lond   =  longitude,
                      elev   =  elevation ,
                      crops  =  NA,
                      aez    =  NA,
                      cont   =  continent,
                      creg   =  cregion,
                      cntry  =  country,
                      adm4   =  locName, #local = localname = adm4
                      adm3   =  admin3,
                      adm2   =  admin2,
                      adm1   =  admin1,
                      comment = NA,
                      user = "local"
      )
      ###

      #Appear two conditions

      print(values$geo_db)
      print(values$geo_db[,"shortn"][[1]])
      print(values$geo_db[,"local"][[1]])
      print(values$geo_db[,"fulln"][[1]])

      print(lgeovar)

      #first condition
      if( is.element(lgeovar$shortn, values$geo_db[,"shortn"][[1]] )){

        msg <- paste("Your locality short name for your location ", lgeovar$shortn, " already exists.", " Please try again.", sep = "")
        #print("no location added")
        shinysky::showshinyalert(session, "alert_fbsites_add_agrofims", msg, styleclass = "danger")

      } else if(is.element(lgeovar$local, values$geo_db[,"local"][[1]])){

        msg <- paste("Your locality ", lgeovar$local, " already exists." , " Please try again.", sep = "")
        #print("no location added")
        shinysky::showshinyalert(session, "alert_fbsites_add_agrofims", msg, styleclass = "danger")

      } else if(is.element(lgeovar$fulln, values$geo_db[,"fulln"][[1]])){

        msg <- paste("Your locality ", lgeovar$fulln, " already exists." , " Please try again.", sep = "")
        #print("no location added")
        shinysky::showshinyalert(session, "alert_fbsites_add_agrofims", msg, styleclass = "danger")

      } else {

        #Convert list in data.frame
        new_instance <- as.data.frame(lgeovar, stringsAsFactors = FALSE)
        #appende the new instance into geo_db()
        values$geo_db <- rbind(values$geo_db, new_instance)


        path <- fbglobal::get_base_dir()
        #print(path)
        geodb_file <- "table_sites.rds"
        #path <- paste(path, geodb_file, sep = "\\")

        path <- file.path(path, geodb_file)

        saveRDS(values$geo_db, file = path)

        msg <- "Your new locality has been successfully added."
        shinysky::showshinyalert(session, "alert_fbsites_add_agrofims", msg, styleclass = "success")


        shinyjs::reset("geo-panel")

        #shinyjs::reset("geo-panel")
        #Reset all geo_variables in shiny UI
        # reset("fbsites_continent")
        # reset("fbsites_country")
        # reset("fbsites_cipregion")
        # reset("fbsites_admin1")
        # reset("fbsites_admin2")
        # reset("fbsites_admin3")
        # reset("fbsites_admin4")
        # reset("fbsites_LocName")
        # reset("fbsites_LocFull")
        # reset("fbsites_LocShort")
        # reset("fbsites_latitude")
        # reset("fbsites_longitude")
        # reset("fbsites_elevation")
        #
        # reset("fbsites_grade_long")
        # reset("fbsites_minute_long")
        # reset("fbsites_second_long")
        # reset("fbsites_orientation_long")
        # reset("fbsites_grade_lat")
        # reset("fbsites_minute_lat")
        # reset("fbsites_second_lat")
        # reset("fbsites_orientation_lat")
        # reset("fbDesign_countryTrial")

      }

    })

  })


  shiny::observeEvent(input$fbsites_refreshpage_agrofims, {
    withProgress(message = 'Refreshing HiDAP to update internal tables...', value = 0, {

      incProgress(3/15)
      shinyjs::js$refresh()

    })
  })

}
