#' Get the country list from CIP Master Trial Sites
#'
#' @author Omar Benites
#' @param sites_data The master trial sites dataset
#' @description Function to get the list of countries of CIP trial sites
#' Function to get the whole list of countries from CIP trial tites
#' @export
#' @return vector
#'
get_country_list <- function(sites_data){

  sites_data <- arrange_(sites_data, "cntry")
  out <- as.list(stringr::str_trim(unique(as.character(dplyr::select_(sites_data,"cntry")[[1]])),side="both"))

}

#' Get the country list from CIP Master Trial Sites
#'
#' @author Omar Benites
#' @param sites_data The master trial sites dataset
#' @param country_input the input country-value using selectrizeInput
#' Function to get the whole list of countries from CIP trial tites
#' @description This function gives the locality lists related to country
#' @export
#' @return vector
#'
get_filter_locality <- function(sites_data,country_input){

#   filter_country_locality <-  dplyr::select_(data_sites,"SHORTN","FULLN","LOCAL","LATD","LOND","ELEV",
#                                              "CROPS","AEZ","CONT","CREG","CNTRY","ADM4","ADM3","ADM2","ADM1") %>%
  #sites_data <- arrange_(sites_data, "cntry")
  #dplyr::filter(.,CNTRY==country_input)
  #rule <- ~cntry == country_input
  #filter_country_locality <- dplyr::filter_(sites_data,rule)

  filter_country_locality <- dplyr::filter(sites_data, cntry == country_input)

  filter_country_locality <- dplyr::arrange_(filter_country_locality, "fulln")

  filter_sites <- filter_country_locality
  #fullname <- stringr::str_trim(filter_sites$fulln,side = "both")
  fullname <- filter_sites$fulln
  #shortname <- stringr::str_trim(filter_sites$shortn,side="both")
  shortname <- filter_sites$shortn
  sites_labels <- paste(fullname," ","(",shortname,")",sep="")

  #sites_list_inputs <- as.list(fullname)
  sites_list_inputs <- as.list(shortname)
  #names(sites_list_inputs) <- sites_labels
  #names(sites_list_inputs) <- shortname
  #names(sites_list_inputs) <- fullname
  names(sites_list_inputs) <- sites_labels
  ##out <- sites_list_inputs
  out <- sites_list_inputs


}

#' Get the geographical information from CIP trial sites
#'
#' @author Omar Benites
#' @param sites_data The master trial sites dataset
#' @param country_input the input country-value using selectrizeInput
#' Function to get the whole list of countries from CIP trial tites
#' @param trial_site_abbr Trial site abbreviation
#' @description This function gives the geographical information
#' @export
#' @return vector
#'
filter_geodata <- function(sites_data,country_input,trial_site_abbr){
  #   filter_country_locality <-  dplyr::select_(data_sites,"SHORTN","FULLN","LOCAL","LATD","LOND","ELEV",
  #                                              "CROPS","AEZ","CONT","CREG","CNTRY","ADM4","ADM3","ADM2","ADM1") %>%
  #
  filter_country_locality <-  dplyr::select_(sites_data,'shortn','fulln','local',
                                             'latd','lond','elev','crops','aez',
                                             'cont','creg','cntry','adm4','adm3',
                                             'adm2','adm1') %>%
    #dplyr::filter(.,CNTRY==country_input)  %>%
    dplyr::filter(.,cntry==country_input)  %>%
    #dplyr::filter(.,LOCAL==trail_site)
    #dplyr::filter(.,FULLN==trail_site)
    dplyr::filter(.,shortn==trial_site_abbr)
}



################################## Agrofims tools ----------------------------------------------------------

#' Get the admins for each country in AGROFIMS TRIALS
#'
#' @author Omar Benites
#' @param sites_data The master trial sites dataset
#' @param country the input country-value using selectrizeInput
#' @param admin1 admin 1
#' @param admin2 admin 2
#' @param admin3 admin 3
#' @param admin4 admin 4
#' @param admin5 admin5
#' Function to get the whole list of countries from CIP trial tites
#' @description This function gives admins related to country
#' @export
#' @return vector
#'
get_admin_agrofims <- function(sites_data, country, admin1=NULL, admin2=NULL, admin3=NULL, admin4=NULL, admin5=NULL){

  NAME_0 <- NAME_1 <- NAME_2 <- NAME_3 <- NAME_4 <- NAME_5 <-NULL

  country <- country
  vadmin1 <- admin1
  vadmin2 <- admin2
  vadmin3 <- admin3
  vadmin4 <- admin4
  vadmin5 <- admin5

  #get aadmin1
  if(!is.null(country)){
    fltAdmin1 <- dplyr::filter(sites_data, NAME_0 == country)
    out <- unique(fltAdmin1$NAME_1)
  } else{
    out <- ""
  }

  #get aadmin2
  if(!is.null(country) && !is.null(vadmin1)){
    fltAdmin2 <- dplyr::filter(sites_data, NAME_0 == country, NAME_1 == vadmin1)
    out <- unique(fltAdmin2$NAME_2)
  }

  #get admin3
  if(!is.null(country) && !is.null(vadmin1) && !is.null(vadmin2)){
    fltAdmin3 <- dplyr::filter(sites_data, NAME_0 == country, NAME_1 == admin1, NAME_2== vadmin2)
    out <- unique(fltAdmin3$NAME_3)
    #print(sadmin3)
  }

  #get adimin 4
  if(!is.null(country) && !is.null(vadmin1) && !is.null(vadmin2) && !is.null(vadmin3)){
    fltAdmin4 <- dplyr::filter(sites_data, NAME_0 == country, NAME_1 == admin1, NAME_2== admin2, NAME_3 == vadmin3)
    out <- unique(fltAdmin4$NAME_4)
    #print(sadmin4)
  }

  #get admin 5
  if(!is.null(country) && !is.null(vadmin2) && !is.null(vadmin3) && !is.null(vadmin4)){
    fltAdmin5 <- dplyr::filter(sites_data, NAME_0 == country, NAME_1 == admin1, NAME_2== admin2, NAME_3 == vadmin3,
                               NAME_4 == vadmin4)
    out <- unique(fltAdmin5$NAME_5)
    #print(sadmin5)
  }

  if(length(out)==0){ out <-NA} #in case of textInput
  #print(out)
  out <- out

}

#' Get the country list from AGROFIMS TRIALS
#'
#' @author Omar Benites
#' @param sites_data The master trial sites dataset
#' @param country_input the input country-value using selectrizeInput
#' Function to get the whole list of countries from CIP trial tites
#' @description This function gives the locality lists related to country
#' @export
#' @return vector
#'
get_filter_locality_agrofims <- function(sites_data, country_input){

  #   filter_country_locality <-  dplyr::select_(data_sites,"SHORTN","local","LOCAL","LATD","LOND","ELEV",
  #                                              "CROPS","AEZ","CONT","CREG","CNTRY","ADM4","ADM3","ADM2","ADM1") %>%
  #sites_data <- arrange_(sites_data, "cntry")
  #dplyr::filter(.,CNTRY==country_input)
  #rule <- ~cntry == country_input
  #filter_country_locality <- dplyr::filter_(sites_data,rule)

  filter_country_locality <- dplyr::filter(sites_data, cntry == country_input)

  filter_country_locality <- arrange_(filter_country_locality, "local")

  filter_sites <- filter_country_locality
  #localame <- stringr::str_trim(filter_sites$local,side = "both")
  localname <- filter_sites$local
  #shortname <- stringr::str_trim(filter_sites$shortn,side="both")
  shortname <- filter_sites$shortn
  sites_labels <- paste(localname," ","(",shortname,")",sep="")

  #sites_list_inputs <- as.list(localame)
  sites_list_inputs <- as.list(shortname)
  #names(sites_list_inputs) <- sites_labels
  #names(sites_list_inputs) <- shortname
  #names(sites_list_inputs) <- localame
  names(sites_list_inputs) <- sites_labels
  ##out <- sites_list_inputs
  out <- sites_list_inputs


}



#' Get the geographical information from AGROFIMS TRIALS
#'
#' @author Omar Benites
#' @param sites_data The master trial sites dataset
#' @param country_input the input country-value using selectrizeInput
#' Function to get the whole list of countries from CIP trial tites
#' @param trial_site_abbr Trial site abbreviation
#' @description This function gives the geographical information
#' @export
#' @return vector
#'
filter_geodata_agrofims <- function(sites_data,country_input,trial_site_abbr){
  #   filter_country_locality <-  dplyr::select_(data_sites,"SHORTN","FULLN","LOCAL","LATD","LOND","ELEV",
  #                                              "CROPS","AEZ","CONT","CREG","CNTRY","ADM4","ADM3","ADM2","ADM1") %>%
  #
  filter_country_locality <-  dplyr::select_(sites_data, "id", "shortn", "Type" , "local", "cntry",
                                                          "adm1", "adm2", "village", "elev", "latd",
                                                          "lond"  ,"nearpop" ,"date_creation") %>%
                              dplyr::filter(.,cntry==country_input)  %>%
                              #dplyr::filter(.,LOCAL==trail_site)
                              #dplyr::filter(.,FULLN==trail_site)
                              dplyr::filter(.,shortn==trial_site_abbr)
}
