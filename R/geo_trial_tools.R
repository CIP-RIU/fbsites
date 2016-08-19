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
  out <- as.list(stringr::str_trim(unique(as.character(dplyr::select_(sites_data,"cntry")[[1]])),side="both"))
  out
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

  #dplyr::filter(.,CNTRY==country_input)
  #rule <- ~cntry == country_input
  #filter_country_locality <- dplyr::filter_(sites_data,rule)
  filter_country_locality <- dplyr::filter(sites_data, cntry == country_input)
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
  out

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
