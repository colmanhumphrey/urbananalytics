#' take in business names,
#' report back Doctors and Nurses etc
#' actually added more
#'
#' @param bus_names
#' text names of businesses
FindDoctors <- function(bus_names){
    doc_codes = c('MD', 'Md', 'DMD', 'DDS', 'CRNP', 'DO', 'PHD', 'PhD',
                  'DPT', 'OT', 'LSW', 'BSN', 'DPM', 'MS', 'OD', 'BS',
                  'LSW', 'MSPT', 'PT', 'ACSW',
                  'PharmD', 'RPH', 'BPharm', 'PHARMD', 'PYSD', 'RPh')

    tempmat = sapply(doc_codes, function(code){grepl(paste0(' ', code), bus_names)})

    return(rowSums(tempmat) > 0)
}

#' take in business names, report back ATMs
#'
#' @param bus_names
#' text names of businesses
FindATM <- function(bus_names){
    atm_ind <- grepl('ATM', bus_names)

    return(atm_ind)
}
