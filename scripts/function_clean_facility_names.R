clean_facility_names <- function(x) {
  x <- x %>% 
  # Removing a curious whitespace character that appears
  # in more recent facility names
  mutate(facility = str_replace_all(facility,
                                    pattern = " ",
                                    replacement = " ")) %>% 
    
    
    # Split up facility name strings separating out inspection date and state
    # Note the use of a double escape character. This was necessary.
    separate(.,
             col = facility,
             into = c("facility","inspection_date"),
             sep = "\\) - ") %>%
    separate(.,
             col = facility,
             into = c("facility","state"),
             sep = "\\(") %>% 
    
    mutate(facility = str_trim(facility, side = "both"),
           state = str_trim(state, side = "both"),
           facility = str_replace_all(facility,
                                      pattern = "^Adelanto ICE Processing Center-East",
                                      replacement = "Adelanto ICE Processing Center - East"),
           facility = str_replace_all(facility,
                                      pattern = "^Adelanto ICE Processing Center-West",
                                      replacement = "Adelanto ICE Processing Center - West"),
           facility = str_replace_all(facility,
                                      pattern = "^Allen Parish Detention Facility",
                                      replacement = "Allen Parish Public Safety Complex"),
           facility = str_replace_all(facility,
                                      pattern = "^Berks County Residential Center",
                                      replacement = "Berks Family Residential Center"),
           inspection_date = replace(inspection_date, inspection_date=="Jan. 29 - 31, 2019", "Jan. 31, 2019"),
           facility = str_replace_all(facility,
                                      pattern = "^Bristol County Jail$",
                                      replacement = "Bristol County Jail and House of Correction"),
           facility = str_replace_all(facility,
                                      pattern = "^Buffalo$",
                                      replacement = "Buffalo Batavia Service Processing Center"),
           state = replace(state, state == "Batavia) Service Processing Center", "NY"),
           facility = str_replace_all(facility,
                                      pattern = "^Calhoun County Jail",
                                      replacement = "Calhoun County Correctional Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Clay County Justice Center",
                                      replacement = "Clay County Jail"),
           facility = str_replace_all(facility,
                                      pattern = "^Coastal Bend Detention Facility",
                                      replacement = "Coastal Bend Detention Center"),
           state = replace(state, state == "David L. Moss Criminal Justice Center)", "OK"),
           facility = str_replace_all(facility,
                                      pattern = "^Dodge County Detention Center",
                                      replacement = "Dodge County Detention Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Donald W. Wyatt Detention Center",
                                      replacement = "Donald W. Wyatt Detention Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Essex County Corrections Facility",
                                      replacement = "Essex County Correctional Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Farmville Detention Center$",
                                      replacement = "Immigration Centers of America - Farmville"),
           facility = str_replace_all(facility,
                                      pattern = "^Florence SPC",
                                      replacement = "Florence Service Processing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Houston CDF",
                                      replacement = "Houston Contract Detention Facility"),
           state = replace(state, state == "Polk)", "TX"),
           facility = str_replace_all(facility,
                                      pattern = "^Immigration Centers of America$",
                                      replacement = "Immigration Centers of America - Farmville"),
           state = replace(state, state =="ICA", "VA"),
           inspection_date = replace(inspection_date, inspection_date == "Farmville Detention Center (FDC) (VA", "Feb. 24, 2021"),
           # This one picks a value in the facility col and changes a value in the state col
           state = replace(state, facility == "Immigration Centers of America - Farmville",
                           "VA"),
           facility = str_replace_all(facility,
                                      pattern = "^Joe Corley Detention Facility",
                                      replacement = "Joe Corley Processing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Karnes County Residential Center",
                                      replacement = "Karnes County Family Residential Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Krome SPC",
                                      replacement = "Krome Service Procesing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Krome Special Processing Center",
                                      replacement = "Krome Service Procesing Center"),
           state = replace(state, state =="SPC)", "FL"),
           facility = str_replace_all(facility,
                                      pattern = "^Mesa Verde Detention Facility",
                                      replacement = "Mesa Verde ICE Processing Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Northwest Contract Detention Center",
                                      replacement = "Northwest ICE Processing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Northwest Detention Center",
                                      replacement = "Northwest ICE Processing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Okmulgee County Jail-Moore Detention Facility",
                                      replacement = "Okmulgee County Jail - Moore Detention Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Orange County Jail",
                                      replacement = "Orange County Correctional Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Otay Mesa Detention Facility",
                                      replacement = "Otay Mesa Detention Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Prarieland Detention Center",
                                      replacement = "Prairieland Detention Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Richwood Correcrtional Center",
                                      replacement = "Richwood Correctional Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Rio Grande Processing Center",
                                      replacement = "Rio Grande Detention Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Robert A. Deyton Correctional Center",
                                      replacement = "Robert A. Deyton Detention Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^Robert A. Deyton Detention Center",
                                      replacement = "Robert A. Deyton Detention Facility"),
           facility = str_replace_all(facility,
                                      pattern = "^South Texas Detention Complex",
                                      replacement = "South Texas ICE Processing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^South Texas Processing Center",
                                      replacement = "South Texas ICE Processing Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Strafford County Corrections",
                                      replacement = "Strafford County Department of Corrections"),
           inspection_date = replace(inspection_date, state =="CO)- Mar. 31, 2021", "Mar. 31, 2021"),
           state = replace(state, state == "CO)- Mar. 31, 2021", "CO"),
           facility = str_replace_all(facility,
                                      pattern = "^T. Don Hutto Detention Center$",
                                      replacement = "T. Don Hutto Residential Center"),
           facility = str_replace_all(facility,
                                      pattern = "Tulsa County Jail - David L. Moss Criminal Jutice Center",
                                      replacement = "David L. Moss Criminal Justice Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Tulsa County Jail$",
                                      replacement = "David L. Moss Criminal Justice Center"),
           state = replace(state, state == "David L. Moss Justice Center)", "OK"),
           facility = str_replace_all(facility,
                                      pattern = "^Washoe County Detention Centerr",
                                      replacement = "Washoe County Detention Center"),
           facility = str_replace_all(facility,
                                      pattern = "^Webb County Detention Facility",
                                      replacement = "Webb County Detention Center"),
    )
    
    
  return(x)
}