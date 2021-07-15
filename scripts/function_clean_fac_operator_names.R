clean_fac_operator_names <- function(x) {
  x <- x %>% 
    mutate(
      fac_operator = replace(fac_operator, fac_operator == "Allen Parish Sheriffs Office", "Allen Parish Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Allen Parish Sheriff’s Office", "Allen Parish Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Boone County", "Boone County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Clay County", "Clay County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Clinton County Board of Comissioners", "Clinton County Prison Board"),
      fac_operator = replace(fac_operator, fac_operator == "Dodge County/Dodge County Sheriff's Office", "Dodge County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Dorchester County Council", "Dorchester County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Geauga County", "Geauga County County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Geo Secure Services", "The GEO Group, Inc."),
      
      
      fac_operator = replace(fac_operator, fac_operator == "Hardin County", "Hardin County Sheriff's Department"),
      fac_operator = replace(fac_operator, fac_operator == "Hudson County", "Hudson County Board of Corrections"),
      fac_operator = replace(fac_operator, fac_operator == "Kankakee County Sheriff's Department", "Kankakee County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "LaSalle Corrections LLC", "LaSalle Corrections"),
      fac_operator = replace(fac_operator, fac_operator == "LaSalle Management Company, LLC", "LaSalle Corrections"),
      fac_operator = replace(fac_operator, fac_operator == "LaSalle Management, LLC", "LaSalle Corrections"),
      fac_operator = replace(fac_operator, fac_operator == "LaSalle Management", "LaSalle Corrections"),
      fac_operator = replace(fac_operator, fac_operator == "LaSalle Corrections/ Winn Parish Sheriff", "LaSalle Corrections"),
      fac_operator = replace(fac_operator, fac_operator == "Management and Training Corporation (MTC)", "Management & Training Corporation (MTC)"),
      fac_operator = replace(fac_operator, fac_operator == "Management Training Corporation", "Management & Training Corporation (MTC)"),
      fac_operator = replace(fac_operator, fac_operator == "Peumansend Regional Jail Authority", "Peumansend Creek Regional Jail Authority"),
      fac_operator = replace(fac_operator, fac_operator == "Seneca County", "Seneca County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "Teller County Sheriff''s Department", "Teller County Sheriff's Department"),
      fac_operator = replace(fac_operator, fac_operator == "Yuba County Sheriff", "Yuba County Sheriff's Office"),
      fac_operator = replace(fac_operator, fac_operator == "\"Jail Commander who reports to Board of Directors\"", "Northern Oregon Corrections"),
    )
  return(x)
}