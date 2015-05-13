CleanRawRelocs <- function(data.in) {
  # Cleans and preps raw relocations dataset
  #
  # Args:
  # data.in = raw relocation dataset
  #
  # Returns:
  #
  # cleaned relocations dataset
  #
  data.in$UTMN <- data.in$Longitude
  data.in$UTME <- data.in$Latitude
  data.in$DATE <- data.in$Date
  
  ram.ids <- c("08AS31", "12AS19", "12AS23", "12AS22", "11AS06", "08AS34", "12AS21", "", "14AS16", "14AS47")
  data.ewes <- subset(data.in, !(EWEID %in% ram.ids))
  
  return(clean.relocs = data.ewes)
}