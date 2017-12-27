# router.R

MakeRouter <- function(routenames) {
  # Takes an R shiny UI routes definition and uses it
  # to create a director.js routes definition and initialize
  routing_script <- readr::read_file("www/js/router.js")
  default_route <- routenames[1]
  routing_table <- sapply(routenames, function(x) {
    sprintf(fmt = "'%s' : ShinyRoute('%s')", x, x)}) %>%
    paste0(collapse = ",\n    ")
  
  sprintf(routing_script, routing_table, default_route)
}
