# router.R



MakeRouter <- function(routes) {
  # Takes an R shiny UI routes definition and uses it
  # to create a director.js routes definition and initialize

  routing_script <-
"
$('document').ready(function() {
  // create some functions to be executed when
  // the correct route is issued by the user.
  var ShinyRoute = function(rt) {
    return function () {setTimeout(function () {
      Shiny.onInputChange('route_clicked', rt);
      }, 0);
    }
  };
  
  // define the routing table.
  var routes = {
    %s
  };
  
  // instantiate the router.
  var router = Router(routes);
  router.init('/');

  $('#searchText').keypress(function (e) {
      if (e.which == 13) {
        setTimeout(function () { 
          Shiny.onInputChange('searchSubmit', Math.random()); 
          $('.close-search, .clear-search').click();
        }, 0);
        router.setRoute('/search');
        //$('.close-search, .clear-search').click();
      }
  });
  router.init('/');
});
"
  
  default_route <- names(routes)[1]
  routing_table <- sapply(
    names(routes), 
    . %>% {sprintf(fmt = "'%s' : ShinyRoute('%s')", ., .)}) %>%
    paste0(collapse = ",\n    ")

  sprintf(routing_script, routing_table, default_route)
}
