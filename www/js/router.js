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
      }
  });
  router.init('/');
});