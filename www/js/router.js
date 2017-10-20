history.pushState({}, "", "#/");
  $('document').ready(function() {
  // create some functions to be executed when
  // the correct route is issued by the user.
  var nofunc = function () {};
  var allroutes = function() {
    var route = window.location.hash.slice(1);
    setTimeout(function () {
      Shiny.onInputChange('route_clicked', route);
    }, 0);
  };

  // define the routing table.
  var routes = {
    '/' : nofunc
    // '/cart': nofunc
  };

  // instantiate the router.
  var router = Router(routes);

  // a global configuration setting.
  router.configure({
    on: allroutes,
    html5history: true,
    // run_handler_in_init: false
    convert_hash_in_init: false
  });

  router.init();
});
