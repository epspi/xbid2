window.addEventListener('load', function() {

  // buttons
  // var btn_login = document.getElementById('btn-login');
  var btn_logout = document.getElementById('btn-logout');

  // lock object
  var lock = new Auth0Lock(AUTH0_CLIENT_ID, AUTH0_DOMAIN, {
    auth: {
      redirectUrl: AUTH0_CALLBACK_URL,
      responseType: 'token'
    }
  });

  // Remove token
  var RemoveToken = function(token) {
    localStorage.removeItem('id_token');
  };

  // logout function
  var logout = function() {
    RemoveToken('id_token');
    window.location.href = AUTH0_CALLBACK_URL;
  };


  // listeners for login/logout clicks
  // btn_login.addEventListener('click', function() {
  //   lock.show();
  // });

  btn_logout.addEventListener('click', function() {
    logout();
  });

  // run when lock becomes authenticated
  lock.on("authenticated", function(authResult) {
    // write id token to local storage
    localStorage.setItem('id_token', authResult.idToken);

    // Retrigger profile retrieval
    // RetrieveProfile();
    location.reload();
  });

  //retrieve the profile upon load
  var RetrieveProfile = function() {
    var id_token = localStorage.getItem('id_token');

    if (id_token == null) {
      id_token = ""
      // $(.account).remove();
      lock.show();
    }

    // Tell shiny to fetch the user profile using token, or pass null
    setTimeout(function () { Shiny.onInputChange('token', id_token); }, 0);
  };

  // updates UI elements with profile info
  var ShowUserProfile = function(profile) {
    $('.user-ava img').attr('src', profile.picture);
    $('.user-name').text(profile.name);
  };

  // Shiny message hanlders to receive profile
  Shiny.addCustomMessageHandler("profile_handler", ShowUserProfile);
  Shiny.addCustomMessageHandler("expired_handler", RemoveToken);

  RetrieveProfile();
});
