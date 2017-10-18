window.addEventListener('load', function() {

  // buttons
  var btn_login = document.getElementById('btn-login');
  var btn_logout = document.getElementById('btn-logout');

  // lock object
  var lock = new Auth0Lock(AUTH0_CLIENT_ID, AUTH0_DOMAIN, {
    auth: {
      redirectUrl: AUTH0_CALLBACK_URL,
      responseType: 'token'
    }
  });

  // logout function
  var logout = function() {
    localStorage.removeItem('id_token');
    //localStorage.removeItem('profile');
    window.location.href = AUTH0_CALLBACK_URL;
  };

  // logout function
  var RemoveToken = function(token) {
    localStorage.removeItem('id_token');
  };

  // listeners for login/logout clicks
  btn_login.addEventListener('click', function() {
    lock.show();
  });

  btn_logout.addEventListener('click', function() {
    logout();
  });

  // run when lock becomes authenticated
  lock.on("authenticated", function(authResult) {
    // write id token to local storage
    localStorage.setItem('id_token', authResult.idToken);

    // Retrigger profile retrieval
    RetrieveProfile();
  });

  //retrieve the profile upon load
  var RetrieveProfile = function() {
    var id_token = localStorage.getItem('id_token');

    // Tell shiny to fetch the user profile using token, or pass null
    setTimeout(function () { Shiny.onInputChange('token', id_token); }, 0);
  };

  // updates UI elements with profile info
  var ShowUserProfile = function(profile) {

    document.getElementById('login').style.display = "none";
    document.getElementById('user-menu').style.display = "inline";

    document.getElementById('avatar').src = profile.picture;
    document.getElementById('name').textContent = profile.name;

    // document.getElementById('nickname').textContent = profile.nickname;
    // document.getElementById("profile-form").style.display = "block";
  };

  // Shiny message hanlders to receive profile
  Shiny.addCustomMessageHandler("profile_handler", ShowUserProfile);
  Shiny.addCustomMessageHandler("expired_handler", RemoveToken);

  RetrieveProfile();
});
