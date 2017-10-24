window.addEventListener('load', function() {
  var userProfile;

  var webAuth = new auth0.WebAuth({
    domain: AUTH0_DOMAIN,
    clientID: AUTH0_CLIENT_ID,
    redirectUri: AUTH0_CALLBACK_URL,
    audience: 'https://' + AUTH0_DOMAIN + '/userinfo',
    responseType: 'token id_token',
    scope: 'profile',
    leeway: 60
  });

  // buttons and event listeners
  var logoutBtn = document.getElementById('btn-logout');
  logoutBtn.addEventListener('click', logout);

  function setSession(authResult) {
    // Set the time that the access token will expire at
    var expiresAt = JSON.stringify(
      authResult.expiresIn * 1000 + new Date().getTime()
    );
    localStorage.setItem('access_token', authResult.accessToken);
    localStorage.setItem('id_token', authResult.idToken);
    localStorage.setItem('expires_at', expiresAt);
  }

  function logout() {
    // Remove tokens and expiry time from localStorage
    localStorage.removeItem('access_token');
    localStorage.removeItem('id_token');
    localStorage.removeItem('expires_at');
    window.location.href = AUTH0_CALLBACK_URL;
  }

  // checks whether token not expired
  function isAuthenticated() {
    // Check whether the current time is past the
    // access token's expiry time
    var expiresAt = JSON.parse(localStorage.getItem('expires_at'));
    return new Date().getTime() < expiresAt;
  }

  // updates UI elements related to profile
  function displayProfile() {
    // display the profile
    $('.user-ava img').attr('src', userProfile.picture);
    $('.user-name').text(userProfile.name);
  }

  // receives user profile from R Shiny
  function handleUserProfile(profile) {
      userProfile = profile;
      displayProfile();
  }

  // clears tokens
  var removeTokens = function(token) {
    logout();
  };

  //retrieve the profile upon load
  var getProfile = function() {
    if (!userProfile) {
      var idToken = localStorage.getItem('id_token');

      if (!idToken) {
        console.log('id_token must exist to fetch profile');
      }

      // Tell shiny to fetch the user profile using token, or pass null
      setTimeout(function () { Shiny.onInputChange('id_token', idToken); }, 0);
    } else {
      displayProfile();
    }
  }

  function handleAuthentication() {
    webAuth.parseHash(function(err, authResult) {
      if (authResult && authResult.accessToken && authResult.idToken) {
        window.location.hash = '';
        setSession(authResult);
      } else if (err) {
        console.log(err);
        alert(
          'Error: ' + err.error + '. Check the console for further details.'
        );
      }

      if (isAuthenticated()) {
        getProfile();
      } else {

        webAuth.authorize();
      }
    });
  }

  // Shiny message hanlders to receive profile
  Shiny.addCustomMessageHandler("profile_handler", handleUserProfile);
  Shiny.addCustomMessageHandler("expired_handler", removeTokens);

  handleAuthentication();
});
