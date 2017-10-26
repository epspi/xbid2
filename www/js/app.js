$('document').ready(function() {

  // Route search form enter submission to window url
  var searchText = $('#searchText')
  searchText.keypress(function (e) {
    if (e.which == 13) {
      window.location.href = '#/search?term=' + searchText.val();
      $('.close-search, .clear-search').click();
    }
  });
});
