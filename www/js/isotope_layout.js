// Isotope Grid
if ($(".isotope-grid").length) {

  // Generate grid
  var $grid = $(".isotope-grid").imagesLoaded(function () {
    $grid.isotope({
      itemSelector: ".grid-item",
      transitionDuration: "0.5s",
      getSortData: {
        time: ".product-title",
        price: function (itemElem) { // function
          var price = $(itemElem).find('.product-price').text();
          if (price === "NA") {
            return 0;
          } else {
            return parseFloat(price);
          }
        }
      },
      masonry: {
        columnWidth: ".grid-sizer",
        gutter: ".gutter-sizer"
      }
    });
  });

  // Respond to sort dropdown
  var $sort_select = $("#sorting");
  $sort_select.change(function () {
    var asc = true;
    var sort_value = $(this).find(":selected").attr("data-sort-value");
    if (sort_value == "price_low_high") {
      sort_value = "price";
    } else if (sort_value == "price_high_low") {
      sort_value = "price";
      asc = false;
    }

    $grid.isotope({
      sortBy: sort_value,
      sortAscending: asc
    });
  });

  // Respond to filter checkboxes
  var filters = {};
  $('#filters').on('change', function (jQEvent) {
    var $checkbox = $(jQEvent.target);
    manageCheckbox($checkbox);
    $grid.isotope({
      filter: function () {
        // Filter Locations
        if (filters.Locations.length > 0) {
          var loc = $(this).find('.product-card').attr('data-location');
          return jQuery.inArray(loc, filters.Locations) > -1;
        } else {
          return true;
        }

        // Filter Condition
      }
    });
  });
}

function manageCheckbox($checkbox) {
  var checkbox = $checkbox[0];

  var group = $checkbox.parents('.option-set').attr('data-group');
  // create array for filter group, if not there yet
  var filterGroup = filters[group];
  if (!filterGroup) {
    filterGroup = filters[group] = [];
  }

  var isAll = $checkbox.hasClass('all');
  // reset filter group if the all box was checked
  if (isAll) {
    delete filters[group];
    if (!checkbox.checked) {
      checkbox.checked = 'checked';
    }
  }
  // index of
  var index = $.inArray(checkbox.value, filterGroup);

  if (checkbox.checked) {
    var selector = isAll ? 'input' : 'input.all';
    $checkbox.siblings(selector).removeAttr('checked');


    if (!isAll && index === -1) {
      // add filter to group
      filters[group].push(checkbox.value);
    }

  } else if (!isAll) {
    // remove filter from group
    filters[group].splice(index, 1);
    // if unchecked the last box, check the all
    if (!$checkbox.siblings('[checked]').length) {
      $checkbox.siblings('input.all').attr('checked', 'checked');
    }
  }

}
