// Isotope Grid
if ($(".isotope-grid").length) {

  
  // Get search results (not displayed)
  var hidden_items = $("#search_results [class*=grid-item ]");
  
  
  // Generate grid
  var $grid = $(".isotope-grid");
  
  $.fn.revealItems = function($items){
		var iso = this.data('isotope');
		var itemSelector = iso.options.itemSelector;
		$items.hide();
		$(this).append($items);
		$items.imagesLoaded().progress(function(imgLoad, image){
			var $item = $(image.img).parents(itemSelector);
		  filterIsotope();
			$item.show();
			iso.appended($item);
		});
	
		return this;
	};
  
  $grid.isotope({
    itemSelector: ".grid-item",
    transitionDuration: "0.0s",
    getSortData: {
      time: ".product-title",
      price: function (itemElem) { 
        return parsePrice($(itemElem).find('.product-price'));
      }
    },
    masonry: {
      columnWidth: ".grid-sizer",
      gutter: ".gutter-sizer"
    }
  });
  
  $grid.imagesLoaded().progress(function(){
		$grid.isotope();
	});
	
	var revealed_count = 0;
	function GenerateItems() {
		var n = 10;
		var items = hidden_items.slice(revealed_count, revealed_count + n);
		items.find("img").attr( "src", function() {
      return this.title;  
    });
    revealed_count += items.length;
		return items;
	}
  
  // SimpleInfiniteScroll
	function Infinite(e){
		if((e.type == 'scroll') || e.type == 'click'){
			var doc = document.documentElement;
			var top = (window.pageYOffset || doc.scrollTop) - (doc.clientTop || 0);
			var bottom = top + $(window).height();
			var docBottom = $(document).height();
			
			if(bottom + 50 >= docBottom){
				$grid.revealItems(GenerateItems());
			}
		}
	}
    
	$grid.revealItems(GenerateItems());

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
    filterIsotope();
  });
  
  // Respond to price range slider
  var msrp_range = [];
  rangeSlider.noUiSlider.on('change', function (values, handle) {
    msrp_range = values.map(parseFloat);
    filterIsotope();
  });
  
  $(window).scroll(Infinite);
}
function filterIsotope() {
  $grid.isotope({
    filter: function () {
      
      // Filter Locations
      if (typeof filters.Locations !== "undefined" && 
          filters.Locations.length > 0) {
        var loc = $(this).find('.product-card').attr('data-location');
        if (jQuery.inArray(loc, filters.Locations) < 0) return false;
      } 
      
      // Filter Condition
      if (typeof filters.Conditions !== "undefined" && 
          filters.Conditions.length > 0) {
        var cond = $(this).find('.product-card').attr('data-condition');
        if (jQuery.inArray(cond, filters.Conditions) < 0) return false;
      }
      // Filter MSRP range
      if (msrp_range.length > 0) {
        var msrp = parsePrice($(this).find('.product-price'));
        if (msrp < msrp_range[0] || msrp > msrp_range[1]) return false;
      }
      
      // Return true if nothing disqualifies the item
      return true;
    }
  });
}
function parsePrice(elem) {
  var price = elem.text().replace(/\$/g,'');
  if (price === "NA") {
    return 0;
  } else {
    return parseFloat(price);
  }
}
function manageCheckbox($checkbox) {
  var checkbox = $checkbox[0];
  debugger;

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
