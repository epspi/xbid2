# global.R
# options(encoding = "utf-8")

## ///////////////////////////////////////////// ##
## LIBRARIES --------------------------------------
## ///////////////////////////////////////////// ##
library(rvest)
library(DT)
library(dplyr)
library(knitr)
library(parallel)
library(urltools)
# library(bubbles)
library(readr)
library(stringr)
library(httr)
library(shiny)
library(shinydashboard)
library(digest)

## ///////////////////////////////////////////// ##
## CONSTANTS---------------------------------------
## ///////////////////////////////////////////// ##

# Local storage
known_links_loc <- "data/current/known_links.csv"
items_loc <- "data/current/items.csv"
auctions_loc <- "data/current/auctions.csv"
timestamp_loc <- "data/current/timestamp.csv"

expired_auctions_loc <- "data/expired/expired_auctions.csv"
expired_items_loc <- "data/expired/expired_items.csv"

user_data_loc <- "data/user"
user_db <- "data/xbid_dev.sqlite3"
default_wishlist_loc <- "data/user/wishlist.csv"
# default_favorites_loc <- "data/user/favorites.csv"
searches_loc <- "data/log/searches.csv"

# Auctions will be considered expired after some delay
kExpiredTimeOffset <- 1800
kTimeFileFormat  <- "%Y-%m-%d %H:%M:%S"
kTZ <- 'EST5EDT'
auto_refresh_time <- 3600 * .5
ending_soon_time <- 3600 * 2

# UI Constants
kMaxPins <- 150

# Scraping & Parsing constants
kLocalLocations <- c("Cincinnati", "Sharonville", "West Chester",
                     "Maineville", "Milford", "Fairfield", "Batavia", "Newport",
                     "Dayton", "Covington")
description_end_regex <- "((Item )?Location:|Front Page:|Contact:|Facebook:|Pinterest:|Twitter:).*"
section_names <- "(Serial #|Lotted By|Load #|(Item )?Brand|(Item )?Desc(ription)?|MSRP|Model|Specifications|Width|Depth|Height|Weight|Item Link Calc|Additional Info(rmation)?) ?:"
keepa_base <- "http://camelcamelcamel.com/search?sq="
camel_base <- "http://camelcamelcamel.com/search?sq="
amazon_base <- "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
gcal_base <- "https://www.google.com/calendar/render?action=TEMPLATE&text=[description]&dates=[start]&details=[details]&location=[location]"

# Original css-only masonry style
kPinsResHTML <-
  '<div class="pin box box-info">
        <div class="box-header with-border">
            <h3 class="box-title">%s</h3></div>
            <div class="box-body">
            <a href="%s" target="_blank"><img src="%s"/></a>
            <p>%s</p></div>
            <div class="box-footer text-center no-padding">
            <div class="box-tools">%s
        </div></div>
    </div>'


# Alternative horizontal masonry style
kPinsResHTML2 <-
  '<div class="pod box box-info">
        <div class="box-header with-border">
            <h3 class="box-title">%s</h3>
        </div>
        <div class="box-body">
            <a href="%s" target="_blank"><img src="%s"/></a>
            <p>%s</p>
        </div>
        <div class="box-footer text-center no-padding">
            <div class="box-tools">%s</div>
        </div>
    </div>'

kTableResHTML <-
  # '<div style="max-width: 500px;">
  '<div style="width: 500px;">
        <p style="word-wrap: break-word;">%s</p>%s
    </div>'

kTableImgHTML <-
  '<a href="%s" target="_blank"><img src="%s" class="img-rounded" width="250"/></a>'

kItemButtonsHTML <-
  '<a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-ils"></i></a>
        <a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-amazon"></i></a>
        <a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-calendar-plus-o"></i></a>
        <a class="btn btn-box-tool item" id="%s"><i class="fa fa-heart"%s></i></a>'

kAuctionsItemsBarSplit <- .5

versionHist <- readLines("VERSION")
version <- regmatches(versionHist[1], regexpr("\\d+[.]\\d+",versionHist[1]))

## ///////////////////////////////////////////// ##
## IO FUNCTIONS------------------------------------
## ///////////////////////////////////////////// ##
GetLastTimestamp <- function(timestamp_loc, time_file_format) {
  read.csv(timestamp_loc, stringsAsFactors = F, header = F)[,1] %>%
    tail(1) %>% strptime(time_file_format) %>% as.POSIXct
}
ReadAuctionsCSV <- function(auctions_loc, time_file_format) {
  read.csv(auctions_loc, stringsAsFactors = F) %>%
    mutate(date = strptime(date, time_file_format, tz = kTZ) %>% as.POSIXct )
}
ReadItemsCSV <- function(items_loc){
  read_csv(items_loc)
}
AppendCSV <- function(data, location){
  write.table(data, location, sep = ",", row.names = F, qmethod = "double",
              append = file.exists(location), col.names = !file.exists(location))
}

## ///////////////////////////////////////////// ##
## HELPER FUNCTIONS--------------------------------
## ///////////////////////////////////////////// ##
enclose <- function(f) f()
SettingInput <- function(inputID,
                         label,
                         description,
                         default_value,
                         type = "checkbox", ...) {
  if (type == "checkbox") {
    inputTag <- tags$input(id = inputID, type = "checkbox", class="pull-right")
    if (!is.null(default_value) && default_value) inputTag$attribs$checked <- "checked"
    tmp_div <- div(class = "form-group", tags$label(label, inputTag))
    
  } else if (type == "slider") {
    tmp_div <- sliderInput(inputID, label = label, value = default_value, ...)
  }
  
  tmp_div$children[[which(sapply(tmp_div$children, "[[", "name") == "label")]]$attribs$class <-
    "control-sidebar-subheading"
  
  tmp_div$children[[length(tmp_div$children) + 1]] <- p(HTML(description))
  # tags$label(class="control-sidebar-subheading", label, inputTag),
  #
  tmp_div
}
GetLastNSearches <- function(file_loc,
                             n) {
  # Reads a local log file for the last n rows
  #
  sprintf('tail -%s %s', n, file_loc) %>%
    system(intern = TRUE) %>%
    textConnection %>%
    read.csv(stringsAsFactors = F) %>%
    .[,2]
}
SaveWishlist <- function(tmp_wishlist) {
  tmp_wishlist %>% data.frame  %>% 
    write.csv(file = default_wishlist_loc, row.names = FALSE)
}
GetWishlist <- function() {
  read.csv(file = default_wishlist_loc, header = T, stringsAsFactors = F)[,1]
}

SaveFavorites <- function(tmp_favorites, user = NULL) {
  if (is.null(user)) {
    file_loc <- default_favorites_loc
  } else {
    file_loc <- user$default_favorites_loc
  }
  write.csv(tmp_favorites, file = file_loc, row.names = FALSE)
}
GetFavorites <- function(user=NULL) {
  
  if (is.null(user)) {
    file_loc <- default_favorites_loc
  } else {
    file_loc <- user$default_favorites_loc
  }
  
  if (!file.exists(file_loc)) {
    default_favorites_loc %>% 
      read.csv(header = T, stringsAsFactors = F) %>% 
      head(0) %>% 
      SaveFavorites(user = user)
  }
  
  read.csv(file = file_loc, header = T, stringsAsFactors = F)
}

ConvertItemsToFavorites <- function(items_df, user) {
  items_df %>% 
    mutate(user_id = user) %>% 
    select(
      user_id, 
      auction_id, 
      item_id = Item, 
      description = Description,
      brand = Brand,
      model = Model,
      item_link = link.item,
      img_link = img_src
    )
}


# --SQLite connectivity for favorites and saved searches-- #
GetUserData_DB <- function(db_loc, table, user=NULL) {
  
  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_loc)
  tt <- tbl(db, table)
  
  if (is.null(user)) {
    result <- collect(head(tt, 0))
  } else {
    result <- collect(filter(tt, user_id == user))
  }
  
  DBI::dbDisconnect(db)
  result
}


InsertUserData_DB <- function(db_loc, table, user = NULL, 
                              new_data) {
  
  if (is.null(user)) stop("No user supplied!")
  if (is.null(new_data)) {
    warning("'new_data' is NULL! Nothing to insert\n")
    return()
  }
  
  user_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_loc)
  DBI::dbWriteTable(conn = user_db, 
                    name = table,
                    value = new_data, 
                    append = TRUE)
  DBI::dbDisconnect(user_db)
}

DeleteUserData_DB <- function(db_loc, table, user = NULL, 
                              extra_sql = NULL, binder = NULL) {
  # Removes specified rows from a table
  
  if (is.null(user)) stop("No user supplied!")
  
  user_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_loc)
  
  sql <- sprintf("DELETE FROM %s WHERE [user_id] = '%s'", table, user)
  
  if (!is.null(extra_sql)) sql <- paste(sql, extra_sql)
  tmp_result <- DBI::dbSendStatement(user_db, sql)
  if(!is.null(binder)) DBI::dbBind(tmp_result, binder)
  
  cat(DBI::dbGetRowsAffected(tmp_result), "rows removed!\n")
  DBI::dbClearResult(tmp_result)
  DBI::dbDisconnect(user_db)
}


DeleteUserFavorites_DB <- function(db_loc, user = NULL, 
                                   removed_favs = NULL, 
                                   all = is.null(removed_favs)) {
  # Removes rows corresponding to removed_favs from the favorites table.
  # 
  # All rows can be removed by setting all = TRUE, however this will be
  # ignored if removed_favs is not NULL. 
  
  extra_sql <- NULL
  binder <- NULL
  
  if (!is.null(removed_favs)) {
    if (!all) {
      # Remove selected rows only
      extra_sql <- "AND [auction_id] = $auction_id AND [item_id] = $item_id"
      binder <- list(
        auction_id = removed_favs$auction_id,
        item_id = removed_favs$item_id
      )
    } else {
      warning("'removed_vars' is not NULL while 'all' flag set to TRUE! Nothing removed!\n")
      return()
    }
  } else {
    if (!all) {
      warning("'removed_vars' is NULL! Nothing to remove.\n")
      return()
    } 
  }
  
  DeleteUserData_DB(db_loc = db_loc, table = "favorites", user = user,
                    extra_sql = extra_sql, binder = binder)
}


## ///////////////////////////////////////////// ##
## PARSING & TEXT ---------------------------------
## ///////////////////////////////////////////// ##
ParseSearchString <- function(str,
                              split = F,
                              sep_char = ", ") {
  # The search string is first parsed for multiple search terms, then cleaned,
  # then either the multiple terms are unlisted (if SPLIT = T) or written back
  # out with some other separator charater, such as a newline.
  
  cleaned <- gsub("[|]", "_", str) %>% gsub("\\W", " ", .) %>% CleanStr()
  cleaned <- if (split) {
    strsplit(cleaned, "_") %>% unlist
  } else {
    gsub("_", sep_char, cleaned)
  }
  return(cleaned)
}
ParseDescription <- function(description) {
  description %>%
    iconv(to = 'latin1', sub = ' ') %>%
    # enc2utf8 %>% 
    gsub(description_end_regex, "", .) %>%
    gsub(section_names, "<<\\1:>>", .) %>%
    CleanStr
}
GrokFeatures <- function(descriptions,
                         list = T) {
  # Takes a parsed description and extracts features into a named list
  mlist <- str_match_all(descriptions, "<<([\\w\\s]+):>> *([^<]+)")
  if (list) {
    lapply(mlist, function(m) split(m[,3], m[,2]))
  } else {
    lapply(mlist, function(m) data.frame(Feature = m[, 2], Value = m[, 3]))
  }
}
FlattenFeatures <- function(list) {
  # Flattens a features list-of-lists into a vector of JSON character strings
  sapply(list, function(item) jsonlite::toJSON(item, auto_unbox = TRUE) %>% as.character)
}
GetFeature <- function(list_items,
                       feature_regex) {
  # Takes a named feature from a list produced by GrokFeatures
  sapply(list_items, function(list) {
    name <- grep(feature_regex, names(list), ignore.case = T, value = T)[1]
    value <- list[[name]]
    ifelse(is.null(value), NA, value)
  }) %>% CleanStr
}
GetMSRP <- function(list_items) {
  options(warn = -1)
  MSRP <- as.numeric(gsub('[$]',"", GetFeature(list_items, "MSRP")))
  options(warn = 0)
  return(MSRP)
}
CleanStr <- function(str) {
  str %>%
    gsub("[\t\n\r\v\f]", " ", .) %>%
    gsub("  +"," ",.) %>%
    gsub("^\\s+|\\s+$", "", .)
}
ExtractSection <- function(description,
                           section) {
  
  sprintf("(?i)%s:>>([^<]*)", section) %>%
    str_match(description, .) %>%
    .[ ,2] %>%
    CleanStr
}
GenTitle <- function(brand,
                     model,
                     description,
                     max_words=8) {
  Map(function(b, m, d) {
    if(is.na(m)) {
      strsplit(d, " ") %>%
        sapply(function(x) {
          paste0(x[1:min(length(x), max_words)], collapse = " ")
        })
    } else {
      paste(b, m)
    }
  }, brand, model, description) %>%
    as.character
}
DoSearch <- function(search_string,
                     df,
                     col.name = "Description",
                     whole_words = T,
                     parse_delim = T) {
  
  # Write search string to log
  
  # Handle search options and do search
  if (!parse_delim) {
    grep_str <- ifelse(whole_words, 
                       paste0("\\W(", search_string, ')\\W'), 
                       search_string)
    # print(grep_str)
    cat("Searching:", grep_str, "\n")
    
    grepl(grep_str, df[[col.name]], ignore.case = T) %>%
      df[.,]
  } else {
    grep_str <- search_string %>% 
      strsplit(" ") %>% 
      unlist %>%
      sapply(function(x) {
        ifelse(whole_words, 
               paste0("\\W(", x, ')\\W'), 
               x)
      }) %>% unlist
    
    # print(grep_str)
    cat("Searching:", grep_str, "\n")
    
    sapply(grep_str, grepl, df[[col.name]], ignore.case = T) %>%
      apply(1, all) %>%
      df[.,]
  }
}
SearchWrapper <- function(search_string,
                          search_df,
                          col.name = "Description",
                          join_df,
                          favs_df,
                          join.col.name = "auction_id",
                          ...) {
  # Performs DoSearch and joins results with auction_df
  # and checks for presence of items in favs_df
  res <- DoSearch(search_string, search_df, col.name, ...) %>%
    left_join(join_df, by = join.col.name)
  
  # Append favorites flag
  if (!is.null(favs_df)) {
    if (nrow(favs_df) > 0) {
      res <- favs_df %>%
        select(auction_id, Item = item_id) %>%
        mutate(is_favorite = T) %>% 
        right_join(res, by = c("auction_id", "Item"))
    }
  } else {
    res <- mutate(res, is_favorite = NA)
  }
  
  # Calculate time remaining
  out_time <- difftime(res$date, Sys.time())
  out_time <- paste(as.integer(out_time), units(out_time))
  
  res %>% 
    mutate(hrs_remaining = (as.numeric(date) - as.numeric(Sys.time()))/3600,
           Remaining = out_time) %>% 
    arrange(date)
}


## ///////////////////////////////////////////// ##
## UI OUTPUT FUNCTIONS ----------------------------
## ///////////////////////////////////////////// ##
StyleDescription <- function(description) {
  gsub("<<", "<br><strong>", description) %>%
    gsub(">>", "</strong>", .) %>%
    CleanStr
}
GenSearchUrl <- function(description,
                         site = c("amazon", "camel", "keepa")) {
  
  if (length(site) > 1) {
    cat("site =", site, "\n")
    stop("Select exactly one site to search")
  }
  
  if (description == "amazon") {
    search_base_url <- amazon_base
  } else if (description == "camel") {
    search_base_url <- camel_base
  } else if (description == "keepa") {
    search_base_url <- keepa_base
  } else {
    stop("Invalid site selected")
  }
  gsub(" +", "+", description) %>% paste0(search_base_url, . )
}
GenCamelUrl <- function(description) {
  gsub(" +", "+", description) %>% paste0(camel_base, . )
}
GenAmazonUrl <- function(description) {
  gsub(" +", "+", description) %>% paste0(amazon_base, . )
}
GenGcalUrl <- function(event_title,
                       stime,
                       description,
                       loc="" ) {
  
  start_time <- (stime - 15*60) %>% strftime(format = "%Y%m%dT%H%M00Z", tz="UTC" )
  end_time <- stime %>% strftime(format = "%Y%m%dT%H%M00Z", tz="UTC")
  
  gcal_base %>%
    rep_len( length( event_title ) ) %>%
    param_set( "text", event_title ) %>%
    param_set( "dates", paste0( start_time,"/", end_time )) %>%
    param_set( "details", description ) %>%
    param_set( "location", loc )
}
GenSearchOutput <- function(search_df,
                            pins = T) {
  res <- search_df %>%
    mutate(id = paste0(auction_id,";", Item),
           desc = ExtractSection(Description, "description"),
           title = GenTitle(Brand, Model, desc), # short title blurb
           camel_url = GenCamelUrl(title), # create amazon url
           amazon_url = GenAmazonUrl(title), # create amazon url
           gcal_url = GenGcalUrl(paste("Bid:", title) %>% url_encode,
                                 date, link.item),
           # Add html styling like <strong>
           description_html = StyleDescription(Description),
           # Style the favorites button accordingly
           heart_style = ifelse(!is.na(is_favorite), ' style="color:red;"', ''),
           buttons_html = sprintf(kItemButtonsHTML, camel_url,
                                  amazon_url, gcal_url, id, heart_style)
    )
  
  # Fill HTML template with values
  if (pins) {
    sprintf(kPinsResHTML2,
            strftime(res$date, format = "%a %r", tz = kTZ),
            res$link.item, res$img_src,
            res$description_html, res$buttons_html)
  } else { # table list
    sprintf(kTableResHTML, res$description_html, res$buttons_html)
  }
  
}
GenFavoritesItem <- function(product_title,
                             product_url,
                             description,
                             img_src,
                             end_time) {
  
  if (is.na(end_time)) {
    out_time <- "EXPIRED"
    style_class <- "default"
  } else {
    mins_remaining <- (as.numeric(end_time) - as.numeric(Sys.time()))/60
    out_time <- difftime(end_time, Sys.time())
    out_time <- paste(as.integer(out_time), units(out_time))
    if (mins_remaining < 15) {
      style_class <- "danger"
    } else if (mins_remaining < 60) {
      style_class <- "warning"
    } else if (mins_remaining < 60*24) {
      style_class <- "info"
    } else {
      out_time <- end_time %>% format("%a %b %d")
      style_class <- "primary"
    }
  }
  
  label_class <- sprintf("label label-%s pull-right", style_class)
  tags$li(class = "item",
          div(class = "product-img",
              a(href = product_url, target="_blank", img(src = img_src))
          ),
          div(class = "product-info",
              span(class="product-title",
                   product_title,
                   span(class = label_class,
                        shiny::icon("clock-o"),
                        out_time
                   )
              ),
              span(class="product-description", description)
          )
  )
}
dropdownFavsMenu <- function(...,
                             badgeStatus = "primary",
                             icon = NULL,
                             .list = NULL) {
  
  type = "messages"
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- "dropdown messages-menu favorites-menu"
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"),
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus),
                  numItems)
  }
  tags$li(class = dropdownClass,
          a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
            icon, badge
          ),
          tags$ul(class = "dropdown-menu dropdown-menu-favorites",
                  tags$li(class = "header",
                          paste("You have", numItems, "favorite items")),
                  tags$li(div(class = "box-body",
                              tags$ul(class = "menu products-list product-list-in-box", items)
                  )),
                  tags$li(div(class = "box-footer clearfix",
                              actionButton(
                                inputId = "clear_favs",
                                class="btn btn-sm btn-info btn-flat pull-left",
                                label = "Remove Expired"
                              ),
                              actionButton(
                                inputId = "clear_all_favs",
                                class="btn btn-sm btn-flat pull-right",
                                label = "Remove All"
                              )
                  ))
          )
  )
}
GenProductsGrid <- function(product_str, res) {
  
  layout_str <- 
  '<!-- Products Grid-->
  <div class="isotope-grid cols-3 mb-2">
    <div class="gutter-sizer"></div>
    <div class="grid-sizer"></div>
    %s
  </div>'
  
  sprintf(product_str,
          res$location,
          res$link.item,
          res$img_src,
          res$link.item,
          res$Item,
          res$MSRP) %>% 
    paste0(collapse="") %>% 
    sprintf(layout_str, .) %>%
    HTML
}
GenProductsList <- function(template_str, res) {
  sprintf(template_str,
          res$link.item,
          res$img_src,
          res$link.item,
          res$Item,
          res$MSRP,
          res$Description) %>% 
    paste0(collapse="") %>%
    HTML
}
GenPagination <- function(sorted_res, page, page_size = 12) {
  page <- as.integer(page)
  pages <- 1:ceiling(nrow(sorted_res) / page_size)
  classes <- character(length(pages))
  classes[page] <- ' class = "active"'
  sprintf('<li%s><a href="#">%s</a></li>', classes, pages) %>% 
    paste0(collapse = "") %>% 
    HTML
}
GenRangeSlider <- function(template_str, res) {
  template_str %>% 
    HTML
}
GenCheckbox <- function(id, label, value, val_label, val_parens) {
    
    html_str <- 
        '<label class="custom-control custom-checkbox d-block">
    <input class="custom-control-input" type="checkbox" name="%s" value="%s">
    <span class="custom-control-indicator"></span>
    <span class="custom-control-description">%s&nbsp;
    <span class="text-muted">(%s)</span>
    </span>
    </label>'
    
    wrapper <- 
        '<!-- Widget %s Filter-->
    <section class="widget option-set" data-group="%s">
    <h3 class="widget-title">%s</h3>%s
    </section>'
    
    sprintf(html_str, id, value, val_label, val_parens) %>% 
        paste0(collapse = "") %>% 
        sprintf(wrapper, label, label, label, .) %>% 
        HTML
}
GenLocationsFilter <- function(res) {
  
  loc <- count(res, location, sort = T) %>% 
    mutate(pretty_loc = gsub("(^[0-9]* )|(,.*$)", "", location))
  
  input_id <- "filter_loc"
  label <- "Locations"
  
  with(loc, GenCheckbox(input_id, label, location, pretty_loc, n))
      
}
GenConditionsFilter <- function(res) {
    
    dat <- count(res, Condition, sort = T) 
    print(dat)
    
    input_id <- "filter_condition"
    label <- "Conditions"
    
    with(dat, GenCheckbox(input_id, label, Condition, Condition, n))
}
GenFilters <- function(slider_template, res) {
  
  locations <- GenLocationsFilter(res)
  conditions <- GenConditionsFilter(res)
  slider <- GenRangeSlider(slider_template, res)
  list(locations, conditions, slider)
}
GetPhotoAreaSRCs <- function(html_obj) {
    html_obj %>% 
        html_nodes('#PhotoArea img') %>% 
        html_attr("src") %>% 
        grep("[.]\\w+$", ., ignore.case = T, value = T)
    
}

GetCurrentPrice <- function(html_obj) {
    html_obj %>% 
        html_nodes(".DataRow td") %>% 
        .[6] %>% 
        html_text %>% 
        as.numeric
}

## ///////////////////////////////////////////// ##
## SCRAPING FUNCTIONS -----------------------------
## ///////////////////////////////////////////// ##
Rescrape <- function(use.progress = T) {
  
  # Create a Progress object
  if (use.progress) {
    progress <- shiny::Progress$new()
    progress$set(message = "Scraping ...", value = 0)
    on.exit(progress$close())
    
    IncrementProgress <- function(incr) {
      # Progress call-back
      value <- progress$getValue()
      progress$set(value = value + incr)
    }
  }
  
  ### GET NEW LINKS
  # Reading in old auction links and comparing with currently-available links.
  # We only scrape links not previously known!
  current_links  <- ScrapeCurrentLinks2() %>% unique
  new_links <- if (file.exists(known_links_loc)) {
    read.csv(known_links_loc, header = F, stringsAsFactors = F)[,1] %>%
      setdiff(current_links, .)
  } else {
    current_links
  } 
  
  new_links <- ValidateLinks(new_links)
  print(new_links)
  
  # Add new timestamp
  sys_time <- Sys.time()
  data.frame(time = sys_time,
             method = ifelse( use.progress, "browser", "cron" )) %>%
    AppendCSV(timestamp_loc)
  
  # Exit if no new links
  if(!length(new_links) > 0) return(invisible())
  
  
  ### GET NEW AUCTIONS & ITEMS
  if (use.progress) {
    progress$set(detail = "Fetching auction list", value=0)
  }
  auctions_df <- new_links %>%
    GetNewAuctionsWrapper(use.progress, if(use.progress) IncrementProgress else NULL) %>%
    RemoveNonLocalAuctions  # Filters out non-local auctions
  
  # Exit if no new auctions to scrape
  if(!nrow(auctions_df) > 0) return(invisible())
  
  if (use.progress) {
    progress$set(detail = "Fetching auction items", value = kAuctionsItemsBarSplit)
  }
  items_df <- auctions_df %>%
    GetNewItemsWrapper(use.progress, if(use.progress) IncrementProgress else NULL)
  
  
  ### COMBINE NEWLY SCRAPED AUCTIONS WITH PREVIOUSLY SCRAPED AUCTIONS
  combined_auctions_df <- if(file.exists(auctions_loc)) {
    ReadAuctionsCSV(auctions_loc, kTimeFileFormat) %>% rbind(auctions_df)
  } else {
    auctions_df
  }
  combined_items_df  <- if(file.exists(items_loc)) {
    ReadItemsCSV(items_loc) %>% rbind(items_df)
  } else {
    items_df
  }
  
  # Find expired auction_ids
  curExpiredAuctionIDs <- combined_auctions_df %>%
    filter(date + kExpiredTimeOffset <= sys_time) %>%
    select(auction_id) %>% unlist
  
  # Subtract IDs already archived to get toArchiveAuctionIDs
  toArchiveAuctionIDs <- if (file.exists(expired_auctions_loc)) {
    ReadAuctionsCSV(expired_auctions_loc, kTimeFileFormat)$auction_id %>%
      setdiff(curExpiredAuctionIDs, .)
  } else {
    curExpiredAuctionIDs
  }
  
  ### SAVE CURRENT & EXPIRED AUCTIONS & ITEMS
  # Archive expired auctions & items where auction_id in toArchiveAuctionIDs
  combined_auctions_df %>%
    filter(auction_id %in% toArchiveAuctionIDs) %>%
    mutate(write_time = sys_time) %>%
    AppendCSV(expired_auctions_loc)
  
  combined_items_df %>%
    filter(auction_id %in% toArchiveAuctionIDs) %>%
    select(Item, Description, link.item, img_src, auction_id, Features) %>%
    mutate(write_time = sys_time) %>%
    AppendCSV(expired_items_loc)
  
  
  # Filter & overwrite those auctions and items where auction_id not in curExpiredAuctionIDs
  cat("\nSaving current auctions...\n")
  combined_auctions_df %>%
    filter(!(auction_id %in% curExpiredAuctionIDs)) %>%
    write.csv(auctions_loc, row.names = F)
  
  cat("Saving current items...\n")
  combined_items_df %>%
    filter(!(auction_id %in% curExpiredAuctionIDs)) %>%
    write.csv(items_loc, row.names = F)
  
  cat("Saving known links...\n")
  write.table(current_links, known_links_loc, sep = ",", row.names = F, col.names = F )
  
  cat("\n|----- ENDED RESCRAPE -----|\n")
}
CheckRescrapeDue <- function(curTime) {
  
  if (file.exists(timestamp_loc)) {
    lastTime <- GetLastTimestamp(timestamp_loc, kTimeFileFormat)
    cat("Last Scrape:  ", 
        format(lastTime, kTimeFileFormat, usetz = T, tz = kTZ), "\n")
    
  } else {
    lastTime <- curTime - auto_refresh_time
    cat("No Scrape History Found!!\n\n")
  }
  
  # Status Updates
  cat("Current Time: ", 
      format(curTime, kTimeFileFormat, usetz = T, tz = kTZ), "\n")
  cat("Refresh Due:  ",
      format(lastTime + auto_refresh_time, kTimeFileFormat,
             usetz = T, tz = "EST5EDT"), "\n\n")
  
  # Rescrape if due time
  if (curTime >= lastTime + auto_refresh_time) Rescrape()
}
ScrapeCurrentLinks <- function() {
  read_html("http://bidfta.com/") %>%
    html_nodes(".auction a[target=_blank]")  %>%
    html_attr("href")
}
ScrapeCurrentLinks2 <- function() {
  
  lnk <- "http://bidfta.bidqt.com/BidFTA/services/invoices/WlAuctions/filter?size=500&sort=endDateTime%20asc"
  
  result <- list()
  page <- 1
  last <- FALSE
  while (!last) {
    a <- POST(lnk, content_type("application/x-www-form-urlencoded"), 
              query = list(page = page))
    
    # Status code checking
    if (a$status_code != 200) {
      stop("Status code not 200!")
    }
    
    # Store results
    res <- content(a)
    result <- c(result, res$content)
    
    # Increment
    last <- res$last
    page <- page + 1
  }
  
  # result
  sapply(result, function(x) paste0(x$auctionUrl, x$auctionNumber)) %>% 
    gsub("mnlist", "mndetails", .)
}
ScrapeAuctionDetails <- function(new_links,
                                 incr,
                                 progress_updater = NULL) {
  # Pulls auction details from an auction description page link
  
  if( !is.null( progress_updater )) progress_updater(incr)
  
  a  <- list()
  a$auction_id <- gsub(".*\\?","",new_links)
  cat( sprintf("%-18s", a$auction_id ) )
  
  tmp <- read_html(new_links) %>%
    html_nodes("table tr td")
  
  try(a$date  <- tmp[[6]] %>%
        html_text %>%
        gsub("\\.", ",", .) %>%
        gsub("(\\d+)(st|nd|rd|th)","\\1", .) %>%
        strptime("%B %e, %Y %I:%M %p", tz = kTZ)
  )
  
  #print(class(a$date))
  try(
    if (is.null(a$date) ) {
      cat (" | no date\n")
      return(NULL)
    }
    else if ( is.na(a$date) ){
      cat (" | no date\n")
      return(NULL)
    }
    # else if (a$date < Sys.time() ) {
    #     cat (" | expired\n")
    #     return(NULL)
    # }
    else {
      cat(" |", a$date %>% format(usetz = T),"\n")
    }
  )
  
  a$title  <- tmp %>% html_nodes("#auction_title") %>% .[[1]] %>% html_text
  
  # 8th entry / row in table contains auction location
  a$location  <- tmp[[8]] %>% html_text %>% CleanStr
  a
}
ScrapeItemlist <- function(lnk,
                           incr,
                           progress_updater = NULL) {
  # Pulls item lists from an auction item list link
  
  if( !is.null( progress_updater )) progress_updater(incr)
  
  auction_id <-  gsub( ".*\\?", "", lnk) %>% gsub("/.*","",.)
  cat( sprintf("%-18s", auction_id ) )
  root_link <- lnk %>% gsub("category/ALL","", .)
  
  itemlist <- root_link %>%
    gsub("mnlist", "mnprint", .) %>%
    read_html(encoding = "ISO-8859-1") %>%
    html_node("#DataTable") %>%
    html_table(header = T, fill = T) %>%
    mutate(Item = gsub("[.]","", Item),
           Description = ParseDescription(Description),
           link.item = paste0(root_link, Item),
           Features = GrokFeatures(Description)
    ) %>%
    # Extract specific features from grokked Features
    mutate(
      Brand = GetFeature(Features, "Brand"),
      Model = GetFeature(Features, "Model"),
      MSRP = GetMSRP(Features),
      Condition = GetFeature(Features, "Additional Info|Condition"),
      Desc = GetFeature(Features, "Desc")
    )
  
  ####
  # Future proofing; encode the Features list in json (character for csv compatibility)
  itemlist$Features <- FlattenFeatures(itemlist$Features)
  ####
  
  
  cat(" |","itemlist ok")
  #itemlist %>%  print
  
  n <- nrow(itemlist)
  img_link <- try( itemlist$link.item %>% .[n] %>%
                     read_html %>%
                     html_node("#DataTable") %>%
                     html_node("img") %>%
                     html_attr("src") )
  
  cat(" |", ifelse( class(img_link) == "try-error", "!img_link missing!" , "img_link present"))
  
  try({
    
    # Form an image url by figuring out what goes before the //
    # and what goes after the item number
    img_prefix <- gsub("/[^/]+$", "/", img_link)
    
    # img_suffix <- gsub(paste0(img_prefix, itemlist$Item[n]), "", img_link)
    img_suffix <- gsub(img_prefix, "", img_link) %>%
      gsub("[a-zA-Z]*[0-9]+","",.)
    
    itemlist %>% mutate(img_src = paste0(img_prefix, Item, img_suffix)) #%>%
    # select(-Features)
    
  })
  
}
ValidateLinks <- function(lnks) {
  # Checks whether auction links and consistent, valid, and bidfta-hosted.
  
  # Fix links that lead directly to page itemlist instead of auction description
  fix_these  <- grep("mnlist",lnks)
  if (length(fix_these) > 0 ) {
    lnks[fix_these] <- lnks[fix_these] %>%
      gsub("mnlist","mndetails",.) %>%
      sub("/category/ALL","",.)
  }
  
  # Filter out blank links
  lnks <- lnks[lnks != '']
  
  # Filter out links on other auction sites
  bidfta_hosted <- grepl("bidfta", lnks, ignore.case = T)
  
  cat( sum(bidfta_hosted) ,"New valid auction links found\n")
  cat( sum(!bidfta_hosted) , "New external auctions were ignored:\n")
  paste("*", lnks[!bidfta_hosted], collapse = "\n") %>%
    cat("\n")
  
  lnks <- lnks[bidfta_hosted]
}
GetNewAuctionsWrapper <- function(new_links, use.progress, progress_updater = NULL) {
  # Loops over new links to call ScrapeAuctionDetails and flattens results
  ptm <- proc.time()
  
  cat("\n|----- GETTING AUCTIONS -----|\n\n")
  auctions_incr <- kAuctionsItemsBarSplit / length(new_links)
  auctions <- 1:length( new_links ) %>%
    lapply(function(i) {
      cat( sprintf("%-4s", i))
      ScrapeAuctionDetails(new_links[i],
                           auctions_incr,
                           if(use.progress) progress_updater else NULL)}
    )
  
  # Report how many null auctions and filter them out
  null.auctions <- auctions %>% sapply(is.null)
  cat("\n", sum( null.auctions ),"expired or invalid auctions removed\n\n")
  auctions <- auctions[!null.auctions]
  
  # Convert auctions list to auctions_df
  auctions_df <- auctions %>%
    lapply(data.frame, stringsAsFactors = FALSE) %>%
    do.call(rbind, .) %>%
    mutate(link.descpage = new_links[!null.auctions],
           link.pageditems  = link.descpage %>%
             gsub("mndetails","mnlist",.) %>% paste0("/category/ALL")
    )
  print(proc.time() - ptm)
  
  # Cleaning location info
  auctions_df$location <- auctions_df$location %>% gsub(" \\d{5}.*","",., ignore.case = T)
  auctions_df$location %>% table %>% data.frame %>% arrange(desc(Freq)) %>% print
  
  return(auctions_df)
}
GetNewItemsWrapper <- function(auctions_df, use.progress, progress_updater) {
  # Loops ScrapeItemlist over auctions in auctions_df
  ptm <- proc.time()
  
  cat("\n|----- GETTING ITEMS -----|\n")
  items_incr <- (1 - kAuctionsItemsBarSplit)/length(auctions_df$auction_id)
  
  items <- 1:nrow(auctions_df) %>%
    lapply( function(i) {
      cat("\n", sprintf("%-4s", i) )
      ScrapeItemlist(auctions_df$link.pageditems[i], items_incr, progress_updater)
    }) %>%
    setNames(auctions_df$auction_id)
  
  items_df <-  do.call(rbind, items) %>%
    mutate(auction_id = gsub("\\.[0-9]+","", row.names(.)))
  
  cat("\n\n", nrow(items_df), "items scraped\n\n")
  print(proc.time() - ptm)
  
  return(items_df)
}
RemoveNonLocalAuctions <- function(auctions_df) {
  # Removes auctions whose locations not on the kLocalLocations list
  good_loc  <- paste(kLocalLocations, sep = "", collapse = "|") %>%
    grepl(auctions_df$location, ignore.case = T)
  
  # Report how many auctions in different locations
  cat("\n", sum(!good_loc), "out-of-town auctions removed")
  cat("\n", sum(good_loc), "local auctions to be scraped\n")
  
  auctions_df <- filter(auctions_df, good_loc)
}
