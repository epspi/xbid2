# app.R
require(dplyr)
require(shiny)
require(shinyjs)
require(httr)
source("router.R")
source("global.R")


# Server-side rendered content definitions
uiSearchResults <- enclose(function() {
  grid_product <- read_file("www/_grid_product_sprintf.html")
  grid_layout <- read_file("www/_grid_isotope.html")
  search_template2 <- read_file("www/_search_results2.html")
  range_slider_template <- read_file("www/_slider.html")
  
  function(search_res, query, opt, grid = T) {
    
    page <- opt$page
    page_size <- opt$page_size
    
    htmlTemplate(
        text_ = search_template2, 
        search_term = query,
        n_items = nrow(search_res),
        products =  GenProductsGrid(grid_product, search_res), 
        pagination = GenPagination(search_res, page, page_size),
        filters = GenFilters(range_slider_template, search_res),
        document_ = F)
  }
})

uiIndex <- tagList(
  htmlTemplate("www/_main_slider.html"),
  htmlTemplate("www/_promo.html"),
  htmlTemplate("www/_top_cats.html")
)

uiCart <- function() htmlTemplate("www/_cart.html")
uiLogin <- function() htmlTemplate("www/_account-login.html")
uiAccountOrders <- function() htmlTemplate("www/_account-orders.html")
uiAccountProfile <- function() htmlTemplate("www/_account-profile.html")
uiAccountWishlist <- function() htmlTemplate("www/_account-wishlist.html")

routenames <- list(
  "/",
  "/cart",
  "/account-orders",
  "/account-profile",
  "/account-wishlist",
  "/search",
  "/login"
)


# ============== UI ================

ui <- htmlTemplate("www/shell.html", 
                   content = uiOutput("inner", class = "offcanvas-wrapper"),
                   document_ = T)

# ============== SERVER ================

server <- function(input, output, session) {
  
  
  #  ------->> Startup ---------
  
  # Check if rescrape needed
  curTime  <- Sys.time()
  CheckRescrapeDue(curTime)
  
  ## Read in data
  # Make sure auction expiration has right time zone
  # After loading auctions, filter out those which have expired
  auctions_df  <-  ReadAuctionsCSV(auctions_loc, kTimeFileFormat) %>%
    filter(date + kExpiredTimeOffset > curTime)
  
  # Filter out items from auctions that have passed
  items_df  <- ReadItemsCSV(items_loc) %>%
    filter(auction_id %in% auctions_df$auction_id) %>%
    mutate(MSRP = as.numeric(MSRP))
  
  
  #  ------->> Authentication ---------
  
  # --User Profile--
  # observe(print(input$id_token))
  usr_profile <- eventReactive(input$id_token, {
    
    if(input$id_token == "") {
      cat("No token!\n")
      return(NULL)
      
    } else {
      
      # User info using id_token JWT
      res <- POST(url = "https://epspi.auth0.com/tokeninfo",
                  body = list(id_token = input$id_token),
                  encode = "form")
      
      if (res$status_code == 200) {
        
        result <- content(res)
        session$sendCustomMessage("profile_handler", jsonlite::toJSON(result))
        cat("LOGGED IN PROFILE:\n")
        # print(result)
        print(jsonlite::toJSON(result, pretty = 4, auto_unbox = T))
        return(result)
      } else {
        
        session$sendCustomMessage("expired_handler", "")
        return(NULL)
      }
    }
  })
  

  #  ------->> Routing ---------
  home_path <- "/"
  
  # --React to hash / clean--
  path <- reactive({
    
    validate(need(usr_profile(), "Please sign in" ))
    session$clientData$url_hash %>% 
      gsub("#", "", .) %>% 
      ifelse(. == "", home_path, .)
  })
  
  # --React to new clean path--
  route <- reactive({
    
    path <- path()
    route <- gsub("[?].*", "", path)
    cat("\n--New URL--\nRoute:", route, "\n")
    
    # Check for query args in Search submission 
    if (route == "/search") {
      
      q <- parseQueryString(gsub(".*[?]", "", path))
      cat("Query:\n ", 
          paste(names(q), unlist(q), sep = "=", collapse = "\n  "), "\n")
      
      # Update reactive values (bad style?)
      if ("term" %in% names(q)) last_query(q$term)
      if ("page" %in% names(q)) search_options$page <- q$page
    }
    
    return(route)
  })
  
  # --Server routed page--
  output$inner <- renderUI({
    
    switch(route(),
           "/" = uiIndex,
           "/cart" = uiCart(),
           "/account-orders" = uiAccountOrders(),
           "/account-profile" = uiAccountProfile(),
           "/account-wishlist" = uiAccountWishlist(),
           "/search" = uiSearchResults(paginated_res(), 
                                       last_query(), search_options, 
                                       grid_view()),
           "/login" = uiLogin(),
           "")
  })
  
  
  #  ------->> Search ---------
  
  # --Search result set--
  last_query <- reactiveVal(NULL)
  grid_view <- reactiveVal(TRUE)
  search_options <- reactiveValues(
    page = 1,
    page_size = 12
  )
  
  
  # --Do Search--
  search_res <- reactive({
    
    validate(need(last_query(), "Nothing to search for..." ))
    isolate({
      result <- SearchWrapper(last_query(), 
                              search_df = items_df,
                              join_df = auctions_df,
                              # favs_df = favorites$data,
                              favs_df = NULL)
      
      if (nrow(result) > 0) mutate(res, Condition = MutateConditions(Condition))
      else result
    })
  })
  
  paginated_res <- reactive({
    validate(need(search_res(), "No items found"))
    search_res()
  })
}


shinyApp(ui = ui, server = server)
