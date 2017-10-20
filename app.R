# app.R
require(dplyr)
require(shiny)
require(shinyjs)
require(httr)
source("router.R")

uiIndex <- tagList(
  # htmlTemplate("www/main_slider.html"),
  htmlTemplate("www/_promo.html"),
  htmlTemplate("www/_top_cats.html")
  # htmlTemplate("www/_footer.html")
)
uiCart <- tagList(
  htmlTemplate("www/_cart.html")
)
uiShopGrid <- tagList(
  htmlTemplate("www/_shop-grid-ls.html")
)
routes <- list(
  "/" = uiIndex,
  "/cart" = uiCart,
  "/shop-grid-ls" = uiShopGrid
) 

# ============== UI ================

ui <- htmlTemplate("www/shell.html", 
                   content = uiOutput("body", class = "offcanvas-wrapper"),
                   # content = uiIndex,
                   document_ = T
)

# ============== SERVER ================
server <- function(input, output, session) {
  
  #  ------->> Authentication ---------
  
  observe(print(usr_profile()))
  usr_profile <- eventReactive(input$token, {
    if(is.null(input$token)) {
      cat("No token!\n")
      return(NULL)
    } else {
      cat("\ntoken:", input$token, "\n\n")
      res <- POST(url = "https://epspi.auth0.com/tokeninfo",
                  body = list(id_token = input$token),
                  encode = "form")
      if (res$status_code == 200) {
        result <- content(res)
        session$sendCustomMessage("profile_handler", jsonlite::toJSON(result))
        return(result)
      } else {
        session$sendCustomMessage("expired_handler", "")
        return(NULL)
      }
    }
  })
  
  
  #  ------->> Routing ---------
  
  path <- reactiveVal("/")
  observeEvent(input$route_clicked, {
    if (input$route_clicked != path()) {
      cat("Clicked:", input$route_clicked, "\n")
      path(input$route_clicked)
    }
  })
  
  route_script <- MakeRouter(routes)
  # cat(route_script)
  runjs(route_script)
  
  
  # --Body--
  output$body <- renderUI({
    routes[[path()]]
  })
  
}


shinyApp(ui = ui, server = server)
