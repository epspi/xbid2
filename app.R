# app.R
require(jsonlite)
require(dplyr)
require(shiny)
require(shinyjs)
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
  
  path <- reactiveVal("/")
  observeEvent(input$route_clicked, {
    if (input$route_clicked != path()) {
      cat("Clicked:", input$route_clicked, "\n")
      path(input$route_clicked)
    }
  })
  
    route_script <- MakeRouter(routes)
    cat(route_script)
    runjs(route_script)
  
  #  ------->> Content ---------
  
  # --Body--
  output$body <- renderUI({
    routes[[path()]]
  })
}


shinyApp(ui = ui, server = server)
