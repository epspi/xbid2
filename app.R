# app.R

require(jsonlite)
require(dplyr)
require(shiny)

# ============== UI ================

ui <- htmlTemplate("www/shell.html", 
                   content = uiOutput("body", class = "offcanvas-wrapper")
)


# ============== SERVER ================
server <- function(input, output, session) {
  
  
  #  ------->> Content ---------
  
  # --Body--
  output$body <- renderUI({
    
    tagList(
      # htmlTemplate("www/main_slider.html"),
      htmlTemplate("www/promo.html"),
      htmlTemplate("www/top_cats.html"),
      htmlTemplate("www/footer.html")
    )
  })
}


shinyApp(ui = ui, server = server)
