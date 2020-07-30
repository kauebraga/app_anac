library(dplyr)
library(data.table)
library(shiny)
library(shinyWidgets)
library(mapdeck)
library(shinyBS)

set_token("pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")


# open data
data <- fread("data/air_odmatrix_month.csv")


server <- function(input, output) {

# filter month
mes_filtrado <- reactive({
  
  data[month == input$mes]
  
})


# filter top
top_filtrado <- reactive({
  
  mes_filtrado() %>%
    arrange(desc(total_pass)) %>%
    slice(1:input$top) %>%  
    # calculate stroke
    mutate(stroke = scales::rescale(total_pass, to = c(3, 12))) %>%
    # make tooltip
    mutate(tooltip = sprintf('<p><strong> %s -> %s </strong><br><strong>Passageiros/mês:</strong> %s</p>',
           name_muni_uf_from, name_muni_uf_to, total_pass))
  
})




# RENDER BRAZIL'S BASEMAP -------------------------------------------------------

output$map <- renderMapdeck({
  
  mapdeck(location = c(-43.95988, -19.902739), zoom = 3)

  
  
})


observeEvent({c(input$mes, input$top)},{
  
  mapdeck_update(map_id = "map") %>%
    add_arc(
      data = top_filtrado()
      , layer_id = "arc_layer"
      , origin = c("lon_from", "lat_from")
      , destination = c("lon_to", "lat_to")
      # , stroke_from = "airport1"
      # , stroke_to = "airport2"
      , stroke_to_opacity = 50
      , stroke_width = "stroke"
      , update_view = FALSE
      , focus_layer = FALSE
      , tooltip = "tooltip"
      , auto_highlight = TRUE
      , tilt = 3
    )
  
  
  
})


}