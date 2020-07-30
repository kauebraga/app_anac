library(shinyBS)
library(shiny)
library(shinyWidgets)
library(mapdeck)

shinyUI(
  
  div(class = "navbar-default",
      # Load Css
      tags$head(includeCSS("www/styles.css")),
      # Select custom slider
      # https://divadnojnarg.github.io/blog/customsliderinput/
      chooseSliderSkin("HTML5", color = "#112446")
      # Start navbar page
      , navbarPage("ANAC Data", id = "tabs",
                   # # Landing page
                   # tabPanel(title = "", value = "tab_home",
                   #          uiOutput('landing_page')),
                   # Map page
                   tabPanel(title = "Mapa", value = "tab_mapa", 
                            # Output map
                            mapdeckOutput("map"),
                            # Create the side panel  
                            absolutePanel(id = "controls", class = "panel panel-default", 
                                          fixed = TRUE, draggable = FALSE,
                                          top = 80, right = 20, width = 350, height = 615,
                                          pickerInput(
                                            inputId = "mes",
                                            label = "Escolha o mes",
                                            choices = c("Janeiro" = 1,
                                                        "Fevereiro" = 2,
                                                        "Março" = 3,
                                                        "Abril" = 4,
                                                        "Maio" = 5,
                                                        "Junho" = 6)
                                            
                                          ),
                                          pickerInput(
                                            inputId = "top",
                                            label = "Escolha a quantidade",
                                            choices = c(10, 20, 30, 40, 50)
                                          )
                                          )
                   )
      )
      # # About page
      # tabPanel(id = "about", title = "Sobre",
      #          includeMarkdown("about.md")
      #          
      #          
      # )
      
      
  )
  
)