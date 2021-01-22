# https://github.com/r4fun/keys
# For more information about what types of hotkeys
# you can use, please take a look at https://craig.is/killing/mice

library(shiny)
library(keys)

ui <- fluidPage(
  useKeys(),
  # si quieres especificar los keys de partida (sin utilizar
  # el action button) activa la siguiente linea)
  #keysInput("keys", c("a", "ctrl", "up", "down", "left", "right")),
  actionButton("add", "Add keybinding"), # action button para activar
  actionButton("rm", "Remove `a` keybinding"), # action button para desactivar
  plotOutput("hist")
)

server <- function(input, output, session) {
  
  observeEvent(input$add, { # activa estas teclas despues de pulsar el action button 1
    addKeys("keys", c("a", "ctrl+shift", "up", "down", "left", "right"))
  })
    
  observeEvent(input$rm, { # desactiva esta tecla despues de pulsar el action button 2
    removeKeys("a")
  })
  
  observeEvent(input$keys, { # output cuando uses los keys arriba especificados
    output$hist <- renderPlot({
      hist(rnorm(100))
    })
    })
    
}

shinyApp(ui, server)