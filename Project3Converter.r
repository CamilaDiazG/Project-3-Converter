library(shiny)
library(igraph)

ui <- fluidPage(
  titlePanel("Converter: Regular Grammar to Finite Automaton"),
  
  sidebarLayout(
    sidebarPanel(
      # Área de texto para la gramática
      textAreaInput("grammarInput", "Type your regular grammar:", 
                    value = "S -> aA\nS -> bA\nA -> aB\nA -> bB\nA -> a\nB -> aA\nB -> bA", 
                    rows = 10),
      br(),
      # 9. Etiqueta dinámica para el tipo de autómata
      h4("Automaton Type:"),
      textOutput("automatonType")
    ),
    
    mainPanel(
      #Sección donde se despliega el autómata
      plotOutput("automatonPlot", height = "500px")
    )
  )
)

server <- function(input, output) {
  
  #procesar la gramática en tiempo real
  parsedGrammar <- reactive({
    req(input$grammarInput)

    
    
    # Separar por líneas limpiando espacios
    lines <- strsplit(input$grammarInput, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    
    # Estructuras para guardar los datos
    from_nodes <- c()
    to_nodes <- c()
    labels <- c()
    
    for (line in lines) {
      if (grepl("->", line)) {
        parts <- strsplit(line, "->")[[1]]
        from <- trimws(parts[1])
        right <- trimws(parts[2])
        
        # Validar formato básico X -> Y
        if (nchar(right) == 2) {
          symbol <- substr(right, 1, 1)
          to <- substr(right, 2, 2)
        } else if (nchar(right) == 1) {
          symbol <- right
          to <- "Z" #El estado final es Z
        } else {
          next
        }
        
        from_nodes <- c(from_nodes, from)
        to_nodes <- c(to_nodes, to)
        labels <- c(labels, symbol)
      }
    }
    
    data.frame(from = from_nodes, to = to_nodes, label = labels, stringsAsFactors = FALSE)
  })
  
  #Determinar si es DFA o NFA
  output$automatonType <- renderText({
    df <- parsedGrammar()
    if (nrow(df) == 0) return("Waiting for input...")
    
    # Validar si un estado va al mismo lugar con el mismo símbolo de forma duplicada
    # O si desde un estado sale el mismo símbolo hacia destinos diferentes
    is_nfa <- any(duplicated(df[, c("from", "label")]))
    
    if (is_nfa) {
      "non-deterministic"
    } else {
      "deterministic"
    }
  })
  
  #Renderizar el autómata en tiempo real
  output$automatonPlot <- renderPlot({
    df <- parsedGrammar()
    req(nrow(df) > 0)

    # Agrupa los símbolos si van del mismo origen al mismo destino
    df <- aggregate(label ~ from + to, data = df, FUN = paste, collapse = ", ")
    
    # Crear los enlaces para igraph (par origen -> destino)
    edges <- as.vector(t(df[, c("from", "to")]))
    g <- graph(edges, directed = TRUE)
    
    # Asignar etiquetas a las aristas
    E(g)$label <- df$label
    
    #colores de los nodos
    v_names <- V(g)$name
    node_colors <- rep("white", length(v_names))
    node_colors[v_names == "S"] <- "green"
    node_colors[v_names == "Z"] <- "red"
    
    # Dibujar el grafo
    plot(g, 
         edge.label = E(g)$label,
         vertex.color = node_colors,
         vertex.size = 30,
         vertex.label.color = "black",
         vertex.label.font = 2,
         edge.arrow.size = 0.5,
         edge.label.color = "black",
         layout = layout_with_kk)
  })
}

shinyApp(ui = ui, server = server)