pacman::p_load(
  leaflet,
  dplyr,
  networkD3,
  shiny,
  bslib,
  DT,
  htmltools
)

# Add value:
# Convert GA solution to a matrix
# Optimal quantities matrix
optimal_quantities <- matrix(c(
  43.306058, 10.87137, 13.26283,
  15.524054, 30.42748, 48.86716,
  7.599769, 44.93531, 27.46289,
  18.556927, 17.60396, 41.85203,
  16.305016, 13.15498, 21.79634
), nrow = 5, ncol = 3, byrow = TRUE)

# Warehouse data:
warehouses <- data.frame(
  ID = 1:5,
  Latitude = c(21.0285, 16.0545, 10.7769, 14.0583, 19.8060),
  Longitude = c(105.8040, 108.2022, 106.6957, 108.2772, 105.7460),
  District = c("Đống Đa", "Hải Châu", "Quận 1", "Mang Yang", "Đông Sơn"),
  Province = c("Hà Nội", "Đà Nẵng", "Hồ Chí Minh", "Gia Lai", "Thanh Hóa"),
  Address = c(
    "2RH3+9HX Đống Đa, Hà Nội, Việt Nam",
    "3632+RV4 Hải Châu, Đà Nẵng, Việt Nam",
    "QMGW+Q74 Quận 1, Hồ Chí Minh, Việt Nam",
    "375G+8VG Mang Yang, Gia Lai, Việt Nam",
    "RP4W+99X Đông Sơn, Thanh Hóa, Việt Nam"
  )
)

# Distribution center data:
distribution_centers <- data.frame(
  ID = 1:3,
  Latitude = c(17.97486, 11.77690, 15.12233),
  Longitude = c(102.6309, 106.6957, 108.7994),
  District = c("Viêng Chăn", "Lộc Ninh", "Quảng Ngãi"),
  Province = c("Lào", "Bình Phước", "Quảng Ngãi"),
  Address = c(
    "XJFJ+W8X Viêng Chăn, Lào",
    "QMGW+Q74 Lộc Ninh, Bình Phước, Việt Nam",
    "4QCX+WPQ Quảng Ngãi, Việt Nam"
  )
)

# Create a data frame with the warehouse data
result <- data.frame(
  Warehouse = c("WH 1", "WH 2", "WH 3", "WH 4", "WH 5",
                "WH 1", "WH 2", "WH 3", "WH 4", "WH 5",
                "WH 1", "WH 2", "WH 3", "WH 4", "WH 5"),
  DC = c("DC 1", "DC 1", "DC 1", "DC 1", "DC 1",
         "DC 2", "DC 2", "DC 2", "DC 2", "DC 2",
         "DC 3", "DC 3", "DC 3", "DC 3", "DC 3"),
  Has_Machine = c("Yes", "No", "No", "No", "No",
                  "No", "No", "No", "No", "No",
                  "No", "No", "No", "No", "No"),
  Total_Weight = c(4.3306058, 1.5524054, 0.7599769, 1.8556927, 1.6305016,
                   1.0871366, 3.0427479, 4.4935308, 1.7603961, 1.3154977,
                   1.3262832, 4.8867159, 2.7462887, 4.1852028, 2.1796338),
  Weight_Category = c("2.5 to 5", "1.5 to 2.5", "< 1.5", "1.5 to 2.5", "1.5 to 2.5",
                      "< 1.5", "2.5 to 5", "2.5 to 5", "1.5 to 2.5", "< 1.5",
                      "< 1.5", "2.5 to 5", "2.5 to 5", "2.5 to 5", "1.5 to 2.5"),
  Loading_Cost = c(64300, 40680, 144640, 38790, 18750,
                   11610, 97870, 36380, 69120, 70030,
                   143740, 91150, 64990, 114230, 96240),
  Transport_cost = c(3802097.2, 5037171.3, 7297102.2, 5952559.7, 3086494.5,
                     8264855.3, 4021256.9, 889559.4, 2449208.3, 7188382.3,
                     5831987.6, 974375.3, 4273915.6, 1047828.2, 4905804.6)
)

# Custom icon:
warehouse_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/Loccx78vn/genetic-algorithm-method/refs/heads/main/img/warehouse_icon.png",
  iconWidth = 30,
  iconHeight = 30
)

dc_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/Loccx78vn/genetic-algorithm-method/refs/heads/main/img/distribution_center.png",
  iconWidth = 30,
  iconHeight = 30
)
# Define a custom theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "litera",
  primary = "#3b5998",
  secondary = "#5cb85c",
  success = "#5cb85c",
  info = "#5bc0de",
  warning = "#f0ad4e",
  danger = "#d9534f"
)

# Define UI
ui <- page_fluid(
  theme = my_theme,
  
  # Custom CSS for better visualization
  tags$head(
    tags$style(HTML("
      .card-header {
        background-color: #3b5998;
        color: white;
        font-weight: bold;
      }
      .nav-tabs .nav-link.active {
        font-weight: bold;
        color: #3b5998;
        border-bottom: 2px solid #3b5998;
      }
      .nav-tabs .nav-link {
        color: #6c757d;
      }
      .checkbox span {
        font-weight: normal;
      }
      .checkbox input:checked + span {
        font-weight: bold;
        color: #3b5998;
      }
      .warehouse-selection {
        border-left: 4px solid #3b5998;
        padding-left: 10px;
      }
      .dataTables_wrapper {
        padding: 10px;
        border-radius: 5px;
      }
      .table thead th {
        background-color: #e9ecef;
      }
      .leaflet-container {
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .sankey-container {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 10px;
        background-color: #f8f9fa;
      }
      .summary-stats {
        background-color: #f8f9fa;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 15px;
        border-left: 4px solid #3b5998;
      }
    "))
  ),
  
  # Page header - simplified
  card(
    card_header(
      tags$div(
        class = "d-flex align-items-center",
        tags$img(
          src = "https://raw.githubusercontent.com/Loccx78vn/genetic-algorithm-method/refs/heads/main/img/supply-chain-management.png", 
          height = "30px", 
          style = "margin-right: 10px;"
        ),
        h3("Warehouse Distribution Dashboard", class = "m-0")
      )
    ),
    card_body(
      "This dashboard shows the optimal distribution of goods from warehouses to distribution centers."
    )
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Controls",
      width = 300,
      bg = "#f8f9fa",
      class = "warehouse-selection",
      
      h4("Warehouse Selection", class = "mb-3 text-primary"),
      
      p("Select one or more warehouses to view their distribution data on the map and in the charts."),
      
      checkboxGroupInput(
        inputId = "warehouse", 
        label = NULL, 
        choices = setNames(
          paste("Warehouse", 1:5),
          paste("Warehouse", 1:5, "-", warehouses$District, ",", warehouses$Province)
        ),
        selected = paste("Warehouse", 1)
      ),
      
      hr(),
      
      tags$div(
        class = "alert alert-info",
        icon("info-circle"), 
        "Click on the map markers for detailed location information."
      )
    ),
    
    # Main content - restructured to put summary above map
    tabsetPanel(
      tabPanel(
        title = "Interactive Map", 
        icon = icon("map"),
        card(
          full_screen = TRUE,
          height = "650px", # Increased height
          card_body(
            # Summary stats placed above the map
            tags$div(
              class = "summary-stats",
              uiOutput("summary_stats")
            ),
            # Map with increased height
            leafletOutput("map", height = "550px") # Increased map height
          )
        )
      ),
      
      tabPanel(
        title = "Cost Analysis", 
        icon = icon("table"),
        card(
          full_screen = TRUE,
          height = "650px", # Increased to match
          card_header("Cost Table"),
          DTOutput("cost_table")
        )
      ),
      
      tabPanel(
        title = "Flow Diagram", 
        icon = icon("diagram-project"),
        card(
          full_screen = TRUE,
          height = "650px", # Increased to match
          card_header("Distribution Flow Diagram"),
          tags$div(
            class = "sankey-container",
            uiOutput("sankey_diagram")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  location <- reactive({
    req(input$warehouse) 
    as.numeric(sub("Warehouse ", "", input$warehouse))
  })
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    req(location())
    
    # Color palette for routes
    route_colors <- c("#FF5733", "#33A8FF", "#33FF57", "#D433FF", "#FFD133")
    
    # Initialize the map
    map <- leaflet() |>
      addTiles() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMarkers(data = warehouses |> filter(ID %in% location()), 
                 lat = ~Latitude, 
                 lng = ~Longitude, 
                 label = ~paste0("<strong> ID Warehouse: </strong> ", ID, "<br/> ",
                                 "<strong> Province: </strong> ", Province, "<br/> ",
                                 "<strong> District: </strong> ", District, "<br/> ",
                                 "<strong> Address: </strong> ", Address, "<br/> ") |> 
                   lapply(htmltools::HTML),
                 icon = warehouse_icon) |>
      addMarkers(data = distribution_centers, 
                 lat = ~Latitude, 
                 lng = ~Longitude, 
                 label = ~paste0("<strong> ID Distribution Center: </strong> ", ID, "<br/> ",
                                 "<strong> Province: </strong> ", Province, "<br/> ",
                                 "<strong> District: </strong> ", District, "<br/> ",
                                 "<strong> Address: </strong> ", Address, "<br/> ") |> 
                   lapply(htmltools::HTML),
                 icon = dc_icon)
    
    qty_data <- optimal_quantities[location(), , drop = FALSE]  
    
    # Add routes based on the optimal quantities with different colors
    route_colors <- c("#FF5733", "#33A8FF", "#33FF57", "#D433FF", "#FFD133")
    line_weights <- c(2, 3, 4, 5, 6)  # Variable line weights based on quantity
    
    # Add routes based on the optimal quantities
    for (i in seq_along(location())) {
      wh_id <- location()[i]
      wh_idx <- which(warehouses$ID == wh_id)
      color_idx <- (wh_id - 1) %% length(route_colors) + 1
      
      for (j in 1:ncol(qty_data)) {
        if (qty_data[i, j] > 0) {
          route_start <- warehouses[warehouses$ID == wh_id, c("Longitude", "Latitude")]
          route_end <- distribution_centers[distribution_centers$ID == j, c("Longitude", "Latitude")]
          
          # Calculate line weight based on quantity (normalized)
          weight_multiplier <- qty_data[i, j] / max(optimal_quantities) * 5
          weight <- max(2, min(6, weight_multiplier + 2))  # Scale between 2-6
          
          map <- map |>
            addPolylines(
              lat = c(route_start$Latitude, route_end$Latitude),
              lng = c(route_start$Longitude, route_end$Longitude),
              color = route_colors[color_idx],
              weight = weight,
              opacity = 0.7,
              label = paste0("Quantity: ", round(qty_data[i, j], 2)),
              dashArray = "5, 5",
              popup = paste0(
                "<strong>From:</strong> Warehouse ", wh_id,
                "<br><strong>To:</strong> Distribution Center ", j,
                "<br><strong>Quantity:</strong> ", round(qty_data[i, j], 2)
              )
            )
        }
      }
    }
    
    map  # Return the modified map
  })
  
  # Render the cost table
  output$cost_table <- renderDT({
    req(location())
    
    cost_data <- result |> 
      filter(Warehouse %in% paste("WH", location())) |> 
      select(c(Warehouse, DC, Loading_Cost, Transport_cost))
    
    # Add a Total Cost column
    cost_data$Total_Cost <- cost_data$Loading_Cost + cost_data$Transport_cost
    
    # Format currency values
    cost_data$Loading_Cost <- formatC(cost_data$Loading_Cost, format="f", digits=0, big.mark=",")
    cost_data$Transport_cost <- formatC(cost_data$Transport_cost, format="f", digits=0, big.mark=",")
    cost_data$Total_Cost <- formatC(cost_data$Total_Cost, format="f", digits=0, big.mark=",")
    
    datatable(cost_data,
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#3b5998', 'color': 'white'});",
                  "}"
                ),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              rownames = FALSE,
              class = 'compact stripe hover') |>
      formatStyle(
        'Warehouse',
        backgroundColor = styleEqual(
          paste("WH", 1:5), 
          c('#FFC3A0', '#A0D2FF', '#C8FFB0', '#D5A0FF', '#FFF0A0')
        )
      ) |>
      formatStyle(
        'DC',
        backgroundColor = styleEqual(
          c("DC 1", "DC 2", "DC 3"), 
          c('#FFDDC3', '#C3E5FF', '#DDFFC3')
        )
      )
  })
  
  
  # Create and render the Sankey diagram
  selected_indices <- reactive({
    as.numeric(sub("Warehouse ", "", input$warehouse))
  })
  
  # Filter data based on selected warehouses
  filtered_data <- reactive({
    result |>
      filter(Warehouse %in% paste("WH",input$warehouse))
  })

  # Create and render the Sankey diagram
  output$sankey_diagram <- renderUI({
    req(length(selected_indices()) > 0)
    
    # Create links data frame
    links <- data.frame()
    
    # Add links for each selected warehouse
    for (i in 1:length(selected_indices())) {
      wh_idx <- selected_indices()[i]
      wh_data <- optimal_quantities[wh_idx, , drop = FALSE]
      
      # Add links for each DC
      for (j in 1:ncol(wh_data)) {
        if (wh_data[1, j] > 0) {
          links <- rbind(links, data.frame(
            source = i - 1,  # Use index in selected warehouses (0-based)
            target = length(selected_indices()) + j - 1,  # DCs come after warehouses
            value = wh_data[1, j]
          ))
        }
      }
    }
    
    # Only proceed if we have links
    req(nrow(links) > 0)
    
    # Create nodes data frame
    nodes <- data.frame(
      name = c(paste("Warehouse", selected_indices()), paste("DC", 1:3))
    )
    
    # Create the Sankey network
    sankey <- sankeyNetwork(
      Links = links, 
      Nodes = nodes, 
      Source = "source", 
      Target = "target", 
      Value = "value",
      NodeID = "name",
      height = 500,  
      width = 800,
      fontSize = 12,
      nodeWidth = 20,
      sinksRight = TRUE
    )
    
    # Customize tooltips and add click functionality to show a value panel
    sankey <- htmlwidgets::onRender(sankey, "
    function(el, x) {
      // Create a div for the value panel
      var valuePanel = d3.select('body').append('div')
        .attr('class', 'value-panel')
        .style('position', 'absolute')
        .style('padding', '8px')
        .style('background', 'lightgray')
        .style('border', '1px solid gray')
        .style('border-radius', '4px')
        .style('opacity', 0);  // Start hidden

      // Append title for tooltips on hover
      d3.selectAll('.link')
        .append('title')
        .text(function(d) { return 'Quantity: ' + d.value.toFixed(2); });  // Show value on hover

      // Add click event to show the value panel
      d3.selectAll('.link')
        .on('click', function(event, d) {
          // Ensure we get the correct value
          var value = d3.select(this).datum().value;
          var source = nodes.name[d.source.index];
          var target = nodes.name[d.target.index];

          // Position the value panel near the mouse cursor
          valuePanel
            .style('left', (event.pageX + 5) + 'px')
            .style('top', (event.pageY + 5) + 'px')
            .style('opacity', 1)  // Make it visible
            .html('From: ' + source + '<br>To: ' + target + '<br>Quantity: ' + value.toFixed(2));  // Set the text with details

          // Hide the panel after a delay
          setTimeout(function() {
            valuePanel.style('opacity', 0);
          }, 3000);  // Change the delay as needed
        });
    }
    ")
    
    # Add title and caption
    tagList(
      sankey,
      tags$p(style = "font-size: 12px; text-align: right;", 
             "A wider arrow indicates that a larger quantity is being sent from that warehouse to a distribution center.")
    )
  })
  
  # Add a summary statistics output at the bottom of the map tab
  output$summary_stats <- renderUI({
    req(location())
    
    selected_data <- result |> 
      filter(Warehouse %in% paste("WH", location()))
    
    total_loading <- sum(selected_data$Loading_Cost)
    total_transport <- sum(selected_data$Transport_cost)
    total_weight <- sum(selected_data$Total_Weight)
    
    div(
      class = "mt-3 p-3 bg-light rounded",
      h4("Summary Statistics", class = "text-primary mb-3"),
      div(class = "row",
          div(class = "col-md-4",
              div(class = "card text-white bg-primary mb-3",
                  div(class = "card-body",
                      h5(class = "card-title", "Total Weight"),
                      h3(class = "card-text", paste0(round(total_weight, 2), " tons"))
                  )
              )
          ),
          div(class = "col-md-4",
              div(class = "card text-white bg-success mb-3",
                  div(class = "card-body",
                      h5(class = "card-title", "Total Loading Cost"),
                      h3(class = "card-text", paste0("₫", formatC(total_loading, format="f", digits=0, big.mark=",")))
                  )
              )
          ),
          div(class = "col-md-4",
              div(class = "card text-white bg-info mb-3",
                  div(class = "card-body",
                      h5(class = "card-title", "Total Transport Cost"),
                      h3(class = "card-text", paste0("₫", formatC(total_transport, format="f", digits=0, big.mark=",")))
                  )
              )
          )
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
