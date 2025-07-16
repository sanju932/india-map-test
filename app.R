library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(janitor)
library(DT)
library(plotly)
library(htmlwidgets)
library(webshot2)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #121212;
        color: #ffffff;
      }
      .sidebar-panel {
        background-color: #2c2c2c;
        border-right: 2px solid #f39c12;
        height: 100vh;
        padding: 20px;
      }
      .sidebar-panel h3 {
        color: #f39c12;
        font-weight: bold;
        margin-top: 0;
      }
      .red-button {
        background-color: #e74c3c !important;
        color: white !important;
        width: 100%;
        margin-bottom: 10px;
        border: none;
      }
      .form-control, .selectize-input, .selectize-dropdown {
        background-color: #333 !important;
        color: white !important;
        border-color: #555 !important;
      }
      .nav-tabs > li > a {
        background-color: #1f1f1f !important;
        color: #ffffff !important;
        border-color: #444 !important;
      }
      .tab-content {
        background-color: #121212;
        padding: 10px;
        border: 1px solid #444;
      }
    "))
  ),
  fluidRow(
    column(
      width = 3,
      div(
        class = "sidebar-panel",
        h3("Crime Explorer"),
        pickerInput("year", "Select Year", choices = NULL,
                    options = list(style = "btn-warning")),
        pickerInput("crime_type", "Select Crime Type", choices = NULL,
                    options = list(style = "btn-danger")),
        downloadButton("download_map", "Export Map", class = "red-button")
      )
    ),
    column(
      width = 9,
      leafletOutput("crime_map", height = 500),
      br(),
      tabsetPanel(
        tabPanel("Plots",
                 plotlyOutput("bar_plot", height = 300),
                 plotlyOutput("line_plot", height = 300),
                 plotlyOutput("pie_plot", height = 400)
        ),
        tabPanel("Data Table",
                 DTOutput("crime_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  india_sf <- st_read("india_st.shp", quiet = TRUE)
  
  crime_data_wide <- read_csv("17_Crime_by_place_of_occurrence_2001_2012.csv", show_col_types = FALSE) %>%
    clean_names()
  
  crime_data_long <- crime_data_wide %>%
    pivot_longer(
      cols = -c(state_ut, year),
      names_to = "crime_type",
      values_to = "number_of_cases"
    ) %>%
    mutate(crime_type = str_replace_all(crime_type, "_", " ") %>% toupper()) %>%
    mutate(STATE = case_when(
      state_ut == "A & N ISLANDS" ~ "ANDAMAN & NICOBAR",
      state_ut == "D & N HAVELI" ~ "DADRA & NAGAR HAVELI",
      state_ut == "DAMAN & DIU" ~ "DAMAN & DIU",
      state_ut == "DELHI UT" ~ "DELHI",
      state_ut == "UTTARANCHAL" ~ "UTTARAKHAND",
      state_ut == "ORISSA" ~ "ODISHA",
      TRUE ~ toupper(state_ut)
    ))
  
  observe({
    updatePickerInput(session, "year",
                      choices = sort(unique(crime_data_long$year)),
                      selected = max(crime_data_long$year, na.rm = TRUE))
    updatePickerInput(session, "crime_type",
                      choices = sort(unique(crime_data_long$crime_type)),
                      selected = unique(crime_data_long$crime_type)[1])
  })
  
  crime_data_mapped <- reactive({
    crime_data_long %>% filter(!is.na(STATE))
  })
  
  filtered_crime <- reactive({
    req(input$year, input$crime_type)
    crime_data_mapped() %>%
      filter(year == input$year, crime_type == input$crime_type) %>%
      group_by(STATE) %>%
      summarise(total_crimes = sum(number_of_cases, na.rm = TRUE))
  })
  
  joined_data <- reactive({
    india_sf %>%
      left_join(filtered_crime(), by = "STATE") %>%
      mutate(total_crimes = replace_na(total_crimes, 0))
  })
  
  map_widget <- reactive({
    data <- joined_data()
    pal <- colorNumeric(palette = "YlOrRd", domain = data$total_crimes)
    leaflet(data) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        fillColor = ~pal(total_crimes),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        label = ~paste0(STATE, ": ", total_crimes, " cases")
      ) %>%
      addLegend(
        pal = pal,
        values = data$total_crimes,
        title = "Number of Crimes"
      )
  })
  
  output$crime_map <- renderLeaflet({ map_widget() })
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("crime_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      saveWidget(map_widget(), "temp_map.html", selfcontained = TRUE)
      webshot("temp_map.html", file = file, cliprect = "viewport")
      unlink("temp_map.html")
    }
  )
  
  output$bar_plot <- renderPlotly({
    plot_data <- filtered_crime() %>%
      arrange(desc(total_crimes)) %>%
      slice_head(n = 10)
    p <- ggplot(plot_data, aes(x = reorder(STATE, total_crimes), y = total_crimes, fill = total_crimes)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_gradient(low = "#f1c40f", high = "#e74c3c") +
      geom_text(aes(label = total_crimes), hjust = -0.2, color = "white") +
      coord_flip() +
      labs(title = "Top 10 States by Crime Count", x = "State/UT", y = "Number of Crimes") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#121212"),
        panel.background = element_rect(fill = "#121212"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", face = "bold")
      )
    ggplotly(p)
  })
  
  output$line_plot <- renderPlotly({
    line_data <- crime_data_mapped() %>%
      filter(crime_type == input$crime_type) %>%
      group_by(year) %>%
      summarise(total_crimes = sum(number_of_cases, na.rm = TRUE))
    p <- ggplot(line_data, aes(x = year, y = total_crimes)) +
      geom_area(fill = "#f39c12", alpha = 0.4) +
      geom_line(color = "#f1c40f", size = 1.5) +
      geom_point(size = 3, color = "#f39c12") +
      labs(title = paste("Trend of", input$crime_type, "in India"),
           x = "Year", y = "Number of Crimes") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#121212"),
        panel.background = element_rect(fill = "#121212"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", face = "bold")
      )
    ggplotly(p)
  })
  
  output$pie_plot <- renderPlotly({
    pie_data <- filtered_crime() %>%
      arrange(desc(total_crimes)) %>%
      slice_head(n = 6)
    plot_ly(
      pie_data,
      labels = ~STATE,
      values = ~total_crimes,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      hole = 0.4,
      marker = list(colors = c("#e74c3c", "#f1c40f", "#f39c12", "#d35400", "#c0392b", "#e67e22"))
    ) %>% layout(title = "Donut Chart: Top 6 Crime States")
  })
  
  output$crime_table <- renderDT({
    datatable(
      filtered_crime(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)