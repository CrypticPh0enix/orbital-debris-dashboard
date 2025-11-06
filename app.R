# Load libraries
library(shiny)
library(shinyWidgets)
library(reshape2)
library(tidyverse)
library(plotly)
library(bslib)
library(shinyjs)

# ======== DATA LOADING AND PREPROCESSING ========
# Load debris data by orbit type
data <- read.csv("space_debris_by_orbit.csv")
long_data <- data %>%
  mutate(Year = as.integer(Year)) %>%
  pivot_longer(-Year, names_to = "Orbit", values_to = "Count") %>%
  filter(!is.na(Count))

# Define orbit label mapping for readability
orbit_labels <- c(
  "Low.Earth.orbit" = "Low Earth Orbit",
  "Medium.Earth.orbit" = "Medium Earth Orbit",
  "Geostationary.orbit" = "Geostationary Orbit",
  "High.Earth.orbit" = "High Earth Orbit"
)

# Load and clean satellite purpose dataset
sat_data <- read.csv("satellite_in_orbits.csv", encoding = "latin1")
sat_clean <- sat_data %>%
  filter(`Class.of.Orbit` %in% c("LEO", "MEO", "GEO")) %>%
  mutate(Function_Group = case_when(
    grepl("Communications", Purpose, ignore.case = TRUE) ~ "Communications",
    grepl("Earth Observation", Purpose, ignore.case = TRUE) ~ "Earth Observation",
    grepl("Earth Science", Purpose, ignore.case = TRUE) ~ "Earth Science",
    grepl("Navigation|Positioning", Purpose, ignore.case = TRUE) ~ "Navigation/GPS",
    grepl("Weather|Meteorological", Purpose, ignore.case = TRUE) ~ "Weather",
    grepl("Surveillance", Purpose, ignore.case = TRUE) ~ "Surveillance",
    grepl("Technology", Purpose, ignore.case = TRUE) ~ "Technology Development",
    grepl("Educational", Purpose, ignore.case = TRUE) ~ "Educational",
    grepl("Space Science|Observation", Purpose, ignore.case = TRUE) ~ "Space Science",
    TRUE ~ "Other"
  )) %>%
  group_by(Function_Group, `Class.of.Orbit`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  rename(Orbit = `Class.of.Orbit`)

# Load debris by country
debris_country <- read.csv("debris_by_country.csv")
country_long <- debris_country %>%
  rename(Year = 1) %>%
  pivot_longer(-Year, names_to = "Country", values_to = "Fragments") %>%
  mutate(Country = case_when(
    Country == "CIS" ~ "CIS (Former Soviet Union)",
    Country == "OTHER" ~ "Other Countries (Aggregated)",
    Country == "ESA" ~ "ESA(European Space Agency)",
    TRUE ~ Country
  )) %>%
  filter(!is.na(Fragments))

options(shiny.fullstacktrace = TRUE)

# Define the UI
ui <- page_navbar(
  title = "Space Debris",
  div(class = "dashboard-intro",
      "What goes up doesn’t always come down. As space fills with decades of satellite launches and missions, Earth’s orbit is becoming dangerously crowded. This growing congestion increases risks for satellites, space exploration, and the systems we rely on for communication, navigation, and weather. This dashboard visualises the growing problem of space debris. Navigate through trends in debris growth, understand satellite functions, and explore which countries contribute most to orbital pollution. Use the filters and timeline sliders to interact with the data.",
      tags$em(
        tags$strong("Tip: Click and drag on any chart to zoom in. Double-click to reset the view.")
      )
  ),
  # Theme settings
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Open Sans"),
    bg = "#f5fafd",
    fg = "#333333",
    primary = "#17827F",
    secondary = "#E38E2C"
  ),
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
    
  ),
  useShinyjs(),
  tags$script(HTML("
  function toggleFullscreen(id) {
    var elem = document.getElementById(id);
    if (!document.fullscreenElement) {
      elem.requestFullscreen().catch(err => {
        console.error(`Error attempting to enable full-screen mode: ${err.message} (${err.name})`);
      });
    } else {
      document.exitFullscreen();
    }
  }
")),
  
  # Panel 1: Debris Growth
  nav_panel(
    "Debris Growth",
    layout_sidebar(
      sidebar = sidebar(
        # Orbit selection input
        helpText("Select one or more orbit types to view debris trends across different space layers."),
        div(
          tags$label("Choose Orbits to Display: ",
                     tags$span(
                       "\u2139",  
                       title = "Orbit definitions:\n• Low Earth orbit (LEO): < 2,000 km\n• Medium Earth orbit (MEO): 2,000–35,586 km\n• Geostationary orbit (GEO): 35,786 km\n• High Earth orbit (HEO): > 35,986 km",
                       style = "cursor: help; color: #17827F; font-weight: bold;"
                     )
          ), br(),
          div(class = "orbit-checkbox", 
            checkboxGroupInput("selectedOrbits", NULL,
                               choices = gsub("\\.", " ", unique(long_data$Orbit)),
                               selected = gsub("\\.", " ", unique(long_data$Orbit)))
          )
        ),
      ),
      card(
        card_header(HTML("<span style='color:#228B22; font-weight: bold;'>Tracked Objects by Orbit Type</span>")),
        card_body(
          div(class = "plot-caption",
              tags$b("Space debris keeps rising! "),
              "The number of active satellites, rocket bodies, and other tracked objects is steadily rising across all orbital layers, especially Low Earth Orbit (", tags$abbr(title = "Low Earth Orbit: ~2000km altitude", "LEO"), 
              "), where most modern satellites operate. This trend highlights the growing congestion and long-term risks in Earth’s orbital environment."),
          
          # Plot container
          div(class = "plot-container",
              div(id = "orbitPlotBox",
                  plotlyOutput("orbitPlot", height = "390px")
              )
          ),
          
          # Play button and slider control
          div(class = "slider-row",
            actionButton("playPauseGrowth", "\u25B6", class = "custom-play", style = "padding: 2px 6px; height: 28px; font-size: 12px;"),
            sliderInput("yearRangeGrowth", NULL,
                        min = min(long_data$Year),
                        max = max(long_data$Year),
                        value = c(min(long_data$Year), max(long_data$Year)),
                        step = 1,
                        sep = "",
                        width = "100%"),
            actionButton("fullscreenOrbit", "⛶", class = "btn-xs", title = "Toggle Fullscreen")
          )
          ),
        
        # Footer
        card_footer(
          div(class = "footer-note",
              HTML("
      <div>Data: United States Space Force. (2025). <em>Number of objects in space.</em> 18th Space Defense Squadron. <a href=' https://www.space-track.org/' target='_blank'> https://www.space-track.org/ </a></div>
      <div>Dashboard by Roja Joshi | RMIT Data Visualisation 2025</div>
    ")
          )
        )
      )
    )
  ),
  
  # Panel 2: Satellite Functions
  nav_panel(
    "Satellite Functions",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("barMode", "Display Mode:",
                     choices = c("Stacked" = "stack", "Grouped" = "dodge"),
                     selected = "stack"),
        helpText("Filter by satellite purpose: useful to compare how different satellites dominate specific orbits."),
        checkboxGroupInput("selectedFunctions", "Choose satellite types",
                           choices = sort(unique(sat_clean$Function_Group)),
                           selected = sort(unique(sat_clean$Function_Group))),
        actionButton("selectAll", "Select All"),
        actionButton("deselectAll", "Deselect All")
      ),
      card(
        card_header(HTML("<span style='color:#228B22; font-weight: bold;'>Satellites by Function and Orbit</span>")),
        card_body(
          div(class = "plot-caption",
              tags$b("Why is LEO the most crowded?"),
              "Most satellites for communication, Earth monitoring, and navigation are placed in Low Earth Orbit, making it a debris hotspot, while medium and geostationary orbits support specialized systems such as GPS and weather monitoring."),
          div(style = "display: flex; justify-content: space-between; align-items: flex-end;",
              div(id = "functionPlotBox", style = "flex: 1;",
                  plotlyOutput("functionPlot", height = "465px")
              ),
              div(style = "margin-left: 10px; margin-bottom: 5px;",
                  actionButton("fullscreenFunction", "⛶", class = "btn-xs", title = "Toggle Fullscreen")
              )
          )
          
        ),
        card_footer(
          div(class = "footer-note",
              HTML("
              <div>Data: Union of Concerned Scientists. (2024). UCS Satellite Database. <a href='https://www.ucsusa.org/resources/satellite-database' target='_blank'> https://www.ucsusa.org/resources/satellite-database</a></div>
              <div>Dashboard by Roja Joshi | RMIT Data Visualisation 2025</div>
            ")
          )
        )
      )
    )
  ),
  
  # Panel 3 – Debris by Country/Org
  nav_panel(
    "Debris by Country/Org",
    layout_sidebar(
      sidebar = sidebar(
        helpText("Choose the countries or organizations to include in the chart."),
        div(class = "country-checkbox",
            checkboxGroupInput("selectedCountries", NULL,
                               choices = sort(unique(country_long$Country)),
                               selected = unique(country_long$Country))
        )
      ),
      card(
        card_header(HTML("<span style='color:#228B22; font-weight: bold;'>Debris Fragments by Country/Organization</span>")),
        card_body(
          div(class = "plot-caption",
              tags$b("Some countries leave a larger footprint in space. Which countries contribute most to debris?"),
              " This plot shows the major nations and alliances that have contributed to orbital debris over time."),
          
          fluidRow(
            column(9,
                   div(id = "countryPlotBox",  
                   plotlyOutput("countryPlot", height = "390px")
                   )
            ),
            column(3,
                   div(class = "notable-events",
                       tags$h5("Notable Events", class = "event-header"),
                       tags$ul(
                         tags$li("2003: No debris data available."),
                         tags$li("2007: Spike in debris from China’s anti-satellite (ASAT) test."),
                         tags$li("2009: Collision between Iridium and Cosmos satellites created thousands of fragments."),
                         tags$li("2021: Russia conducted an ASAT test.")
                       )
                   )
            )
          ),
          div(class = "slider-row",
              actionButton("playPause", "\u25B6", class = "custom-play"),
              sliderInput("yearRangeCountry", NULL,
                          min = min(country_long$Year),
                          max = max(country_long$Year),
                          value = c(min(country_long$Year), max(country_long$Year)),
                          step = 1,
                          sep = "",
                          width = "100%",
                          animationOptions(interval = 1000, loop = FALSE)),
              actionButton("fullscreenCountry", "⛶", class = "btn-xs", title = "Toggle Fullscreen")
          )
        ),
        
        card_footer(
          div(class = "footer-note small-text",
              HTML("
              <div>Data: NASA. (2024). <em>Orbital Debris Quarterly News</em>. NASA Orbital Debris Program Office. <a href='https://orbitaldebris.jsc.nasa.gov/quarterly-news/' target='_blank'>https://orbitaldebris.jsc.nasa.gov/quarterly-news/</a></div>
              <div>Dashboard by Roja Joshi | RMIT Data Visualisation 2025</div>
            ")
          )
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # === Reusable animation handler ===
  handle_animation <- function(play_id, slider_id, year_range_getter, playing_flag) {
    autoInvalidate <- reactiveVal(NULL)
    
    observeEvent(input[[play_id]], {
      if (playing_flag()) {
        playing_flag(FALSE)
        autoInvalidate(NULL)
        updateActionButton(session, play_id, label = "\u25B6")  # ▶
      } else {
        playing_flag(TRUE)
        updateActionButton(session, play_id, label = "\u23F8")  # ⏸
        autoInvalidate(invalidateLater(1000, session))
      }
    })
    
    observe({
      req(playing_flag())
      isolate({
        current_range <- input[[slider_id]]
        current_start <- current_range[1]
        current_end <- current_range[2]
        max_year <- year_range_getter()[2]
        min_year <- year_range_getter()[1]
        
        if (current_end < max_year) {
          updateSliderInput(session, slider_id, value = c(current_start, current_end + 1))
        } else {
          updateSliderInput(session, slider_id, value = c(min_year, min_year))
        }
      })
      autoInvalidate(invalidateLater(1000, session))
    })
  }
  observeEvent(input$fullscreenOrbit, {
    runjs("toggleFullscreen('orbitPlotBox')")
  })
  observeEvent(input$fullscreenFunction, {
    runjs("toggleFullscreen('functionPlotBox')")
  })
  observeEvent(input$fullscreenCountry, {
    runjs("toggleFullscreen('countryPlotBox')")
  })
  
  # === Flags for play/pause buttons ===
  playingGrowth <- reactiveVal(FALSE)
  playingCountry <- reactiveVal(FALSE)
  
  # === Call reusable handler for each animation ===
  handle_animation(
    play_id = "playPauseGrowth",
    slider_id = "yearRangeGrowth",
    year_range_getter = reactive(c(min(long_data$Year), max(long_data$Year))),
    playing_flag = playingGrowth
  )
  
  handle_animation(
    play_id = "playPause",
    slider_id = "yearRangeCountry",
    year_range_getter = reactive(c(min(country_long$Year), max(country_long$Year))),
    playing_flag = playingCountry
  )
  
  # === Debris Growth Plot ===
  filtered_data <- reactive({
    long_data %>%
      filter(
        Year >= input$yearRangeGrowth[1],
        Year <= input$yearRangeGrowth[2],
        Orbit %in% gsub(" ", ".", input$selectedOrbits)
      ) %>%
      mutate(Orbit_Label = orbit_labels[Orbit]) 
  })
  
  create_plot <- function(gg) {
    ggplotly(gg, tooltip = "text") %>%
      config(
        scrollZoom = FALSE,
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"
        )
      )
  }
  
  output$orbitPlot <- renderPlotly({
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No data available for the selected orbits and year range."))
    
    year_start <- input$yearRangeGrowth[1]
    year_end <- input$yearRangeGrowth[2]
    year_range <- year_end - year_start
    tick_interval <- if (year_range <= 10) 1 else if (year_range <= 30) 5 else 10
    
    gg <- ggplot(data, aes(
      x = Year, y = Count,
      color = Orbit_Label, group = Orbit_Label,
      text = paste0("Year: ", Year, "<br>Orbit: ", Orbit_Label, "<br>Count: ", Count)
    )) +
      geom_line(linewidth = 0.7) +
      labs(x = "Year", y = "Number of Tracked Objects", color = "Orbit") +
      scale_color_manual(values = c(
        "Low Earth Orbit" = "#51AADE",
        "Medium Earth Orbit" = "#7570b3",
        "Geostationary Orbit" = "#17827F",
        "High Earth Orbit" = "#E38E2C"
      )) +
      scale_x_continuous(
        limits = c(year_start, year_end),
        breaks = seq(year_start, year_end, by = tick_interval),
        labels = scales::number_format(accuracy = 1, big.mark = "")
      ) +
      theme_minimal(base_size = 9) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text( hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9),
        plot.margin = margin(8, 8, 8, 8)
      )
    
    create_plot(gg)
  })
  
  # === Satellite Function Plot ===
  filtered_sat_data <- reactive({
    sat_clean %>%
      filter(Function_Group %in% input$selectedFunctions)
  })
  
  output$functionPlot <- renderPlotly({
    validate(
      need(length(input$selectedFunctions) > 0, 
           "Please select at least one satellite type to display the chart.")
    )
    
    position_mode <- if (input$barMode == "stack") position_stack() else position_dodge2(width = 0.9, preserve = "single")
    
    gg <- ggplot(filtered_sat_data(), aes(
      x = Function_Group,
      y = Count,
      fill = Orbit,
      text = paste0(
        "Function: ", Function_Group,
        "<br>Orbit: ", Orbit,
        "<br>Count: ", Count
      )
    )) +
      geom_bar(stat = "identity", position = position_mode) +
      scale_fill_manual(values = c("LEO" = "#51AADE", "MEO" = "#7570b3", "GEO" = "#17827F")) +
      labs(
        x = "Satellite Function",
        y = "Number of Satellites",
        fill = "Orbit Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9)
      )
    
    create_plot(gg)
  })
  
  # === Debris by Country/Org Plot ===
  
  filtered_country_data <- reactive({
    country_long %>%
      filter(Year >= input$yearRangeCountry[1], Year <= input$yearRangeCountry[2]) %>%
      filter(Country %in% input$selectedCountries)
  })
  
  # Define a fixed color palette for all countries
  country_colors <- c(
    "CHINA" = "#FF5733",                      # Bright red-orange
    "CIS (Former Soviet Union)" = "#3366CC",  # Vivid blue
    "ESA(European Space Agency)" = "#00BFC4", # Bright cyan
    "FRANCE" = "#FF8C00",                     # Strong orange
    "INDIA" = "#2ECC71",                      # Bright green
    "JAPAN" = "#FFD700",                      # Bold yellow-gold
    "Other Countries (Aggregated)" = "#C71585", # Rich magenta
    "PRC" = "#FF69B4",                        # Hot pink
    "UK" = "#8E44AD",                         # Vibrant purple
    "USA" = "#1ABC9C"                         # Aqua-turquoise
  )
  
  output$countryPlot <- renderPlotly({
    df <- filtered_country_data()
    validate(need(nrow(df) > 0, "No data available for the selected country/organization and year range."))
    year_span <- input$yearRangeCountry[2] - input$yearRangeCountry[1]
    tick_gap <- if (year_span <= 10) 1 else if (year_span <= 20) 2 else 5
    
    china_2008 <- df %>% filter(Country == "CHINA", Year == 2008)
    cis_2010 <- df %>% filter(Country == "CIS (Former Soviet Union)", Year == 2010)
    usa_2010 <- df %>% filter(Country == "USA", Year == 2010)
    cis_2022 <- df %>% filter(Country == "CIS (Former Soviet Union)", Year == 2022)
    
    df$Country <- factor(df$Country, levels = names(country_colors))  # Fixed levels
    
    gg <- ggplot(df, aes(x = Year, y = Fragments, color = Country,
                         text = paste0("Country/Org: ", Country,
                                       "<br>Year: ", Year,
                                       "<br>Fragments: ", Fragments))) +
      geom_point(size = 2, alpha = 0.7) +
      labs(x = "Year", y = "Debris Fragments", color = "Country/Organization") +
      scale_color_manual(values = country_colors, drop = FALSE) +
      scale_x_continuous(
        breaks = seq(input$yearRangeCountry[1], input$yearRangeCountry[2], by = tick_gap),
        labels = function(x) format(x, digits = 4)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
    
    if (nrow(china_2008) > 0) {
      gg <- gg +
        annotate("text", x = 2008, y = china_2008$Fragments - 450, label = "2007: China ASAT Test", size = 3, hjust = 0, fontface = "bold") +
        annotate("segment", x = 2008.5, xend = 2008, y = china_2008$Fragments - 280, yend = china_2008$Fragments, arrow = arrow(length = unit(0.2, "cm")), color = "black")
    }
    if (nrow(cis_2010) > 0 && nrow(usa_2010) > 0) {
      peak_val <- max(cis_2010$Fragments, usa_2010$Fragments, na.rm = TRUE)
      gg <- gg +
        annotate("text", x = 2006, y = peak_val + 500, label = "2009: Iridium–Cosmos Collision", size = 3, hjust = 0, fontface = "bold") +
        annotate("segment", x = 2010, xend = 2008, y = usa_2010$Fragments, yend = peak_val + 440,
                 arrow = arrow(length = unit(0.2, "cm")), color = "black") +
        annotate("segment", x = 2010, xend = 2008, y = cis_2010$Fragments, yend = peak_val + 430,
                 arrow = arrow(length = unit(0.2, "cm")), color = "black")
    }
    if (nrow(cis_2022) > 0) {
      gg <- gg +
        annotate("text", x = 2022, y = cis_2022$Fragments + 330, label = "2021: Russia ASAT Test", size = 3, hjust = 0, fontface = "bold") +
        annotate("segment", x = 2022.4, xend = 2022, y = cis_2022$Fragments + 260, yend = cis_2022$Fragments,
                 arrow = arrow(length = unit(0.2, "cm")), color = "black")
    }
    
    create_plot(gg)
  })
  
  # === Function Checkbox Controls ===
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(inputId = "selectedFunctions", selected = sort(unique(sat_clean$Function_Group)))
  })
  
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(inputId = "selectedFunctions", selected = character(0))
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)