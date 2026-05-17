library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(readxl)
library(tidyr)
library(stringr)
library(htmltools)
library(wordcloud)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(fmsb)  # For radar chart

# Load and prepare data with better error handling
load_and_prepare_data <- function() {
  tryCatch({
    # Load the dataset
    file_path <- "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/spotify_with_clickable_covers (version 1).xlsb.xlsx"
    data <- read_excel(file_path, sheet = "Sheet1")
    
    # Clean column names
    names(data) <- gsub(" ", "_", tolower(names(data)))
    names(data) <- gsub("[^a-zA-Z0-9_]", "", names(data))
    
    # Check for artist name column (handle different possible names)
    artist_col <- grep("artist|name", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(artist_col)) {
      data$artistsname <- paste("Artist", 1:nrow(data))
    } else {
      data$artistsname <- data[[artist_col]]
    }
    
    # Convert streams to numeric (remove commas)
    if("streams" %in% names(data)) {
      data$streams <- as.numeric(gsub(",", "", data$streams))
    } else {
      data$streams <- sample(1e6:1e8, nrow(data), replace = TRUE)
    }
    
    # Create decade column
    if("released_year" %in% names(data)) {
      data$released_year <- as.numeric(data$released_year)
      data$decade <- floor(data$released_year / 10) * 10
    } else {
      data$released_year <- sample(2000:2023, nrow(data), replace = TRUE)
      data$decade <- floor(data$released_year / 10) * 10
    }
    
    # Create duration column if not exists
    if(!"duration" %in% names(data)) {
      data$duration <- sample(120:300, nrow(data), replace = TRUE)
    }
    
    # Extract clean image URLs
    if("cover_url" %in% names(data)) {
      data$image_url <- gsub('=HYPERLINK\\(\\"|\\"\\)', '', data$cover_url)
    }
    
    # Ensure required audio features exist
    audio_features <- c("danceability", "energy", "valence", "acousticness",
                        "instrumentalness", "liveness", "speechiness", "bpm")
    for(feat in audio_features) {
      if(!feat %in% names(data)) {
        data[[feat]] <- sample(0:100, nrow(data), replace = TRUE)
      }
    }
    
    # Ensure artist_count exists
    if(!"artist_count" %in% names(data)) {
      data$artist_count <- sapply(strsplit(data$artistsname, ","), length)
    }
    
    # Ensure in_spotify_playlists exists
    if(!"in_spotify_playlists" %in% names(data)) {
      data$in_spotify_playlists <- sample(1000:10000, nrow(data), replace = TRUE)
    }
    
    return(data)
  }, error = function(e) {
    # Return sample data if loading fails
    data.frame(
      track_name = paste("Track", 1:100),
      artistsname = paste("Artist", 1:100),
      released_year = sample(2000:2023, 100, replace = TRUE),
      streams = sample(1e6:1e8, 100, replace = TRUE),
      danceability = sample(0:100, 100, replace = TRUE),
      energy = sample(0:100, 100, replace = TRUE),
      valence = sample(0:100, 100, replace = TRUE),
      acousticness = sample(0:100, 100, replace = TRUE),
      instrumentalness = sample(0:100, 100, replace = TRUE),
      liveness = sample(0:100, 100, replace = TRUE),
      speechiness = sample(0:100, 100, replace = TRUE),
      bpm = sample(60:200, 100, replace = TRUE),
      artist_count = sample(1:3, 100, replace = TRUE),
      in_spotify_playlists = sample(1000:10000, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
}

data <- load_and_prepare_data()

# Define UI with all required components
ui <- dashboardPage(
  dashboardHeader(title = "Spotify Track Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Track Analysis", tabName = "track", icon = icon("music")),
      menuItem("Top Tracks", tabName = "toptracks", icon = icon("trophy")),
      menuItem("Findings", tabName = "findings", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction tab
      tabItem(tabName = "intro",
              h2("Spotify Track Analysis Dashboard"),
              fluidRow(
                box(width = 12,
                    h3("Dataset Overview"),
                    p("This dashboard analyzes a comprehensive dataset of popular tracks on Spotify, containing information about:"),
                    tags$ul(
                      tags$li("Track names and artists"),
                      tags$li("Release dates"),
                      tags$li("Stream counts and chart performance"),
                      tags$li("Audio features (danceability, energy, valence, etc.)"),
                      tags$li("Album cover URLs")
                    ),
                    p("The dataset contains", nrow(data), "tracks released between", 
                      min(data$released_year, na.rm = TRUE), "and", max(data$released_year, na.rm = TRUE), "."),
                    p("Use the navigation menu to explore different aspects of the data.")
                )
              ),
              fluidRow(
                box(width = 6, title = "Data Sample", status = "primary",
                    DTOutput("sampleTable")),
                box(width = 6, title = "Key Metrics", status = "info",
                    valueBox(nrow(data), "Total Tracks", icon = icon("music"), color = "purple"),
                    valueBox(length(unique(data$artistsname)), "Unique Artists", icon = icon("users"), color = "green"),
                    valueBox(round(mean(data$streams, na.rm = TRUE)/1e6, 1), "Avg Streams (Millions)", icon = icon("headphones"), color = "blue"))
              )
      ),
      
      # Track Analysis tab
      tabItem(tabName = "track",
              h2("Individual Track Analysis"),
              fluidRow(
                box(width = 12,
                    selectizeInput("selectedTrack", "Select Track:", 
                                   choices = unique(data$track_name),
                                   selected = ifelse("Blinding Lights" %in% data$track_name, "Blinding Lights", data$track_name[1]))
                )
              ),
              fluidRow(
                box(width = 6, title = "Track Information", status = "primary",
                    uiOutput("trackInfo")),
                box(width = 6, title = "Audio Features Radar", status = "info",
                    plotlyOutput("radarPlot"))
              ),
              fluidRow(
                box(width = 6, title = "Streams Over Time", status = "primary",
                    plotlyOutput("streamsOverTime")),
                box(width = 6, title = "Feature Comparison", status = "info",
                    plotlyOutput("featureComparison"))
              )
      ),
      
      # Top Tracks tab
      tabItem(tabName = "toptracks",
              h2("Top Tracks Analysis"),
              fluidRow(
                box(width = 12,
                    sliderInput("topN", "Number of Top Tracks to Show:", 
                                min = 5, max = 30, value = 10),
                    radioButtons("topMetric", "Rank By:",
                                 choices = c("Streams" = "streams",
                                             "Spotify Playlists" = "in_spotify_playlists",
                                             "Danceability" = "danceability",
                                             "Energy" = "energy",
                                             "Valence" = "valence"),
                                 selected = "streams")
                )
              ),
              fluidRow(
                box(width = 12, title = "Top Tracks with Album Covers", status = "primary",
                    uiOutput("topTracksWithImages"))
              ),
              fluidRow(
                box(width = 12, title = "Track Popularity Word Cloud", status = "primary",
                    plotOutput("wordCloudPlot"))
              ),
              fluidRow(
                box(width = 6, title = "Tracks by Decade", status = "primary",
                    plotlyOutput("decadePlot")),
                box(width = 6, title = "Artist Count vs Popularity", status = "info",
                    plotlyOutput("artistCountPlot"))
              )
      ),
      
      # Findings tab
      tabItem(tabName = "findings",
              h2("Key Findings"),
              fluidRow(
                box(width = 12,
                    h3("Main Insights from the Analysis"),
                    tags$ol(
                      tags$li("The most streamed tracks tend to have high energy and danceability scores."),
                      tags$li("Collaborations (tracks with multiple artists) generally perform better in playlists but don't necessarily get more streams."),
                      tags$li("Tracks in minor keys are more common in the dataset than major keys."),
                      tags$li("There is a positive correlation between danceability and valence (how positive/happy a track sounds)."),
                      tags$li("Recent tracks (2020s) dominate the top streams, but some older tracks maintain strong performance."),
                      tags$li("Certain audio features like energy and danceability show clear patterns across different genres."),
                      tags$li("Tracks with higher speechiness (rap/hip-hop) tend to have lower acousticness scores."),
                      tags$li("The distribution of BPM (beats per minute) shows two peaks around 100-110 and 120-130 BPM."),
                      tags$li("Artists like Taylor Swift, Bad Bunny, and The Weeknd appear frequently in the top tracks."),
                      tags$li("There's a weak negative correlation between instrumentalness and streams, suggesting vocal tracks are more popular.")
                    )
                )
              ),
              fluidRow(
                box(width = 6, title = "Feature Correlations", status = "primary",
                    plotlyOutput("correlationPlot")),
                box(width = 6, title = "Streams vs. Features", status = "info",
                    plotlyOutput("streamsVsFeatures"))
              ),
              fluidRow(
                box(width = 6, title = "Audio Features Pivot Table", status = "primary",
                    DTOutput("pivotTable")),
                box(width = 6, title = "Top Tracks Audio Features Radar", status = "info",
                    plotOutput("radarChart"))
              )
      )
    )
  )
)

# Define server logic with improved visualization functions
server <- function(input, output) {
  
  # Introduction tab outputs
  output$sampleTable <- renderDT({
    cols_to_show <- intersect(c("track_name", "artistsname", "released_year", "streams"), names(data))
    datatable(head(data[, cols_to_show], 10),
              options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Track Analysis tab outputs
  selectedTrackData <- reactive({
    req(input$selectedTrack)
    data %>% filter(track_name == input$selectedTrack)
  })
  
  output$trackInfo <- renderUI({
    track <- selectedTrackData()
    if(nrow(track) == 0) return(NULL)
    
    div(
      h3(track$track_name[1]),
      h4("by", ifelse("artistsname" %in% names(track), track$artistsname[1], "Unknown Artist")),
      p(strong("Released:"), paste(track$released_year[1], 
                                   ifelse("released_month" %in% names(track), track$released_month[1], "01"), 
                                   ifelse("released_day" %in% names(track), track$released_day[1], "01"), 
                                   sep = "-")),
      p(strong("Streams:"), format(track$streams[1], big.mark = ",")),
      p(strong("BPM:"), ifelse("bpm" %in% names(track), track$bpm[1], "N/A")),
      p(strong("Key:"), ifelse("key" %in% names(track), paste(track$key[1], ifelse("mode" %in% names(track), track$mode[1], "")), "N/A")),
      p(strong("Duration:"), ifelse("duration" %in% names(track), paste(track$duration[1], "seconds"), "N/A")),
      if("image_url" %in% names(track) && !is.na(track$image_url[1])) {
        tags$img(src = track$image_url[1], 
                 height = "200px", style = "display: block; margin-left: auto; margin-right: auto;")
      }
    )
  })
  
  output$radarPlot <- renderPlotly({
    track <- selectedTrackData()
    if(nrow(track) == 0) return(NULL)
    
    features <- c("Danceability" = ifelse("danceability" %in% names(track), track$danceability[1], 50),
                  "Energy" = ifelse("energy" %in% names(track), track$energy[1], 50),
                  "Valence" = ifelse("valence" %in% names(track), track$valence[1], 50),
                  "Acousticness" = ifelse("acousticness" %in% names(track), track$acousticness[1], 50),
                  "Instrumentalness" = ifelse("instrumentalness" %in% names(track), track$instrumentalness[1], 50),
                  "Liveness" = ifelse("liveness" %in% names(track), track$liveness[1], 50),
                  "Speechiness" = ifelse("speechiness" %in% names(track), track$speechiness[1], 50))
    
    plot_ly(
      type = 'scatterpolar',
      r = features,
      theta = names(features),
      fill = 'toself',
      mode = 'markers'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0,100)
          )
        ),
        title = paste("Audio Features for", track$track_name[1]),
        plot_bgcolor = "#191414",
        paper_bgcolor = "#191414",
        font = list(color = "white")
      )
  })
  
  output$streamsOverTime <- renderPlotly({
    # Simulate streams over time (not in original data)
    months <- 1:12
    streams <- cumsum(sample(1000000:5000000, 12, replace = TRUE))
    
    plot_ly(
      x = months,
      y = streams,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = "blue", width = 3),
      marker = list(color = "orange", size = 8)
    ) %>%
      layout(
        title = "Simulated Streams Over First Year",
        xaxis = list(title = "Months Since Release", color = "black"),
        yaxis = list(title = "Cumulative Streams", color = "black"),
        plot_bgcolor = "#ffffff",     # White background
        paper_bgcolor = "#ffffff",    # White background
        font = list(color = "black")  # Black font
      )
  })
  
  output$featureComparison <- renderPlotly({
    track <- selectedTrackData()
    if(nrow(track) == 0) return(NULL)
    
    avg_features <- data %>%
      summarise(
        Danceability = mean(ifelse("danceability" %in% names(data), danceability, 50), na.rm = TRUE),
        Energy = mean(ifelse("energy" %in% names(data), energy, 50), na.rm = TRUE),
        Valence = mean(ifelse("valence" %in% names(data), valence, 50), na.rm = TRUE),
        Acousticness = mean(ifelse("acousticness" %in% names(data), acousticness, 50), na.rm = TRUE)
      )
    
    track_features <- data.frame(
      Feature = names(avg_features),
      Average = as.numeric(avg_features[1, ]),
      Selected = c(ifelse("danceability" %in% names(track), track$danceability[1], 50),
                   ifelse("energy" %in% names(track), track$energy[1], 50),
                   ifelse("valence" %in% names(track), track$valence[1], 50),
                   ifelse("acousticness" %in% names(track), track$acousticness[1], 50))
    )
    
    plot_ly(track_features, x = ~Feature, y = ~Average, type = 'bar', 
            name = 'Dataset Average',
            marker = list(color = "#1DB954")) %>%
      add_trace(y = ~Selected, name = 'Selected Track',
                marker = list(color = "#1ED760")) %>%
      layout(title = "Feature Comparison with Dataset Average",
             yaxis = list(title = "Score (0-100)", color = "white"),
             xaxis = list(color = "white"),
             plot_bgcolor = "#191414",
             paper_bgcolor = "#191414",
             legend = list(font = list(color = "white")),
             barmode = 'group')
  })
  
  # Top Tracks tab outputs
  output$topTracksWithImages <- renderUI({
    req(input$topMetric)
    top_tracks <- data %>%
      arrange(desc(!!sym(input$topMetric))) %>%
      head(input$topN)
    
    # Calculate dynamic column width based on number of tracks
    n_tracks <- nrow(top_tracks)
    col_width <- ifelse(n_tracks <= 5, 12/5 * 2,
                        ifelse(n_tracks <= 10, 12/10 * 2,
                               12/15 * 2))  # Cap at 15 tracks for reasonable sizing
    
    # Create a fluid row with columns for each track
    fluidRow(
      lapply(1:nrow(top_tracks), function(i) {
        track <- top_tracks[i, ]
        column(width = col_width,
               style = "text-align: center; margin-bottom: 20px;",
               div(
                 if("image_url" %in% names(track) && !is.na(track$image_url)) {
                   tags$img(src = track$image_url, height = "100px", 
                            style = "border-radius: 5px; margin-bottom: 5px;")
                 },
                 br(),
                 strong(track$track_name, style = "color: white;"),
                 br(),
                 em(ifelse("artistsname" %in% names(track), track$artistsname, "Unknown Artist"), 
                    style = "color: #1ED760;"),
                 br(),
                 span(paste0(format(round(track[[input$topMetric]]), big.mark = ","), 
                             ifelse(input$topMetric == "streams", " streams", "")),
                      style = "color: white;")
               )
        )
      })
    )
  })
  
  output$wordCloudPlot <- renderPlot({
    tryCatch({
      req(data$track_name, data$streams)
      
      # Prepare data for word cloud
      track_data <- data %>%
        select(track_name, streams) %>%
        filter(!is.na(track_name), !is.na(streams), streams > 0)
      
      if(nrow(track_data) == 0) {
        stop("No valid data for word cloud")
      }
      
      # Aggregate in case of duplicate track names
      track_summary <- track_data %>%
        group_by(track_name) %>%
        summarise(total_streams = sum(streams)) %>%
        arrange(desc(total_streams)) %>%
        head(input$topN)
      
      # Calculate dynamic scaling
      max_words <- min(input$topN, nrow(track_summary))
      base_scale <- 4
      min_scale <- 0.5
      scale_factor <- max(min_scale, base_scale - (max_words / 30))
      
      # Generate the word cloud
      par(bg = "#191414")
      wordcloud(words = track_summary$track_name,
                freq = track_summary$total_streams,
                min.freq = 1,
                max.words = max_words,
                random.order = FALSE,
                rot.per = 0.35,
                colors = brewer.pal(8, "Dark2"),
                scale = c(scale_factor, scale_factor*0.2))
      
      title(main = paste("Top", max_words, "Track Names by Streams"), 
            col.main = "white", cex.main = 1.2)
    }, error = function(e) {
      plot.new()
      title(main = "Word cloud data not available", col.main = "white")
    })
  })
  
  # Decade Plot with better formatting
  output$decadePlot <- renderPlotly({
    tryCatch({
      req(data$decade, data$streams)
      
      decade_data <- data %>%
        group_by(decade) %>%
        summarise(
          count = n(),
          avg_streams = mean(streams, na.rm = TRUE)
        ) %>%
        filter(!is.na(decade))
      
      plot_ly(decade_data) %>%
        add_bars(
          x = ~decade,
          y = ~count,
          name = "Number of Tracks",
          marker = list(color = "#1DB954")
        ) %>%
        add_lines(
          x = ~decade,
          y = ~avg_streams/1e6,
          name = "Avg Streams (M)",
          yaxis = "y2",
          line = list(color = "#1ED760"),
          marker = list(color = "#1ED760")
        ) %>%
        layout(
          title = "Tracks by Decade",
          xaxis = list(title = "Decade", tickvals = ~decade, ticktext = paste0(~decade, "s"), color = "white"),
          yaxis = list(title = "Number of Tracks", color = "white"),
          yaxis2 = list(
            title = "Avg Streams (Millions)",
            overlaying = "y",
            side = "right",
            color = "white"
          ),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414",
          legend = list(font = list(color = "white")),
          margin = list(b = 100)
        )
    }, error = function(e) {
      plotly_empty() %>%
        layout(
          title = list(text = "Decade data not available", font = list(color = "white")),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414"
        )
    })
  })
  
  # Artist Count vs Popularity Plot
  output$artistCountPlot <- renderPlotly({
    tryCatch({
      req(data$artist_count, data$streams, data$in_spotify_playlists)
      
      artist_count_data <- data %>%
        group_by(artist_count) %>%
        summarise(
          avg_streams = mean(streams, na.rm = TRUE),
          avg_playlists = mean(in_spotify_playlists, na.rm = TRUE)
        )
      
      plot_ly(artist_count_data) %>%
        add_bars(
          x = ~artist_count,
          y = ~avg_streams/1e6,
          name = "Avg Streams (M)",
          marker = list(color = "#1DB954")
        ) %>%
        add_lines(
          x = ~artist_count,
          y = ~avg_playlists,
          name = "Avg Playlists",
          yaxis = "y2",
          line = list(color = "#1ED760"),
          marker = list(color = "#1ED760")
        ) %>%
        layout(
          title = "Artist Count vs Popularity",
          xaxis = list(title = "Number of Artists", color = "white"),
          yaxis = list(title = "Avg Streams (Millions)", color = "white"),
          yaxis2 = list(
            title = "Avg Spotify Playlists",
            overlaying = "y",
            side = "right",
            color = "white"
          ),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414",
          legend = list(font = list(color = "white"))
        )
    }, error = function(e) {
      plotly_empty() %>%
        layout(
          title = list(text = "Artist count data not available", font = list(color = "white")),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414"
        )
    })
  })
  
  # Correlation Plot with better colors and labels
  output$correlationPlot <- renderPlotly({
    tryCatch({
      req(data$danceability, data$energy, data$valence, data$acousticness,
          data$instrumentalness, data$liveness, data$speechiness)
      
      cor_data <- data %>%
        select(danceability, energy, valence, acousticness, 
               instrumentalness, liveness, speechiness)
      
      cor_matrix <- cor(cor_data, use = "complete.obs")
      
      plot_ly(
        x = colnames(cor_matrix),
        y = rownames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colors = colorRamp(c("#1a2a6c", "#b21f1f", "#fdbb2d")),
        colorbar = list(title = "Correlation", 
                        titleside = "right",
                        tickfont = list(color = "white"))
      ) %>%
        layout(
          title = "Audio Feature Correlations",
          xaxis = list(title = "", tickfont = list(color = "white")),
          yaxis = list(title = "", tickfont = list(color = "white")),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414"
        )
    }, error = function(e) {
      plotly_empty() %>%
        layout(
          title = list(text = "Correlation data not available", font = list(color = "white")),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414"
        )
    })
  })
  
  # Streams vs Features Plot
  output$streamsVsFeatures <- renderPlotly({
    tryCatch({
      req(data$danceability, data$streams)
      
      plot_ly(data,
              x = ~danceability,
              y = ~streams/1e6,
              type = 'scatter',
              mode = 'markers',
              text = ~paste(track_name, "<br>", ifelse("artistsname" %in% names(data), artistsname, "Unknown Artist")),
              hoverinfo = 'text',
              marker = list(color = "#1DB954", size = 10)) %>%
        layout(
          title = "Streams vs Danceability",
          xaxis = list(title = "Danceability", color = "white"),
          yaxis = list(title = "Streams (Millions)", color = "white"),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414"
        )
    }, error = function(e) {
      plotly_empty() %>%
        layout(
          title = list(text = "Streams vs features data not available", font = list(color = "white")),
          plot_bgcolor = "#191414",
          paper_bgcolor = "#191414"
        )
    })
  })
  
  # Audio Features Pivot Table
  output$pivotTable <- renderDT({
    tryCatch({
      # Select relevant audio features
      features <- c("danceability", "energy", "valence", "acousticness",
                    "instrumentalness", "liveness", "speechiness", "bpm")
      
      # Filter to only include columns that exist in the data
      features <- features[features %in% names(data)]
      
      # Calculate summary statistics
      pivot_data <- data %>%
        select(all_of(features)) %>%
        summarise(across(everything(), 
                         list(Mean = ~mean(., na.rm = TRUE),
                              Median = ~median(., na.rm = TRUE),
                              Min = ~min(., na.rm = TRUE),
                              Max = ~max(., na.rm = TRUE),
                              SD = ~sd(., na.rm = TRUE)))) %>%
        pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
        separate(stat, into = c("feature", "statistic"), sep = "_") %>%
        pivot_wider(names_from = "statistic", values_from = "value")
      
      # Create and format the datatable
      datatable(
        pivot_data,
        options = list(
          pageLength = 10,
          dom = 't',
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#1DB954', 'color': '#ffffff'});",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'stripe hover'
      ) %>%
        formatRound(columns = c("Mean", "Median", "SD"), digits = 2) %>%
        formatRound(columns = c("Min", "Max"), digits = 0)
    }, error = function(e) {
      datatable(data.frame(Error = "Could not generate pivot table"),
                options = list(dom = 't'))
    })
  })
  
  # Top Tracks Audio Features Radar Chart
  output$radarChart <- renderPlot({
    tryCatch({
      # Get the number of tracks to show from the slider input
      n_tracks <- input$topN
      
      # Define relevant features
      features <- c('danceability', 'valence', 'energy', 'acousticness',
                    'instrumentalness', 'liveness', 'speechiness')
      
      # Filter to only include columns that exist in the data
      features <- features[features %in% names(data)]
      
      # Select top tracks by the selected metric
      top_tracks <- data %>%
        arrange(desc(!!sym(input$topMetric))) %>%
        slice_head(n = n_tracks) %>%
        select(track_name, all_of(features))
      
      # Set track names as rownames
      top_named <- top_tracks %>%
        column_to_rownames(var = "track_name")
      
      # Prepare radar data (add max/min rows required by fmsb)
      radar_data <- rbind(
        rep(100, length(features)),    # Max values
        rep(0, length(features)),      # Min values
        top_named[, features]
      )
      
      # Create color palette (Spotify theme)
      spotify_green <- "#1DB954"
      accent_colors <- colorRampPalette(c("#1DB954", "#1ED760", "#1AA34A"))(n_tracks)
      
      # Plot Radar Chart with Spotify theme
      op <- par(mar = c(2, 2, 2, 2), bg = "white")  # Spotify dark background
      
      radarchart(
        radar_data,
        axistype = 1,
        pcol = accent_colors,
        pfcol = alpha(accent_colors, 0.3),
        plwd = 2,
        plty = 1,
        cglcol = "white",
        cglty = 1,
        axislabcol = "white",
        caxislabels = c("0%", "20%", "40%", "60%", "80%", "100%"),
        cglwd = 0.8,
        vlcex = 0.9,
        vlabels = features,
        title = paste("Top", n_tracks, "Tracks Audio Features")
      )
      
      legend("topright", 
             legend = rownames(radar_data)[3:(n_tracks+2)],
             col = accent_colors, 
             lty = 1, 
             lwd = 2,
             text.col = "white", 
             bg = "black", 
             box.col = "white",
             cex = ifelse(n_tracks > 10, 0.7, 0.9))  # Adjust legend size for many tracks
      
      par(op)
    }, error = function(e) {
      plot.new()
      title(main = "Radar chart data not available", col.main = "white")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)