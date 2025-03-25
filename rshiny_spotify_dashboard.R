#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
#library(Rspotify)
library(spotifyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinythemes)
library(bslib)
library(plotly)
library(leaflet)
library(readr)
library(dplyr)
library(reactable)
library(plotly)


Sys.setenv(SPOTIFY_CLIENT_ID = '4cc947da074c483a83711e8d9ecb2db5')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '62146858c5824b79b93ab09b39717bb2')
Sys.setenv(SPOTIFY_REDIRECT_URI = "http://localhost:1410/") 
access_token <- get_spotify_access_token()

# Load the reference data set and preprocess it
data_url = "https://github.com/TheGooseGuy/RShiny-Spotify-Dashboard/raw/main/spotify-2023.csv"
data_reference = read_csv(data_url)
data_reference$streams = as.numeric(data_reference$streams)
data_reference = data_reference %>% rename(artist_name = 'artist(s)_name')

# Get the top 10 streams
top10_streams_list = data_reference %>% arrange(desc(streams)) %>% 
  slice_head(n = 10) %>% select(track_name,artist_name,streams)


# Get the top 10 artists by total streams
top10_total_artists_stream = data_reference %>% group_by(artist_name) %>% 
  summarise(total_streams = sum(streams,na.rm = TRUE),track_num = n()) %>% 
  arrange(desc(total_streams)) %>% slice_head(n = 10)

# Get the top 10 Spotify playlists
top10_spotify_playlists = data_reference %>% arrange(desc(in_spotify_playlists)) %>% 
  slice_head(n = 10) %>% select(track_name,artist_name,in_spotify_playlists)

# Gee the top 10 artists by total palylists
top10_total_artists_playlists = data_reference %>% group_by(artist_name) %>% 
  summarise(total_playlists = sum(in_spotify_playlists,na.rm = TRUE),track_num = n()) %>% 
  arrange(desc(total_playlists)) %>% slice_head(n = 10)

# Check for valid values for min and max
playlist_min = ifelse(is.na(min(data_reference$in_spotify_playlists, na.rm = TRUE)),
                      0, min(data_reference$in_spotify_playlists, na.rm = TRUE))
playlist_max = ifelse(is.na(max(data_reference$in_spotify_playlists, na.rm = TRUE)),
                      1, max(data_reference$in_spotify_playlists, na.rm = TRUE))

stream_min = ifelse(is.na(min(data_reference$streams, na.rm = TRUE)),
                    0, min(data_reference$streams, na.rm = TRUE))
stream_max = ifelse(is.na(max(data_reference$streams, na.rm = TRUE)),
                    1, max(data_reference$streams, na.rm = TRUE))

format_artists <- function(artists) {
  # If artists is a data frame or list, extract names
  if (is.data.frame(artists) || is.list(artists)) {
    artist_names <- sapply(artists, function(artist) {
      if (is.list(artist)) artist$name else as.character(artist)
    })
  } else {
    # If it's already a character vector
    artist_names <- as.character(artists)
  }
  
  # Combine unique artists
  unique_artists <- unique(artist_names)
  
  # Join artists
  paste(unique_artists, collapse = ", ")
}


#Listening Trends

process_spotify_listening_trend <- function(limit = 50) {
  # Validate input
  if (limit < 20 || limit > 50) {
    stop("Limit must be between 20 and 50")
  }
  
  # Fetch recently played tracks
  recent_tracks <- get_my_recently_played(limit = limit)
  
  # Extract relevant information: played time and duration
  listening_data <- recent_tracks %>%
    transmute(
      played_at = ymd_hms(played_at),
      hour = hour(played_at),
      minute = minute(played_at),
      duration_minutes = track.duration_ms / 60000  # Convert ms to minutes
    )
  
  # Aggregate the total lengths of songs per hour
  hourly_summary <- listening_data %>%
    group_by(hour) %>%
    summarise(
      total_minutes = sum(duration_minutes),
      song_count = n()
    ) %>%
    ungroup()
  
  # Create the visualization
  trend_plot <- ggplot(hourly_summary, aes(x = hour, y = total_minutes)) +
    geom_bar(
      stat = "identity", 
      fill = "#1DB954",  # Spotify green
      color = "white",   # White border
      width = 0.7        # Slightly narrower bars
    ) +
    scale_x_continuous(
      breaks = seq(min(hourly_summary$hour), max(hourly_summary$hour), 1),
      labels = function(x) paste0(x, ":00")
    ) +
    scale_y_continuous(
      limits = c(0, max(hourly_summary$total_minutes) * 1.1),  # Dynamic y-axis limit
      breaks = seq(0, max(hourly_summary$total_minutes), 10)
    ) +
    labs(
      title = paste("Listening Trend of Last", limit, "Songs"),
      x = "Hour of the Day",
      y = "Total Listening Time (Minutes)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "#121212", color = NA),
      plot.background = element_rect(fill = "#121212", color = NA),
      panel.grid.major = element_line(color = "white", size = 0.05),
      panel.grid.minor = element_line(color = "white", size = 0.05),
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  
  # Return a list with the plot and summary data
  return(list(
    plot = trend_plot,
    hourly_summary = hourly_summary,
    total_listening_time = sum(hourly_summary$total_minutes)
  ))
}


####

create_interactive_listening_plot <- function(limit = 50) {
  # Fetch recently played tracks
  recent_tracks <- get_my_recently_played(limit = limit)
  
  # Extract relevant information: played time and duration
  listening_data <- recent_tracks %>%
    mutate(
      played_at = ymd_hms(played_at),
      hour = hour(played_at)
    ) %>%
    transmute(
      hour = hour,
      duration_minutes = track.duration_ms / 60000,  # Convert ms to minutes
      track_name = track.name,
    )
  
  # Aggregate the total lengths of songs per hour
  hourly_summary <- listening_data %>%
    group_by(hour) %>%
    summarise(
      total_minutes = sum(duration_minutes),
      song_count = n(),
      song_details = paste(paste(track_name), collapse = "<br>")
    ) %>%
    ungroup()
  
  # Create base ggplot
  trend_plot <- ggplot(hourly_summary, aes(
    x = hour, 
    y = total_minutes, 
    text = paste(
      "Hour:", hour, ":00<br>",
      "Total Listening Time:", round(total_minutes, 2), "minutes<br>",
      "Songs Played:", song_count, "<br>",
      "Songs:<br>", song_details
    )
  )) +
    geom_bar(
      stat = "identity", 
      fill = "#1DB954",  # S  potify green
      color = "white",   # White border
      width = 0.7        # Slightly narrower bars
    ) +
    scale_x_continuous(
      breaks = seq(min(hourly_summary$hour), max(hourly_summary$hour), 1),
      labels = function(x) paste0(x, ":00")
    ) +
    scale_y_continuous(
      limits = c(0, max(hourly_summary$total_minutes) * 1.1),  # Dynamic y-axis limit
      breaks = seq(0, max(hourly_summary$total_minutes), 10)
    ) +
    labs(
      title = paste("Listening Trend of Last", limit, "Songs"),
      x = "Hour of the Day",
      y = "Total Listening Time (Minutes)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "#121212", color = NA),
      plot.background = element_rect(fill = "#121212", color = NA),
      panel.grid.major = element_line(color = "white", size = 0.05),
      panel.grid.minor = element_line(color = "white", size = 0.05),
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  
  # Convert to interactive plotly
  interactive_plot <- ggplotly(trend_plot, tooltip = "text") %>%
    layout(
      hoverlabel = list(
        bgcolor = "black",  # Black background
        font = list(color = "white")  # White text
      ),
      title = list(font = list(color = "white"))
    )
  
  return(list(
    plot = interactive_plot,
    hourly_summary = hourly_summary,
    total_listening_time = sum(hourly_summary$total_minutes)
  ))
}

####

#map
plot_top_artists_map <- function(limit = 10) {
  # Fetch top artists from Spotify
  top_artists <- get_my_top_artists_or_tracks(
    type="artists",
    limit = limit) %>%
    transmute(Artist = name) # Extract artist names
  
  # Load your CSV with geo data
  geo_data <- read_csv("artist-countries-geo.csv")
  
  # Merge Spotify artist data with geo data
  artist_geo_data <- geo_data %>%
    inner_join(top_artists, by = "Artist") # Match artists from Spotify with geo data
  
  # Group by country and calculate number of artists per country
  country_artist_data <- artist_geo_data %>%
    group_by(Country, Latitude, Longitude) %>%
    summarise(
      Artists = paste(Artist, collapse = ", "),
      ArtistCount = n(), # Count the number of artists per country
      .groups = "drop"
    )
  
  # Create a leaflet map with artist locations
  map <- leaflet(country_artist_data) %>%
    addProviderTiles("CartoDB.DarkMatter") %>%  # Changed to dark theme
    addCircleMarkers(
      lng = ~Longitude, lat = ~Latitude,
      popup = ~paste("Country:", Country, "<br>Artists:", Artists, "<br>Number of Artists:", ArtistCount),
      color = "#1DB954",  # Spotify green for visibility
      fillColor = "#1DB954",  # Same color for fill
      fill = TRUE, 
      fillOpacity = 0.7,
      radius = ~ArtistCount * 3 # Scale the circle size by the number of artists
    )
  
  # Return the map
  return(map)
}

#Function for top artist details
get_top_artists_details <- function(limit = 50) {
  # Fetch top artists
  top_artists <- get_my_top_artists_or_tracks(type = "artists", limit = limit)
  
  # Safely extract the first (largest) image URL from the nested column
  extract_image_url <- function(images) {
    # If images is NULL or empty, return NA
    if (is.null(images) || length(images) == 0) {
      return(NA_character_)
    }
    
    # The images are a data frame, so we can directly select the 640px URL
    largest_image <- images[images$height == 640, ]
    
    # Return the URL if found, otherwise NA
    if (nrow(largest_image) > 0) {
      return(largest_image$url)
    }
    
    return(NA_character_)
  }
  
  # Safely extract all relevant details into a consistent data frame
  artist_details <- tibble(
    image_url = sapply(top_artists$images, extract_image_url),
    name = top_artists$name,
    popularity = top_artists$popularity,
    followers = top_artists$followers.total
    
  ) %>%
    arrange(desc(popularity))  # Sort by popularity
  
  return(artist_details)
}


#Top tracks
get_top_tracks_details <- function(limit = 50) {
  # Fetch top tracks 
  top_tracks <- get_my_top_artists_or_tracks(type = "tracks", limit = limit)
  artist_names <- lapply(top_tracks$artists, function(x) x$name)
  #formatted_artist_names <- format_artists(artist_names)
  #print(artist_names)
  
  
  # Safely extract the album image URL
  extract_image_url <- function(images) {
    if (is.null(images) || length(images) == 0) {
      return(NA_character_)
    }
    largest_image <- images[images$height == 640, ]
    if (nrow(largest_image) > 0) {
      return(largest_image$url)
    }
    return(NA_character_)
  }
  
  
  # Create a tibble with relevant track details
  artist_names <- sapply(top_tracks$artists, function(x) paste(x$name, collapse = ", "))
  
  # Create the track_details tibble with the formatted artist names
  track_details <- tibble(
    track_name = top_tracks$name,
    artists = artist_names,  # Using the formatted artist names
    album_name = top_tracks$album.name,
    popularity = top_tracks$popularity,
    release_date = top_tracks$album.release_date,
    album_image_url = sapply(top_tracks$album.images, extract_image_url)
  ) %>% 
    arrange(desc(popularity))
  return(track_details)
}




# Function to get track features based on user preferences
get_audio_features <- function(tracks) {
  audio_features <- spotifyr::get_audio_features(tracks, access_token = access_token)
  return(audio_features)
}


dashboard_ui <- tabsetPanel(
  tabPanel("Profile",
           icon = icon("fa-regular fa-file"),
           textOutput("summary"),
           fluidRow(
             column(6, 
                    h3("User Profile"),
                    icon("user"), # Icon representation of the user
                    textOutput("user_name"),
                    textOutput("user_followers"),
                    fluidRow(actionButton("refresh", "Refresh Info", style="margin-left: 5px"),
                             actionButton("pause", "Pause Playback"))
             ),
             column(6,
                    h3("Currently Playing"),
                    uiOutput("current_track_album"),
                    uiOutput("current_track_info")
                    
             )
           ),
           br(),
           
           br(),
           fluidRow(
             
             
             
           ),
           
           class="inner-tab"),
  
  tabPanel("Top Tracks",
           icon = icon("music"),
           fluidPage(
             br(),
             fluidRow(
               column(3,sliderInput("top_track_limit", 
                                    "Number of Top Tracks", 
                                    min = 10, 
                                    max = 50, 
                                    value = 20, 
                                    step = 1)),
               column(9,actionButton("update_top_tracks_table","Generate Plot", style="margin-top: 3%"))
             ),
             br(),
             reactableOutput("top_tracks_table"),
             p("This shows your top tracks along with their album cover, artists, album name, and popularity."),
             class = "inner-tab"
           )
  ),
  tabPanel("Top Artists",
           icon = icon("microphone"),
           fluidRow(
             column(3,sliderInput("top_artist_limit", 
                                  "Number of Top Artists", 
                                  min = 10, 
                                  max = 20, 
                                  value = 10, 
                                  step = 1)),
             column(9, actionButton("update_top_artists_table","Generate Plot", style="margin-top: 3%"))
           ),
           # Use fluidRow to create a horizontal layout
           fluidRow(  
             # Use column to define how much space each output takes up
             column(6, reactableOutput("top_artist_table")),  # 50% width for the table
             column(6, leafletOutput("top_artists_map"))     # 50% width for the map
           ),
           p("This shows your top artists."), class="inner-tab")
  
  ,
  tabPanel("Genres", 
           icon = icon("bars-staggered"),
           fluidRow(
             column(12, 
                    plotOutput("genrePlot", height = "800px", width = "100%"))
           )),
  
  tabPanel("Listening Trends",
           icon = icon("arrow-trend-up"),
           fluidRow(
             column(3,sliderInput("spotify_limit", 
                                  "Number of Recent Tracks", 
                                  min = 20, 
                                  max = 50, 
                                  value = 50, 
                                  step = 10)),
             column(9, actionButton("update_recent_tracks_plot","Generate Plot", style="margin-top: 3%"))
           ),
           plotlyOutput("listeningTrendsPlot"),
           p("This chart shows how your listening habits have changed over time."), class="inner-tab")
)


main_ui <- navbarPage(
  title = tags$img(src = "spotify_logo.png", height = "32px"),  # Add Spotify logo
  theme = shinythemes::shinytheme("cosmo"),  # Apply a dark theme
  tabPanel("Dashboard",
           icon = icon("fa-solid fa-user"),
           fluidRow(
             column(3, class = "sidebar",
                    #style = "border: 2px solid white; padding: 15px; border-radius: 10px;", # White frame style
                    textInput("CID", "Enter your Client ID:", ""),
                    textInput("CST", "Enter your Client Secret:", ""),
                    textInput("RURL", "Enter your Redirect URL:", "http://localhost:1410/"),
                    #textInput("token", "Enter your Spotify Token:", ""),
                    actionButton("analyze", "Lock In")
                    
                    #selectInput("metric", "Choose a Metric:", 
                    #            choices = c("Speechiness", "Danceability", "Energy", "Valence")),
                    #sliderInput("metricRange", "Select Metric Range:", 
                    #            min = 0, max = 1, value = c(0.4, 0.6)),
                    #actionButton("filter", "Filter Songs")
             ),
             column(9, 
                    dashboard_ui),
           )
  ),
  
  tabPanel("Top Pick 2023",
           icon = icon("arrow-down-wide-short"),
           tabsetPanel(
             tabPanel("Introduction",
                      fluidRow(
                        column(width = 8,
                               h3("About Dataset"),
                               uiOutput("description")),
                        column(width = 4,
                               h3("Key features"),
                               uiOutput("features"))
                      )   
                      
             ),
             tabPanel("Top 10 Streams",
                      fluidRow(
                        column(width = 6,
                               h3("2023 Top 10 Tracks by Streams"),
                               uiOutput("topstreamsummary", style = "margin-bottom: 60px;")),
                        column(width = 6,
                               h3("2023 Top 10 Artists by total Streams"),
                               uiOutput("topartiststreamsummary", style = "margin-bottom: 60px;"))),
                      
                      fluidRow(
                        column(width = 6,
                               plotOutput("topstreamplot",height = "400px")),
                        column(width = 6,
                               plotOutput("totalstreamplot",height = "400px")))),
             
             tabPanel("Top 10 playlists",
                      fluidRow(
                        column(width = 6,
                               h3("2023 Top 10 Tracks by Playlist Count"),
                               uiOutput("topplaylistsummary", style = "margin-bottom: 60px;")
                        ),
                        column(width = 6,
                               h3("2023 Top 10 Artists by Total Playlists"),
                               uiOutput("topartistplaylistsummary", style = "margin-bottom: 60px;")
                        )
                      ),
                      fluidRow(
                        column(width = 6, plotOutput("topplaylistplot", height = "400px")),
                        column(width = 6, plotOutput("totalplaylistplot", height = "400px"))
                      )),
           )),
  
  tabPanel("Audio Feature",
           icon = icon('music'),
           sidebarLayout(
             sidebarPanel(textInput("spotify_token", "Enter your Spotify Token:"),
                          selectInput("artist3", "Choose Artist for Tracks:", choices = c("Speechiness", "Danceability", "Energy", "Valence")),
                          actionButton("generate_plot", "Generate Plot")
             ),
             mainPanel(h3("Audio Feature Scatter Plot"),
                       plotlyOutput("scatter_plot"))
           )),
  tabPanel(
    "Playlist Generate",
    icon = icon("list"),
    fluidPage(
      column(4, 
             h3("Customize Your Playlist"),
             selectInput("artist2", "Choose Similar Artist:", 
                         choices = NULL, # Will be populated dynamically
                         multiple = FALSE),
             selectInput("mood", "Select Mood:", 
                         choices = c("Happy", "Sad", "Energetic", "Chill")),
             sliderInput("danceability", "Danceability:", 
                         min = 0, max = 1, value = c(0.4, 0.8)),
             sliderInput("energy", "Energy:", 
                         min = 0, max = 1, value = c(0.4, 0.8)),
             sliderInput("valence", "Valence (Mood):", 
                         min = 0, max = 1, value = c(0.4, 0.8)),
             sliderInput("popularity", "Track Popularity:", 
                         min = 0, max = 100, value = c(50, 100)),
             actionButton("generate_playlist", "Generate Playlist", class = "btn-success")
      ),
      column(8,
             h3("Generated Playlist"),
             tableOutput("playlist_table"), # To display playlist details
             actionButton("save_playlist", "Save Playlist", class = "btn-primary")
      )
    )
  ),
  tabPanel("Share this App",
           icon = icon("arrow-up-right-from-square"),
  ),
)



ui <- fluidPage(
  tags$style(HTML("
 /* Global Styles */
    body {
      background-color: #100000; /* Dark background */
      color: #FFFFFF; /* White text */
      font-family: Arial, sans-serif; /* Consistent font */
    }

    /* Navbar Styles */
    .navbar {
      background-color: #1f1f1f !important; /* Black background */
      border: none; /* Remove border */
    }
    .navbar-nav > li > a {
      color: white !important; /* White text */
      font-weight: bold; /* Bold tabs */
      padding: 15px 30px; /* Spacing for tabs */
      text-align: center; /* Center-align */
    }
    .navbar-nav > li > a:hover {
      background-color: #444 !important; /* Dark gray hover */
    }
    .navbar-nav > li.active > a {
      background-color: #1DB954 !important; /* Spotify green */
      color: white !important; /* White text for active */
    }
    .navbar-header .navbar-brand {
      color: white !important; /* White logo text */
      font-weight: bold;
    }

    /* Tab Styles */
    .nav-tabs > li > a {
      color: #FFFFFF; /* White text */
      background-color: transparent; /* Transparent tabs */
      border: 1px solid #444; /* Subtle border */
      border-radius: 10px 10px 0 0; /* Rounded corners */
      padding: 8px 15px; /* Padding inside tabs */
      margin-right: 5px; /* Space between tabs */
    }
    .nav-tabs > li.active > a {
      background-color: #1DB954 !important; /* Active Spotify green */
      border-color: #1DB954; /* Match border */
    }

    /* Checkbox Styles */
    input[type='checkbox'] {
      accent-color: #1DB954; /* Spotify green checkmark */
      background-color: black; /* Dark box */
      border: 1px solid #444; /* Border */
    }
    input[type='checkbox']:hover {
      background-color: #1DB954; /* Green hover */
    }
    input[type='checkbox']:checked {
      background-color: #1DB954; /* Checked color */
    }

    /* Sidebar Styles */
    .sidebar {
      border-radius: 10px; /* Rounded corners */
      padding: 10px; /* Inner padding */
      background-color: #1f1f1f; /* Dark sidebar background */
      color: #FFFFFF; /* White text */
    }

    /* Slider Styles */
    
    .irs-bar, .irs-bar-edge, .irs-single, .irs-slider {
      background: #1DB954 !important; /* Spotify green */
      border-color: #1DB954 !important;
    }
    .irs-grid-pol {
      background: #FFFFFF !important; /* White grid lines */
    }
    .irs-grid-text {
      color: #FFFFFF !important; /* White grid text */
    } 
                    /* Change the background and text color for active tabs inside a tabPanel */
.nav-tabs > li > a.active,
.nav-tabs > li > a.active:focus,
.nav-tabs > li > a.active:hover {
  background-color: #1DB954 !important; /* Spotify green for active tab */
  color: #000000 !important; /* Black text for active tab */
  border-color: #1DB954; /* Match border with background */
}

/* Optional: Adjust inactive tabs for better contrast */
.nav-tabs > li > a {
  background-color: transparent; /* Transparent background for unselected tabs */
  color: #FFFFFF !important; /* White text for unselected tabs */
  border: 1px solid #444; /* Subtle border */
  border-radius: 10px 10px 0 0; /* Rounded corners */
}

/* Adjust hover effect for inactive tabs */
.nav-tabs > li > a:hover {
  background-color: #444 !important; /* Darker gray for hover */
  color: #FFFFFF !important; /* Keep text white */
}
")
  ),
  main_ui
)


server <- function(input, output, session) {
  # Reactive value to store user data
  user_data <- reactiveVal()
  
  # Fetch and process Spotify data on 'Analyze' button click
  observeEvent(input$analyze, {
    #req(input$token)  # Ensure token is provided
    Sys.setenv(SPOTIFY_CLIENT_ID = input$CID)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = input$CST)
    Sys.setenv(SPOTIFY_REDIRECT_URI = "http://localhost:1410/") 
    access_token <- get_spotify_access_token()
    #auth_header <- paste("Bearer", access_token)
    
    tryCatch({
      # Fetch data from Spotify API
      tracks <- get_my_top_artists_or_tracks(
        type = "tracks",
        time_range = "long_term",
        limit = 50
      )
      
      artists <- get_my_top_artists_or_tracks(
        type = "artists", 
        time_range = "long_term", 
        limit = 50
      )
      
      # Process genres
      # Use sapply to extract genres from each artist
      genres <- unlist(sapply(artists$genres, function(x) if(length(x) > 0) x else NA))
      genre_count <- as.data.frame(table(genres))
      genre_count <- genre_count[order(-genre_count$Freq), ]
      
      # Calculate total listening time
      total_time <- sum(tracks$duration_ms, na.rm = TRUE) / (1000 * 60)  # Convert ms to minutes
      
      # Store results in a reactive value
      user_data(list(
        tracks = tracks,
        artists = artists,
        genres = genre_count,
        total_time = total_time
      ))
      
      # Notify success
      showNotification("Data successfully fetched!", type = "message")
    }, error = function(e) {
      # Handle errors
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Reactive data for trends
  trends_data <- reactive({
    req(user_data())
    
    user_data()$tracks %>%
      mutate(month = floor_date(as.Date(added_at), "month")) %>%  # Adjust for available timestamp field
      group_by(month) %>%
      summarize(
        total_songs = n(),
        total_duration = sum(duration_ms, na.rm = TRUE) / 60000  # Duration in minutes
      )
  })
  
  # EventReactive to process trends only when button is clicked
  trend_results <- eventReactive(input$update_recent_tracks_plot, {
    validate(need(input$spotify_limit, "Please select a limit"))
    
    tryCatch({
      create_interactive_listening_plot(limit = input$spotify_limit)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
  })
  
  # Listening Trends Plot
  output$listeningTrendsPlot <- renderPlotly({
    req(trend_results())  # Ensure trend_results is available
    trend_results()$plot
  })
  
  #Top artist table
  # EventReactive to fetch top artists details only when the button is clicked
  top_artist_data <- eventReactive(input$update_top_artists_table, {
    req(input$top_artist_limit)  # Ensure that limit is selected
    
    # Fetch top artist details based on the limit
    get_top_artists_details(input$top_artist_limit)
  })
  
  # Render the top artist table
  output$top_artist_table <- renderReactable({
    artist_details <- top_artist_data()  # Use the data triggered by the button click
    
    # Render the reactable table
    reactable(
      artist_details, 
      columns = list(
        image_url = colDef(
          name = "Image",
          cell = function(value) {
            if (!is.na(value)) {
              tags$img(src = value, height = "50px", width = "50px")
            } else {
              "No Image"
            }
          }
        ),
        name = colDef(name = "Name", width = 200),
        popularity = colDef(name = "Popularity"),
        followers = colDef(name = "Followers")
      ),
      defaultColDef = colDef(align = "center"),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      theme = reactable::reactableTheme(backgroundColor = "#121212")
    )
  })
  
  # EventReactive to plot top artists map only when the button is clicked
  top_artists_map_data <- eventReactive(input$update_top_artists_table, {
    validate(need(input$top_artist_limit, "Please select a limit"))
    
    tryCatch({
      plot_top_artists_map(limit = input$top_artist_limit)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
  })
  
  # Render the top artists map
  output$top_artists_map <- renderLeaflet({
    top_artist_map <- top_artists_map_data()  # Use the data triggered by the button click
    top_artist_map
  })
  
  # Output for total listening time summary
  output$summary <- renderText({
    req(user_data())
    paste0("Total time spent listening: ", round(user_data()$total_time, 2), " minutes")
  })
  
  # Genre Analysis Plot
  output$genrePlot <- renderPlot({
    req(input$show_genres, user_data())
    ggplot(user_data()$genres, aes(x = reorder(genres, Freq), y = Freq)) +
      geom_bar(stat = "identity", fill = "#1DB954", color = "black") +  # Spotify green with black borders
      coord_flip() +
      labs(
        title = "Top Genres",
        x = "Genres",
        y = "Count"
      ) +
      theme_minimal(base_family = "Arial") +  # Clean, minimal theme
      theme(
        plot.background = element_rect(fill = "#191414", color = NA),  # Spotify dark background
        panel.background = element_rect(fill = "#191414", color = NA),
        text = element_text(color = "white", size = 14),  # Increased text size
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14),
        plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better visibility
      )
  })
  
  
  
  # Top Tracks Table
  # EventReactive to fetch top tracks only when button is clicked
  top_tracks_data <- eventReactive(input$update_top_tracks_table, {
    req(input$top_track_limit)  # Ensure that limit is selected
    
    # Fetch track details based on the limit
    track_details <- get_top_tracks_details(input$top_track_limit)
    track_details
  })
  
  # Render the top tracks table
  output$top_tracks_table <- renderReactable({
    track_details <- top_tracks_data()  # Use the data triggered by the button click
    
    
    # Render the reactable table
    reactable(
      track_details,
      columns = list(
        track_name = colDef(name = "Track Name", width = 200),
        artists = colDef(name = "Artists", width = 300),
        album_name = colDef(name = "Album Name", width = 200),
        popularity = colDef(name = "Popularity"),
        release_date = colDef(name = "Release Date"),
        album_image_url = colDef(
          name = "Album Cover",
          cell = function(value) {
            if (!is.na(value)) {
              tags$img(src = value, height = "50px", width = "50px")
            } else {
              "No Image"
            }
          }
        )
      ),
      defaultColDef = colDef(align = "center"),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      theme = reactable::reactableTheme(
        backgroundColor = "#121212"
      )
    )
  })
  
  #User Profile
  
  authorization <- reactive({
    get_spotify_authorization_code()
  })
  
  # Fetch user profile info
  user_info <- reactive({
    get_my_profile(authorization = authorization())
  })
  
  output$user_name <- renderText({
    paste("Username:", user_info()$display_name)
  })
  
  output$user_followers <- renderText({
    paste("Followers:", user_info()$followers.total)
  })
  
  # Fetch currently playing track
  currently_playing <- reactive({
    tryCatch({
      get_my_currently_playing(authorization = authorization())
    }, error = function(e) {
      message("Error fetching currently playing track: ", e$message)
      NULL
    })
  })
  
  output$current_track_info <- renderUI({
    req(currently_playing())
    
    track <- currently_playing()$item
    song_name = track$name
    # Extract artist names
    #artists <- sapply(track$artists, function(artist) artist$name)
    artist_names <- track$artists$name
    formatted_artist_names <- format_artists(artist_names)
    div(
      h3("Title:", song_name),
      h4(paste("By:", formatted_artist_names))
    )
  })
  
  output$current_track_album <- renderUI({
    req(currently_playing())
    
    track <- currently_playing()$item
    album <- track$album
    if (!is.null(album$images) && length(album$images) >= 2) {
      tags$img(src = album$images[2, "url"], height = "300px")
    } else {
      div("No album image available")
    }
  })
  
  # Pause playback action
  observeEvent(input$pause, {
    pause_my_playback(authorization = authorization())
  })
  
  # Refresh info
  observeEvent(input$refresh, {
    currently_playing <- reactive({
      tryCatch({
        get_my_currently_playing(authorization = authorization())
      }, error = function(e) {
        message("Error fetching currently playing track: ", e$message)
        NULL
      })
    })
    
    output$user_name <- renderText({
      paste("Username:", user_info()$display_name)
    })
    
    output$user_followers <- renderText({
      paste("Followers:", user_info()$followers.total)
    })
    
    output$current_track_info <- renderUI({
      req(currently_playing())
      
      track <- currently_playing()$item
      song_name = track$name
      # Extract artist names
      #artists <- sapply(track$artists, function(artist) artist$name)
      artist_names <- track$artists$name
      formatted_artist_names <- format_artists(artist_names)
      div(
        h3("Title:", song_name),
        h4(paste("By:", formatted_artist_names))
      )
    })
    
    output$current_track_album <- renderUI({
      req(currently_playing())
      
      track <- currently_playing()$item
      album <- track$album
      if (!is.null(album$images) && nrow(album$images) >= 2) {
        tags$img(src = album$images[2, "url"], height = "300px")
      } else {
        div("No album image available")
      }
    })
    
    
  })
  
  # Placeholder for additional trends
  output$timePlot <- renderPlot({
    req(input$show_time)
    ggplot() +
      geom_line() +
      labs(title = "Listening Time Trends", x = "Date", y = "Time Spent")
  })
  
  # Sample Spotify track data
  all_tracks <- reactive({
    req(user_data()) # Assuming `user_data` holds your tracks data
    user_data()$tracks %>%
      mutate(
        album_image = paste0("<img src='", album.images[[1]]$url, "' height='50'>") # Add album image as HTML
      )
  })
  
  # Reactive filter based on user inputs
  filtered_tracks <- reactive({
    req(all_tracks())
    all_tracks() %>%
      filter(
        danceability >= input$danceability[1],
        danceability <= input$danceability[2],
        energy >= input$energy[1],
        energy <= input$energy[2],
        valence >= input$valence[1],
        valence <= input$valence[2]
      )
  })
  
  # Render dynamic DataTable
  output$filtered_songs_table <- DT::renderDataTable({
    req(filtered_tracks())
    DT::datatable(
      filtered_tracks() %>%
        select(
          "Song Name" = name,
          "Artist" = artists.name,
          "Genre" = genres,   # Assuming `genres` is available
          "Album Image" = album_image
        ),
      escape = FALSE,  # Allow HTML rendering for album images
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  # Generate Playlist
  observeEvent(input$artist, {
    related_artists <- spotifyr::get_related_artists(input$artist_id) # Replace with actual ID
    updateSelectInput(session, "artist", choices = related_artists$name)
  })
  
  observeEvent(input$generate_playlist, {
    playlist <- spotifyr::get_recommendations(
      seed_artists = selected_artist_id, # Obtain from input
      target_danceability = mean(input$danceability),
      target_energy = mean(input$energy),
      target_valence = mean(input$valence),
      min_popularity = input$popularity[1],
      max_popularity = input$popularity[2]
    )
    
    output$playlist_table <- renderTable({
      playlist %>% 
        select(track_name, artist_name, album_name, popularity) # Customize displayed columns
    })
  })
  
  observeEvent(input$save_playlist, {
    playlist_id <- spotifyr::create_playlist(
      user_id = spotifyr::get_my_profile()$id,
      name = paste("Custom Playlist -", Sys.Date()),
      description = "Generated using the Spotify R App"
    )
    
    spotifyr::add_tracks_to_playlist(playlist_id, tracks = playlist$track_id)
    showNotification("Playlist saved successfully!", type = "message")
  })
  
  
  # Top Pick 2023 output
  # Introduction 
  output$description = renderUI({
    tags$div(
      tags$h3("Description:"),
      tags$p("This dataset contains a comprehensive list of the most famous songs of 2023 as listed on Spotify. The dataset offers a wealth of features beyond what is typically available in similar datasets. It provides insights into each song's attributes, popularity, and presence on various music platforms.",style = "font-size: 18px;"),
      tags$p("The dataset includes information such as ",style = "font-size: 18px;", 
             tags$strong("track name, artist(s) name, release date, Spotify playlists and charts, streaming statistics, Apple Music presence, Deezer presence, Shazam charts, and various audio features.",style = "font-size: 18px;")),
      tags$p(
        "Source: ", 
        tags$a(href = "https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023", target = "_blank", "Spotify Dataset Source"),
        style = "margin-top: 200px; font-size: 14px; color: #555;"
      ),
      style = "background-color:#100000 ;color:#ffffff padding: 15px; border-radius: 5px;"
    )
  })
  output$features = renderUI({
    tags$div(
      tags$ul(
        tags$li(tags$strong("track_name:",style = "font-size: 16px;"), " Name of the song",style = "font-size: 16px;"),
        tags$li(tags$strong("artist(s)_name:",style = "font-size: 16px;"), " Name of the artist(s) of the song",style = "font-size: 16px;"),
        tags$li(tags$strong("artist_count:",style = "font-size: 16px;"), " Number of artists contributing to the song",style = "font-size: 16px;"),
        tags$li(tags$strong("released_year:",style = "font-size: 16px;"), " Year when the song was released",style = "font-size: 16px;"),
        tags$li(tags$strong("released_month:",style = "font-size: 16px;"), " Month when the song was released",style = "font-size: 16px;"),
        tags$li(tags$strong("released_day:",style = "font-size: 16px;"), " Day of the month when the song was released",style = "font-size: 16px;"),
        tags$li(tags$strong("in_spotify_playlists:",style = "font-size: 16px;"), " Number of Spotify playlists the song is included in",style = "font-size: 16px;"),
        tags$li(tags$strong("in_spotify_charts:",style = "font-size: 16px;"), " Presence and rank of the song on Spotify charts",style = "font-size: 16px;"),
        tags$li(tags$strong("streams:",style = "font-size: 16px;"), " Total number of streams on Spotify",style = "font-size: 16px;"),
        tags$li(tags$strong("in_apple_playlists:",style = "font-size: 16px;"), " Number of Apple Music playlists the song is included in",style = "font-size: 16px;"),
        tags$li(tags$strong("in_apple_charts:",style = "font-size: 16px;"), " Presence and rank of the song on Apple Music charts",style = "font-size: 16px;"),
        tags$li(tags$strong("in_deezer_playlists:",style = "font-size: 16px;"), " Number of Deezer playlists the song is included in",style = "font-size: 16px;"),
        tags$li(tags$strong("in_deezer_charts:",style = "font-size: 16px;"), " Presence and rank of the song on Deezer charts",style = "font-size: 16px;"),
        tags$li(tags$strong("in_shazam_charts:",style = "font-size: 16px;"), " Presence and rank of the song on Shazam charts",style = "font-size: 16px;")
      ),
      style = "background-color: #100000 ; color:#ffffff padding: 15px; border-radius: 5px;"
    )
  })
  
  
  
  
  
  
  # Render the top 10 tracks by stream summary
  output$topstreamsummary = renderUI({
    top10_streams = HTML(paste0(lapply(1:nrow(top10_streams_list),function(i){
      sprintf("%d. <strong>%s</strong> by %s with %s streams<br>",
              i,
              top10_streams_list$track_name[i],
              top10_streams_list$artist_name[i],
              format(as.numeric(top10_streams_list$streams[i])))
    }),
    collapse = ""))
  })
  # Render the top 10 artists by total streams summary
  output$topartiststreamsummary = renderUI({
    top10_artists_streams = HTML(paste0(
      lapply(1:nrow(top10_total_artists_stream), function(i) {
        sprintf(
          "%d. <strong>%s</strong> with %d tracks and %s streams<br>",
          i,
          top10_total_artists_stream$artist_name[i],
          top10_total_artists_stream$track_num[i],
          format(top10_total_artists_stream$total_streams[i])
        )
      }),
      collapse = ""
    ))
  })
  # Plot for top 10 tracks by stream
  output$topstreamplot = renderPlot({
    ggplot(top10_streams_list,aes(x = reorder(track_name,streams),y = streams,fill = streams))+
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Top 10 Tracks by Streams with Artists",
        x = "Track Name",
        y = "Streams",
      ) +
      geom_text(
        aes(label = artist_name),  # Use artist name as label
        vjust = 0.5,
        hjust = 1.1,
        size = 4,
        face = "bold",
        color = "#100000"
      ) +
      scale_y_continuous(labels = scales::comma_format(scale=0.001,suffix = "k")) +
      scale_fill_gradient(low = "lightgreen", high = "#1DB954")+
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5,size = 14,color = "white",face = "bold"),
        axis.text = element_text(size = 10,color = "white",face = "bold"),
        axis.title = element_text(size = 10,color = "white",face = "bold"),
        plot.margin = unit(c(1, 1, 1, 0), "cm"),
        plot.background = element_rect(fill = "#100000", color = NA),
        panel.background = element_rect(fill = "#100000", color = NA),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        legend.background = element_rect(fill = "#100000"),
        legend.position = "None",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  # Plot for top 10 artists by total streams
  output$totalstreamplot = renderPlot({
    ggplot(top10_total_artists_stream,aes(x = reorder(artist_name,total_streams),y = total_streams,fill = total_streams))+
      geom_bar(stat = "identity")+
      geom_text(
        aes(label = paste0("Tracks number: ", track_num)),
        vjust = 0.5,
        hjust = 1.1,
        size = 4,
        face = "bold",
        color = "#100000")+
      coord_flip()+
      labs(
        title = "Top 10 Artists by Total Streams with Track number",
        x = "Artist",
        y = "Total Streams"
      )+
      scale_y_continuous(labels = scales::comma_format(scale=0.001,suffix = "k")) +
      scale_fill_gradient(low = "lightgreen", high = "#1DB954")+
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5,size = 14,color = "white",face = "bold"),
        axis.text = element_text(size = 10,color = "white",face = "bold"),
        axis.title = element_text(size = 10,color = "white",face = "bold"),
        plot.margin = unit(c(1, 1, 1, 0), "cm"),
        plot.background = element_rect(fill = "#100000", color = NA),
        panel.background = element_rect(fill = "#100000", color = NA),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        legend.background = element_rect(fill = "#100000"),
        legend.position = "None",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Render the top 10 tracks by playlist summary
  output$topplaylistsummary = renderUI({
    HTML(paste0(
      lapply(1:nrow(top10_spotify_playlists), function(i) {
        sprintf(
          "%d. <strong>%s</strong> by %s with %s playlists<br>",
          i,
          top10_spotify_playlists$track_name[i],
          top10_spotify_playlists$artist_name[i],
          format(as.numeric(top10_spotify_playlists$in_spotify_playlists[i]), big.mark = ",")
        )
      }),
      collapse = ""
    ))
  })
  
  # Render the top 10 artists by total playlists summary
  output$topartistplaylistsummary = renderUI({
    HTML(paste0(
      lapply(1:nrow(top10_total_artists_playlists), function(i) {
        sprintf(
          "%d. <strong>%s</strong> with %d tracks and %s playlists<br>",
          i,
          top10_total_artists_playlists$artist_name[i],
          top10_total_artists_playlists$track_num[i],
          format(top10_total_artists_playlists$total_playlists[i], big.mark = ",")
        )
      }),
      collapse = ""
    ))
  })
  
  # Plot for Top 10 Tracks by Playlist Count
  output$topplaylistplot = renderPlot({
    ggplot(top10_spotify_playlists, aes(x = reorder(track_name, in_spotify_playlists), y = in_spotify_playlists, fill = in_spotify_playlists)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Top 10 Tracks by Playlist Count with Artists",
        x = "Track Name",
        y = "Playlist Count"
      ) +
      geom_text(
        aes(label = artist_name),  # Use artist name as label
        vjust = 0.5,
        hjust = 1.1,
        size = 4,
        face = "bold",
        color = "#100000"
      ) +
      scale_y_continuous(labels = scales::comma_format(scale = 0.001, suffix = "k")) +
      scale_fill_gradient(low = "lightgreen", high = "#1DB954") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
        axis.text = element_text(size = 10, color = "white", face = "bold"),
        axis.title = element_text(size = 10, color = "white", face = "bold"),
        plot.margin = unit(c(1, 1, 1, 0), "cm"),
        plot.background = element_rect(fill = "#100000", color = NA),
        panel.background = element_rect(fill = "#100000", color = NA),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        legend.background = element_rect(fill = "#100000"),
        legend.position = "None",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Plot for Top 10 Artists by Total Playlist Count
  output$totalplaylistplot = renderPlot({
    ggplot(top10_total_artists_playlists, aes(x = reorder(artist_name, total_playlists), y = total_playlists, fill = total_playlists)) +
      geom_bar(stat = "identity") +
      geom_text(
        aes(label = paste0("Tracks number: ", track_num)),
        vjust = 0.5,
        hjust = 1.1,
        size = 4,
        face = "bold",
        color = "#100000"
      ) +
      coord_flip() +
      labs(
        title = "Top 10 Artists by Total Playlists with Track Number",
        x = "Artist",
        y = "Total Playlists"
      ) +
      scale_y_continuous(labels = scales::comma_format(scale = 0.001, suffix = "k")) +
      scale_fill_gradient(low = "lightgreen", high = "#1DB954") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
        axis.text = element_text(size = 10, color = "white", face = "bold"),
        axis.title = element_text(size = 10, color = "white", face = "bold"),
        plot.margin = unit(c(1, 1, 1, 0), "cm"),
        plot.background = element_rect(fill = "#100000", color = NA),
        panel.background = element_rect(fill = "#100000", color = NA),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        legend.background = element_rect(fill = "#100000"),
        legend.position = "None",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Create dynamic filter UI based on the selection
  output$dynamic_filter = renderUI({
    if (input$selection == "Track Name") {
      selectInput("track_artist_filter", 
                  "Select Track Name:", 
                  choices = unique(data_reference$track_name))
    } else {
      artist_ordered = data_reference %>%
        group_by(artist_name) %>%
        summarize(total_streams = sum(streams, na.rm = TRUE)) %>%
        arrange(desc(total_streams))
      
      selectInput("track_artist_filter", 
                  "Select Artist Name:", 
                  choices = unique(artist_ordered))
    }
  })
  
  # Filter the data re-actively
  filtered_data = reactive({
    if (input$selection == "Track Name") {
      data_reference %>%
        filter(track_name == input$track_artist_filter &
                 in_spotify_playlists >= input$playlist_filter[1] &
                 in_spotify_playlists <= input$playlist_filter[2] &
                 streams >= input$stream_filter[1] &
                 streams <= input$stream_filter[2])
    } else {
      data_reference %>%
        filter(artist_name == input$track_artist_filter &
                 in_spotify_playlists >= input$playlist_filter[1] &
                 in_spotify_playlists <= input$playlist_filter[2] &
                 streams >= input$stream_filter[1] &
                 streams <= input$stream_filter[2])
    }
  })
  
  
  # Split the track list into two parts
  split_tracks <- reactive({
    if (input$selection == "Artist Name") {
      tracks = data_reference %>%
        filter(artist_name == input$track_artist_filter) %>%
        select(track_name, streams, in_spotify_playlists) %>%
        arrange(desc(streams))
      mid_point = ceiling(nrow(tracks) / 2)
      list(
        left = tracks[1:mid_point, ],
        right = tracks[(mid_point + 1):nrow(tracks), ]
      )
    } else {
      list(left = NULL, right = NULL)  # Return empty lists if "Track Name" is selected
    }
  })
  
  # Render the left column table
  output$track_table_left = renderTable({
    split_tracks()$left
  })
  
  # Render the right column table
  output$track_table_right = renderTable({
    split_tracks()$right
  })
}

# Run the application 
shinyApp(ui = ui, server = server)