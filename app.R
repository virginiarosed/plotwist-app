# ==============================================================================
# PLOTWIST
# ==============================================================================

library(shiny)
library(shinyjs)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(shinyWidgets)
library(bcrypt)

# ==============================================================================
# DATABASE & API CONFIGURATION
# ==============================================================================

get_db_connection <- function() {
  tryCatch({
    db_url <- Sys.getenv("DATABASE_URL")
    
    message("=== DATABASE CONNECTION DEBUG ===")
    message("1. DATABASE_URL is set: ", db_url != "")
    message("2. DATABASE_URL preview: ", ifelse(db_url != "", substr(db_url, 1, 30), "EMPTY"))
    
    if (db_url != "") {
      message("3. Using PRODUCTION database")
      
      con <- dbConnect(RPostgres::Postgres(), 
                       dbname = db_url,
                       sslmode = "require",
                       sslrootcert = system.file("certs/ca-certificates.crt", package = "RPostgres"))
      
      message("4. PRODUCTION connection SUCCESS!")
      return(con)
    } else {
      message("3. Using LOCAL database")
      con <- dbConnect(
        Postgres(),
        host = "localhost",
        port = 5432,
        dbname = "plotwist_db",
        user = "postgres",
        password = "Plotwist@2026"
      )
      message("4. Local connection SUCCESS!")
      return(con)
    }
  }, error = function(e) {
    message("❌ ERROR in get_db_connection(): ", e$message)
    message("Trying alternative connection method...")
    
    tryCatch({
      db_url <- Sys.getenv("DATABASE_URL")
      if (db_url != "") {
        
        library(stringr)
        
        pattern <- "postgresql://([^:]+):([^@]+)@([^/]+)/(.+)"
        matches <- str_match(db_url, pattern)
        
        if (!is.na(matches[1,1])) {
          user <- matches[1,2]
          password <- matches[1,3]
          host_port <- matches[1,4]
          dbname <- matches[1,5]
          
          host_parts <- str_split(host_port, ":")[[1]]
          if (length(host_parts) == 2) {
            host <- host_parts[1]
            port <- as.numeric(host_parts[2])
          } else {
            host <- host_parts[1]
            port <- 5432
          }
          
          message("Alternative connection - Host:", host, " Port:", port, " DB:", dbname)
          
          con <- dbConnect(
            Postgres(),
            host = host,
            port = port,
            dbname = dbname,
            user = user,
            password = password,
            sslmode = "require"
          )
          message("Alternative connection SUCCESS!")
          return(con)
        }
      }
    }, error = function(e2) {
      message("❌ Alternative method also failed: ", e2$message)
    })
    
    return(NULL)
  })
}

# Helper function to get watch date from watch_history
get_watch_date <- function(movie_id) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  tryCatch({
    query <- sprintf("SELECT watch_date FROM watch_history WHERE movie_id = %d ORDER BY watch_date DESC LIMIT 1", movie_id)
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (nrow(result) > 0) {
      return(result$watch_date[1])
    } else {
      return(NULL)
    }
  }, error = function(e) {
    if (!is.null(con)) dbDisconnect(con)
    return(NULL)
  })
}

migrate_achievement_notifications <- function() {
  con <- get_db_connection()
  if (is.null(con)) return()
  
  tryCatch({
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS achievement_notifications (
          user_id INTEGER NOT NULL,
          achievement_id VARCHAR(50) NOT NULL,
          shown_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (user_id, achievement_id),
          FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
      )
    ")
    dbDisconnect(con)
    message("✅ Achievement notifications table created successfully!")
  }, error = function(e) {
    if (!is.null(con)) dbDisconnect(con)
    message("❌ Error creating achievement notifications table: ", e$message)
  })
}

migrate_achievement_notifications()

TMDB_API_KEY <- "3634119368f89a9c28e1768f1a04cefd"
TMDB_BASE_URL <- "https://api.themoviedb.org/3"
TMDB_IMAGE_BASE <- "https://image.tmdb.org/t/p/w500"

# ==============================================================================
# FEATURED CONTENT DATA
# ==============================================================================

FEATURED_CONTENT <- list(
  list(
    title = "Avengers: Endgame",
    logo_url = "https://image.tmdb.org/t/p/original/pjZSBgMDYjEhyanp8aahfE1KcAn.png",
    poster_url = "https://image.tmdb.org/t/p/w500/or06FN3Dka5tukK1e9sl16pB3iy.jpg",
    backdrop_url = "https://image.tmdb.org/t/p/original/7RyHsO4yDXtBv1zUU3mTpHeQ0d5.jpg",
    year = "2019",
    duration = "3h 1m",
    genre = "Action",
    plot = "After the devastating events of Avengers: Infinity War, the universe is in ruins. With the help of remaining allies, the Avengers assemble once more to reverse Thanos' actions and restore balance to the universe."
  ),
  list(
    title = "Black Mirror",
    logo_url = "https://image.tmdb.org/t/p/original/hmS9vRMSyzYK3D2fLoM97O4liqZ.png",
    poster_url = "https://image.tmdb.org/t/p/w500/seN6rRfN0I6n8iDXjlSMk1QjNcq.jpg",
    backdrop_url = "https://image.tmdb.org/t/p/original/roAMc6HxdXun60b001YBrpSH9at.jpg",
    year = "2011",
    duration = "7 episodes",
    genre = "Sci-Fi",
    plot = "Twisted tales run wild in this mind-bending anthology series that reveals humanity's worst traits, greatest innovations and more."
  ),
  list(
    title = "X-Men: Apocalypse",
    logo_url = "https://image.tmdb.org/t/p/original/rvVpfnBzNUL3xiFQxyndjDXPJsO.png",
    poster_url = "https://image.tmdb.org/t/p/original/ikA8UhYdTGpqbatFa93nIf6noSr.jpg",
    backdrop_url = "https://image.tmdb.org/t/p/original/26tqYn7onNctz1hCWBLYuS5cOnt.jpg",
    year = "2016",
    duration = "2h 24m",
    genre = "Action",
    plot = "After the re-emergence of the world's first mutant, world-destroyer Apocalypse, the X-Men must unite to defeat his extinction level plan."
  ),
  list(
    title = "Game of Thrones",
    logo_url = "https://image.tmdb.org/t/p/original/6pObznbCoxVpY1lPQwJxETd7Phe.png",
    poster_url = "https://image.tmdb.org/t/p/original/1XS1oqL89opfnbLl8WnZY1O1uJx.jpg",
    backdrop_url = "https://image.tmdb.org/t/p/original/zIhCxjl0ieuMEELSuVkP43SDwRq.jpg",
    year = "2011",
    duration = "8 episodes",
    genre = "Drama",
    plot = "Seven noble families fight for control of the mythical land of Westeros. Friction between the houses leads to full-scale war. All while a very ancient evil awakens in the farthest north. Amidst the war, a neglected military order of misfits, the Night's Watch, is all that stands between the realms of men and icy horrors beyond."
  ),
  list(
    title = "Venom",
    logo_url = "https://image.tmdb.org/t/p/original/5JNhTDkT7yXhoLlwCaS3hAnavHi.png",
    poster_url = "https://image.tmdb.org/t/p/original/2uNW4WbgBXL25BAbXGLnLqX71Sw.jpg",
    backdrop_url = "https://image.tmdb.org/t/p/original/VuukZLgaCrho2Ar8Scl9HtV3yD.jpg",
    year = "2018",
    duration = "1h 52m",
    genre = "Action",
    plot = "Investigative journalist Eddie Brock attempts a comeback following a scandal, but accidentally becomes the host of Venom, a violent, super powerful alien symbiote. Soon, he must rely on his newfound powers to protect the world from a shadowy organization looking for a symbiote of their own."
  )
)

# ==============================================================================
# INITIALIZE MODAL FOR RECOMMENDATIONS ADDITION
# ==============================================================================

initialize_modal_for_recommendations <- function(session, media_type) {
  # Reset all fields to defaults with Unwatched status
  updateSelectizeInput(session, "modal_status", selected = "Unwatched")
  updateSelectizeInput(session, "modal_media_type", selected = media_type)
  
  # Reset numeric inputs based on media type
  if (media_type == "Movie") {
    updateNumericInput(session, "modal_watched_duration", value = 0)
    updateNumericInput(session, "modal_total_episodes_watched", value = 0)
  } else if (media_type == "TV Series") {
    updateNumericInput(session, "modal_current_season", value = 0)
    updateNumericInput(session, "modal_current_episode", value = 0)
    updateNumericInput(session, "modal_total_episodes_watched", value = 0)
    updateNumericInput(session, "modal_watched_duration", value = 0)
  }
  
  # Reset rating
  session$sendCustomMessage(
    type = "eval",
    message = "
      setTimeout(function() {
        if (typeof setRating === 'function') {
          setRating(0, false);
        }
      }, 100);
    "
  )
}

# ==============================================================================
# AUTHENTICATION FUNCTIONS
# ==============================================================================

authenticate_user <- function(email, password) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  tryCatch({
    query <- sprintf("SELECT * FROM users WHERE email = '%s'", 
                     gsub("'", "''", email))
    user <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (nrow(user) == 0) return(NULL)
    
    # Verify password
    if (checkpw(password, user$password[1])) {
      # Update last login
      con <- get_db_connection()
      if (!is.null(con)) {
        update_query <- sprintf("UPDATE users SET last_login = NOW() WHERE id = %d", 
                                user$id[1])
        dbExecute(con, update_query)
        dbDisconnect(con)
      }
      return(user)
    }
    return(NULL)
  }, error = function(e) {
    if (!is.null(con)) dbDisconnect(con)
    return(NULL)
  })
}

# ==============================================================================
# TMDB SEARCH FUNCTIONS
# ==============================================================================

search_tmdb <- function(query) {
  if (nchar(query) < 2) return(NULL)
  
  tryCatch({
    url <- paste0(TMDB_BASE_URL, "/search/multi",
                  "?api_key=", TMDB_API_KEY,
                  "&query=", URLencode(query),
                  "&page=1")
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      results <- data$results
      
      if (length(results) > 0) {
        filtered <- results[sapply(results, function(x) x$media_type %in% c("movie", "tv"))]
        filtered <- head(filtered, 5)
        
        if (length(filtered) > 0) {
          return(lapply(filtered, function(x) {
            list(
              id = x$id,
              title = if (x$media_type == "movie") x$title else x$name,
              year = if (!is.null(x$release_date) && x$release_date != "") {
                substr(x$release_date, 1, 4)
              } else if (!is.null(x$first_air_date) && x$first_air_date != "") {
                substr(x$first_air_date, 1, 4)
              } else {
                "N/A"
              },
              poster = if (!is.null(x$poster_path)) {
                paste0(TMDB_IMAGE_BASE, x$poster_path)
              } else {
                "https://via.placeholder.com/92x138/1A1F29/00A8E8?text=No+Poster"
              },
              overview = if (!is.null(x$overview) && x$overview != "") {
                substr(x$overview, 1, 100)
              } else {
                "No overview available"
              },
              media_type = if (x$media_type == "movie") "Movie" else "TV Series"
            )
          }))
        }
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Search error: ", e$message)
    return(NULL)
  })
}

get_tmdb_details <- function(tmdb_id, media_type) {
  tryCatch({
    endpoint <- if (media_type == "Movie") "movie" else "tv"
    
    url <- paste0(TMDB_BASE_URL, "/", endpoint, "/", tmdb_id,
                  "?api_key=", TMDB_API_KEY,
                  "&append_to_response=credits")
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      
      genres <- if (!is.null(data$genres) && length(data$genres) > 0) {
        paste(sapply(data$genres, function(g) g$name), collapse = ", ")
      } else ""
      
      year <- if (media_type == "Movie" && !is.null(data$release_date) && data$release_date != "") {
        as.numeric(substr(data$release_date, 1, 4))
      } else if (media_type == "TV Series" && !is.null(data$first_air_date) && data$first_air_date != "") {
        as.numeric(substr(data$first_air_date, 1, 4))
      } else year(Sys.Date())
      
      director <- ""
      if (media_type == "Movie" && !is.null(data$credits$crew)) {
        directors <- Filter(function(x) x$job == "Director", data$credits$crew)
        if (length(directors) > 0) {
          director <- paste(sapply(head(directors, 2), function(d) d$name), collapse = ", ")
        }
      } else if (media_type == "TV Series" && !is.null(data$created_by)) {
        if (length(data$created_by) > 0) {
          director <- paste(sapply(head(data$created_by, 2), function(c) c$name), collapse = ", ")
        }
      }
      
      cast <- ""
      if (!is.null(data$credits$cast) && length(data$credits$cast) > 0) {
        cast <- paste(sapply(head(data$credits$cast, 10), function(c) c$name), collapse = ", ")
      }
      
      duration <- if (media_type == "Movie" && !is.null(data$runtime)) {
        as.numeric(data$runtime)
      } else 0
      
      plot_summary <- if (!is.null(data$overview) && data$overview != "") {
        data$overview
      } else ""
      
      poster_url <- if (!is.null(data$poster_path)) {
        paste0(TMDB_IMAGE_BASE, data$poster_path)
      } else ""
      
      total_episodes <- if (media_type == "TV Series" && !is.null(data$number_of_episodes)) {
        as.numeric(data$number_of_episodes)
      } else 0
      
      total_seasons <- if (media_type == "TV Series" && !is.null(data$number_of_seasons)) {
        as.numeric(data$number_of_seasons)
      } else 1
      
      # Get TMDB rating
      rating <- if (!is.null(data$vote_average)) {
        round(data$vote_average, 1)
      } else 0
      
      # Get popularity
      popularity <- if (!is.null(data$popularity)) {
        round(data$popularity, 1)
      } else 0
      
      # Get status (for TV series)
      status <- if (media_type == "TV Series" && !is.null(data$status)) {
        data$status
      } else if (media_type == "Movie" && !is.null(data$status)) {
        data$status
      } else "N/A"
      
      # Get tagline (for movies)
      tagline <- if (media_type == "Movie" && !is.null(data$tagline)) {
        data$tagline
      } else ""
      
      return(list(
        title = if (media_type == "Movie") data$title else data$name,
        media_type = media_type,
        genres = genres,
        year = year,
        director = director,
        cast = cast,
        duration = duration,
        plot_summary = plot_summary,
        poster_url = poster_url,
        total_episodes = total_episodes,
        total_seasons = total_seasons,
        tmdb_id = tmdb_id,
        rating = rating,
        popularity = popularity,
        status = status,
        tagline = tagline
      ))
    }
    return(NULL)
  }, error = function(e) {
    message("Details error: ", e$message)
    return(NULL)
  })
}

get_tv_season_details <- function(tmdb_id, season_number) {
  tryCatch({
    url <- paste0(TMDB_BASE_URL, "/tv/", tmdb_id, "/season/", season_number,
                  "?api_key=", TMDB_API_KEY)
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      
      return(list(
        episodes_count = if (!is.null(data$episodes)) length(data$episodes) else 0,
        season_name = if (!is.null(data$name)) data$name else paste("Season", season_number)
      ))
    }
    return(NULL)
  }, error = function(e) {
    message("Season details error: ", e$message)
    return(NULL)
  })
}

# NEW: Get collection details from TMDB
get_tmdb_collection <- function(tmdb_id, media_type) {
  tryCatch({
    endpoint <- if (media_type == "Movie") "movie" else "tv"
    
    # First, get the item details to find collection ID
    url <- paste0(TMDB_BASE_URL, "/", endpoint, "/", tmdb_id,
                  "?api_key=", TMDB_API_KEY)
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      
      # For movies, check if it belongs to a collection
      if (media_type == "Movie" && !is.null(data$belongs_to_collection)) {
        collection_id <- data$belongs_to_collection$id
        
        # Fetch collection details
        collection_url <- paste0(TMDB_BASE_URL, "/collection/", collection_id,
                                 "?api_key=", TMDB_API_KEY)
        
        collection_response <- GET(collection_url)
        
        if (status_code(collection_response) == 200) {
          collection_data <- content(collection_response, "parsed")
          
          if (!is.null(collection_data$parts) && length(collection_data$parts) > 0) {
            return(list(
              collection_name = collection_data$name,
              items = lapply(collection_data$parts, function(part) {
                list(
                  tmdb_id = part$id,
                  title = part$title,
                  year = if (!is.null(part$release_date) && part$release_date != "") {
                    substr(part$release_date, 1, 4)
                  } else "N/A",
                  poster = if (!is.null(part$poster_path)) {
                    paste0(TMDB_IMAGE_BASE, part$poster_path)
                  } else "https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster",
                  rating = if (!is.null(part$vote_average)) round(part$vote_average, 1) else 0,
                  overview = if (!is.null(part$overview)) part$overview else ""
                )
              })
            ))
          }
        }
      }
      
      # For TV series, get recommendations/similar shows
      if (media_type == "TV Series") {
        similar_url <- paste0(TMDB_BASE_URL, "/tv/", tmdb_id, "/similar",
                              "?api_key=", TMDB_API_KEY,
                              "&page=1")
        
        similar_response <- GET(similar_url)
        
        if (status_code(similar_response) == 200) {
          similar_data <- content(similar_response, "parsed")
          
          if (!is.null(similar_data$results) && length(similar_data$results) > 0) {
            
            similar_items <- head(similar_data$results, 6)
            
            return(list(
              collection_name = paste0("Similar to ", data$name),
              items = lapply(similar_items, function(item) {
                list(
                  tmdb_id = item$id,
                  title = item$name,
                  year = if (!is.null(item$first_air_date) && item$first_air_date != "") {
                    substr(item$first_air_date, 1, 4)
                  } else "N/A",
                  poster = if (!is.null(item$poster_path)) {
                    paste0(TMDB_IMAGE_BASE, item$poster_path)
                  } else "https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster",
                  rating = if (!is.null(item$vote_average)) round(item$vote_average, 1) else 0,
                  overview = if (!is.null(item$overview)) item$overview else ""
                )
              })
            ))
          }
        }
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Collection fetch error: ", e$message)
    return(NULL)
  })
}

get_movie_recommendations <- function(genre_ids, media_type = "movie", page = 1, exclude_titles = NULL, exclude_tmdb_ids = NULL) {
  tryCatch({
    if (media_type == "movie") {
      url <- paste0(TMDB_BASE_URL, "/discover/movie",
                    "?api_key=", TMDB_API_KEY,
                    "&with_genres=", paste(genre_ids, collapse = ","),
                    "&sort_by=popularity.desc&page=", page)
    } else {
      url <- paste0(TMDB_BASE_URL, "/discover/tv",
                    "?api_key=", TMDB_API_KEY,
                    "&with_genres=", paste(genre_ids, collapse = ","),
                    "&sort_by=popularity.desc&page=", page)
    }
    
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      results <- data$results
      if (length(results) > 0) {
        recommendations_df <- data.frame(
          tmdb_id = sapply(results, function(x) x$id),
          title = sapply(results, function(x) if (media_type == "movie") x$title else x$name),
          year = sapply(results, function(x) {
            date <- if (media_type == "movie") x$release_date else x$first_air_date
            if (!is.null(date) && date != "") as.numeric(substr(date, 1, 4)) else NA
          }),
          poster = sapply(results, function(x) {
            if (!is.null(x$poster_path)) paste0(TMDB_IMAGE_BASE, x$poster_path)
            else "https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster"
          }),
          rating = sapply(results, function(x) if (!is.null(x$vote_average)) round(x$vote_average, 1) else 0),
          overview = sapply(results, function(x) if (!is.null(x$overview)) x$overview else ""),
          stringsAsFactors = FALSE
        )
        
        if (!is.null(exclude_titles) && length(exclude_titles) > 0) {
          recommendations_df <- recommendations_df[!tolower(recommendations_df$title) %in% tolower(exclude_titles), ]
        }
        
        if (!is.null(exclude_tmdb_ids) && length(exclude_tmdb_ids) > 0) {
          recommendations_df <- recommendations_df[!recommendations_df$tmdb_id %in% exclude_tmdb_ids, ]
        }
        
        return(recommendations_df)
      }
    }
    return(data.frame(tmdb_id = numeric(0), title = character(0), year = numeric(0), poster = character(0), rating = numeric(0), overview = character(0)))
  }, error = function(e) {
    return(data.frame(tmdb_id = numeric(0), title = character(0), year = numeric(0), poster = character(0), rating = numeric(0), overview = character(0)))
  })
}

GENRE_TO_TMDB_ID <- list(
  "Action" = 28, "Adventure" = 12, "Animation" = 16, "Comedy" = 35,
  "Crime" = 80, "Documentary" = 99, "Drama" = 18, "Family" = 10751,
  "Fantasy" = 14, "History" = 36, "Horror" = 27, "Music" = 10402,
  "Mystery" = 9648, "Romance" = 10749, "Science Fiction" = 878,
  "Thriller" = 53, "War" = 10752, "Western" = 37
)

TV_GENRE_TO_TMDB_ID <- list(
  "Action" = 10759, "Adventure" = 10759, "Animation" = 16, "Comedy" = 35,
  "Crime" = 80, "Documentary" = 99, "Drama" = 18, "Family" = 10751,
  "Fantasy" = 10765, "History" = 36, "Horror" = 27, "Music" = 10402,
  "Mystery" = 9648, "Romance" = 10749, "Science Fiction" = 10765,
  "Thriller" = 53, "War" = 10752, "Western" = 37
)

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  title = "Plotwist | Turn Your Watching Into a Plotwist",
  tags$head(
    includeCSS("www/style.css"),
    
    # Favicon
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon.png"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        console.log('DOM loaded - setting up logo animation');
        
        function initLogoFlip() {
  const flipContainer = document.querySelector('.logo-flip-container');
  
  if (!flipContainer) {
    console.log('Logo flip container not found, retrying...');
    return false;
  }
  
  console.log('Logo flip container found, initializing animation');
  
  let isFlipped = false;
  
  function flipLogo() {
    if (isFlipped) {
      flipContainer.style.transform = 'rotateY(0deg)';
    } else {
      flipContainer.style.transform = 'rotateY(180deg)';
    }
    isFlipped = !isFlipped;
  }
  
  setTimeout(flipLogo, 500);
  setInterval(flipLogo, 3000);
  
  return true;
}

function tryInitLogoFlip(maxAttempts = 10, delay = 300) {
  let attempts = 0;
  
  const attemptInit = () => {
    attempts++;
    console.log('Logo flip init attempt', attempts);
    
    if (initLogoFlip()) {
      console.log('Logo flip initialized successfully');
      return;
    }
    
    if (attempts < maxAttempts) {
      setTimeout(attemptInit, delay);
    } else {
      console.log('Logo flip initialization failed after', maxAttempts, 'attempts');
      
      const flipContainer = document.querySelector('.logo-flip-container');
      if (flipContainer) {
        flipContainer.classList.add('flip-animation');
      }
    }
  };
  
  attemptInit();
}

document.addEventListener('DOMContentLoaded', function() {
  console.log('DOM loaded - starting logo animation setup');
  tryInitLogoFlip();
});

$(document).on('shiny:connected', function() {
  console.log('Shiny connected - starting logo animation setup');
  setTimeout(() => tryInitLogoFlip(), 500);
});

window.addEventListener('load', function() {
  console.log('Window loaded - final logo animation attempt');
  setTimeout(() => tryInitLogoFlip(5, 500), 1000);
});
        
        // ==============================================================================
        // NAVIGATION SCROLL STATE MANAGEMENT
        // ==============================================================================
        let isScrolling = false;
        let scrollTimer;
        
        const nav = document.querySelector('.modern-nav');
        if (nav) {
        
          nav.classList.add('no-scroll');
          nav.classList.remove('scrolling');
          
          window.addEventListener('scroll', function() {
            if (nav) {
            
              nav.classList.remove('no-scroll');
              nav.classList.add('scrolling');
              
              clearTimeout(scrollTimer);
              
              isScrolling = true;
              
              scrollTimer = setTimeout(function() {
                isScrolling = false;
                
                if (window.scrollY === 0) {
                  nav.classList.remove('scrolling');
                  nav.classList.add('no-scroll');
                }
              }, 150);
            }
          });
          
          if (window.scrollY === 0) {
            nav.classList.remove('scrolling');
            nav.classList.add('no-scroll');
          } else {
            nav.classList.remove('no-scroll');
            nav.classList.add('scrolling');
          }
        }
        
        function checkScrollPosition() {
          if (nav) {
            if (window.scrollY === 0) {
              
              nav.classList.remove('scrolling');
              nav.classList.add('no-scroll');
            } else {
              
              nav.classList.remove('no-scroll');
              nav.classList.add('scrolling');
            }
          }
        }
        
        
        window.addEventListener('scroll', checkScrollPosition);
        checkScrollPosition();

let currentFeaturedIndex = 0;
let featuredAutoAdvance = null;

function initFeaturedCarousel() {
  const featuredItems = document.querySelectorAll('.featured-poster-item');
  if (featuredItems.length === 0) return;

  if (featuredAutoAdvance) {
    clearInterval(featuredAutoAdvance);
  }
  
  // Function to update carousel positions based on active index
function updateCarouselPositions(activeIndex) {
  const totalItems = featuredItems.length;

  requestAnimationFrame(() => {
    for (let i = 0; i < totalItems; i++) {
      const item = featuredItems[i];
      let relativePosition;
      
      if (i === activeIndex) {
        relativePosition = 3; // Center position
      } else {
      
        let diff = (i - activeIndex + totalItems) % totalItems;
        
        if (diff === 1 || diff === totalItems - 1) {

          if (diff === 1) {
            relativePosition = 4; // Right inner
          } else {
            relativePosition = 2; // Left inner
          }
        } else if (diff === 2 || diff === totalItems - 2) {

          if (diff === 2) {
            relativePosition = 5; // Rightmost
          } else {
            relativePosition = 1; // Leftmost
          }
        } else {

          relativePosition = 0; // Hidden
          item.style.display = 'none';
          continue;
        }
      }
      
      item.style.display = 'block';

      item.classList.remove('pos-1', 'pos-2', 'pos-3', 'pos-4', 'pos-5');

      setTimeout(() => {
        item.classList.add(`pos-${relativePosition}`);
      }, 10 * Math.abs(i - activeIndex)); 
    }
  });
  
  updateFeaturedContent(activeIndex);
}
  
  // Function to update featured content
function updateFeaturedContent(index) {
  const featuredSection = document.querySelector('.featured-section');
  const featuredContent = document.querySelector('.featured-content');
  
  if (!featuredSection || !featuredContent) return;
  
  // Get data from data attributes
  const featuredItems = document.querySelectorAll('.featured-poster-item');
  const item = featuredItems[index];
  
  if (!item) return;
  
  // Get data from data attributes
  const backdrop = item.getAttribute('data-backdrop');
  const logo = item.getAttribute('data-logo');
  const title = item.getAttribute('data-title');
  const year = item.getAttribute('data-year');
  const duration = item.getAttribute('data-duration');
  const genre = item.getAttribute('data-genre');
  const plot = item.getAttribute('data-plot');
  
  // Update background image
  if (backdrop) {
    requestAnimationFrame(() => {

      const tempBg = document.createElement('div');
      tempBg.style.position = 'absolute';
      tempBg.style.top = '0';
      tempBg.style.left = '0';
      tempBg.style.width = '100%';
      tempBg.style.height = '100%';
      tempBg.style.backgroundImage = `linear-gradient(to right, rgba(26, 31, 41, 0.95) 0%, rgba(26, 31, 41, 0.85) 50%, rgba(26, 31, 41, 0.3) 100%), url('${backdrop}')`;
      tempBg.style.backgroundSize = 'cover';
      tempBg.style.backgroundPosition = 'center';
      tempBg.style.opacity = '0';
      tempBg.style.transition = 'opacity 0.8s ease';
      tempBg.style.zIndex = '0';
      tempBg.style.pointerEvents = 'none'; // Don't interfere with carousel clicks
      
      featuredSection.appendChild(tempBg);

      setTimeout(() => {
        tempBg.style.opacity = '1';
      }, 50);

      setTimeout(() => {
        featuredSection.style.backgroundImage = `linear-gradient(to right, rgba(26, 31, 41, 0.95) 0%, rgba(26, 31, 41, 0.85) 50%, rgba(26, 31, 41, 0.3) 100%), url('${backdrop}')`;
        featuredSection.removeChild(tempBg);
      }, 850);
    });
  }

  setTimeout(() => {
    const logoImg = featuredContent.querySelector('.featured-logo');
    const titleFallback = featuredContent.querySelector('.featured-title-fallback');
    const yearSpan = featuredContent.querySelector('.featured-year');
    const durationSpan = featuredContent.querySelector('.featured-duration');
    const genreSpan = featuredContent.querySelector('.featured-genre');
    const plotP = featuredContent.querySelector('.featured-plot');

    const fadeElement = (element, newValue, display = null) => {
      if (!element || !newValue) return;
      
      // Fade out
      element.style.transition = 'opacity 0.3s ease';
      element.style.opacity = '0';
      
      // Update and fade back in
      setTimeout(() => {
        if (display !== null) {
          element.style.display = display;
        }
        element.textContent = newValue;
        element.style.opacity = '1';
      }, 300);
    };

    if (logo && logoImg) {
      logoImg.style.transition = 'opacity 0.3s ease';
      logoImg.style.opacity = '0';
      
      setTimeout(() => {
        logoImg.src = logo;
        logoImg.style.opacity = '1';
        logoImg.style.display = 'block';
        if (titleFallback) {
          titleFallback.style.display = 'none';
        }
      }, 300);
    }
    
    // Handle title fallback
    if (title && titleFallback) {
      if (!logo || logo === '') {
        titleFallback.style.transition = 'opacity 0.3s ease';
        titleFallback.style.opacity = '0';
        
        setTimeout(() => {
          titleFallback.textContent = title;
          titleFallback.style.display = 'block';
          titleFallback.style.opacity = '1';
          if (logoImg) logoImg.style.display = 'none';
        }, 300);
      } else {
        titleFallback.textContent = title;
      }
    }
    
    // Fade other text elements
    fadeElement(yearSpan, year);
    fadeElement(durationSpan, duration);
    fadeElement(genreSpan, genre);
    
    if (plot && plotP) {
      plotP.style.transition = 'opacity 0.4s ease';
      plotP.style.opacity = '0';
      
      setTimeout(() => {
        plotP.textContent = plot;
        plotP.style.opacity = '1';
      }, 400);
    }
    
  }, 200);
}
  
  // Click handlers for featured posters
  featuredItems.forEach((item, index) => {
    item.addEventListener('click', function() {
      currentFeaturedIndex = index;
      updateCarouselPositions(index);

      if (featuredAutoAdvance) {
        clearInterval(featuredAutoAdvance);
      }
      featuredAutoAdvance = setInterval(function() {
        if (featuredItems.length > 0) {
          currentFeaturedIndex = (currentFeaturedIndex + 1) % featuredItems.length;
          updateCarouselPositions(currentFeaturedIndex);
        }
      }, 5000);
    });
  });
  
  // Initialize with first item in center
  updateCarouselPositions(currentFeaturedIndex);

  featuredAutoAdvance = setInterval(function() {
    if (featuredItems.length > 0) {
      currentFeaturedIndex = (currentFeaturedIndex + 1) % featuredItems.length;
      updateCarouselPositions(currentFeaturedIndex);
    }
  }, 5000);
}

function getCircularPosition(currentIndex, itemIndex, totalItems) {

  let diff = (itemIndex - currentIndex + totalItems) % totalItems;

  if (diff > 2 && diff < totalItems - 2) {
    return 0; // Hidden
  }
  
  // Map to visible positions
  switch(diff) {
    case 0: return 3; // Center
    case 1: return 4; // Right inner
    case totalItems - 1: return 2; // Left inner
    case 2: return 5; // Rightmost
    case totalItems - 2: return 1; // Leftmost
    default: return 0; // Hidden
  }
}

setTimeout(initFeaturedCarousel, 1000);

$(document).on('shiny:value', function(event) {
  if (event.name === 'main_content') {
    setTimeout(initFeaturedCarousel, 500);
  }
});
        
        // ==============================================================================
        // PREVENT BODY SCROLLING WHEN MODAL IS OPEN
        // ==============================================================================
        function preventBodyScroll() {
          document.body.style.overflow = 'hidden';
          document.body.style.position = 'fixed';
          document.body.style.width = '100%';
          document.body.style.height = '100%';
        }
        
        function allowBodyScroll() {
          document.body.style.overflow = '';
          document.body.style.position = '';
          document.body.style.width = '';
          document.body.style.height = '';
        }
        
        // Observe modal openings
const observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    if (mutation.type === 'childList') {
      const addModal = document.querySelector('.modal-overlay:has(.modal-box:not(.details-modal))');
      const detailsModal = document.querySelector('.modal-overlay:has(.details-modal)');
      const loginModal = document.querySelector('.modal-overlay:has(.login-modal)');
      const editModal = document.querySelector('.modal-overlay:has(.edit-modal)');
      const confirmModal = document.querySelector('.modal-overlay:has(.confirm-delete-modal)');
      const recDetailsModal = document.querySelector('.modal-overlay:has(.rec-details-modal)');
      const achievementModal = document.querySelector('.achievement-modal-overlay');
      const userMenuOverlay = document.querySelector('.user-menu-overlay');
      
      if (addModal || detailsModal || loginModal || editModal || confirmModal || recDetailsModal || achievementModal || userMenuOverlay) {
        preventBodyScroll();
      } else {
        allowBodyScroll();
      }
    }
  });
});

        // Start observing the body for modal changes
        observer.observe(document.body, { childList: true, subtree: true });
        
        // Initial check
        if (document.querySelector('.modal-overlay') || document.querySelector('.user-menu-overlay')) {
          preventBodyScroll();
        }
      });
      
      $(document).on('shiny:connected', function() {
        console.log('Shiny connected - setting up logo animation');
        setTimeout(function() {
          const flipContainer = document.querySelector('.logo-flip-container');
          
          if (flipContainer) {
            console.log('Found flip container via Shiny');
            
            let isFlipped = false;
            
            function flipLogo() {
              if (isFlipped) {
                flipContainer.style.transform = 'rotateY(0deg)';
              } else {
                flipContainer.style.transform = 'rotateY(180deg)';
              }
              isFlipped = !isFlipped;
            }
            
            flipLogo();
            setInterval(flipLogo, 3000);
          }
        }, 1500);
      });
      
      Shiny.addCustomMessageHandler('updateNavButtons', function(message) {
        const navButtons = document.querySelectorAll('.nav-btn');
        navButtons.forEach(btn => {
          btn.classList.remove('active');
        });
        
        const activeBtn = document.querySelector('.nav-btn[data-page=\"' + message.active + '\"]');
        if (activeBtn) {
          activeBtn.classList.add('active');
        }
      });
      
      // ==============================================================================
      // USER MENU FUNCTIONS
      // ==============================================================================
      
      function toggleUserMenu() {
        Shiny.setInputValue('toggle_user_menu', Math.random(), {priority: 'event'});
      }
      
      function closeUserMenu() {
        Shiny.setInputValue('close_user_menu', Math.random(), {priority: 'event'});
      }
      
      document.addEventListener('click', function(event) {
        const userMenuOverlay = document.querySelector('.user-menu-overlay');
        const navIcon = event.target.closest('.nav-icon');
        
        if (userMenuOverlay && !navIcon) {
          const userMenu = document.querySelector('.user-menu');
          const isClickInsideMenu = userMenu && userMenu.contains(event.target);
          
          if (!isClickInsideMenu) {
            closeUserMenu();
          }
        }
      });
      
      window.toggleUserMenu = toggleUserMenu;
      window.closeUserMenu = closeUserMenu;
      
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        window.scrollTo({ top: 0, behavior: 'smooth' });
      });
      
      // ==============================================================================
      // EDIT MODAL FUNCTIONS
      // ==============================================================================
      
      function openEditModal(itemId) {
        console.log('Opening edit modal for item ID:', itemId);
        Shiny.setInputValue('edit_item', itemId, {priority: 'event'});
      }
      
      // ==============================================================================
      // DELETE FUNCTION
      // ==============================================================================
      
      function openDeleteModal(itemId) {
        console.log('Opening delete confirmation modal for item ID:', itemId);
        Shiny.setInputValue('delete_item', itemId, {priority: 'event'});
      }
      
      function toggleDropdown(dropdownId, event) {
        event.stopPropagation();
        const dropdown = document.getElementById(dropdownId);
        const isActive = dropdown.classList.contains('active');
        
        document.querySelectorAll('.dropdown-menu').forEach(d => {
          d.classList.remove('active');
        });
        
        if (!isActive) {
          dropdown.classList.add('active');
        }
      }
      
      document.addEventListener('click', function(event) {
        const isDropdown = event.target.closest('.dropdown-menu');
        const isSortBtn = event.target.closest('.sort-btn');
        const isFilterBtn = event.target.closest('.filter-btn');
        
        if (!isDropdown && !isSortBtn && !isFilterBtn) {
          document.querySelectorAll('.dropdown-menu').forEach(d => {
            d.classList.remove('active');
          });
        }
      });
      
      let searchTimer;
      let lastSearchValue = '';
      
      function handleSearchInput(event) {
        const currentValue = event.target.value;
        clearTimeout(searchTimer);
        
        if (currentValue !== lastSearchValue) {
          searchTimer = setTimeout(function() {
            Shiny.setInputValue('search_input', currentValue);
            Shiny.setInputValue('search_trigger', Math.random());
            lastSearchValue = currentValue;
          }, 300);
        }
      }
      
      function handleSearchKeyPress(event) {
        if (event.key === 'Enter') {
          clearTimeout(searchTimer);
          Shiny.setInputValue('search_input', event.target.value);
          Shiny.setInputValue('search_trigger', Math.random());
          lastSearchValue = event.target.value;
        }
      }
      
      function clearSearchInput(inputId) {
        const input = document.getElementById(inputId);
        if (input) {
          input.value = '';
          Shiny.setInputValue('search_input', '');
          Shiny.setInputValue('search_trigger', Math.random());
          lastSearchValue = '';
        }
      }
      
      function setRating(rating, isEditModal = false) {
  const prefix = isEditModal ? 'edit_' : '';

  const stars = document.querySelectorAll(`.${isEditModal ? 'edit-modal' : 'modal-box:not(.details-modal)'} .star-rating-input .star`);
  
  stars.forEach((star, index) => {
    if (index < rating) {
      star.classList.add('filled');
    } else {
      star.classList.remove('filled');
    }
  });

  const ratingInput = document.getElementById(`${prefix}modal_rating_value`);
  if (ratingInput) {
    ratingInput.value = rating;
    Shiny.setInputValue(`${prefix}modal_rating_value`, rating);
  }
}
      
      function initStarRating(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const currentRating = parseInt(document.getElementById(`${prefix}modal_rating_value`).value) || 0;
        
        const stars = document.querySelectorAll(`#${prefix}modal_rating_value + .star-rating-input .star`);
        stars.forEach((star, index) => {
          if (index < currentRating) {
            star.classList.add('filled');
          } else {
            star.classList.remove('filled');
          }
          
          star.onclick = function() {
            setRating(index + 1, isEditModal);
          };
        });
      }
      
      function updateModalDisplay(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
        const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
        const modalTitle = document.querySelector(`.${isEditModal ? 'edit-modal' : 'modal-box'}:not(.details-modal) .modal-title`);
        const ratingField = document.querySelector(`.${isEditModal ? 'edit-modal' : 'modal-box'}:not(.details-modal) .rating-field`);
        const reviewField = document.querySelector(`.${isEditModal ? 'edit-modal' : 'modal-box'}:not(.details-modal) .review-field`);
        const durationField = document.querySelector(`.${isEditModal ? 'edit-modal' : 'modal-box'}:not(.details-modal) .duration-field`);
        const tvSeriesFields = document.querySelector(`.${isEditModal ? 'edit-modal' : 'modal-box'}:not(.details-modal) .tv-series-fields`);
        
        if (mediaTypeSelect && modalTitle) {
          const mediaType = mediaTypeSelect.textContent.trim();
          if (isEditModal) {
            modalTitle.textContent = 'Edit Item';
          } else {
            if (mediaType === 'Movie') {
              modalTitle.textContent = 'Add New Movie';
            } else if (mediaType === 'TV Series') {
              modalTitle.textContent = 'Add New TV Series';
            } else {
              modalTitle.textContent = 'Add New Movie/TV Series';
            }
          }
        }
        
        if (statusSelect && ratingField && reviewField) {
          const status = statusSelect.textContent.trim();
          if (status === 'Watched') {
            ratingField.style.display = 'block';
            reviewField.style.display = 'block';
          } else {
            ratingField.style.display = 'none';
            reviewField.style.display = 'none';
          }
        }
        
        if (mediaTypeSelect && durationField && tvSeriesFields) {
          const mediaType = mediaTypeSelect.textContent.trim();
          if (mediaType === 'Movie') {
            durationField.style.display = 'flex';
            tvSeriesFields.style.display = 'none';
          } else if (mediaType === 'TV Series') {
            durationField.style.display = 'none';
            tvSeriesFields.style.display = 'flex';
            updateTVSeriesFields(isEditModal);
          }
        }
      }
      
      function updateTVSeriesFields(isEditModal = false) {
  const prefix = isEditModal ? 'edit_' : '';
  const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
  const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
  
  if (!statusSelect || !mediaTypeSelect) return;
  
  const status = statusSelect.textContent.trim();
  const mediaType = mediaTypeSelect.textContent.trim();
  
  if (mediaType !== 'TV Series') return;
  
  const currentSeasonInput = document.getElementById(`${prefix}modal_current_season`);
  const currentEpisodeInput = document.getElementById(`${prefix}modal_current_episode`);
  const totalEpisodesWatchedInput = document.getElementById(`${prefix}modal_total_episodes_watched`);
  const totalSeasonsInput = document.getElementById(`${prefix}modal_total_seasons`);
  const episodesCurrentSeasonInput = document.getElementById(`${prefix}modal_episodes_current_season`);
  const totalEpisodesInput = document.getElementById(`${prefix}modal_total_episodes`);
  
  if (!currentSeasonInput || !currentEpisodeInput || !totalEpisodesWatchedInput || 
      !totalSeasonsInput || !episodesCurrentSeasonInput || !totalEpisodesInput) return;
  
  if (status === 'Unwatched') {
    // For Unwatched: set to 0 and disable
    currentSeasonInput.value = '0';
    currentSeasonInput.disabled = true;
    currentEpisodeInput.value = '0';
    currentEpisodeInput.disabled = true;
    totalEpisodesWatchedInput.value = '0';
    totalEpisodesWatchedInput.disabled = true;
    
    // Remove readonly
    currentSeasonInput.readOnly = false;
    currentEpisodeInput.readOnly = false;
    totalEpisodesWatchedInput.readOnly = false;
    
    // Reset opacity
    currentSeasonInput.style.opacity = '';
    currentEpisodeInput.style.opacity = '';
    totalEpisodesWatchedInput.style.opacity = '';
    
  } else if (status === 'Watching') {
    // For Watching: ENABLE all fields, NOT readonly
    currentSeasonInput.disabled = false;
    currentEpisodeInput.disabled = false;
    totalEpisodesWatchedInput.disabled = false;
    
    // REMOVE readonly
    currentSeasonInput.readOnly = false;
    currentEpisodeInput.readOnly = false;
    totalEpisodesWatchedInput.readOnly = false;
    
    // Reset opacity
    currentSeasonInput.style.opacity = '';
    currentEpisodeInput.style.opacity = '';
    totalEpisodesWatchedInput.style.opacity = '';
    
    // Keep existing values if they're valid, otherwise set defaults
    const currentSeasonVal = parseInt(currentSeasonInput.value) || 0;
    const currentEpisodeVal = parseInt(currentEpisodeInput.value) || 0;
    
    if (currentSeasonVal < 1) {
      currentSeasonInput.value = '1';
    }
    if (currentEpisodeVal < 1) {
      currentEpisodeInput.value = '1';
    }
    
    // Update total episodes watched
    updateTotalEpisodesWatched(isEditModal);
    
  } else if (status === 'Watched') {
    // For Watched: set to final values and make readonly (not disabled)
    const totalSeasons = parseInt(totalSeasonsInput.value) || 1;
    const episodesInFinalSeason = parseInt(episodesCurrentSeasonInput.value) || 1;
    const totalEps = parseInt(totalEpisodesInput.value) || 1;
    
    // Set the final season and episode values
    currentSeasonInput.value = totalSeasons;
    currentEpisodeInput.value = episodesInFinalSeason;
    totalEpisodesWatchedInput.value = totalEps;
    
    // ENABLE but set readonly for Watched status
    currentSeasonInput.disabled = false;
    currentEpisodeInput.disabled = false;
    totalEpisodesWatchedInput.disabled = false;
    
    currentSeasonInput.readOnly = true;
    currentEpisodeInput.readOnly = true;
    totalEpisodesWatchedInput.readOnly = true;
    
    // Add visual indication that fields are readonly
    currentSeasonInput.style.opacity = '0.6';
    currentEpisodeInput.style.opacity = '0.6';
    totalEpisodesWatchedInput.style.opacity = '0.6';
  }
  
  updateEpisodeIndicator(isEditModal);
}
      
      function updateEpisodeIndicator(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
        const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
        const titleInput = document.getElementById(`${prefix}modal_title`);
        
        if (!mediaTypeSelect || !statusSelect) return;
        
        const mediaType = mediaTypeSelect.textContent.trim();
        const status = statusSelect.textContent.trim();
        
        if (mediaType !== 'TV Series') return;
        
        const currentSeasonInput = document.getElementById(`${prefix}modal_current_season`);
        const currentEpisodeInput = document.getElementById(`${prefix}modal_current_episode`);
        const episodesCurrentSeasonInput = document.getElementById(`${prefix}modal_episodes_current_season`);
        const totalEpisodesInput = document.getElementById(`${prefix}modal_total_episodes`);
        const totalSeasonsInput = document.getElementById(`${prefix}modal_total_seasons`);
        
        if (!currentSeasonInput || !currentEpisodeInput || !episodesCurrentSeasonInput || !totalEpisodesInput || !totalSeasonsInput) return;
        
        const currentSeason = parseInt(currentSeasonInput.value) || 0;
        const currentEpisode = parseInt(currentEpisodeInput.value) || 0;
        const episodesInCurrentSeason = parseInt(episodesCurrentSeasonInput.value) || 0;
        const totalEpisodes = parseInt(totalEpisodesInput.value) || 0;
        const totalSeasons = parseInt(totalSeasonsInput.value) || 0;
        const title = titleInput.value || 'this series';
        
        const indicator = document.getElementById(`${prefix}episode-indicator`);
        if (indicator) {
          indicator.setAttribute('data-status', status.toLowerCase());
          indicator.innerHTML = '';
          
          if (status === 'Unwatched') {
            indicator.innerHTML = '<span class=\"status-icon\"></span> You haven\\'t watched <strong class=\"title-emphasis\">' + title + '</strong> yet.';
            indicator.style.display = 'block';
          } else if (status === 'Watched') {
            indicator.innerHTML = '<span class=\"status-icon\"></span> You\\'ve completed <strong class=\"title-emphasis\">' + title + '</strong>.';
            indicator.style.display = 'block';
          } else if (status === 'Watching') {
            if (currentSeason > 0 && currentEpisode > 0 && episodesInCurrentSeason > 0) {
              indicator.innerHTML = '<span class=\"status-icon\"></span> Currently watching <strong>Season ' + currentSeason + ', Episode ' + currentEpisode + '</strong> of <strong class=\"title-emphasis\">' + title + '</strong>.';
              indicator.style.display = 'block';
            } else {
              indicator.innerHTML = '<span class=\"status-icon\"></span> Currently watching <strong class=\"title-emphasis\">' + title + '</strong>.';
              indicator.style.display = 'block';
            }
          }
        }
      }
      
function updateMovieDurationFields(isEditModal = false) {
  const prefix = isEditModal ? 'edit_' : '';
  const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
  const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
  
  if (!mediaTypeSelect || !statusSelect) return;
  
  const mediaType = mediaTypeSelect.textContent.trim();
  const status = statusSelect.textContent.trim();
  
  if (mediaType !== 'Movie') return;
  
  const totalDurationInput = document.getElementById(`${prefix}modal_total_duration`);
  const watchedDurationInput = document.getElementById(`${prefix}modal_watched_duration`);
  
  if (!totalDurationInput || !watchedDurationInput) return;
  
  if (status === 'Unwatched') {
    watchedDurationInput.value = '0';
    watchedDurationInput.disabled = true;
    watchedDurationInput.readOnly = false;
    watchedDurationInput.style.opacity = '';
  } else if (status === 'Watching') {
    watchedDurationInput.disabled = false;
    watchedDurationInput.readOnly = false;
    watchedDurationInput.style.opacity = '';
  } else if (status === 'Watched') {
    watchedDurationInput.value = totalDurationInput.value || '0';
    watchedDurationInput.disabled = false;
    watchedDurationInput.readOnly = true;
    watchedDurationInput.style.opacity = '0.6';
  }
}
      
      function syncMovieDuration(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
        const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
        
        if (!mediaTypeSelect || !statusSelect) return;
        
        const mediaType = mediaTypeSelect.textContent.trim();
        const status = statusSelect.textContent.trim();
        
        if (mediaType !== 'Movie' || status !== 'Watched') return;
        
        const totalDurationInput = document.getElementById(`${prefix}modal_total_duration`);
        const watchedDurationInput = document.getElementById(`${prefix}modal_watched_duration`);
        
        if (totalDurationInput && watchedDurationInput) {
          watchedDurationInput.value = totalDurationInput.value;
        }
      }
      
      function fetchSeasonEpisodes(tmdbId, seasonNumber, isEditModal = false) {
        if (!tmdbId || !seasonNumber || seasonNumber <= 0) return;
        
        Shiny.setInputValue(isEditModal ? 'edit_fetch_season_episodes' : 'fetch_season_episodes', JSON.stringify({
          tmdb_id: tmdbId,
          season_number: seasonNumber
        }), {priority: 'event'});
      }
      
      // Event handlers for add modal
      $(document).on('change', '#modal_media_type, #modal_status', function() {
        setTimeout(function() {
          updateModalDisplay(false);
          updateTVSeriesFields(false);
          updateMovieDurationFields(false);
          updateEpisodeIndicator(false);
        }, 100);
      });
      
      $(document).on('input', '#modal_current_season, #modal_current_episode', function() {
        updateEpisodeIndicator(false);
        updateTotalEpisodesWatched(false);
      });
      
      $(document).on('input', '#modal_title', function() {
        updateEpisodeIndicator(false);
      });
      
      $(document).on('input', '#modal_total_duration', function() {
        syncMovieDuration(false);
      });
      
      // Event handlers for edit modal
      $(document).on('change', '#edit_modal_media_type, #edit_modal_status', function() {
        setTimeout(function() {
          updateModalDisplay(true);
          updateTVSeriesFields(true);
          updateMovieDurationFields(true);
          updateEpisodeIndicator(true);
        }, 100);
      });
      
      $(document).on('input', '#edit_modal_current_season, #edit_modal_current_episode', function() {
        updateEpisodeIndicator(true);
        updateTotalEpisodesWatched(true);
      });
      
      $(document).on('input', '#edit_modal_title', function() {
        updateEpisodeIndicator(true);
      });
      
      $(document).on('input', '#edit_modal_total_duration', function() {
        syncMovieDuration(true);
      });
      
      function updateTotalEpisodesWatched(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
        const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
        
        if (!mediaTypeSelect || !statusSelect) return;
        
        const mediaType = mediaTypeSelect.textContent.trim();
        const status = statusSelect.textContent.trim();
        
        if (mediaType !== 'TV Series' || status !== 'Watching') return;
        
        const currentSeasonInput = document.getElementById(`${prefix}modal_current_season`);
        const currentEpisodeInput = document.getElementById(`${prefix}modal_current_episode`);
        const episodesCurrentSeasonInput = document.getElementById(`${prefix}modal_episodes_current_season`);
        const totalSeasonsInput = document.getElementById(`${prefix}modal_total_seasons`);
        const totalEpisodesInput = document.getElementById(`${prefix}modal_total_episodes`);
        const totalEpisodesWatchedInput = document.getElementById(`${prefix}modal_total_episodes_watched`);
        
        if (!currentSeasonInput || !currentEpisodeInput || !episodesCurrentSeasonInput || 
            !totalSeasonsInput || !totalEpisodesInput || !totalEpisodesWatchedInput) return;
        
        const currentSeason = parseInt(currentSeasonInput.value) || 1;
        const currentEpisode = parseInt(currentEpisodeInput.value) || 1;
        const episodesPerSeason = parseInt(episodesCurrentSeasonInput.value) || 10;
        const totalSeasons = parseInt(totalSeasonsInput.value) || 1;
        const totalEpisodes = parseInt(totalEpisodesInput.value) || episodesPerSeason * totalSeasons;
        
        let totalWatched = 0;
        if (currentSeason > 1) {
          totalWatched = (currentSeason - 1) * episodesPerSeason;
        }
        totalWatched += currentEpisode;
        
        totalWatched = Math.min(totalWatched, totalEpisodes);
        
        totalEpisodesWatchedInput.value = totalWatched;
      }
      
      $(document).on('shiny:value', function(event) {
        if (event.name === 'add_modal') {
          setTimeout(function() {
            updateModalDisplay(false);
            updateTVSeriesFields(false);
            updateMovieDurationFields(false);
            updateEpisodeIndicator(false);
            const currentSeasonInput = document.getElementById('modal_current_season');
            if (currentSeasonInput) {
              currentSeasonInput.disabled = false;
            }
          }, 500);
        } else if (event.name === 'edit_modal') {
          setTimeout(function() {
            updateModalDisplay(true);
            updateTVSeriesFields(true);
            updateMovieDurationFields(true);
            updateEpisodeIndicator(true);
            const currentSeasonInput = document.getElementById('edit_modal_current_season');
            if (currentSeasonInput) {
              currentSeasonInput.disabled = false;
            }
          }, 500);
        }
      });
      
      let tmdbSearchTimer;
      let lastTmdbSearch = '';
      let currentSearchType = 'add'; // 'add' or 'edit'
      
      function handleTitleSearch(event, isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        currentSearchType = isEditModal ? 'edit' : 'add';
        const value = event.target.value.trim();
        
        clearTimeout(tmdbSearchTimer);
        
        if (value === '') {
          hideTmdbSuggestions(isEditModal);
          lastTmdbSearch = '';
          return;
        }
        
        if (value !== lastTmdbSearch && value.length >= 2) {
          showTmdbLoading(isEditModal);
          
          tmdbSearchTimer = setTimeout(function() {
            Shiny.setInputValue(isEditModal ? 'edit_tmdb_search' : 'tmdb_search', value, {priority: 'event'});
            lastTmdbSearch = value;
          }, 500);
        } else if (value.length < 2) {
          hideTmdbSuggestions(isEditModal);
        }
      }
      
      function showTmdbLoading(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const container = document.getElementById(`${prefix}tmdb-suggestions`);
        if (container) {
          container.innerHTML = '<div class=\"suggestion-loading\"><i class=\"fas fa-spinner fa-spin\"></i> Searching TMDB...</div>';
          container.classList.add('active');
          container.style.opacity = '1';
        }
      }
      
      function hideTmdbSuggestions(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const container = document.getElementById(`${prefix}tmdb-suggestions`);
        if (container) {
          container.style.opacity = '0';
          setTimeout(function() {
            container.classList.remove('active');
            container.innerHTML = '';
          }, 300);
        }
      }
      
      function selectTmdbSuggestion(tmdbId, mediaType, title, isEditModal = false) {
        console.log('Selecting TMDB suggestion:', tmdbId, mediaType, title, 'isEditModal:', isEditModal);
        
        const prefix = isEditModal ? 'edit_' : '';
        const titleInput = document.getElementById(`${prefix}modal_title`);
        if (titleInput) {
          titleInput.value = title;
        }
        
        hideTmdbSuggestions(isEditModal);
        
        Shiny.setInputValue(isEditModal ? 'edit_tmdb_fetch' : 'tmdb_fetch', JSON.stringify({
          id: tmdbId,
          media_type: mediaType,
          title: title
        }), {priority: 'event'});
      }
      
      document.addEventListener('click', function(event) {

        const isSuggestion = event.target.closest('.tmdb-suggestion');
        const isTitleInput = event.target.id === 'modal_title' || 
                            event.target.id === 'edit_modal_title' ||
                            event.target.closest('#modal_title') ||
                            event.target.closest('#edit_modal_title');
        const isSuggestionContainer = event.target.id === 'tmdb-suggestions' || 
                                     event.target.id === 'edit_tmdb-suggestions' ||
                                     event.target.closest('#tmdb-suggestions') ||
                                     event.target.closest('#edit_tmdb-suggestions');
        
        if (!isSuggestion && !isTitleInput && !isSuggestionContainer) {

          hideTmdbSuggestions(false);
          hideTmdbSuggestions(true);
        }
      });
      
      Shiny.addCustomMessageHandler('tmdb_suggestions', function(message) {
        console.log('Received TMDB suggestions for:', message.type, 'HTML present:', !!message.html);
        const prefix = message.type === 'edit' ? 'edit_' : '';
        const container = document.getElementById(`${prefix}tmdb-suggestions`);
        if (container) {
          if (message.html) {
            container.innerHTML = message.html;
            container.classList.add('active');
            container.style.opacity = '0';
            setTimeout(function() {
              container.style.opacity = '1';
            }, 10);
          } else {
            container.innerHTML = '<div class=\"suggestion-empty\">No results found in TMDB. Try a different search term.</div>';
            container.classList.add('active');
          }
        }
      });
      
      function setupMovieCardClicks() {
        document.addEventListener('click', function(event) {
          const movieCard = event.target.closest('.movie-card');
          if (movieCard && !event.target.closest('.movie-actions') && !event.target.classList.contains('star')) {
            const itemId = movieCard.getAttribute('data-item-id');
            if (itemId) {
              Shiny.setInputValue('show_item_details', itemId, {priority: 'event'});
            }
          }
        });
      }
      
      function setupRecCardClicks() {
        document.addEventListener('click', function(event) {
          const recCard = event.target.closest('.rec-card');
          if (recCard && !event.target.closest('.movie-actions') && !event.target.classList.contains('star')) {
            const tmdbId = recCard.getAttribute('data-tmdb-id');
            const mediaType = recCard.getAttribute('data-media-type');
            const title = recCard.getAttribute('data-title');
            
            if (tmdbId && mediaType && title) {
              console.log('Rec card clicked:', tmdbId, mediaType, title);
              Shiny.setInputValue('show_rec_details', JSON.stringify({
                tmdb_id: tmdbId,
                media_type: mediaType,
                title: title
              }), {priority: 'event'});
            }
          }
        });
      }
      
      setTimeout(function() {
        setupMovieCardClicks();
        setupRecCardClicks();
      }, 1000);
      
      $(document).on('shiny:value', function(event) {
        if (event.name === 'main_content' || event.name === 'library_items') {
          setTimeout(setupMovieCardClicks, 500);
        }
        if (event.name === 'main_content') {
          setTimeout(setupRecCardClicks, 500);
        }
      });
      
      function calculateAndSendTotalEpisodesWatched(isEditModal = false) {
        const prefix = isEditModal ? 'edit_' : '';
        const mediaTypeSelect = document.querySelector(`#${prefix}modal_media_type + .selectize-control .selectize-input .item`);
        const statusSelect = document.querySelector(`#${prefix}modal_status + .selectize-control .selectize-input .item`);
        
        if (!mediaTypeSelect || !statusSelect) return 0;
        
        const mediaType = mediaTypeSelect.textContent.trim();
        const status = statusSelect.textContent.trim();
        
        if (mediaType !== 'TV Series') return 0;
        
        const currentSeasonInput = document.getElementById(`${prefix}modal_current_season`);
        const currentEpisodeInput = document.getElementById(`${prefix}modal_current_episode`);
        const episodesCurrentSeasonInput = document.getElementById(`${prefix}modal_episodes_current_season`);
        const totalEpisodesInput = document.getElementById(`${prefix}modal_total_episodes`);
        const totalSeasonsInput = document.getElementById(`${prefix}modal_total_seasons`);
        
        if (!currentSeasonInput || !currentEpisodeInput || !episodesCurrentSeasonInput || !totalEpisodesInput || !totalSeasonsInput) return 0;
        
        const currentSeason = parseInt(currentSeasonInput.value) || 0;
        const currentEpisode = parseInt(currentEpisodeInput.value) || 0;
        const episodesPerSeason = parseInt(episodesCurrentSeasonInput.value) || 10;
        const totalEpisodes = parseInt(totalEpisodesInput.value) || episodesPerSeason;
        const totalSeasons = parseInt(totalSeasonsInput.value) || 1;
        
        let totalWatched = 0;
        
        if (status === 'Unwatched') {
          totalWatched = 0;
        } else if (status === 'Watched') {
          totalWatched = totalEpisodes;
        } else if (status === 'Watching') {
          if (currentSeason > 1) {
            totalWatched = (currentSeason - 1) * episodesPerSeason;
          }
          totalWatched += currentEpisode;
          totalWatched = Math.min(totalWatched, totalEpisodes);
        }
        
        console.log('Calculated total episodes watched:', totalWatched, 
                    'Status:', status, 
                    'Current Season:', currentSeason, 
                    'Current Episode:', currentEpisode,
                    'Total Episodes:', totalEpisodes);
        
        return totalWatched;
      }
      
      // Handle add modal submission
      $(document).on('click', '.modal-box:not(.edit-modal) .btn-primary', function(e) {
        const modal = this.closest('.modal-box');
        if (!modal.classList.contains('edit-modal')) {
          const mediaTypeSelect = document.querySelector('#modal_media_type + .selectize-control .selectize-input .item');
          
          if (mediaTypeSelect && mediaTypeSelect.textContent.trim() === 'TV Series') {
            console.log('TV Series detected in add modal, calculating total episodes watched before submission...');
            
            const calculatedValue = calculateAndSendTotalEpisodesWatched(false);
            
            Shiny.setInputValue('calculated_total_episodes_watched', calculatedValue, {priority: 'event'});
            
            console.log('Sent calculated_total_episodes_watched to Shiny:', calculatedValue);
            
            setTimeout(function() {
              console.log('Triggering submit_movie...');
              Shiny.setInputValue('submit_movie', Math.random());
            }, 100);
            
            e.preventDefault();
            e.stopPropagation();
            return false;
          } else {
            console.log('Movie detected in add modal, proceeding with normal submission...');
            Shiny.setInputValue('submit_movie', Math.random());
          }
        }
      });
      
      // Handle edit modal submission
      $(document).on('click', '.edit-modal .btn-primary', function(e) {
        console.log('Edit modal save button clicked');
        const mediaTypeSelect = document.querySelector('#edit_modal_media_type + .selectize-control .selectize-input .item');
        
        if (mediaTypeSelect && mediaTypeSelect.textContent.trim() === 'TV Series') {
          console.log('TV Series detected in edit modal, calculating total episodes watched before submission...');
          
          const calculatedValue = calculateAndSendTotalEpisodesWatched(true);
          
          Shiny.setInputValue('edit_calculated_total_episodes_watched', calculatedValue, {priority: 'event'});
          
          console.log('Sent edit_calculated_total_episodes_watched to Shiny:', calculatedValue);
          
          setTimeout(function() {
            console.log('Triggering update_movie...');
            Shiny.setInputValue('update_movie', Math.random());
          }, 100);
          
          e.preventDefault();
          e.stopPropagation();
          return false;
        } else {
          console.log('Movie detected in edit modal, proceeding with normal submission...');
          Shiny.setInputValue('update_movie', Math.random());
        }
      });
      
      function togglePasswordVisibility(inputId) {
        const input = document.getElementById(inputId);
        const icon = document.getElementById('password-icon');
        
        if (!input || !icon) return;
        
        if (input.type === 'password') {
          input.type = 'text';
          icon.classList.remove('fa-eye');
          icon.classList.add('fa-eye-slash');
        } else {
          input.type = 'password';
          icon.classList.remove('fa-eye-slash');
          icon.classList.add('fa-eye');
        }
      }

      document.addEventListener('keydown', function(event) {
        const loginModal = document.querySelector('.login-modal');
        if (loginModal && event.key === 'Enter' && 
            (document.getElementById('login_email') || document.getElementById('login_password'))) {

          const loginBtn = document.getElementById('login_submit_btn');
          if (loginBtn) {
            event.preventDefault();
            event.stopPropagation();
            loginBtn.click();
          }
        }
      });

      $(document).on('click', '.login-modal .btn-primary', function(e) {
        e.stopPropagation();
        return true;
      });
      
      window.toggleDropdown = toggleDropdown;
      window.handleSearchInput = handleSearchInput;
      window.handleSearchKeyPress = handleSearchKeyPress;
      window.clearSearchInput = clearSearchInput;
      window.setRating = setRating;
      window.initStarRating = initStarRating;
      window.updateModalDisplay = updateModalDisplay;
      window.updateTVSeriesFields = updateTVSeriesFields;
      window.updateMovieDurationFields = updateMovieDurationFields;
      window.updateEpisodeIndicator = updateEpisodeIndicator;
      window.syncMovieDuration = syncMovieDuration;
      window.handleTitleSearch = handleTitleSearch;
      window.selectTmdbSuggestion = selectTmdbSuggestion;
      window.hideTmdbSuggestions = hideTmdbSuggestions;
      window.updateTotalEpisodesWatched = updateTotalEpisodesWatched;
      window.togglePasswordVisibility = togglePasswordVisibility;
      window.showWelcomingNotification = showWelcomingNotification;
      window.closeWelcomingToast = closeWelcomingToast;
      window.openEditModal = openEditModal;
      window.openDeleteModal = openDeleteModal;
    "))
  ),
  useShinyjs(),

  uiOutput("app_ui")
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  observe({
    db_url <- Sys.getenv("DATABASE_URL")
    message("=== DEBUG ===")
    message("DATABASE_URL exists: ", db_url != "")
    message("DATABASE_URL value: ", ifelse(db_url != "", substr(db_url, 1, 30), "EMPTY"))
    message("=== END DEBUG ===")
  })
  
  rv <- reactiveValues(
    # Authentication state
    authenticated = FALSE,
    current_user = NULL,
    show_login_modal = FALSE,
    show_user_menu = FALSE,

    page = "home",
    show_modal = FALSE,
    show_details_modal = FALSE,
    refresh = 0,
    rec_type = "movie",
    stats_media_type = "all",
    library_view = "all",
    modal_media_type = "Movie",
    modal_status = "Unwatched",
    rec_page = 1,
    rec_total_pages = 5,
    rec_movies_clicked = 0,
    rec_series_clicked = 0,
    library_page = 1,
    library_items_per_page = 15,
    search_query = "",
    sort_by = "date_added",
    sort_order = "desc",
    filter_status = "all",
    filter_genre = "all",
    filter_rating = "all",
    filter_year_min = 1900,
    filter_year_max = 2025,
    tmdb_results = NULL,
    tmdb_loading = FALSE,
    current_tmdb_id = NULL,
    tv_current_season = 1,
    tv_current_episode = 1,
    is_fetching_season = FALSE,
    selected_item = NULL,
    calculated_total_episodes_watched = 0,
    
    # Edit modal reactive values
    show_edit_modal = FALSE,
    editing_item = NULL,
    edit_tmdb_results = NULL,
    edit_current_tmdb_id = NULL,
    edit_tv_current_season = 1,
    edit_tv_current_episode = 1,
    edit_is_fetching_season = FALSE,
    edit_calculated_total_episodes_watched = 0,
    
    # Delete confirmation modal reactive values
    show_delete_modal = FALSE,
    deleting_item = NULL,
    
    # Recommendation details modal reactive values
    show_rec_details_modal = FALSE,
    rec_details = NULL,
    modal_source = "manual",
    rec_details_loading = FALSE,
    
    # Featured section state
    featured_index = 0,
    
    achievement_to_show = NULL,
    collection_data = NULL,
    
    # Achievement reactive values
    achievements_unlocked = list(),
    badge_progress = list(
      first_watch = FALSE,
      media_mogul = FALSE,
      marathon_master = FALSE,
      critic_training = FALSE,
      harsh_critic = FALSE,
      genre_sampler = FALSE,
      time_traveler = FALSE,
      classic_connoisseur = FALSE,
      streak_keeper = FALSE,
      plotwist_badge = FALSE,
      essayist = FALSE,
      haiku_reviewer = FALSE,
      actor_apprentice = FALSE,
      director_devotee = FALSE,
      romance_royalty = FALSE,
      mad_scientist = FALSE,
      perfectionist = FALSE,
      numerical = FALSE,
      developers_cut = FALSE,
      developers_season = FALSE,
      plotwist_special = FALSE
    ),
    current_streak = 0,
    last_watch_date = NULL
  )
  
  # Toggle user menu
  observeEvent(input$toggle_user_menu, {
    rv$show_user_menu <- !rv$show_user_menu
  })
  
  # Close user menu
  observeEvent(input$close_user_menu, {
    rv$show_user_menu <- FALSE
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    # Reset all authentication state
    rv$authenticated <- FALSE
    rv$current_user <- NULL
    rv$show_user_menu <- FALSE
    rv$show_modal <- FALSE
    rv$show_details_modal <- FALSE
    rv$show_edit_modal <- FALSE
    rv$show_delete_modal <- FALSE
    rv$show_rec_details_modal <- FALSE
    
    # Reset page state
    rv$page <- "home"
    rv$refresh <- 0
    
    # Show notification
    showNotification("Logged out successfully!", type = "message", duration = 3)
    
    # Reload session to clear everything
    session$reload()
  })
  
  # Show login modal when button clicked
  observeEvent(input$show_login_btn, {
    rv$show_login_modal <- TRUE
  })
  
  # Close login modal
  observeEvent(input$close_login_modal, {
    rv$show_login_modal <- FALSE
  })
  
  # Handle login
  observeEvent(input$login_submit, {
    email <- input$login_email
    password <- input$login_password
    
    if (is.null(email) || email == "" || is.null(password) || password == "") {
      showNotification("Please enter both email and password", type = "error", duration = 3)
      return()
    }
    
    user <- authenticate_user(email, password)
    
    if (!is.null(user)) {
      rv$authenticated <- TRUE
      rv$current_user <- user
      rv$show_login_modal <- FALSE

      observe({
        isolate({
          delay(500, {
            updateTextInput(session, "modal_title", value = "")
            updateSelectizeInput(session, "modal_media_type", selected = "Movie")
            updateSelectizeInput(session, "modal_status", selected = "Unwatched")
            updateSelectizeInput(session, "modal_genre", selected = character(0))
            updateNumericInput(session, "modal_year", value = year(Sys.Date()))
            updateTextInput(session, "modal_director", value = "")
            updateTextAreaInput(session, "modal_cast", value = "")
            updateTextAreaInput(session, "modal_plot_summary", value = "")
            updateTextInput(session, "modal_poster", value = "")
            updateTextAreaInput(session, "modal_review", value = "")
            updateNumericInput(session, "modal_total_duration", value = 0)
            updateNumericInput(session, "modal_watched_duration", value = 0)
            updateNumericInput(session, "modal_total_seasons", value = 1)
            updateNumericInput(session, "modal_current_season", value = 0)
            updateNumericInput(session, "modal_episodes_current_season", value = 1)
            updateNumericInput(session, "modal_current_episode", value = 0)
            updateNumericInput(session, "modal_total_episodes", value = 1)
            updateNumericInput(session, "modal_total_episodes_watched", value = 0)
          })
        })
      })
    } else {
      showNotification("Invalid email or password. Please try again.", type = "error", duration = 5)
    }
  })
  
  # Conditional UI
  output$app_ui <- renderUI({
    if (!rv$authenticated) {
      # Show landing page
      render_landing_page()
    } else {
      # Show main app
      tagList(
        # ==============================================================================
        # NAVIGATION
        # ==============================================================================
        tags$div(class = "modern-nav no-scroll",
                 tags$div(class = "nav-brand",
                          tags$img(src = "logo.png", 
                                   alt = "PLOTWIST Logo",
                                   style = "height: 40px; width: auto;"),
                 ),
                 tags$div(class = "nav-menu",
                          actionLink("nav_home", "Home", class = "nav-btn", `data-page` = "home"),
                          actionLink("nav_stats", "Stats", class = "nav-btn", `data-page` = "stats"),
                          actionLink("nav_library", "Library", class = "nav-btn", `data-page` = "library"),
                          actionLink("nav_recommendations", "Recommendations", class = "nav-btn", `data-page` = "recommendations"),
                          actionLink("nav_achievements", "Achievements", class = "nav-btn", `data-page` = "achievements")
                 ),
                 tags$div(class = "nav-icons",
                          tags$a(href = "#", class = "nav-icon",
                                 onclick = "toggleUserMenu();",
                                 tags$i(class = "fas fa-user")
                          )
                 )
        ),
        
        # Main Content
        uiOutput("main_content"),
        
        # FAB
        uiOutput("fab_button"),
        
        # Add Modal
        uiOutput("add_modal"),
        
        # Details Modal
        uiOutput("details_modal"),
        
        # Edit Modal
        uiOutput("edit_modal"),
        
        # Delete Confirmation Modal
        uiOutput("delete_modal"),
        
        # Recommendation Details Modal
        uiOutput("rec_details_modal"),
        
        # Achievement Modal
        uiOutput("achievement_modal"),
        
        # User Menu
        uiOutput("user_menu")
      )
    }
  })

  output$user_menu <- renderUI({
    if (!rv$authenticated || !rv$show_user_menu) return(NULL)
    
    user_name <- if (!is.null(rv$current_user)) {
      rv$current_user$name[1]
    } else {
      "User"
    }
    
    tags$div(class = "user-menu-overlay",
             tags$div(class = "user-menu",
                      tags$div(class = "user-menu-header",
                               tags$div(class = "user-menu-greeting", "Hello,"),
                               tags$h2(class = "user-menu-name", user_name)
                      ),
                      tags$div(class = "user-menu-items",
                               tags$a(class = "user-menu-item logout",
                                      onclick = "Shiny.setInputValue('logout_btn', Math.random()); closeUserMenu();",
                                      tags$i(class = "fas fa-sign-out-alt"),
                                      tags$span("Logout")
                               )
                      )
             )
    )
  })

  render_landing_page <- function() {
    tagList(

      tags$div(class = "hero",
               
               tags$div(class = "logo-animation-container",
                        tags$div(class = "logo-flip-container",
                                 tags$div(class = "logo-front",
                                          tags$img(src = "logo_icon.png", 
                                                   class = "logo-original",
                                                   alt = "PLOTWIST Icon")
                                 ),
                                 tags$div(class = "logo-back",
                                          tags$img(src = "logo_text.png", 
                                                   class = "logo-original",
                                                   alt = "PLOTWIST")
                                 )
                        )
               ),
               
               # Tagline
               tags$p(class = "hero-subtitle", "Turn Your Watching Into a Plotwist."),
               
               # Main headline
               tags$h1(class = "hero-title", 
                       "Do you need ",
                       tags$span(class = "gradient-word", "recommendations"),
                       " or do you just need someone to tell you what to do?"
               ),
               
               # Login button
               tags$button(class = "hero-button", 
                           onclick = "Shiny.setInputValue('show_login_btn', Math.random());",
                           "Login")
      ),
      
      # Login Modal
      if (rv$show_login_modal) {
        tags$div(class = "modal-overlay",
                 tags$div(class = "modal-box login-modal",
                          tags$div(class = "modal-header",
                                   tags$h2(class = "modal-title gradient-title", "Welcome Back"),
                                   tags$div(class = "modal-close-container",
                                            tags$button(class = "modal-close", "×", 
                                                        onclick = "Shiny.setInputValue('close_login_modal', Math.random());")
                                   )
                          ),
                          tags$div(class = "modal-body",
                                   # Email field
                                   tags$div(class = "form-group full-width",
                                            tags$label(class = "form-label", "Email ", 
                                                       tags$span(class = "required", "*")),
                                            textInput("login_email", NULL, 
                                                      placeholder = "Enter your email",
                                                      width = "100%")
                                   ),
                                   
                                   # Password field
                                   tags$div(class = "form-group full-width",
                                            tags$label(class = "form-label", "Password ", 
                                                       tags$span(class = "required", "*")),
                                            tags$div(class = "password-input-wrapper",
                                                     passwordInput("login_password", NULL, 
                                                                   placeholder = "Enter your password",
                                                                   width = "100%"),
                                                     tags$span(class = "password-toggle-icon",
                                                               onclick = "togglePasswordVisibility('login_password');",
                                                               tags$i(class = "fas fa-eye", id = "password-icon"))
                                            )
                                   ),
                                   
                                   # Login button
                                   tags$button(class = "btn-primary", 
                                               id = "login_submit_btn",  # Added ID
                                               onclick = "Shiny.setInputValue('login_submit', Math.random());",
                                               "Login")
                          )
                 )
        )
      }
    )
  }
  
  # ==============================================================================
  # ACHIEVEMENT NOTIFICATION
  # ==============================================================================
  
  # Helper function to check if notification was already shown
  has_shown_notification <- function(achievement_id) {
    if (is.null(rv$current_user)) return(TRUE)
    
    con <- get_db_connection()
    if (is.null(con)) return(FALSE)
    
    tryCatch({
      query <- sprintf("SELECT COUNT(*) as count FROM achievement_notifications WHERE user_id = %d AND achievement_id = '%s'",
                       rv$current_user$id[1], achievement_id)
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      return(result$count[1] > 0)
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      return(FALSE)
    })
  }
  
  # Helper function to mark notification as shown
  mark_notification_shown <- function(achievement_id) {
    if (is.null(rv$current_user)) return()
    
    con <- get_db_connection()
    if (is.null(con)) return()
    
    tryCatch({
      query <- sprintf("INSERT INTO achievement_notifications (user_id, achievement_id) VALUES (%d, '%s') ON CONFLICT DO NOTHING",
                       rv$current_user$id[1], achievement_id)
      dbExecute(con, query)
      dbDisconnect(con)
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
    })
  }
  
  # ==============================================================================
  # ACHIEVEMENT CHECKING FUNCTION
  # ==============================================================================
  
  check_achievements <- function() {
    items <- get_all_items()
    if (nrow(items) == 0) return()
    
    newly_unlocked <- list()
    
    # 1. First Watch
    if (!rv$badge_progress$first_watch && nrow(items) >= 1) {
      rv$badge_progress$first_watch <- TRUE
      if (!"first_watch" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "first_watch")
        if (!has_shown_notification("first_watch")) {
          newly_unlocked[["first_watch"]] <- list(
            name = "First Watch",
            description = "Added your first item to your library",
            badge_image = "badges/first_watch.png"
          )
          mark_notification_shown("first_watch")
        }
      }
    }
    
    # 2. Media Mogul
    check_media_mogul <- function() {
      items <- get_all_items()
      return(nrow(items) >= 100)
    }
    
    if (!rv$badge_progress$media_mogul) {
      rv$badge_progress$media_mogul <- check_media_mogul()
      if (rv$badge_progress$media_mogul && !"media_mogul" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "media_mogul")
        if (!has_shown_notification("media_mogul")) {
          newly_unlocked[["media_mogul"]] <- list(
            name = "Media Mogul",
            description = "Added 100 items to your library",
            badge_image = "badges/media_mogul.png"
          )
          mark_notification_shown("media_mogul")
        }
      }
    }
    
    # 3. Marathon Master
    marathon_items <- items[items$media_type == "TV Series" & 
                              items$watch_status == "Watched" & 
                              !is.na(items$total_episodes) & 
                              items$total_episodes >= 50, ]
    if (!rv$badge_progress$marathon_master && nrow(marathon_items) > 0) {
      rv$badge_progress$marathon_master <- TRUE
      if (!"marathon_master" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "marathon_master")
        if (!has_shown_notification("marathon_master")) {
          newly_unlocked[["marathon_master"]] <- list(
            name = "Marathon Master",
            description = "Completed a TV series with 50+ episodes",
            badge_image = "badges/marathon_master.png"
          )
          mark_notification_shown("marathon_master")
        }
      }
    }
    
    # 4. Critic in Training
    rated_items <- items[items$rating > 0 & !is.na(items$rating), ]
    if (!rv$badge_progress$critic_training && nrow(rated_items) >= 10) {
      rv$badge_progress$critic_training <- TRUE
      if (!"critic_training" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "critic_training")
        if (!has_shown_notification("critic_training")) {
          newly_unlocked[["critic_training"]] <- list(
            name = "Critic in Training",
            description = "Rated 10 different items",
            badge_image = "badges/critic_training.png"
          )
          mark_notification_shown("critic_training")
        }
      }
    }
    
    # 5. Harsh Critic
    one_star_items <- items[items$rating == 1 & !is.na(items$rating), ]
    if (!rv$badge_progress$harsh_critic && nrow(one_star_items) > 0) {
      rv$badge_progress$harsh_critic <- TRUE
      if (!"harsh_critic" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "harsh_critic")
        if (!has_shown_notification("harsh_critic")) {
          newly_unlocked[["harsh_critic"]] <- list(
            name = "Harsh Critic",
            description = "Gave a 1-star rating to an item",
            badge_image = "badges/harsh_critic.png"
          )
          mark_notification_shown("harsh_critic")
        }
      }
    }
    
    # 6. Genre Sampler
    all_genres <- unlist(strsplit(items$genre, ", "))
    unique_genres <- unique(trimws(all_genres))
    if (!rv$badge_progress$genre_sampler && length(unique_genres) >= 5) {
      rv$badge_progress$genre_sampler <- TRUE
      if (!"genre_sampler" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "genre_sampler")
        if (!has_shown_notification("genre_sampler")) {
          newly_unlocked[["genre_sampler"]] <- list(
            name = "Genre Sampler",
            description = "Watched items from 5+ different genres",
            badge_image = "badges/genre_sampler.png"
          )
          mark_notification_shown("genre_sampler")
        }
      }
    }
    
    # 7. Time Traveler
    decades <- floor(items$year / 10) * 10
    unique_decades <- unique(decades)
    if (!rv$badge_progress$time_traveler && length(unique_decades) >= 5) {
      rv$badge_progress$time_traveler <- TRUE
      if (!"time_traveler" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "time_traveler")
        if (!has_shown_notification("time_traveler")) {
          newly_unlocked[["time_traveler"]] <- list(
            name = "Time Traveler",
            description = "Watched items from 5 different decades",
            badge_image = "badges/time_traveler.png"
          )
          mark_notification_shown("time_traveler")
        }
      }
    }
    
    # 8. Classic Connoisseur
    classic_items <- items[items$year <= 1979 & !is.na(items$year), ]
    if (!rv$badge_progress$classic_connoisseur && nrow(classic_items) > 0) {
      rv$badge_progress$classic_connoisseur <- TRUE
      if (!"classic_connoisseur" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "classic_connoisseur")
        if (!has_shown_notification("classic_connoisseur")) {
          newly_unlocked[["classic_connoisseur"]] <- list(
            name = "Classic Connoisseur",
            description = "Watched an item from the 1970s or earlier",
            badge_image = "badges/classic_connoisseur.png"
          )
          mark_notification_shown("classic_connoisseur")
        }
      }
    }
    
    # 9. Streak Keeper
    check_streak <- function() {
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          query <- "SELECT DISTINCT watch_date FROM watch_history 
                 ORDER BY watch_date DESC LIMIT 7"
          result <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if (nrow(result) < 7) return(FALSE)
          
          dates <- as.Date(result$watch_date)
          current_date <- Sys.Date()
          
          for (i in 1:6) {
            if (as.numeric(difftime(dates[i], dates[i+1], units = "days")) != 1) {
              return(FALSE)
            }
          }
          return(TRUE)
        }, error = function(e) {
          if (!is.null(con)) dbDisconnect(con)
          return(FALSE)
        })
      }
      return(FALSE)
    }
    
    if (!rv$badge_progress$streak_keeper) {
      rv$badge_progress$streak_keeper <- check_streak()
      if (rv$badge_progress$streak_keeper && !"streak_keeper" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "streak_keeper")
        if (!has_shown_notification("streak_keeper")) {
          newly_unlocked[["streak_keeper"]] <- list(
            name = "Streak Keeper",
            description = "Watched items for 7 consecutive days",
            badge_image = "badges/streak_keeper.png"
          )
          mark_notification_shown("streak_keeper")
        }
      }
    }
    
    # 10. Essayist
    if (!rv$badge_progress$essayist && nrow(items) > 0) {
      has_long_review <- any(nchar(items$review[!is.na(items$review)]) >= 500)
      rv$badge_progress$essayist <- has_long_review
      if (rv$badge_progress$essayist && !"essayist" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "essayist")
        if (!has_shown_notification("essayist")) {
          newly_unlocked[["essayist"]] <- list(
            name = "Essayist",
            description = "Wrote a review with 500+ characters",
            badge_image = "badges/essayist.png"
          )
          mark_notification_shown("essayist")
        }
      }
    }
    
    # 11. Haiku Reviewer
    if (!rv$badge_progress$haiku_reviewer && nrow(items) > 0) {
      has_short_review <- any(!is.na(items$review) & 
                                nchar(items$review) > 0 & 
                                nchar(items$review) <= 100)
      rv$badge_progress$haiku_reviewer <- has_short_review
      if (rv$badge_progress$haiku_reviewer && !"haiku_reviewer" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "haiku_reviewer")
        if (!has_shown_notification("haiku_reviewer")) {
          newly_unlocked[["haiku_reviewer"]] <- list(
            name = "Haiku Reviewer",
            description = "Wrote a review with 100 characters or less",
            badge_image = "badges/haiku_reviewer.png"
          )
          mark_notification_shown("haiku_reviewer")
        }
      }
    }
    
    # 12. Actor Apprentice
    if (!rv$badge_progress$actor_apprentice && nrow(items) > 0) {
      all_actors <- unlist(strsplit(items$cast[!is.na(items$cast)], ", "))
      actor_counts <- table(trimws(all_actors))
      has_five_actor <- any(actor_counts >= 5)
      rv$badge_progress$actor_apprentice <- has_five_actor
      if (rv$badge_progress$actor_apprentice && !"actor_apprentice" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "actor_apprentice")
        if (!has_shown_notification("actor_apprentice")) {
          newly_unlocked[["actor_apprentice"]] <- list(
            name = "Actor Apprentice",
            description = "Watched 5+ works by the same actor",
            badge_image = "badges/actor_apprentice.png"
          )
          mark_notification_shown("actor_apprentice")
        }
      }
    }
    
    # 13. Director Devotee
    if (!rv$badge_progress$director_devotee && nrow(items) > 0) {
      all_directors <- unlist(strsplit(items$director[!is.na(items$director)], ", "))
      director_counts <- table(trimws(all_directors))
      has_five_director <- any(director_counts >= 5)
      rv$badge_progress$director_devotee <- has_five_director
      if (rv$badge_progress$director_devotee && !"director_devotee" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "director_devotee")
        if (!has_shown_notification("director_devotee")) {
          newly_unlocked[["director_devotee"]] <- list(
            name = "Director Devotee",
            description = "Watched 5+ works by the same director",
            badge_image = "badges/director_devotee.png"
          )
          mark_notification_shown("director_devotee")
        }
      }
    }
    
    # 14. Romance Royalty
    if (!rv$badge_progress$romance_royalty && nrow(items) > 0) {
      romance_items <- items[grepl("romance", items$genre, ignore.case = TRUE) & 
                               items$media_type == "Movie", ]
      rv$badge_progress$romance_royalty <- nrow(romance_items) >= 10
      if (rv$badge_progress$romance_royalty && !"romance_royalty" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "romance_royalty")
        if (!has_shown_notification("romance_royalty")) {
          newly_unlocked[["romance_royalty"]] <- list(
            name = "Romance Royalty",
            description = "Watched 10+ romance movies",
            badge_image = "badges/romance_royalty.png"
          )
          mark_notification_shown("romance_royalty")
        }
      }
    }
    
    # 15. Mad Scientist
    if (!rv$badge_progress$mad_scientist && nrow(items) > 0) {
      scifi_items <- items[grepl("science fiction|sci-fi", items$genre, ignore.case = TRUE) & 
                             items$media_type == "Movie", ]
      rv$badge_progress$mad_scientist <- nrow(scifi_items) >= 10
      if (rv$badge_progress$mad_scientist && !"mad_scientist" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "mad_scientist")
        if (!has_shown_notification("mad_scientist")) {
          newly_unlocked[["mad_scientist"]] <- list(
            name = "Mad Scientist",
            description = "Watched 10+ science fiction movies",
            badge_image = "badges/mad_scientist.png"
          )
          mark_notification_shown("mad_scientist")
        }
      }
    }
    
    # 16. Perfectionist
    if (!rv$badge_progress$perfectionist && nrow(items) > 0) {
      all_rated <- all(!is.na(items$rating) & items$rating > 0)
      rv$badge_progress$perfectionist <- all_rated
      if (rv$badge_progress$perfectionist && !"perfectionist" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "perfectionist")
        if (!has_shown_notification("perfectionist")) {
          newly_unlocked[["perfectionist"]] <- list(
            name = "Perfectionist",
            description = "Rated every item in your library",
            badge_image = "badges/perfectionist.png"
          )
          mark_notification_shown("perfectionist")
        }
      }
    }
    
    # 17. Numerical
    if (!rv$badge_progress$numerical && nrow(items) > 0) {
      has_number_title <- any(grepl("\\d", items$title))
      rv$badge_progress$numerical <- has_number_title
      if (rv$badge_progress$numerical && !"numerical" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "numerical")
        if (!has_shown_notification("numerical")) {
          newly_unlocked[["numerical"]] <- list(
            name = "Numerical",
            description = "Watched an item with numbers in the title",
            badge_image = "badges/numerical.png"
          )
          mark_notification_shown("numerical")
        }
      }
    }
    
    # 18. The Developer's Cut
    if (!rv$badge_progress$developers_cut && nrow(items) > 0) {
      has_interstellar <- any(grepl("interstellar", items$title, ignore.case = TRUE) & 
                                items$watch_status == "Watched")
      rv$badge_progress$developers_cut <- has_interstellar
      if (rv$badge_progress$developers_cut && !"developers_cut" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "developers_cut")
        if (!has_shown_notification("developers_cut")) {
          newly_unlocked[["developers_cut"]] <- list(
            name = "The Developer's Cut",
            description = "Watched the developer's favorite movie: Interstellar",
            badge_image = "badges/developers_cut.png"
          )
          mark_notification_shown("developers_cut")
        }
      }
    }
    
    # 19. The Developer's Season
    if (!rv$badge_progress$developers_season && nrow(items) > 0) {
      has_walking_dead <- any(grepl("walking dead", items$title, ignore.case = TRUE) & 
                                items$media_type == "TV Series" & 
                                items$watch_status == "Watched")
      rv$badge_progress$developers_season <- has_walking_dead
      if (rv$badge_progress$developers_season && !"developers_season" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "developers_season")
        if (!has_shown_notification("developers_season")) {
          newly_unlocked[["developers_season"]] <- list(
            name = "The Developer's Season",
            description = "Watched the developer's favorite TV series: The Walking Dead",
            badge_image = "badges/developers_season.png"
          )
          mark_notification_shown("developers_season")
        }
      }
    }
    
    # 20. THE PLOTWIST BADGE
    plotwist_rated_items <- items[grepl("plotwist|plot twist", items$title, ignore.case = TRUE) & 
                                    items$rating >= 4 &
                                    !is.na(items$rating), ]
    
    if (!rv$badge_progress$plotwist_badge && nrow(plotwist_rated_items) > 0) {
      rv$badge_progress$plotwist_badge <- TRUE
      rv$badge_progress$plotwist_special <- TRUE
      if (!"plotwist_badge" %in% rv$achievements_unlocked) {
        rv$achievements_unlocked <- c(rv$achievements_unlocked, "plotwist_badge")
        if (!has_shown_notification("plotwist_badge")) {
          newly_unlocked[["plotwist_badge"]] <- list(
            name = "THE PLOTWIST BADGE",
            description = "Rated 'Plotwist' as a movie/TV series (4+ stars)",
            badge_image = "badges/plotwist_badge.png"
          )
          mark_notification_shown("plotwist_badge")
        }
      }
    }
    
    if (!rv$badge_progress$plotwist_special) {
      plotwist_items <- items[grepl("plotwist", items$title, ignore.case = TRUE), ]
      if (nrow(plotwist_items) > 0) {
        rv$badge_progress$plotwist_special <- TRUE
      }
    }

    if (length(newly_unlocked) > 0) {

      achievement_id <- names(newly_unlocked)[1]
      rv$achievement_to_show <- list(
        id = achievement_id,
        details = newly_unlocked[[achievement_id]]
      )
    }
  }
  
  # Check achievements when items change
  observeEvent(rv$refresh, {
    check_achievements()
  })
  
  # Initialize achievements on app start
  observe({
    if (rv$authenticated) {
      check_achievements()
    }
  })

  observeEvent(input$nav_home, { 
    rv$page <- "home" 
    updateNavButtons("home")
    session$sendCustomMessage("scrollToTop", list())
  })
  observeEvent(input$nav_stats, { 
    rv$page <- "stats" 
    updateNavButtons("stats")
    session$sendCustomMessage("scrollToTop", list())
  })
  observeEvent(input$nav_library, { 
    rv$page <- "library" 
    rv$library_page <- 1
    updateNavButtons("library")
    session$sendCustomMessage("scrollToTop", list())
  })
  observeEvent(input$nav_recommendations, { 
    rv$page <- "recommendations" 
    updateNavButtons("recommendations")
    session$sendCustomMessage("scrollToTop", list())
  })
  observeEvent(input$nav_achievements, { 
    rv$page <- "achievements" 
    updateNavButtons("achievements")
    session$sendCustomMessage("scrollToTop", list())
  })
  
  observeEvent(input$show_add_modal, { rv$show_modal <- TRUE })
  observeEvent(input$close_add_modal, { 
    rv$show_modal <- FALSE 
    rv$tmdb_results <- NULL
    rv$current_tmdb_id <- NULL
    rv$is_fetching_season <- FALSE
    rv$calculated_total_episodes_watched <- 0
    rv$modal_source <- "manual" 
  })
  
  observeEvent(input$close_details_modal, { 
    rv$show_details_modal <- FALSE 
    rv$selected_item <- NULL
    rv$collection_data <- NULL
  })

  observeEvent(input$show_rec_details, {
    if (!is.null(input$show_rec_details) && input$show_rec_details != "") {
      tryCatch({
        data <- fromJSON(input$show_rec_details)
        tmdb_id <- as.numeric(data$tmdb_id)
        media_type <- data$media_type
        title <- data$title

        rv$rec_details_loading <- TRUE
        rv$show_rec_details_modal <- TRUE

        rv$modal_source <- "recommendations"
        
        # Fetch details from TMDB
        details <- get_tmdb_details(tmdb_id, media_type)
        
        if (!is.null(details)) {
          rv$rec_details <- details
          rv$rec_details_loading <- FALSE
        } else {
          showNotification("Failed to fetch details from TMDB.", type = "error", duration = 3)
          rv$show_rec_details_modal <- FALSE
          rv$rec_details_loading <- FALSE
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 3)
        rv$show_rec_details_modal <- FALSE
        rv$rec_details_loading <- FALSE
      })
    }
  })
  
  # Close recommendation details modal
  observeEvent(input$close_rec_details_modal, { 
    rv$show_rec_details_modal <- FALSE 
    rv$rec_details <- NULL
    rv$rec_details_loading <- FALSE
  })
  
  # Handle edit button click
  observeEvent(input$edit_item, {
    if (!is.null(input$edit_item) && input$edit_item != "") {
      item_id <- as.numeric(input$edit_item)
      
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          query <- sprintf("SELECT * FROM movies_series WHERE id = %d", item_id)
          item <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if (nrow(item) == 1) {
            rv$editing_item <- item[1, ]
            rv$show_edit_modal <- TRUE
            rv$show_details_modal <- FALSE  # Close details modal if open
          } else {
            showNotification("Item not found!", type = "error")
          }
        }, error = function(e) {
          showNotification(paste("Error fetching item:", e$message), type = "error")
          if (!is.null(con)) dbDisconnect(con)
        })
      }
    }
  })
  
  # Close edit modal
  observeEvent(input$close_edit_modal, { 
    rv$show_edit_modal <- FALSE 
    rv$editing_item <- NULL
    rv$edit_tmdb_results <- NULL
    rv$edit_current_tmdb_id <- NULL
    rv$edit_is_fetching_season <- FALSE
    rv$edit_calculated_total_episodes_watched <- 0
  })
  
  # Handle delete button click
  observeEvent(input$delete_item, {
    if (!is.null(input$delete_item) && input$delete_item != "") {
      item_id <- as.numeric(input$delete_item)
      
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          query <- sprintf("SELECT * FROM movies_series WHERE id = %d", item_id)
          item <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if (nrow(item) == 1) {
            rv$deleting_item <- item[1, ]
            rv$show_delete_modal <- TRUE
          } else {
            showNotification("Item not found!", type = "error")
          }
        }, error = function(e) {
          showNotification(paste("Error fetching item:", e$message), type = "error")
          if (!is.null(con)) dbDisconnect(con)
        })
      }
    }
  })
  
  # Close delete confirmation modal
  observeEvent(input$close_delete_modal, { 
    rv$show_delete_modal <- FALSE 
    rv$deleting_item <- NULL
  })
  
  # Handle delete confirmation
  observeEvent(input$confirm_delete, {
    if (!is.null(rv$deleting_item)) {
      item_id <- rv$deleting_item$id
      item_title <- rv$deleting_item$title
      
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          # Delete from watch_history first (foreign key constraint)
          delete_history_query <- sprintf("DELETE FROM watch_history WHERE movie_id = %d", item_id)
          dbExecute(con, delete_history_query)
          
          # Delete from movies_series
          delete_query <- sprintf("DELETE FROM movies_series WHERE id = %d", item_id)
          dbExecute(con, delete_query)
          dbDisconnect(con)
          
          showNotification(paste("Successfully deleted:", item_title), type = "message", duration = 3)
          
          # Close all modals
          rv$show_delete_modal <- FALSE
          rv$show_details_modal <- FALSE
          rv$deleting_item <- NULL
          
          # Refresh the data
          rv$refresh <- rv$refresh + 1
          
        }, error = function(e) {
          showNotification(paste("Failed to delete item:", e$message), type = "error", duration = 5)
          if (!is.null(con)) dbDisconnect(con)
        })
      }
    }
  })
  
  observeEvent(input$show_item_details, {
    if (!is.null(input$show_item_details) && input$show_item_details != "") {
      item_id <- as.numeric(input$show_item_details)
      
      cat("=== DETAILS MODAL DEBUG ===\n")
      cat("Item ID:", item_id, "\n")
      
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          query <- sprintf("SELECT * FROM movies_series WHERE id = %d", item_id)
          item <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          cat("Query returned", nrow(item), "row(s)\n")
          
          if (nrow(item) == 1) {
            cat("Item title:", item$title[1], "\n")
            cat("Watch status:", item$watch_status[1], "\n")
            cat("TMDB ID:", item$tmdb_id[1], "\n")
            cat("Media type:", item$media_type[1], "\n")
            
            rv$selected_item <- item[1, ]
            rv$show_details_modal <- TRUE
            
            # ALWAYS fetch collection data when modal opens (regardless of watch status)
            # Reset collection data first to clear any previous data
            rv$collection_data <- NULL
            
            # Fetch collection data if TMDB ID exists and is valid
            if (!is.na(item$tmdb_id[1]) && !is.null(item$tmdb_id[1]) && 
                item$tmdb_id[1] != "" && item$tmdb_id[1] > 0) {
              cat("Fetching collection for TMDB ID:", item$tmdb_id[1], "\n")
              
              # Use tryCatch to handle any TMDB API errors gracefully
              tryCatch({
                collection_data <- get_tmdb_collection(item$tmdb_id[1], item$media_type[1])
                
                if (!is.null(collection_data) && !is.null(collection_data$items) && 
                    length(collection_data$items) > 0) {
                  rv$collection_data <- collection_data
                  cat("Collection found:", collection_data$collection_name, "\n")
                  cat("Number of items:", length(collection_data$items), "\n")
                } else {
                  cat("No collection found or collection is empty\n")
                  rv$collection_data <- NULL
                }
              }, error = function(e) {
                cat("Error fetching collection from TMDB:", e$message, "\n")
                rv$collection_data <- NULL
              })
            } else {
              cat("No valid TMDB ID, skipping collection fetch\n")
              rv$collection_data <- NULL
            }
            
            cat("Details modal should now be visible\n")
          } else {
            showNotification("Item not found!", type = "error")
          }
        }, error = function(e) {
          cat("ERROR in show_item_details:", e$message, "\n")
          showNotification(paste("Error fetching item:", e$message), type = "error")
          if (!is.null(con)) dbDisconnect(con)
        })
      } else {
        cat("Failed to get database connection\n")
      }
    }
    cat("=== END DEBUG ===\n")
  })
  
  # Handle collection item click
  observeEvent(input$show_collection_item, {
    if (!is.null(input$show_collection_item) && input$show_collection_item != "") {
      tryCatch({
        data <- fromJSON(input$show_collection_item)
        tmdb_id <- as.numeric(data$tmdb_id)
        media_type <- data$media_type
        title <- data$title
        
        # Check if this item already exists in the library
        con <- get_db_connection()
        if (!is.null(con)) {
          query <- sprintf("SELECT id FROM movies_series WHERE tmdb_id = %d", tmdb_id)
          existing <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if (nrow(existing) > 0) {
            # Item exists, show its details
            isolate({
              input$show_item_details <- existing$id[1]
            })
            Sys.sleep(0.1)
            session$sendCustomMessage(type = "eval", 
                                      message = sprintf("Shiny.setInputValue('show_item_details', %d, {priority: 'event'});", 
                                                        existing$id[1]))
          } else {
            # Item doesn't exist, show recommendation details to add it
            rv$rec_details_loading <- TRUE
            rv$show_rec_details_modal <- TRUE
            rv$show_details_modal <- FALSE
            rv$modal_source <- "collection"
            
            # Fetch details from TMDB
            details <- get_tmdb_details(tmdb_id, media_type)
            
            if (!is.null(details)) {
              rv$rec_details <- details
              rv$rec_details_loading <- FALSE
            } else {
              showNotification("Failed to fetch details from TMDB.", type = "error", duration = 3)
              rv$show_rec_details_modal <- FALSE
              rv$rec_details_loading <- FALSE
            }
          }
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 3)
      })
    }
  })
  
  observeEvent(rv$refresh, {
    # Only fetch if details modal is open and we have a selected item
    if (rv$show_details_modal && !is.null(rv$selected_item)) {
      cat("=== REFRESH TRIGGERED, RE-FETCHING COLLECTION ===\n")
      
      item <- rv$selected_item
      
      # Fetch collection data if TMDB ID exists
      if (!is.na(item$tmdb_id) && !is.null(item$tmdb_id) && 
          item$tmdb_id != "" && item$tmdb_id > 0) {
        cat("Re-fetching collection for TMDB ID:", item$tmdb_id, "\n")
        
        tryCatch({
          collection_data <- get_tmdb_collection(item$tmdb_id, item$media_type)
          
          if (!is.null(collection_data) && !is.null(collection_data$items) && 
              length(collection_data$items) > 0) {
            rv$collection_data <- collection_data
            cat("Collection re-fetched:", collection_data$collection_name, "\n")
          } else {
            cat("No collection found on re-fetch\n")
            rv$collection_data <- NULL
          }
        }, error = function(e) {
          cat("Error re-fetching collection:", e$message, "\n")
          rv$collection_data <- NULL
        })
      }
    }
  })

  updateNavButtons <- function(active_page) {
    session$sendCustomMessage(type = "updateNavButtons", message = list(active = active_page))
  }

  observe({
    updateNavButtons(rv$page)
  })
  
  get_all_items <- reactive({
    rv$refresh
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    data <- dbGetQuery(con, "SELECT * FROM movies_series ORDER BY date_added DESC")
    dbDisconnect(con)
    return(data)
  })

  get_existing_titles_and_ids <- reactive({
    items <- get_all_items()
    if (nrow(items) == 0) {
      return(list(titles = character(0), tmdb_ids = numeric(0)))
    }
    
    titles <- tolower(items$title)
    
    tmdb_ids <- if ("tmdb_id" %in% colnames(items)) {
      items$tmdb_id[!is.na(items$tmdb_id)]
    } else {
      numeric(0)
    }
    
    return(list(titles = titles, tmdb_ids = tmdb_ids))
  })
  
  # Get filtered and sorted library items
  get_library_items <- reactive({
    items <- get_all_items()
    
    if (nrow(items) == 0) return(data.frame())
    
    # Apply search filter
    if (nchar(rv$search_query) > 0) {
      items <- items %>% 
        filter(grepl(rv$search_query, title, ignore.case = TRUE) |
                 grepl(rv$search_query, director, ignore.case = TRUE) |
                 grepl(rv$search_query, genre, ignore.case = TRUE))
    }
    
    # Apply media type filter
    if (rv$library_view == "movies") {
      items <- items %>% filter(media_type == "Movie")
    } else if (rv$library_view == "series") {
      items <- items %>% filter(media_type == "TV Series")
    }
    
    # Apply status filter
    if (rv$filter_status != "all") {
      items <- items %>% filter(watch_status == rv$filter_status)
    }
    
    # Apply genre filter
    if (rv$filter_genre != "all") {
      items <- items %>% filter(grepl(rv$filter_genre, genre, ignore.case = TRUE))
    }
    
    # Apply rating filter
    if (rv$filter_rating != "all") {
      if (rv$filter_rating == "1") {
        items <- items %>% filter(rating == 1)
      } else if (rv$filter_rating == "2") {
        items <- items %>% filter(rating == 2)
      } else if (rv$filter_rating == "3") {
        items <- items %>% filter(rating == 3)
      } else if (rv$filter_rating == "4") {
        items <- items %>% filter(rating == 4)
      } else if (rv$filter_rating == "5") {
        items <- items %>% filter(rating == 5)
      } else if (rv$filter_rating == "4-5") {
        items <- items %>% filter(rating >= 4)
      }
    }
    
    # Apply year range filter
    if (!is.na(rv$filter_year_min) && !is.na(rv$filter_year_max)) {
      items <- items %>% 
        filter(year >= rv$filter_year_min & year <= rv$filter_year_max)
    }
    
    # Apply sorting
    if (rv$sort_by == "title") {
      items <- items %>% arrange(if (rv$sort_order == "asc") title else desc(title))
    } else if (rv$sort_by == "year") {
      items <- items %>% arrange(if (rv$sort_order == "asc") year else desc(year))
    } else if (rv$sort_by == "rating") {
      items <- items %>% arrange(if (rv$sort_order == "asc") rating else desc(rating))
    } else if (rv$sort_by == "date_added") {
      items <- items %>% arrange(if (rv$sort_order == "asc") date_added else desc(date_added))
    }
    
    return(items)
  })
  
  # Get empty state message based on current filters
  get_empty_state_message <- reactive({
    items <- get_all_items()
    filtered_items <- get_library_items()
    
    # If the entire database is empty
    if (nrow(items) == 0) {
      return(list(
        title = "The only plot twist here is an empty list.",
        message = "Time for a premiere! Add your first movie or show to get started."
      ))
    }
    
    # If filters return no results
    if (nrow(filtered_items) == 0) {
      # Check which filters are active
      has_search <- nchar(rv$search_query) > 0
      has_status_filter <- rv$filter_status != "all"
      has_genre_filter <- rv$filter_genre != "all"
      has_rating_filter <- rv$filter_rating != "all"
      
      # Search results empty
      if (has_search) {
        return(list(
          title = "Not in our database. Or any database. Possibly not even real.",
          message = "Try searching for something that exists."
        ))
      }
      
      # Status filter empty states
      if (has_status_filter) {
        if (rv$filter_status == "Unwatched") {
          return(list(
            title = "Your Unwatched list has left the building. Elvis style.",
            message = "Add something new or give an existing title the 'pending' status it deserves."
          ))
        } else if (rv$filter_status == "Watching") {
          return(list(
            title = "Your 'Watching' list is currently... in progress... of being empty.",
            message = "Start a series or movie to leave it hanging."
          ))
        } else if (rv$filter_status == "Watched") {
          return(list(
            title = "Your 'Watched' list is as empty as a sequel nobody asked for.",
            message = "Mark something as watched to fill this cinematic void."
          ))
        }
      }
      
      # Genre filter empty states
      if (has_genre_filter) {
        genre <- rv$filter_genre
        if (genre == "Comedy") {
          return(list(
            title = "No comedies? That explains why you're not laughing.",
            message = "Add some funny stuff or try another genre."
          ))
        } else if (genre == "Horror") {
          return(list(
            title = "Zero horror films. Are you... not afraid of the dark?",
            message = "Add something spooky or pick a different genre."
          ))
        } else if (genre == "Romance") {
          return(list(
            title = "No romance? Even this empty state feels lonely.",
            message = "Add something heartwarming or try another category."
          ))
        } else if (genre == "Action") {
          return(list(
            title = "No explosions? No car chases? Your action genre needs some... action.",
            message = "Add something explosive or try a genre with less adrenaline."
          ))
        } else if (genre == "Science Fiction") {
          return(list(
            title = "Zero alien invasions. Your future looks... suspiciously normal.",
            message = "Add something futuristic or explore a different dimension of genres."
          ))
        } else if (genre == "Drama") {
          return(list(
            title = "No dramas. Is your life interesting enough without them?",
            message = "Add something emotionally intense or try a lighter category."
          ))
        } else if (genre == "Documentary") {
          return(list(
            title = "No documentaries. Are you avoiding learning today?",
            message = "Add something educational or pick a genre that's less factual."
          ))
        } else if (genre == "Animation") {
          return(list(
            title = "No cartoons. Who silenced the Looney Tunes?",
            message = "Add something animated or try a genre for grown-ups."
          ))
        } else if (genre == "Fantasy") {
          return(list(
            title = "Zero dragons or wizards. How do you escape reality?",
            message = "Add something magical or stick to more grounded genres."
          ))
        } else if (genre == "Thriller") {
          return(list(
            title = "No thrillers. Is your heart rate... stable?",
            message = "Add something suspenseful or try a genre that won't spike your adrenaline."
          ))
        } else if (genre == "Mystery") {
          return(list(
            title = "Nothing mysterious here. Case closed.",
            message = "Add a whodunit or explore a genre with fewer questions."
          ))
        } else if (genre == "Music") {
          return(list(
            title = "No spontaneous singing. Your life must be... quietly efficient.",
            message = "Add something with show tunes or pick a genre that doesn't break into song."
          ))
        } else if (genre == "Western") {
          return(list(
            title = "No cowboys. This town ain't big enough for the lack of Westerns.",
            message = "Add something with tumbleweeds or try a more modern genre."
          ))
        } else if (genre == "Crime") {
          return(list(
            title = "Zero heists. Your watchlist is following the law.",
            message = "Add something illegal (fictionally) or try a genre that won't get you arrested."
          ))
        } else {
          return(list(
            title = paste0("The '", genre, "' genre is missing. Like the second season of that show they canceled."),
            message = "Try another genre or add titles to this one."
          ))
        }
      }
      
      # Rating filter empty states
      if (has_rating_filter) {
        if (rv$filter_rating == "1") {
          return(list(
            title = "Nothing rated 1 star. Are you too nice or just picky in a different way?",
            message = "Find something truly terrible and give it the lone star it deserves."
          ))
        } else if (rv$filter_rating == "2") {
          return(list(
            title = "No 2-star ratings. The 'meh' department is closed.",
            message = "Rate something as mediocre to fill this lukewarm category."
          ))
        } else if (rv$filter_rating == "3") {
          return(list(
            title = "Nothing 3-star. The 'perfectly adequate' shelf is empty.",
            message = "Find something decent but not amazing to give it a solid three."
          ))
        } else if (rv$filter_rating == "4") {
          return(list(
            title = "No 4-star ratings. Your 'almost perfect' club has no members.",
            message = "Find something great (but not quite perfect) and give it four shining stars."
          ))
        } else if (rv$filter_rating == "5") {
          return(list(
            title = "No 5-star ratings yet. Are you a harsh critic?",
            message = "Find cinematic perfection and give it the stars it deserves."
          ))
        } else if (rv$filter_rating == "4-5") {
          return(list(
            title = "No high ratings. Your standards are either sky-high or... you haven't found 'the one' yet.",
            message = "Watch something amazing and give it the stars it deserves."
          ))
        }
      }
      
      # Recently added sort empty state
      if (rv$sort_by == "date_added" && rv$sort_order == "desc") {
        return(list(
          title = "Nothing new here. It's all reruns.",
          message = "Add a fresh title to see it appear recently added."
        ))
      }
      
      # Default empty state for filtered results
      return(list(
        title = "No items match your current filters.",
        message = "Try adjusting your search or filters to find what you're looking for."
      ))
    }
    
    return(NULL)
  })
  
  observeEvent(input$tmdb_search, {
    query <- input$tmdb_search
    
    cat("TMDB Search triggered with query:", query, "\n")
    
    if (is.null(query) || query == "" || nchar(query) < 2) {
      session$sendCustomMessage(
        type = "eval",
        message = "
          const container = document.getElementById('tmdb-suggestions');
          if (container) {
            container.classList.remove('active');
          }
        "
      )
      return()
    }
    
    rv$tmdb_loading <- TRUE
    
    # Perform TMDB search
    cat("Calling search_tmdb()...\n")
    results <- search_tmdb(query)
    cat("Results received:", length(results), "items\n")
    
    rv$tmdb_results <- results
    rv$tmdb_loading <- FALSE

    if (!is.null(results) && length(results) > 0) {
      cat("Building HTML for", length(results), "results\n")
      suggestions_html <- ""
      for (i in seq_along(results)) {
        result <- results[[i]]

        safe_title <- gsub("'", "\\\\'", result$title)
        safe_title <- gsub('"', '\\\\"', safe_title)
        
        suggestions_html <- paste0(suggestions_html,
                                   '<div class="tmdb-suggestion" onclick=\'selectTmdbSuggestion(',
                                   result$id, ', "', result$media_type, '", "', safe_title, '", false)\'>',
                                   '<img src="', result$poster, '" class="suggestion-poster" alt="', safe_title, '">',
                                   '<div class="suggestion-content">',
                                   '<div class="suggestion-type">', result$media_type, '</div>',
                                   '<div class="suggestion-title">', result$title, '</div>',
                                   '<div class="suggestion-year">', result$year, '</div>',
                                   '<div class="suggestion-overview">', result$overview, '</div>',
                                   '</div></div>'
        )
      }
      cat("HTML built successfully\n")
    } else {
      cat("No results found\n")
      suggestions_html <- '<div class="suggestion-empty">No results found in TMDB. Try a different search term.</div>'
    }

    cat("Sending suggestions...\n")
    session$sendCustomMessage(
      type = "tmdb_suggestions",
      message = list(html = suggestions_html, type = "add")
    )
    cat("Message sent!\n")
  })
  
  observeEvent(input$edit_tmdb_search, {
    query <- input$edit_tmdb_search
    
    cat("EDIT TMDB Search triggered with query:", query, "\n")
    
    if (is.null(query) || query == "" || nchar(query) < 2) {
      session$sendCustomMessage(
        type = "eval",
        message = "
          const container = document.getElementById('edit_tmdb-suggestions');
          if (container) {
            container.classList.remove('active');
          }
        "
      )
      return()
    }
    
    # Perform TMDB search
    cat("Calling search_tmdb() for edit modal...\n")
    results <- search_tmdb(query)
    cat("Results received:", length(results), "items\n")
    
    rv$edit_tmdb_results <- results

    if (!is.null(results) && length(results) > 0) {
      cat("Building HTML for edit modal", length(results), "results\n")
      suggestions_html <- ""
      for (i in seq_along(results)) {
        result <- results[[i]]
        # Escape single quotes and special characters
        safe_title <- gsub("'", "\\\\'", result$title)
        safe_title <- gsub('"', '\\\\"', safe_title)
        
        suggestions_html <- paste0(suggestions_html,
                                   '<div class="tmdb-suggestion" onclick=\'selectTmdbSuggestion(',
                                   result$id, ', "', result$media_type, '", "', safe_title, '", true)\'>',
                                   '<img src="', result$poster, '" class="suggestion-poster" alt="', safe_title, '">',
                                   '<div class="suggestion-content">',
                                   '<div class="suggestion-type">', result$media_type, '</div>',
                                   '<div class="suggestion-title">', result$title, '</div>',
                                   '<div class="suggestion-year">', result$year, '</div>',
                                   '<div class="suggestion-overview">', result$overview, '</div>',
                                   '</div></div>'
        )
      }
      cat("Edit modal HTML built successfully\n")
    } else {
      cat("No results found for edit modal\n")
      suggestions_html <- '<div class="suggestion-empty">No results found in TMDB. Try a different search term.</div>'
    }
    
    # Send suggestions to client
    cat("Sending edit modal suggestions...\n")
    session$sendCustomMessage(
      type = "tmdb_suggestions",
      message = list(html = suggestions_html, type = "edit")
    )
    cat("Edit modal message sent!\n")
  })
  
  # TMDB Fetch Details Observer (Add Modal)
  observeEvent(input$tmdb_fetch, {
    if (!is.null(input$tmdb_fetch) && input$tmdb_fetch != "") {
      tryCatch({
        data <- fromJSON(input$tmdb_fetch)
        tmdb_id <- as.numeric(data$id)
        media_type <- data$media_type
        
        # Store TMDB ID for season fetching AND for database insertion
        rv$current_tmdb_id <- tmdb_id
        
        # Show loading notification
        showNotification("Fetching details from TMDB...", type = "message", duration = 2)
        
        # Get detailed information
        details <- get_tmdb_details(tmdb_id, media_type)
        
        if (!is.null(details)) {
          # Update form fields with TMDB data
          updateTextInput(session, "modal_title", value = details$title)
          updateSelectizeInput(session, "modal_media_type", selected = details$media_type)
          
          # Parse and set genres
          if (details$genres != "") {
            genre_list <- unlist(strsplit(details$genres, ", "))
            updateSelectizeInput(session, "modal_genre", selected = genre_list)
          }
          
          updateNumericInput(session, "modal_year", value = details$year)
          updateTextInput(session, "modal_director", value = details$director)
          updateTextAreaInput(session, "modal_cast", value = details$cast)
          updateTextAreaInput(session, "modal_plot_summary", value = details$plot_summary)
          updateTextInput(session, "modal_poster", value = details$poster_url)
          
          # Set duration for movies
          if (details$media_type == "Movie" && details$duration > 0) {
            updateNumericInput(session, "modal_total_duration", value = details$duration)
            updateNumericInput(session, "modal_watched_duration", value = 0)
          }
          
          # Set TV Series tracking info
          if (details$media_type == "TV Series") {
            if (details$total_seasons > 0) {
              updateNumericInput(session, "modal_total_seasons", value = details$total_seasons)
            }
            if (details$total_episodes > 0) {
              updateNumericInput(session, "modal_total_episodes", value = details$total_episodes)
            }
            # Set default values for current season/episode
            updateNumericInput(session, "modal_current_season", value = 1)
            updateNumericInput(session, "modal_current_episode", value = 0)
            updateNumericInput(session, "modal_total_episodes_watched", value = 0)
            
            # If we have TMDB ID and seasons, fetch episodes for season 1
            if (details$total_seasons > 0 && tmdb_id) {
              # Fetch episodes count for season 1
              season_details <- get_tv_season_details(tmdb_id, 1)
              if (!is.null(season_details) && season_details$episodes_count > 0) {
                updateNumericInput(session, "modal_episodes_current_season", value = season_details$episodes_count)
              } else {
                updateNumericInput(session, "modal_episodes_current_season", value = 10) 
              }
            } else {
              updateNumericInput(session, "modal_episodes_current_season", value = 10)
            }
          }

          session$sendCustomMessage(
            type = "eval",
            message = "setTimeout(function() { 
            if (typeof updateModalDisplay === 'function') { 
              updateModalDisplay(false); 
            }
            if (typeof updateTVSeriesFields === 'function') { 
              updateTVSeriesFields(false); 
            }
            if (typeof updateMovieDurationFields === 'function') {
              updateMovieDurationFields(false);
            }
            if (typeof updateEpisodeIndicator === 'function') {
              updateEpisodeIndicator(false);
            }
          }, 100);"
          )
          
          showNotification("✅ TMDB data loaded successfully! You can edit any field.", 
                           type = "message", duration = 3)
        } else {
          showNotification("Could not fetch details from TMDB.", type = "warning", duration = 3)
        }
      }, error = function(e) {
        showNotification(paste("Error fetching TMDB data:", e$message), 
                         type = "error", duration = 3)
      })
    }
  })
  
  # TMDB Fetch Details Observer (Edit Modal)
  observeEvent(input$edit_tmdb_fetch, {
    if (!is.null(input$edit_tmdb_fetch) && input$edit_tmdb_fetch != "") {
      tryCatch({
        data <- fromJSON(input$edit_tmdb_fetch)
        tmdb_id <- as.numeric(data$id)
        media_type <- data$media_type
        
        # Store TMDB ID for season fetching
        rv$edit_current_tmdb_id <- tmdb_id
        
        # Show loading notification
        showNotification("Fetching details from TMDB...", type = "message", duration = 2)
        
        # Get detailed information
        details <- get_tmdb_details(tmdb_id, media_type)
        
        if (!is.null(details)) {
          # Update form fields with TMDB data
          updateTextInput(session, "edit_modal_title", value = details$title)
          updateSelectizeInput(session, "edit_modal_media_type", selected = details$media_type)
          
          # Parse and set genres
          if (details$genres != "") {
            genre_list <- unlist(strsplit(details$genres, ", "))
            updateSelectizeInput(session, "edit_modal_genre", selected = genre_list)
          }
          
          updateNumericInput(session, "edit_modal_year", value = details$year)
          updateTextInput(session, "edit_modal_director", value = details$director)
          updateTextAreaInput(session, "edit_modal_cast", value = details$cast)
          updateTextAreaInput(session, "edit_modal_plot_summary", value = details$plot_summary)
          updateTextInput(session, "edit_modal_poster", value = details$poster_url)
          
          # Set duration for movies
          if (details$media_type == "Movie" && details$duration > 0) {
            updateNumericInput(session, "edit_modal_total_duration", value = details$duration)
            updateNumericInput(session, "edit_modal_watched_duration", value = 0)
          }
          
          # Set TV Series tracking info
          if (details$media_type == "TV Series") {
            if (details$total_seasons > 0) {
              updateNumericInput(session, "edit_modal_total_seasons", value = details$total_seasons)
            }
            if (details$total_episodes > 0) {
              updateNumericInput(session, "edit_modal_total_episodes", value = details$total_episodes)
            }
            # Set default values for current season/episode
            updateNumericInput(session, "edit_modal_current_season", value = 1)
            updateNumericInput(session, "edit_modal_current_episode", value = 0)
            updateNumericInput(session, "edit_modal_total_episodes_watched", value = 0)
            
            # If we have TMDB ID and seasons, fetch episodes for season 1
            if (details$total_seasons > 0 && tmdb_id) {
              # Fetch episodes count for season 1
              season_details <- get_tv_season_details(tmdb_id, 1)
              if (!is.null(season_details) && season_details$episodes_count > 0) {
                updateNumericInput(session, "edit_modal_episodes_current_season", value = season_details$episodes_count)
              } else {
                updateNumericInput(session, "edit_modal_episodes_current_season", value = 10)
              }
            } else {
              updateNumericInput(session, "edit_modal_episodes_current_season", value = 10)
            }
          }
          
          # Update JavaScript functions
          session$sendCustomMessage(
            type = "eval",
            message = "setTimeout(function() { 
              if (typeof updateModalDisplay === 'function') { 
                updateModalDisplay(true); 
              }
              if (typeof updateTVSeriesFields === 'function') { 
                updateTVSeriesFields(true); 
              }
              if (typeof updateMovieDurationFields === 'function') {
                updateMovieDurationFields(true);
              }
              if (typeof updateEpisodeIndicator === 'function') {
                updateEpisodeIndicator(true);
              }
            }, 100);"
          )
          
          showNotification("✅ TMDB data loaded successfully! You can edit any field.", 
                           type = "message", duration = 3)
        } else {
          showNotification("Could not fetch details from TMDB.", type = "warning", duration = 3)
        }
      }, error = function(e) {
        showNotification(paste("Error fetching TMDB data:", e$message), 
                         type = "error", duration = 3)
      })
    }
  })
  
  # Handle manual input for current season
  observeEvent(input$modal_current_season, {

    if (!is.null(input$modal_current_season)) {
      new_value <- as.numeric(input$modal_current_season)
      old_value <- rv$tv_current_season

      if (is.na(new_value) || new_value == old_value) return()

      rv$tv_current_season <- new_value

      session$sendCustomMessage(
        type = "eval",
        message = "setTimeout(function() { 
          if (typeof updateEpisodeIndicator === 'function') {
            updateEpisodeIndicator(false);
          }
          if (typeof updateTotalEpisodesWatched === 'function') {
            updateTotalEpisodesWatched(false);
          }
        }, 100);"
      )

      if (!is.null(rv$current_tmdb_id) && 
          !is.null(input$modal_media_type) && 
          input$modal_media_type == "TV Series" &&
          new_value > 0 &&
          !rv$is_fetching_season) {

        rv$is_fetching_season <- TRUE

        delay(500, {
          tryCatch({
            # Fetch season details from TMDB
            season_details <- get_tv_season_details(rv$current_tmdb_id, new_value)
            
            if (!is.null(season_details) && season_details$episodes_count > 0) {
              # Update episodes in current season
              updateNumericInput(session, "modal_episodes_current_season", value = season_details$episodes_count)
              
              # Update episode indicator
              session$sendCustomMessage(
                type = "eval",
                message = "setTimeout(function() { 
                  if (typeof updateEpisodeIndicator === 'function') {
                    updateEpisodeIndicator(false);
                  }
                  if (typeof updateTotalEpisodesWatched === 'function') {
                    updateTotalEpisodesWatched(false);
                  }
                }, 100);"
              )
              
              # Show notification
              showNotification(paste("Season", new_value, "has", season_details$episodes_count, "episodes"), 
                               type = "message", duration = 3)
            }
          }, error = function(e) {
            message("Error fetching season details: ", e$message)
          }, finally = {
            rv$is_fetching_season <- FALSE
          })
        })
      } else {
        rv$is_fetching_season <- FALSE
      }
    }
  })
  
  # Handle manual input for current season (Edit Modal)
  observeEvent(input$edit_modal_current_season, {

    if (!is.null(input$edit_modal_current_season)) {
      new_value <- as.numeric(input$edit_modal_current_season)
      old_value <- rv$edit_tv_current_season

      if (is.na(new_value) || new_value == old_value) return()

      rv$edit_tv_current_season <- new_value

      session$sendCustomMessage(
        type = "eval",
        message = "setTimeout(function() { 
          if (typeof updateEpisodeIndicator === 'function') {
            updateEpisodeIndicator(true);
          }
          if (typeof updateTotalEpisodesWatched === 'function') {
            updateTotalEpisodesWatched(true);
          }
        }, 100);"
      )

      if (!is.null(rv$edit_current_tmdb_id) && 
          !is.null(input$edit_modal_media_type) && 
          input$edit_modal_media_type == "TV Series" &&
          new_value > 0 &&
          !rv$edit_is_fetching_season) {

        rv$edit_is_fetching_season <- TRUE
        
        # Use a delay to prevent too many API calls during typing
        delay(500, {
          tryCatch({
            # Fetch season details from TMDB
            season_details <- get_tv_season_details(rv$edit_current_tmdb_id, new_value)
            
            if (!is.null(season_details) && season_details$episodes_count > 0) {
              # Update episodes in current season
              updateNumericInput(session, "edit_modal_episodes_current_season", value = season_details$episodes_count)
              
              # Update episode indicator
              session$sendCustomMessage(
                type = "eval",
                message = "setTimeout(function() { 
                  if (typeof updateEpisodeIndicator === 'function') {
                    updateEpisodeIndicator(true);
                  }
                  if (typeof updateTotalEpisodesWatched === 'function') {
                    updateTotalEpisodesWatched(true);
                  }
                }, 100);"
              )
              
              # Show notification
              showNotification(paste("Season", new_value, "has", season_details$episodes_count, "episodes"), 
                               type = "message", duration = 3)
            }
          }, error = function(e) {
            message("Error fetching season details: ", e$message)
          }, finally = {
            rv$edit_is_fetching_season <- FALSE
          })
        })
      } else {
        rv$edit_is_fetching_season <- FALSE
      }
    }
  })
  
  # Handle current episode changes for indicator (Add Modal)
  observeEvent(input$modal_current_episode, {
    if (!is.null(input$modal_current_episode)) {
      new_value <- as.numeric(input$modal_current_episode)
      old_value <- rv$tv_current_episode
      
      # Only process if value has actually changed
      if (is.na(new_value) || new_value == old_value) return()
      
      rv$tv_current_episode <- new_value
      
      # Update episode indicator and total episodes watched
      session$sendCustomMessage(
        type = "eval",
        message = "setTimeout(function() { 
          if (typeof updateEpisodeIndicator === 'function') {
            updateEpisodeIndicator(false);
          }
          if (typeof updateTotalEpisodesWatched === 'function') {
            updateTotalEpisodesWatched(false);
          }
        }, 100);"
      )
    }
  })
  
  # Handle current episode changes for indicator (Edit Modal)
  observeEvent(input$edit_modal_current_episode, {
    if (!is.null(input$edit_modal_current_episode)) {
      new_value <- as.numeric(input$edit_modal_current_episode)
      old_value <- rv$edit_tv_current_episode

      if (is.na(new_value) || new_value == old_value) return()
      
      rv$edit_tv_current_episode <- new_value
      
      # Update episode indicator and total episodes watched
      session$sendCustomMessage(
        type = "eval",
        message = "setTimeout(function() { 
          if (typeof updateEpisodeIndicator === 'function') {
            updateEpisodeIndicator(true);
          }
          if (typeof updateTotalEpisodesWatched === 'function') {
            updateTotalEpisodesWatched(true);
          }
        }, 100);"
      )
    }
  })
  
  # Handle title changes for indicator (Add Modal)
  observeEvent(input$modal_title, {
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(false);
        }
      }, 100);"
    )
  })
  
  # Handle title changes for indicator (Edit Modal)
  observeEvent(input$edit_modal_title, {
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(true);
        }
      }, 100);"
    )
  })
  
  # Handle media type changes to update watched duration (Add Modal)
  observeEvent(input$modal_media_type, {
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof updateMovieDurationFields === 'function') {
          updateMovieDurationFields(false);
        }
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(false);
        }
      }, 100);"
    )
  })
  
  # Handle media type changes to update watched duration (Edit Modal)
  observeEvent(input$edit_modal_media_type, {
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof updateMovieDurationFields === 'function') {
          updateMovieDurationFields(true);
        }
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(true);
        }
      }, 100);"
    )
  })
  
  # Handle status changes to update watched duration and episode indicator (Add Modal)
  observeEvent(input$modal_status, {
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof updateMovieDurationFields === 'function') {
          updateMovieDurationFields(false);
        }
        if (typeof updateTVSeriesFields === 'function') {
          updateTVSeriesFields(false);
        }
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(false);
        }
      }, 100);"
    )
    
    # Also sync movie duration if status is Watched
    if (!is.null(input$modal_status) && input$modal_status == "Watched" &&
        !is.null(input$modal_media_type) && input$modal_media_type == "Movie") {
      # Sync watched duration with total duration
      if (!is.null(input$modal_total_duration) && input$modal_total_duration > 0) {
        updateNumericInput(session, "modal_watched_duration", value = input$modal_total_duration)
      }
    }
  })
  
  # Handle status changes to update watched duration and episode indicator (Edit Modal)
  observeEvent(input$edit_modal_status, {
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof updateMovieDurationFields === 'function') {
          updateMovieDurationFields(true);
        }
        if (typeof updateTVSeriesFields === 'function') {
          updateTVSeriesFields(true);
        }
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(true);
        }
      }, 100);"
    )
    
    # Also sync movie duration if status is Watched
    if (!is.null(input$edit_modal_status) && input$edit_modal_status == "Watched" &&
        !is.null(input$edit_modal_media_type) && input$edit_modal_media_type == "Movie") {
      # Sync watched duration with total duration
      if (!is.null(input$edit_modal_total_duration) && input$edit_modal_total_duration > 0) {
        updateNumericInput(session, "edit_modal_watched_duration", value = input$edit_modal_total_duration)
      }
    }
  })
  
  # Handle total duration changes for movies (Add Modal)
  observeEvent(input$modal_total_duration, {
    # If status is Watched and media type is Movie, sync watched duration
    if (!is.null(input$modal_status) && input$modal_status == "Watched" &&
        !is.null(input$modal_media_type) && input$modal_media_type == "Movie" &&
        !is.null(input$modal_total_duration) && input$modal_total_duration > 0) {
      updateNumericInput(session, "modal_watched_duration", value = input$modal_total_duration)
    }
    
    # Update JavaScript
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof syncMovieDuration === 'function') {
          syncMovieDuration(false);
        }
      }, 100);"
    )
  })
  
  # Handle total duration changes for movies (Edit Modal)
  observeEvent(input$edit_modal_total_duration, {
    # If status is Watched and media type is Movie, sync watched duration
    if (!is.null(input$edit_modal_status) && input$edit_modal_status == "Watched" &&
        !is.null(input$edit_modal_media_type) && input$edit_modal_media_type == "Movie" &&
        !is.null(input$edit_modal_total_duration) && input$edit_modal_total_duration > 0) {
      updateNumericInput(session, "edit_modal_watched_duration", value = input$edit_modal_total_duration)
    }
    
    # Update JavaScript
    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function() { 
        if (typeof syncMovieDuration === 'function') {
          syncMovieDuration(true);
        }
      }, 100);"
    )
  })
  
  # Add Modal
  output$add_modal <- renderUI({
    if (!rv$show_modal) return(NULL)
    
    tags$div(class = "modal-overlay",
             tags$div(class = "modal-box",
                      tags$div(class = "modal-header",
                               tags$h2(class = "modal-title", "Add New Movie/TV Series"),
                               tags$div(class = "modal-close-container",
                                        tags$button(class = "modal-close", "×", 
                                                    onclick = "Shiny.setInputValue('close_add_modal', Math.random());")
                               )
                      ),
                      tags$div(class = "modal-body",
                               # Hidden input to store rating value without triggering reactive updates
                               tags$input(type = "hidden", id = "modal_rating_value", value = "0"),
                               
                               # Title and Media Type on same row - UPDATED with TMDB search
                               tags$div(class = "form-row",
                                        tags$div(class = "form-group half", style = "position: relative;",
                                                 tags$label(class = "form-label", "Title ", tags$span(class = "required", "*")),
                                                 tags$input(type = "text", 
                                                            id = "modal_title", 
                                                            class = "form-control",
                                                            placeholder = "Search for a movie or TV series...",
                                                            style = "width: 100%;",
                                                            oninput = "handleTitleSearch(event, false);"),
                                                 tags$div(id = "tmdb-suggestions", class = "tmdb-suggestions-container")
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Media Type ", tags$span(class = "required", "*")),
                                                 selectizeInput("modal_media_type", NULL,
                                                                choices = c("Movie", "TV Series"),
                                                                selected = rv$modal_media_type,
                                                                options = list(placeholder = "Select media type"))
                                        )
                               ),
                               
                               # Watch Status and Genre
                               tags$div(class = "form-row",
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Watch Status ", tags$span(class = "required", "*")),
                                                 selectizeInput("modal_status", NULL,
                                                                choices = c("Unwatched", "Watching", "Watched"),
                                                                selected = rv$modal_status,
                                                                options = list(placeholder = "Select watch status"))
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Genres ", tags$span(class = "required", "*")),
                                                 selectizeInput("modal_genre", NULL,
                                                                choices = c("Action", "Adventure", "Animation", "Comedy", "Crime",
                                                                            "Documentary", "Drama", "Family", "Fantasy", "History",
                                                                            "Horror", "Music", "Mystery", "Romance", "Science Fiction",
                                                                            "Thriller", "War", "Western"),
                                                                multiple = TRUE, options = list(placeholder = "Select genres"))
                                        )
                               ),
                               
                               # Year and Director/Creator
                               tags$div(class = "form-row",
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Year"),
                                                 numericInput("modal_year", NULL, value = year(Sys.Date()), min = 1900, max = year(Sys.Date()) + 10,
                                                              width = "100%")
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Director / Creator"),
                                                 textInput("modal_director", NULL, placeholder = "", width = "100%")
                                        )
                               ),
                               
                               # Duration fields for Movies (conditional)
                               tags$div(class = "form-row duration-field", style = "display: none;",
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Movie Duration (minutes) ", tags$span(class = "required", "*")),
                                                 numericInput("modal_total_duration", NULL, 
                                                              value = 0, min = 1, max = 600, step = 1,
                                                              width = "100%")
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Watched Duration (minutes) ", tags$span(class = "required", "*")),
                                                 numericInput("modal_watched_duration", NULL, 
                                                              value = 0, min = 0, max = 600, step = 1,
                                                              width = "100%")
                                        )
                               ),
                               
                               # TV Series tracking fields (conditional)
                               tags$div(class = "tv-series-fields", style = "display: none;",
                                        # Total Seasons and Current Season
                                        tags$div(class = "tv-series-row",
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Total Seasons ", tags$span(class = "required", "*")),
                                                          numericInput("modal_total_seasons", NULL, 
                                                                       value = 1, min = 1, max = 100, step = 1,
                                                                       width = "100%")
                                                 ),
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Current Season ", tags$span(class = "required", "*")),
                                                          numericInput("modal_current_season", NULL, 
                                                                       value = 0, min = 0, max = 100, step = 1,
                                                                       width = "100%")
                                                 )
                                        ),
                                        
                                        # Episodes in Current Season and Current Episode
                                        tags$div(class = "tv-series-row",
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Episodes in Current Season ", tags$span(class = "required", "*")),
                                                          numericInput("modal_episodes_current_season", NULL, 
                                                                       value = 1, min = 1, max = 100, step = 1,
                                                                       width = "100%")
                                                 ),
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Current Episode ", tags$span(class = "required", "*")),
                                                          numericInput("modal_current_episode", NULL, 
                                                                       value = 0, min = 0, max = 100, step = 1,
                                                                       width = "100%")
                                                 )
                                        ),
                                        
                                        # Episode indicator
                                        tags$div(class = "form-row",
                                                 tags$div(class = "form-group full-width",
                                                          tags$div(id = "episode-indicator", 
                                                                   class = "episode-indicator",
                                                                   style = "display: none;",
                                                                   "")
                                                 )
                                        ),
                                        
                                        # Total Episodes and Total Episodes Watched
                                        tags$div(class = "tv-series-row",
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Total Episodes ", tags$span(class = "required", "*")),
                                                          numericInput("modal_total_episodes", NULL, 
                                                                       value = 1, min = 1, max = 1000, step = 1,
                                                                       width = "100%")
                                                 ),
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Total Episodes Watched ", tags$span(class = "required", "*")),
                                                          numericInput("modal_total_episodes_watched", NULL, 
                                                                       value = 0, min = 0, max = 1000, step = 1,
                                                                       width = "100%")
                                                 )
                                        )
                               ),
                               
                               # Cast
                               tags$div(class = "form-group full-width",
                                        tags$label(class = "form-label", "Cast"),
                                        textAreaInput("modal_cast", NULL, 
                                                      placeholder = "Enter cast members separated by commas", 
                                                      rows = 3,
                                                      width = "100%")
                               ),
                               
                               # Plot Summary
                               tags$div(class = "form-group full-width",
                                        tags$label(class = "form-label", "Plot Summary"),
                                        textAreaInput("modal_plot_summary", NULL, 
                                                      placeholder = "Enter a brief plot summary...", 
                                                      rows = 4,
                                                      width = "100%")
                               ),
                               
                               # Poster URL
                               tags$div(class = "form-group full-width",
                                        tags$label(class = "form-label", "Poster URL"),
                                        textInput("modal_poster", NULL, 
                                                  placeholder = "https://example.com/poster.jpg",
                                                  width = "100%")
                               ),
                               
                               # Your Rating CONDITIONAL: Only visible when Watch Status is "Watched"
                               tags$div(class = "form-group full-width rating-field", style = "display: none;",
                                        tags$label(class = "form-label", "Your Rating"),
                                        tags$div(class = "star-rating-input",
                                                 lapply(1:5, function(i) {
                                                   tags$span(class = "star",
                                                             onclick = sprintf("setRating(%d, false);", i),
                                                             "★")
                                                 })
                                        )
                               ),
                               
                               # Review/Notes CONDITIONAL: Only visible when Watch Status is "Watched"
                               tags$div(class = "form-group full-width review-field", style = "display: none;",
                                        tags$label(class = "form-label", "Review / Notes"),
                                        textAreaInput("modal_review", NULL, 
                                                      placeholder = "Share your thoughts...", 
                                                      rows = 3,
                                                      width = "100%")
                               ),
                               
                               tags$button(class = "btn-primary", 
                                           "Add to Library")
                      )
             )
    )
  })
  
  # Edit Modal
  output$edit_modal <- renderUI({
    if (!rv$show_edit_modal || is.null(rv$editing_item)) return(NULL)
    
    item <- rv$editing_item
    
    tags$div(class = "modal-overlay",
             tags$div(class = "modal-box edit-modal",
                      tags$div(class = "modal-header",
                               tags$h2(class = "modal-title", "Edit Item"),
                               tags$div(class = "modal-close-container",
                                        tags$button(class = "modal-close", "×", 
                                                    onclick = "Shiny.setInputValue('close_edit_modal', Math.random());")
                               )
                      ),
                      tags$div(class = "modal-body",
                               # Hidden input to store rating value without triggering reactive updates
                               tags$input(type = "hidden", id = "edit_modal_rating_value", value = item$rating),
                               
                               # Title and Media Type on same row - UPDATED with TMDB search
                               tags$div(class = "form-row",
                                        tags$div(class = "form-group half", style = "position: relative;",
                                                 tags$label(class = "form-label", "Title ", tags$span(class = "required", "*")),
                                                 tags$input(type = "text", 
                                                            id = "edit_modal_title", 
                                                            class = "form-control",
                                                            placeholder = "Search for a movie or TV series...",
                                                            style = "width: 100%;",
                                                            value = item$title,
                                                            oninput = "handleTitleSearch(event, true);"),
                                                 tags$div(id = "edit_tmdb-suggestions", class = "tmdb-suggestions-container")
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Media Type ", tags$span(class = "required", "*")),
                                                 selectizeInput("edit_modal_media_type", NULL,
                                                                choices = c("Movie", "TV Series"),
                                                                selected = item$media_type,
                                                                options = list(placeholder = "Select media type"))
                                        )
                               ),
                               
                               # Watch Status and Genre
                               tags$div(class = "form-row",
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Watch Status ", tags$span(class = "required", "*")),
                                                 selectizeInput("edit_modal_status", NULL,
                                                                choices = c("Unwatched", "Watching", "Watched"),
                                                                selected = item$watch_status,
                                                                options = list(placeholder = "Select watch status"))
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Genres ", tags$span(class = "required", "*")),
                                                 selectizeInput("edit_modal_genre", NULL,
                                                                choices = c("Action", "Adventure", "Animation", "Comedy", "Crime",
                                                                            "Documentary", "Drama", "Family", "Fantasy", "History",
                                                                            "Horror", "Music", "Mystery", "Romance", "Science Fiction",
                                                                            "Thriller", "War", "Western"),
                                                                multiple = TRUE, 
                                                                selected = if (!is.na(item$genre) && item$genre != "") {
                                                                  unlist(strsplit(item$genre, ", "))
                                                                } else character(0),
                                                                options = list(placeholder = "Select genres"))
                                        )
                               ),
                               
                               # Year and Director/Creator
                               tags$div(class = "form-row",
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Year"),
                                                 numericInput("edit_modal_year", NULL, value = item$year, min = 1900, max = year(Sys.Date()) + 10,
                                                              width = "100%")
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Director / Creator"),
                                                 textInput("edit_modal_director", NULL, 
                                                           value = ifelse(is.na(item$director), "", item$director), 
                                                           placeholder = "", width = "100%")
                                        )
                               ),
                               
                               # Duration fields for Movies
                               tags$div(class = "form-row duration-field", style = if(item$media_type == "Movie") "display: flex;" else "display: none;",
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Movie Duration (minutes) ", tags$span(class = "required", "*")),
                                                 numericInput("edit_modal_total_duration", NULL, 
                                                              value = ifelse(is.na(item$total_duration), 0, item$total_duration), 
                                                              min = 1, max = 600, step = 1,
                                                              width = "100%")
                                        ),
                                        tags$div(class = "form-group half",
                                                 tags$label(class = "form-label", "Watched Duration (minutes) ", tags$span(class = "required", "*")),
                                                 numericInput("edit_modal_watched_duration", NULL, 
                                                              value = ifelse(is.na(item$watched_duration), 0, item$watched_duration), 
                                                              min = 0, max = 600, step = 1,
                                                              width = "100%")
                                        )
                               ),
                               
                               # TV Series tracking fields
                               tags$div(class = "tv-series-fields", style = if(item$media_type == "TV Series") "display: block;" else "display: none;",
                                        # Total Seasons and Current Season
                                        tags$div(class = "tv-series-row",
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Total Seasons ", tags$span(class = "required", "*")),
                                                          numericInput("edit_modal_total_seasons", NULL, 
                                                                       value = ifelse(is.na(item$total_seasons), 1, item$total_seasons), 
                                                                       min = 1, max = 100, step = 1,
                                                                       width = "100%")
                                                 ),
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Current Season ", tags$span(class = "required", "*")),
                                                          numericInput("edit_modal_current_season", NULL, 
                                                                       value = ifelse(is.na(item$current_season), 0, item$current_season), 
                                                                       min = 0, max = 100, step = 1,
                                                                       width = "100%")
                                                 )
                                        ),
                                        
                                        # Episodes in Current Season and Current Episode
                                        tags$div(class = "tv-series-row",
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Episodes in Current Season ", tags$span(class = "required", "*")),
                                                          numericInput("edit_modal_episodes_current_season", NULL, 
                                                                       value = ifelse(is.na(item$episodes_current_season), 1, item$episodes_current_season), 
                                                                       min = 1, max = 100, step = 1,
                                                                       width = "100%")
                                                 ),
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Current Episode ", tags$span(class = "required", "*")),
                                                          numericInput("edit_modal_current_episode", NULL, 
                                                                       value = ifelse(is.na(item$current_episode), 0, item$current_episode), 
                                                                       min = 0, max = 100, step = 1,
                                                                       width = "100%")
                                                 )
                                        ),
                                        
                                        # Episode indicator
                                        tags$div(class = "form-row",
                                                 tags$div(class = "form-group full-width",
                                                          tags$div(id = "edit_episode-indicator", 
                                                                   class = "episode-indicator",
                                                                   style = "display: none;",
                                                                   "")
                                                 )
                                        ),
                                        
                                        # Total Episodes and Total Episodes Watched
                                        tags$div(class = "tv-series-row",
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Total Episodes ", tags$span(class = "required", "*")),
                                                          numericInput("edit_modal_total_episodes", NULL, 
                                                                       value = ifelse(is.na(item$total_episodes), 1, item$total_episodes), 
                                                                       min = 1, max = 1000, step = 1,
                                                                       width = "100%")
                                                 ),
                                                 tags$div(class = "form-group half",
                                                          tags$label(class = "form-label", "Total Episodes Watched ", tags$span(class = "required", "*")),
                                                          numericInput("edit_modal_total_episodes_watched", NULL, 
                                                                       value = ifelse(is.na(item$total_episodes_watched), 0, item$total_episodes_watched), 
                                                                       min = 0, max = 1000, step = 1,
                                                                       width = "100%")
                                                 )
                                        )
                               ),
                               
                               # Cast
                               tags$div(class = "form-group full-width",
                                        tags$label(class = "form-label", "Cast"),
                                        textAreaInput("edit_modal_cast", NULL, 
                                                      value = ifelse(is.na(item$cast), "", item$cast),
                                                      placeholder = "Enter cast members separated by commas", 
                                                      rows = 3,
                                                      width = "100%")
                               ),
                               
                               # Plot Summary
                               tags$div(class = "form-group full-width",
                                        tags$label(class = "form-label", "Plot Summary"),
                                        textAreaInput("edit_modal_plot_summary", NULL, 
                                                      value = ifelse(is.na(item$plot_summary), "", item$plot_summary),
                                                      placeholder = "Enter a brief plot summary...", 
                                                      rows = 4,
                                                      width = "100%")
                               ),
                               
                               # Poster URL
                               tags$div(class = "form-group full-width",
                                        tags$label(class = "form-label", "Poster URL"),
                                        textInput("edit_modal_poster", NULL, 
                                                  value = ifelse(is.na(item$poster_url), "", item$poster_url),
                                                  placeholder = "https://example.com/poster.jpg",
                                                  width = "100%")
                               ),
                               
                               # Your Rating CONDITIONAL: Only visible when Watch Status is "Watched"
                               tags$div(class = "form-group full-width rating-field", 
                                        style = if(item$watch_status == "Watched") "display: block;" else "display: none;",
                                        tags$label(class = "form-label", "Your Rating"),
                                        tags$div(class = "star-rating-input",
                                                 lapply(1:5, function(i) {
                                                   tags$span(class = "star",
                                                             onclick = sprintf("setRating(%d, true);", i),
                                                             "★")
                                                 })
                                        )
                               ),
                               
                               # Review/Notes CONDITIONAL: Only visible when Watch Status is "Watched"
                               tags$div(class = "form-group full-width review-field", 
                                        style = if(item$watch_status == "Watched") "display: block;" else "display: none;",
                                        tags$label(class = "form-label", "Review / Notes"),
                                        textAreaInput("edit_modal_review", NULL, 
                                                      value = ifelse(is.na(item$review), "", item$review),
                                                      placeholder = "Share your thoughts...", 
                                                      rows = 3,
                                                      width = "100%")
                               ),
                               
                               tags$button(class = "btn-primary", 
                                           "Save Changes")
                      )
             )
    )
  })
  
  # Delete Confirmation Modal
  output$delete_modal <- renderUI({
    if (!rv$show_delete_modal || is.null(rv$deleting_item)) return(NULL)
    
    item <- rv$deleting_item
    
    tags$div(class = "modal-overlay",
             tags$div(class = "modal-box confirm-delete-modal",
                      tags$div(class = "modal-header",
                               tags$h2(class = "modal-title", "Delete Confirmation"),
                               tags$div(class = "modal-close-container",
                                        tags$button(class = "modal-close", "×", 
                                                    onclick = "Shiny.setInputValue('close_delete_modal', Math.random());")
                               )
                      ),
                      tags$div(class = "modal-body",
                               tags$div(class = "delete-warning",
                                        tags$i(class = "fas fa-exclamation-triangle", style = "font-size: 3rem; color: #EF476F; margin-bottom: 1rem;"),
                                        tags$h3(style = "color: #FFF; margin-bottom: 1rem;", "Are you sure?"),
                                        tags$p(style = "color: #9BA3B0; text-align: center; line-height: 1.5;",
                                               paste0("You are about to delete \"", item$title, "\" from your library.")),
                                        tags$p(style = "color: #EF476F; font-weight: 600; margin-top: 1rem;",
                                               "This action cannot be undone!")
                               ),
                               
                               tags$div(class = "delete-item-info",
                                        tags$div(class = "delete-item-row",
                                                 tags$span(style = "color: #9BA3B0;", "Media Type:"),
                                                 tags$span(style = "color: #FFF; font-weight: 600;", item$media_type)
                                        ),
                                        tags$div(class = "delete-item-row",
                                                 tags$span(style = "color: #9BA3B0;", "Genre:"),
                                                 tags$span(style = "color: #FFF; font-weight: 600;", item$genre)
                                        ),
                                        tags$div(class = "delete-item-row",
                                                 tags$span(style = "color: #9BA3B0;", "Status:"),
                                                 tags$span(style = "color: #FFF; font-weight: 600;", item$watch_status)
                                        ),
                                        tags$div(class = "delete-item-row",
                                                 tags$span(style = "color: #9BA3B0;", "Added:"),
                                                 tags$span(style = "color: #FFF; font-weight: 600;", 
                                                           format(as.Date(item$date_added), "%B %d, %Y"))
                                        )
                               ),
                               
                               tags$div(class = "delete-actions",
                                        tags$button(class = "btn-secondary", 
                                                    onclick = "Shiny.setInputValue('close_delete_modal', Math.random());",
                                                    "Cancel"),
                                        tags$button(class = "btn-danger", 
                                                    onclick = "Shiny.setInputValue('confirm_delete', Math.random());",
                                                    "Delete Permanently")
                               )
                      )
             )
    )
  })
  
  # Details Modal
  output$details_modal <- renderUI({
    if (!rv$show_details_modal || is.null(rv$selected_item)) return(NULL)
    
    item <- rv$selected_item
    
    # Calculate progress for display
    if (item$media_type == "TV Series" && !is.na(item$total_episodes) && item$total_episodes > 0) {
      progress <- round((item$total_episodes_watched / item$total_episodes) * 100)
      progress_text <- paste0("S", item$current_season, "E", item$current_episode, " (", 
                              item$total_episodes_watched, "/", item$total_episodes, " eps)")
      progress_color <- if (progress < 26) "#EF476F" else if (progress < 76) "#FFB800" else "#06D6A0"
    } else if (item$media_type == "Movie" && !is.na(item$total_duration) && item$total_duration > 0) {
      progress <- round((item$watched_duration / item$total_duration) * 100)
      total_duration_str <- if (item$total_duration >= 60) {
        hours <- floor(item$total_duration / 60)
        mins <- item$total_duration %% 60
        paste0(hours, "h ", mins, "m")
      } else {
        paste0(item$total_duration, "m")
      }
      watched_duration_str <- if (item$watched_duration >= 60) {
        hours <- floor(item$watched_duration / 60)
        mins <- item$watched_duration %% 60
        paste0(hours, "h ", mins, "m")
      } else {
        paste0(item$watched_duration, "m")
      }
      progress_text <- paste0(watched_duration_str, " / ", total_duration_str)
      progress_color <- if (progress < 26) "#EF476F" else if (progress < 76) "#FFB800" else "#06D6A0"
    } else {
      progress <- 0
      progress_text <- "No progress data"
      progress_color <- "#9BA3B0"
    }
    
    # Format date added
    date_added <- tryCatch({
      if (!is.na(item$date_added) && !is.null(item$date_added)) {
        format(as.Date(item$date_added), "%B %d, %Y")
      } else {
        "Unknown"
      }
    }, error = function(e) {
      "Unknown"
    })
    
    # Format date watched
    date_watched <- tryCatch({
      if (item$watch_status == "Watched") {
        watch_date <- get_watch_date(item$id)
        
        if (!is.null(watch_date) && !is.na(watch_date)) {
          format(as.Date(watch_date), "%B %d, %Y")
        } else {
          if (!is.na(item$date_watched) && item$date_watched != "") {
            format(as.Date(item$date_watched), "%B %d, %Y")
          } else {
            "Unknown date"
          }
        }
      } else {
        "Not watched yet"
      }
    }, error = function(e) {
      "Unknown date"
    })
    
    # Rating stars
    rating_value <- ifelse(is.na(item$rating), 0, item$rating)
    filled_stars <- if (rating_value > 0) paste(rep("★", rating_value), collapse = "") else ""
    empty_stars <- if (rating_value < 5) paste(rep("☆", 5 - rating_value), collapse = "") else ""
    
    tags$div(class = "modal-overlay",
             tags$div(class = "modal-box details-modal",
                      tags$div(class = "modal-header",
                               tags$h2(class = "modal-title", item$title),
                               tags$div(class = "modal-close-container",
                                        tags$button(class = "modal-close", "×", 
                                                    onclick = "Shiny.setInputValue('close_details_modal', Math.random());")
                               )
                      ),
                      tags$div(class = "modal-body",
                               # Poster and basic info
                               tags$div(class = "details-row",
                                        tags$div(class = "details-poster",
                                                 tags$img(src = if (!is.na(item$poster_url) && item$poster_url != "") 
                                                   item$poster_url 
                                                   else "https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster",
                                                   alt = item$title)
                                        ),
                                        tags$div(class = "details-info",
                                                 # Status badge, edit button, and delete button
                                                 tags$div(class = "details-status-row",
                                                          tags$div(class = paste("details-badge", 
                                                                                 switch(item$watch_status,
                                                                                        "Watched" = "badge-watched",
                                                                                        "Watching" = "badge-watching",
                                                                                        "Unwatched" = "badge-unwatched")),
                                                                   toupper(item$watch_status)),
                                                          tags$div(class = "details-action-buttons",
                                                                   tags$button(class = "edit-button",
                                                                               onclick = sprintf("openEditModal(%d);", item$id),
                                                                               tags$i(class = "fas fa-edit"),
                                                                               "Edit"),
                                                                   tags$button(class = "delete-button",
                                                                               onclick = sprintf("openDeleteModal(%d);", item$id),
                                                                               tags$i(class = "fas fa-trash"),
                                                                               "Delete")
                                                          )
                                                 ),
                                                 
                                                 # Media type and year
                                                 tags$div(class = "details-meta",
                                                          tags$span(class = "media-type", item$media_type),
                                                          tags$span("•"),
                                                          tags$span(class = "year", item$year),
                                                          tags$span("•"),
                                                          tags$span(class = "genre", item$genre)
                                                 ),
                                                 
                                                 # Director/Creator
                                                 if (!is.na(item$director) && item$director != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Director/Creator:"),
                                                            tags$span(class = "field-value", item$director)
                                                   )
                                                 },
                                                 
                                                 # Cast
                                                 if (!is.na(item$cast) && item$cast != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Cast:"),
                                                            tags$span(class = "field-value", 
                                                                      if (nchar(item$cast) > 100) 
                                                                        paste0(substr(item$cast, 1, 100), "...") 
                                                                      else item$cast)
                                                   )
                                                 },
                                                 
                                                 # Dates and Rating
                                                 tags$div(class = "details-field",
                                                          tags$span(class = "field-label", "Date Added:"),
                                                          tags$span(class = "field-value", date_added)
                                                 ),
                                                 
                                                 if (item$watch_status == "Watched") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Date Watched:"),
                                                            tags$span(class = "field-value", date_watched)
                                                   )
                                                 },
                                                 
                                                 # Plot summary 
                                                 if (!is.na(item$plot_summary) && item$plot_summary != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Plot Summary:"),
                                                            tags$span(class = "field-value", item$plot_summary)
                                                   )
                                                 },
                                                 
                                                 if (progress > 0) {
                                                   # Progress 
                                                   tags$div(class = "details-field progress-field",
                                                            tags$span(class = "field-label", "Progress:"),
                                                            tags$span(class = "field-value progress-value",
                                                                      tags$span(class = "progress-text", progress_text),
                                                                      tags$div(class = "progress-bg",
                                                                               tags$div(class = "progress-fill", 
                                                                                        style = paste0("width: ", progress, "%; background: ", progress_color, ";"))
                                                                      )
                                                            )
                                                   )
                                                 }
                                        )
                               ),
                               
                               tags$div(style = "border-bottom: 1px solid rgba(255, 255, 255, 0.15); margin: 1.5rem 0; width: 100%;"),
                               
                               # ==============================================================================
                               # Rating display
                               # ==============================================================================
                               
                               if (rating_value > 0) {
                                 tags$div(class = "details-section rating-section-top",
                                          style = "padding-top: -0.5rem;",
                                          tags$h3(class = "section-title", "Your Rating"),
                                          tags$div(class = "details-rating",
                                                   tags$div(class = "star-rating-display",
                                                            tags$span(style = "color: #FFB800;", filled_stars),
                                                            tags$span(style = "color: rgba(155, 163, 176, 0.4);", empty_stars),
                                                            tags$span(class = "rating-value", rating_value)
                                                   )
                                          )
                                 )
                               },
                               
                               # ==============================================================================
                               # Your Review section
                               # ==============================================================================
                               
                               if (item$watch_status == "Watched" && !is.na(item$review) && item$review != "") {
                                 tags$div(class = "details-section review-section-middle",
                                          tags$h3(class = "section-title", "Your Review"),
                                          tags$p(class = "review", item$review)
                                 )
                               },
                               
                               # ==============================================================================
                               # TV Series and Movie details
                               # ==============================================================================
                               
                               # TV Series specific details
                               if (item$media_type == "TV Series") {
                                 tags$div(class = "details-section",
                                          tags$h3(class = "section-title", "TV Series Details"),
                                          tags$div(class = "tv-details-grid",
                                                   tags$div(class = "tv-detail",
                                                            tags$span(class = "detail-label", "Total Seasons:"),
                                                            tags$span(class = "detail-value", item$total_seasons)
                                                   ),
                                                   tags$div(class = "tv-detail",
                                                            tags$span(class = "detail-label", "Current Season:"),
                                                            tags$span(class = "detail-value", item$current_season)
                                                   ),
                                                   tags$div(class = "tv-detail",
                                                            tags$span(class = "detail-label", "Episodes in Current Season:"),
                                                            tags$span(class = "detail-value", item$episodes_current_season)
                                                   ),
                                                   tags$div(class = "tv-detail",
                                                            tags$span(class = "detail-label", "Current Episode:"),
                                                            tags$span(class = "detail-value", item$current_episode)
                                                   ),
                                                   tags$div(class = "tv-detail",
                                                            tags$span(class = "detail-label", "Total Episodes:"),
                                                            tags$span(class = "detail-value", item$total_episodes)
                                                   ),
                                                   tags$div(class = "tv-detail",
                                                            tags$span(class = "detail-label", "Total Episodes Watched:"),
                                                            tags$span(class = "detail-value", item$total_episodes_watched)
                                                   )
                                          )
                                 )
                               },
                               
                               # Movie specific details
                               if (item$media_type == "Movie") {
                                 tags$div(class = "details-section",
                                          tags$h3(class = "section-title", "Movie Details"),
                                          tags$div(class = "movie-details-grid",
                                                   tags$div(class = "movie-detail",
                                                            tags$span(class = "detail-label", "Total Duration:"),
                                                            tags$span(class = "detail-value", 
                                                                      if (!is.na(item$total_duration) && item$total_duration > 0) {
                                                                        if (item$total_duration >= 60) {
                                                                          hours <- floor(item$total_duration / 60)
                                                                          mins <- item$total_duration %% 60
                                                                          paste0(hours, "h ", mins, "m")
                                                                        } else {
                                                                          paste0(item$total_duration, "m")
                                                                        }
                                                                      } else "N/A")
                                                   ),
                                                   tags$div(class = "movie-detail",
                                                            tags$span(class = "detail-label", "Watched Duration:"),
                                                            tags$span(class = "detail-value",
                                                                      if (!is.na(item$watched_duration) && item$watched_duration > 0) {
                                                                        if (item$watched_duration >= 60) {
                                                                          hours <- floor(item$watched_duration / 60)
                                                                          mins <- item$watched_duration %% 60
                                                                          paste0(hours, "h ", mins, "m")
                                                                        } else {
                                                                          paste0(item$watched_duration, "m")
                                                                        }
                                                                      } else "0m")
                                                   ),
                                                   tags$div(class = "movie-detail",
                                                            tags$span(class = "detail-label", "Completion:"),
                                                            tags$span(class = "detail-value", 
                                                                      if (!is.na(item$total_duration) && item$total_duration > 0) {
                                                                        paste0(round((item$watched_duration / item$total_duration) * 100), "%")
                                                                      } else "N/A")
                                                   )
                                          )
                                 )
                               },
                               
                               # ==============================================================================
                               # COLLECTION SECTION - ALWAYS SHOW IF AVAILABLE
                               # ==============================================================================
                               
                               # Check if collection data exists and has items
                               if (!is.null(rv$collection_data) && 
                                   !is.null(rv$collection_data$items) && 
                                   length(rv$collection_data$items) > 0) {
                                 
                                 # Get collection name or use default
                                 collection_name <- if (!is.null(rv$collection_data$collection_name) && 
                                                        rv$collection_data$collection_name != "") {
                                   rv$collection_data$collection_name
                                 } else {
                                   if (rv$selected_item$media_type == "Movie") {
                                     "Related Movies"
                                   } else {
                                     "Similar TV Series"
                                   }
                                 }
                                 
                                 tagList(
                                   tags$div(class = "details-section", style = "margin-top: 2rem;",
                                            tags$h3(class = "section-title", 
                                                    tags$i(class = "fas fa-film", style = "margin-right: 0.5rem; color: #00A8E8;"),
                                                    collection_name),
                                            
                                            tags$div(class = "collection-scroll-container",
                                                     style = "display: flex; gap: 1.5rem; overflow-x: auto; padding: 1.5rem 0; margin-top: 1rem;",
                                                     lapply(rv$collection_data$items, function(col_item) {
                                                       # Calculate if item exists in library
                                                       con <- get_db_connection()
                                                       item_exists <- FALSE
                                                       watch_status <- NULL
                                                       if (!is.null(con)) {
                                                         tryCatch({
                                                           check_query <- sprintf("SELECT id, watch_status FROM movies_series WHERE tmdb_id = %d", col_item$tmdb_id)
                                                           existing <- dbGetQuery(con, check_query)
                                                           dbDisconnect(con)
                                                           if (nrow(existing) > 0) {
                                                             item_exists <- TRUE
                                                             watch_status <- existing$watch_status[1]
                                                             status_class <- switch(watch_status,
                                                                                    "Watched" = "badge-watched",
                                                                                    "Watching" = "badge-watching",
                                                                                    "Unwatched" = "badge-unwatched")
                                                           }
                                                         }, error = function(e) {
                                                           if (!is.null(con)) dbDisconnect(con)
                                                         })
                                                       }
                                                       
                                                       tags$div(class = "movie-card collection-movie-card",
                                                                style = "min-width: 200px; flex-shrink: 0; cursor: pointer;",
                                                                `data-tmdb-id` = col_item$tmdb_id,
                                                                onclick = sprintf("
                                   Shiny.setInputValue('show_collection_item', JSON.stringify({
                                     tmdb_id: %d,
                                     media_type: '%s',
                                     title: '%s'
                                   }), {priority: 'event'});
                                 ", 
                                                                                  col_item$tmdb_id,
                                                                                  rv$selected_item$media_type,
                                                                                  gsub("'", "\\\\'", col_item$title)),
                                                                
                                                                tags$img(class = "movie-poster", 
                                                                         src = if (!is.null(col_item$poster) && col_item$poster != "") 
                                                                           col_item$poster 
                                                                         else "https://via.placeholder.com/200x300/1A1F29/00A8E8?text=No+Poster",
                                                                         onerror = "this.src='https://via.placeholder.com/200x300/1A1F29/00A8E8?text=No+Poster';"),
                                                                
                                                                if (item_exists) {
                                                                  tags$div(class = paste("movie-badge", status_class), 
                                                                           toupper(watch_status))
                                                                } else {
                                                                  tags$div(class = "movie-badge badge-info",
                                                                           title = "Recommended for you",
                                                                           tags$img(src = "info.png", 
                                                                                    alt = "Recommended",
                                                                                    style = "width: 16px; height: 16px; filter: invert(60%) sepia(100%) saturate(500%) hue-rotate(180deg);"))
                                                                },
                                                                
                                                                tags$div(class = "movie-info",
                                                                         tags$h3(class = "movie-title", 
                                                                                 if (!is.null(col_item$title)) col_item$title else "Unknown Title"),
                                                                         tags$div(class = "movie-meta",
                                                                                  if (!is.null(col_item$year) && col_item$year != "N/A") {
                                                                                    tags$span(col_item$year)
                                                                                  },
                                                                                  if (!is.null(col_item$year) && col_item$year != "N/A") {
                                                                                    tags$span("•")
                                                                                  },
                                                                                  tags$span(rv$selected_item$media_type)
                                                                         ),
                                                                         # TMDB Rating
                                                                         if (!is.null(col_item$rating) && col_item$rating > 0) {
                                                                           tags$div(class = "movie-rating",
                                                                                    tags$span("⭐"),
                                                                                    tags$span(col_item$rating)
                                                                           )
                                                                         }
                                                                )
                                                       )
                                                     })
                                            )
                                   )
                                 )
                               } else if (!is.null(rv$selected_item) && 
                                          !is.na(rv$selected_item$tmdb_id) && 
                                          rv$selected_item$tmdb_id > 0) {
                                 # Show a message if we have a TMDB ID but no collection data
                                 tags$div(class = "details-section", style = "margin-top: 2rem;",
                                          tags$h3(class = "section-title", 
                                                  tags$i(class = "fas fa-film", style = "margin-right: 0.5rem; color: #00A8E8;"),
                                                  "Collection"),
                                          tags$p(style = "color: #9BA3B0; font-style: italic; text-align: center; padding: 1rem;",
                                                 "No collection or similar items found for this title.")
                                 )
                               }
                      )
             )
    )
  })
  
  # Recommendation Details Modal
  output$rec_details_modal <- renderUI({
    if (!rv$show_rec_details_modal || is.null(rv$rec_details)) return(NULL)
    
    details <- rv$rec_details
    
    # Format duration for movies
    duration_str <- if (details$media_type == "Movie" && details$duration > 0) {
      if (details$duration >= 60) {
        hours <- floor(details$duration / 60)
        mins <- details$duration %% 60
        paste0(hours, "h ", mins, "m")
      } else {
        paste0(details$duration, "m")
      }
    } else ""
    
    # Format TMDB rating stars
    tmdb_rating <- if (!is.null(details$rating)) details$rating else 0
    tmdb_stars <- if (tmdb_rating > 0) {
      stars_count <- round(tmdb_rating / 2)  # Convert 10-point scale to 5 stars
      filled_stars <- paste(rep("★", stars_count), collapse = "")
      empty_stars <- paste(rep("☆", 5 - stars_count), collapse = "")
      list(filled = filled_stars, empty = empty_stars, value = tmdb_rating)
    } else {
      list(filled = "", empty = "☆☆☆☆☆", value = 0)
    }
    
    tags$div(class = "modal-overlay",
             tags$div(class = "modal-box rec-details-modal",
                      tags$div(class = "modal-header",
                               tags$h2(class = "modal-title", details$title),
                               tags$div(class = "modal-close-container",
                                        tags$button(class = "modal-close", "×", 
                                                    onclick = "Shiny.setInputValue('close_rec_details_modal', Math.random());")
                               )
                      ),
                      tags$div(class = "modal-body",
                               # Poster and basic info
                               tags$div(class = "details-row",
                                        tags$div(class = "details-poster",
                                                 tags$img(src = if (!is.null(details$poster_url) && details$poster_url != "") 
                                                   details$poster_url 
                                                   else "https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster",
                                                   alt = details$title)
                                        ),
                                        tags$div(class = "details-info",
                                                 # Media type and year
                                                 tags$div(class = "details-meta",
                                                          tags$span(class = "media-type", details$media_type),
                                                          tags$span("•"),
                                                          tags$span(class = "year", details$year),
                                                          tags$span("•"),
                                                          tags$span(class = "genre", details$genres)
                                                 ),
                                                 
                                                 # Tagline (for movies)
                                                 if (!is.null(details$tagline) && details$tagline != "") {
                                                   tags$div(class = "details-tagline",
                                                            tags$p(style = "font-style: italic; color: #9BA3B0; margin-bottom: 1rem;",
                                                                   paste0('"', details$tagline, '"'))
                                                   )
                                                 },
                                                 
                                                 # TMDB Rating
                                                 tags$div(class = "details-field",
                                                          tags$span(class = "field-label", "TMDB Rating:"),
                                                          tags$span(class = "field-value",
                                                                    tags$span(style = "color: #FFB800;", tmdb_stars$filled),
                                                                    tags$span(style = "color: rgba(155, 163, 176, 0.4);", tmdb_stars$empty),
                                                                    tags$span(class = "rating-value", tmdb_stars$value, "/10")
                                                          )
                                                 ),
                                                 
                                                 # Popularity
                                                 if (!is.null(details$popularity) && details$popularity > 0) {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Popularity:"),
                                                            tags$span(class = "field-value", 
                                                                      tags$span(style = "color: #06D6A0;", "🔥"),
                                                                      tags$span(style = "margin-left: 5px;", details$popularity))
                                                   )
                                                 },
                                                 
                                                 # Status
                                                 if (!is.null(details$status) && details$status != "N/A") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Status:"),
                                                            tags$span(class = "field-value", details$status)
                                                   )
                                                 },
                                                 
                                                 # Director/Creator
                                                 if (!is.null(details$director) && details$director != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", 
                                                                      if(details$media_type == "Movie") "Director:" else "Creator:"),
                                                            tags$span(class = "field-value", details$director)
                                                   )
                                                 },
                                                 
                                                 # Duration (for movies)
                                                 if (details$media_type == "Movie" && duration_str != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Duration:"),
                                                            tags$span(class = "field-value", duration_str)
                                                   )
                                                 },
                                                 
                                                 # Total seasons and episodes (for TV series)
                                                 if (details$media_type == "TV Series") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Seasons:"),
                                                            tags$span(class = "field-value", 
                                                                      if (!is.null(details$total_seasons) && details$total_seasons > 0) 
                                                                        details$total_seasons else "N/A")
                                                   )
                                                   
                                                   if (!is.null(details$total_episodes) && details$total_episodes > 0) {
                                                     tags$div(class = "details-field",
                                                              tags$span(class = "field-label", "Episodes:"),
                                                              tags$span(class = "field-value", details$total_episodes)
                                                     )
                                                   }
                                                 },
                                                 
                                                 # Cast as basic info field
                                                 if (!is.null(details$cast) && details$cast != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Cast:"),
                                                            tags$span(class = "field-value", details$cast)
                                                   )
                                                 },
                                                 
                                                 # Plot Summary as basic info field
                                                 if (!is.null(details$plot_summary) && details$plot_summary != "") {
                                                   tags$div(class = "details-field",
                                                            tags$span(class = "field-label", "Plot Summary:"),
                                                            tags$span(class = "field-value", details$plot_summary)
                                                   )
                                                 }
                                        )
                               )
                      ),
                      
                      # MODAL FOOTER 
                      tags$div(class = "modal-footer",
                               tags$div(class = "modal-footer-content",
                                        tags$button(class = "btn-primary rec-add-btn",
                                                    onclick = sprintf("
                      Shiny.setInputValue('modal_title', '%s');
                      Shiny.setInputValue('modal_media_type', '%s');
                      Shiny.setInputValue('modal_genre', '%s');
                      Shiny.setInputValue('modal_year', %d);
                      Shiny.setInputValue('modal_director', '%s');
                      Shiny.setInputValue('modal_cast', '%s');
                      Shiny.setInputValue('modal_plot_summary', '%s');
                      Shiny.setInputValue('modal_poster', '%s');
                      Shiny.setInputValue('modal_total_duration', %d);
                      Shiny.setInputValue('modal_total_seasons', %d);
                      Shiny.setInputValue('modal_total_episodes', %d);
                      Shiny.setInputValue('modal_current_season', %d);
                      Shiny.setInputValue('modal_current_episode', %d);
                      Shiny.setInputValue('modal_total_episodes_watched', %d);
                      Shiny.setInputValue('modal_watched_duration', %d);
                      Shiny.setInputValue('modal_status', 'Unwatched');
                      Shiny.setInputValue('modal_rating_value', 0);
                      Shiny.setInputValue('show_add_modal', Math.random());
                      Shiny.setInputValue('close_rec_details_modal', Math.random());
                    ",
                                                                      gsub("'", "\\\\'", details$title),
                                                                      details$media_type,
                                                                      gsub("'", "\\\\'", details$genres),
                                                                      details$year,
                                                                      gsub("'", "\\\\'", details$director),
                                                                      gsub("'", "\\\\'", details$cast),
                                                                      gsub("'", "\\\\'", details$plot_summary),
                                                                      gsub("'", "\\\\'", details$poster_url),
                                                                      if(details$media_type == "Movie") details$duration else 0,
                                                                      if(details$media_type == "TV Series") details$total_seasons else 1,
                                                                      if(details$media_type == "TV Series") details$total_episodes else 1,
                                                                      0,  # current_season set to 0 for Unwatched
                                                                      0,  # current_episode set to 0 for Unwatched
                                                                      0,  # total_episodes_watched set to 0 for Unwatched
                                                                      0), # watched_duration set to 0 for Unwatched
                                                    "Add to Library")
                               )
                      )
             )
    )
  })
  
  # ==============================================================================
  # ACHIEVEMENT MODAL
  # ==============================================================================
  
  output$achievement_modal <- renderUI({
    if (is.null(rv$achievement_to_show)) return(NULL)
    
    achievement <- rv$achievement_to_show$details
    achievement_id <- rv$achievement_to_show$id

    sparkle_js <- "
  setTimeout(function() {

    const badgeContainer = document.querySelector('.achievement-badge-container');
    if (badgeContainer) {
      const colors = ['#FFD700', '#00A8E8', '#06D6A0', '#EF476F', '#FFB800'];
      
      for (let i = 0; i < 25; i++) {
        const particle = document.createElement('div');
        particle.className = 'particle';

        const size = Math.random() * 5 + 3;
        particle.style.width = size + 'px';
        particle.style.height = size + 'px';

        particle.style.left = Math.random() * 100 + '%';
        particle.style.top = Math.random() * 100 + '%';

        particle.style.backgroundColor = colors[Math.floor(Math.random() * colors.length)];

        const duration = 2 + Math.random() * 3;
        const delay = Math.random() * 2;
        particle.style.animation = `particleFloatEnhanced ${duration}s ease-in-out ${delay}s infinite`;
        particle.style.opacity = Math.random() * 0.6 + 0.4;
        particle.style.transform = `scale(${Math.random() * 0.7 + 0.3})`;
        
        badgeContainer.appendChild(particle);
      }

      const style = document.createElement('style');
      style.textContent = `
        @keyframes particleFloatEnhanced {
          0%, 100% { 
            transform: translate(0, 0) rotate(0deg) scale(var(--scale, 1)); 
            opacity: 0.4;
          }
          25% { 
            transform: translate(-15px, -25px) rotate(90deg) scale(var(--scale, 1.2)); 
            opacity: 0.8;
          }
          50% { 
            transform: translate(10px, -40px) rotate(180deg) scale(var(--scale, 0.8)); 
            opacity: 1;
          }
          75% { 
            transform: translate(15px, -20px) rotate(270deg) scale(var(--scale, 1.1)); 
            opacity: 0.6;
          }
        }
      `;
      document.head.appendChild(style);
    }

    try {
      const audioContext = new (window.AudioContext || window.webkitAudioContext)();
      const now = audioContext.currentTime;

      const oscillator = audioContext.createOscillator();
      const gainNode = audioContext.createGain();
      
      oscillator.connect(gainNode);
      gainNode.connect(audioContext.destination);
      
      oscillator.frequency.setValueAtTime(523.25, now); // C5
      oscillator.frequency.exponentialRampToValueAtTime(659.25, now + 0.5); // E5
      
      oscillator.type = 'sine';
      
      gainNode.gain.setValueAtTime(0, now);
      gainNode.gain.linearRampToValueAtTime(0.1, now + 0.1);
      gainNode.gain.exponentialRampToValueAtTime(0.001, now + 1.5);
      
      oscillator.start(now);
      oscillator.stop(now + 1.5);
      
    } catch (e) {

      console.log('Audio not available');
    }

    const modal = document.querySelector('.achievement-modal');
    if (modal) {
      modal.style.animation = 'modalSlideUp 0.6s cubic-bezier(0.34, 1.56, 0.64, 1) forwards, modalShake 0.5s ease 0.6s';

      const shakeStyle = document.createElement('style');
      shakeStyle.textContent = `
        @keyframes modalShake {
          0%, 100% { transform: translateX(0); }
          10%, 30%, 50%, 70%, 90% { transform: translateX(-2px); }
          20%, 40%, 60%, 80% { transform: translateX(2px); }
        }
      `;
      document.head.appendChild(shakeStyle);
    }
    
  }, 150);
  "

    session$sendCustomMessage(type = "eval", message = sparkle_js)
    
    tags$div(class = "achievement-modal-overlay",
             tags$div(class = "achievement-modal",
                      
                      tags$div(class = "achievement-count",
                               paste0(length(rv$achievements_unlocked), "/20")
                      ),
                      
                      tags$div(class = "achievement-modal-tag",
                               tags$span("✨ "),
                               "ACHIEVEMENT UNLOCKED!",
                               tags$span(" ✨")
                      ),
                      
                      tags$div(class = "achievement-badge-container",
                               tags$img(src = achievement$badge_image,
                                        class = "achievement-badge-image",
                                        alt = paste(achievement$name, "Badge"),
                                        onerror = "this.src='https://via.placeholder.com/180/1A1F29/FFD700?text=BADGE';"),

                               tags$div(class = "sparkle sparkle-1"),
                               tags$div(class = "sparkle sparkle-2"),
                               tags$div(class = "sparkle sparkle-3"),
                               tags$div(class = "sparkle sparkle-4"),
                               tags$div(class = "sparkle sparkle-5")
                      ),
                      
                      tags$h1(class = "achievement-modal-title",
                              achievement$name
                      ),
                      
                      tags$p(class = "achievement-modal-subtitle",
                             "A New Milestone Unlocked! 🎯"
                      ),
                      
                      tags$p(class = "achievement-modal-description",
                             achievement$description
                      ),
                      
                      tags$div(class = "achievement-modal-buttons",
                               tags$button(class = "achievement-close-btn",
                                           onclick = "Shiny.setInputValue('close_achievement_modal', Math.random());",
                                           tags$span("Continue Exploring")
                               ),
                               
                               tags$button(class = "achievement-view-btn",
                                           onclick = paste0("Shiny.setInputValue('close_achievement_modal', Math.random()); ",
                                                            "Shiny.setInputValue('nav_achievements', Math.random()); ",
                                                            "Shiny.setInputValue('view_all_achievements', Math.random());"),
                                           tags$span("View All Achievements")
                               )
                      )
             )
    )
  })
  
  # ==============================================================================
  # CLOSE ACHIEVEMENT MODAL HANDLER
  # ==============================================================================
  
  observeEvent(input$close_achievement_modal, {
    rv$achievement_to_show <- NULL
  })
  
  # ==============================================================================
  # HANDLE VIEW ALL ACHIEVEMENTS BUTTON
  # ==============================================================================
  
  observeEvent(input$view_all_achievements, {
    showNotification("Redirecting to Achievements...", type = "message", duration = 2)
  })
  
  # Initialize modal display when opened (Add Modal)
  observeEvent(rv$show_modal, {
    if (rv$show_modal) {
      # Check if this is triggered from recommendations
      is_from_recommendations <- isolate({
        !is.null(input$modal_status) && input$modal_status == "Unwatched"
      })
      
      # Run JavaScript to initialize star rating and modal display
      session$sendCustomMessage(
        type = "eval",
        message = paste0("setTimeout(function() { 
        if (typeof initStarRating === 'function') { 
          initStarRating(false); 
        } 
        if (typeof updateModalDisplay === 'function') { 
          updateModalDisplay(false); 
        } 
        if (typeof updateTVSeriesFields === 'function') { 
          updateTVSeriesFields(false); 
        }
        if (typeof updateMovieDurationFields === 'function') {
          updateMovieDurationFields(false);
        }
        if (typeof updateEpisodeIndicator === 'function') {
          updateEpisodeIndicator(false);
        }
        // Clear any existing TMDB suggestions
        const suggestions = document.getElementById('tmdb-suggestions');
        if (suggestions) {
          suggestions.innerHTML = '';
          suggestions.classList.remove('active');
        }
        // Ensure current season input is editable
        const currentSeasonInput = document.getElementById('modal_current_season');
        if (currentSeasonInput) {
          currentSeasonInput.disabled = false;
        }
        ", 
                         # If coming from recommendations, force Unwatched status
                         if(is_from_recommendations) {
                           "
          // Force Unwatched status for recommendations
          const statusSelect = document.querySelector('#modal_status + .selectize-control .selectize-input');
          if (statusSelect) {
            const items = statusSelect.querySelectorAll('.item');
            items.forEach(item => item.remove());
            const newItem = document.createElement('div');
            newItem.className = 'item';
            newItem.textContent = 'Unwatched';
            statusSelect.appendChild(newItem);
          }
          "
                         } else "",
                         "}, 200);")
      )
    }
  })
  
  # Initialize modal display when opened (Edit Modal)
  observeEvent(rv$show_edit_modal, {
    if (rv$show_edit_modal && !is.null(rv$editing_item)) {
      # Run JavaScript to initialize star rating and modal display for edit modal
      session$sendCustomMessage(
        type = "eval",
        message = "setTimeout(function() { 
          if (typeof initStarRating === 'function') { 
            initStarRating(true); 
          }
          if (typeof updateModalDisplay === 'function') { 
            updateModalDisplay(true); 
          } 
          if (typeof updateTVSeriesFields === 'function') { 
            updateTVSeriesFields(true); 
          }
          if (typeof updateMovieDurationFields === 'function') {
            updateMovieDurationFields(true);
          }
          if (typeof updateEpisodeIndicator === 'function') {
            updateEpisodeIndicator(true);
          }
          // Clear any existing TMDB suggestions
          const suggestions = document.getElementById('edit_tmdb-suggestions');
          if (suggestions) {
            suggestions.innerHTML = '';
            suggestions.classList.remove('active');
          }
          // Ensure current season input is editable
          const currentSeasonInput = document.getElementById('edit_modal_current_season');
          if (currentSeasonInput) {
            currentSeasonInput.disabled = false;
          }
        }, 200);"
      )
    }
  })
  
  # ==============================================================================
  # Submit Movie handler
  # ==============================================================================
  
  observeEvent(input$submit_movie, {
    if (!rv$show_modal) {
      return()
    }
    
    # Validation
    if (is.null(input$modal_title) || input$modal_title == "") {
      showNotification("Title is required!", type = "error")
      return()
    }
    if (is.null(input$modal_media_type) || input$modal_media_type == "") {
      showNotification("Media type is required!", type = "error")
      return()
    }
    if (length(input$modal_genre) == 0) {
      showNotification("Please select at least one genre!", type = "error")
      return()
    }
    if (is.null(input$modal_status) || input$modal_status == "") {
      showNotification("Watch status is required!", type = "error")
      return()
    }
    
    # Get rating value from hidden input
    rating_value <- if (!is.null(input$modal_status) && input$modal_status == "Watched" && !is.null(input$modal_rating_value)) {
      as.numeric(input$modal_rating_value)
    } else {
      0  # Default to 0 for Unwatched and Watching
    }
    
    cat("Watch Status:", input$modal_status, "| Rating Value:", rating_value, "\n")
    
    # Get TMDB ID if available
    tmdb_id <- rv$current_tmdb_id
    
    # Initialize variables for both movie and TV series
    total_duration <- 0
    watched_duration <- 0
    total_seasons <- 0
    current_season <- 0
    episodes_current_season <- 0
    current_episode <- 0
    total_episodes <- 0
    total_episodes_watched <- 0
    
    # Validate based on media type
    if (input$modal_media_type == "Movie") {
      # Movie validation
      if (is.null(input$modal_total_duration) || input$modal_total_duration < 1) {
        showNotification("Movie duration is required and must be at least 1 minute!", type = "error")
        return()
      }
      if (is.null(input$modal_watched_duration) || input$modal_watched_duration < 0) {
        showNotification("Watched duration is required and cannot be negative!", type = "error")
        return()
      }
      
      total_duration <- if (!is.null(input$modal_total_duration)) as.numeric(input$modal_total_duration) else 0
      watched_duration <- if (!is.null(input$modal_watched_duration)) as.numeric(input$modal_watched_duration) else 0
      
      if (watched_duration > total_duration) {
        showNotification("Watched duration cannot exceed total duration!", type = "error")
        return()
      }
      
      # For movies, set TV series fields to 0 or NULL values
      total_seasons <- 0
      current_season <- 0
      episodes_current_season <- 0
      current_episode <- 0
      total_episodes <- 0
      total_episodes_watched <- 0
      
    } else if (input$modal_media_type == "TV Series") {
      # TV Series validation
      if (is.null(input$modal_total_seasons) || input$modal_total_seasons < 1) {
        showNotification("Total Seasons is required and must be at least 1!", type = "error")
        return()
      }
      if (is.null(input$modal_current_season) || input$modal_current_season < 0) {
        showNotification("Current Season is required!", type = "error")
        return()
      }
      if (is.null(input$modal_episodes_current_season) || input$modal_episodes_current_season < 1) {
        showNotification("Episodes in Current Season is required and must be at least 1!", type = "error")
        return()
      }
      if (is.null(input$modal_current_episode) || input$modal_current_episode < 0) {
        showNotification("Current Episode is required!", type = "error")
        return()
      }
      if (is.null(input$modal_total_episodes) || input$modal_total_episodes < 1) {
        showNotification("Total Episodes is required and must be at least 1!", type = "error")
        return()
      }
      
      # Get all values from inputs properly
      total_seasons <- as.numeric(input$modal_total_seasons)
      episodes_current_season <- as.numeric(input$modal_episodes_current_season)
      total_episodes <- as.numeric(input$modal_total_episodes)
      
      # Handle current_season and current_episode based on status
      if (input$modal_status == "Unwatched") {
        current_season <- 0
        current_episode <- 0
        total_episodes_watched <- 0
        
      } else if (input$modal_status == "Watched") {
        # For "Watched", set current_season and current_episode to final values
        current_season <- total_seasons
        current_episode <- episodes_current_season
        total_episodes_watched <- total_episodes
        
        cat("WATCHED STATUS - Setting values:\n")
        cat("  current_season =", current_season, "(should be total_seasons =", total_seasons, ")\n")
        cat("  current_episode =", current_episode, "(should be episodes_current_season =", episodes_current_season, ")\n")
        cat("  total_episodes_watched =", total_episodes_watched, "\n")
        
      } else if (input$modal_status == "Watching") {
        # For "Watching", read values directly from inputs
        current_season <- as.numeric(input$modal_current_season)
        current_episode <- as.numeric(input$modal_current_episode)
        
        # Calculate total_episodes_watched
        if (current_season > 1) {
          total_episodes_watched <- (current_season - 1) * episodes_current_season
        } else {
          total_episodes_watched <- 0
        }
        total_episodes_watched <- total_episodes_watched + current_episode
        total_episodes_watched <- min(total_episodes_watched, total_episodes)
        
        cat("WATCHING STATUS - Reading values:\n")
        cat("  current_season =", current_season, "(from input)\n")
        cat("  current_episode =", current_episode, "(from input)\n")
        cat("  total_episodes_watched =", total_episodes_watched, "(calculated)\n")
      }
      
      # Validation
      if (current_season < 0 || current_season > total_seasons) {
        showNotification("Current Season must be between 0 and Total Seasons!", type = "error")
        return()
      }
      if (current_episode < 0 || current_episode > episodes_current_season) {
        showNotification("Current Episode must be between 0 and Episodes in Current Season!", type = "error")
        return()
      }
      if (total_episodes_watched < 0 || total_episodes_watched > total_episodes) {
        showNotification("Total Episodes Watched must be between 0 and Total Episodes!", type = "error")
        return()
      }
      
      # For TV series, set movie duration fields to 0
      total_duration <- 0
      watched_duration <- 0
    }
    
    con <- get_db_connection()
    if (is.null(con)) return()
    
    tryCatch({
      escape_sql <- function(x) {
        if (is.null(x) || is.na(x) || x == "") return("")
        gsub("'", "''", as.character(x))
      }
      
      genre_str <- paste(input$modal_genre, collapse = ", ")
      date_watched <- if (input$modal_status == "Watched") Sys.Date() else NULL
      
      # SQL query with all fields
      query <- sprintf(
        "INSERT INTO movies_series 
    (title, media_type, genre, year, watch_status, rating, review, director, cast_members, plot_summary, poster_url, 
    total_duration, watched_duration, total_seasons, current_season, episodes_current_season, 
    current_episode, total_episodes, total_episodes_watched, date_watched, tmdb_id)
    VALUES ('%s', '%s', '%s', %s, '%s', %d, '%s', '%s', '%s', '%s', '%s', %d, %d, %d, %d, %d, %d, %d, %d, %s, %s)",
        escape_sql(input$modal_title),
        input$modal_media_type,
        escape_sql(genre_str),
        ifelse(is.na(input$modal_year) || is.null(input$modal_year), "NULL", input$modal_year),
        input$modal_status,
        rating_value,
        escape_sql(input$modal_review),
        escape_sql(input$modal_director),
        escape_sql(input$modal_cast),
        escape_sql(input$modal_plot_summary),
        escape_sql(input$modal_poster),
        total_duration,
        watched_duration,
        total_seasons,
        current_season,
        episodes_current_season,
        current_episode,
        total_episodes,
        total_episodes_watched,
        ifelse(is.null(date_watched), "NULL", paste0("'", date_watched, "'")),
        ifelse(is.null(tmdb_id), "NULL", tmdb_id)
      )
      
      cat("\n=== FINAL SQL QUERY ===\n")
      cat("current_season:", current_season, "\n")
      cat("current_episode:", current_episode, "\n")
      cat("total_episodes_watched:", total_episodes_watched, "\n")
      cat("Query:", query, "\n\n")
      
      result <- dbGetQuery(con, paste(query, "RETURNING id"))
      item_id <- result$id[1]
      
      if (input$modal_status == "Watched") {
        history_query <- sprintf(
          "INSERT INTO watch_history (movie_id, watch_date) VALUES (%d, '%s')",
          item_id, Sys.Date()
        )
        dbExecute(con, history_query)
      }
      
      dbDisconnect(con)
      
      showNotification("🎬 Added to your library!", type = "message", duration = 3)
      rv$show_modal <- FALSE
      rv$refresh <- rv$refresh + 1
      rv$modal_status <- "Unwatched"
      rv$modal_media_type <- "Movie"
      rv$current_tmdb_id <- NULL
      rv$tv_current_season <- 1
      rv$tv_current_episode <- 1
      rv$is_fetching_season <- FALSE
      rv$calculated_total_episodes_watched <- 0
      
      # Clear TMDB results
      rv$tmdb_results <- NULL
      
      # Reset all form fields
      updateTextInput(session, "modal_title", value = "")
      updateSelectizeInput(session, "modal_genre", selected = character(0))
      updateTextAreaInput(session, "modal_review", value = "")
      updateTextInput(session, "modal_director", value = "")
      updateTextAreaInput(session, "modal_cast", value = "")
      updateTextAreaInput(session, "modal_plot_summary", value = "")
      updateTextInput(session, "modal_poster", value = "")
      updateSelectizeInput(session, "modal_media_type", selected = "Movie")
      updateSelectizeInput(session, "modal_status", selected = "Unwatched")
      updateNumericInput(session, "modal_year", value = year(Sys.Date()))
      updateNumericInput(session, "modal_total_duration", value = 0)
      updateNumericInput(session, "modal_watched_duration", value = 0)
      updateNumericInput(session, "modal_total_seasons", value = 1)
      updateNumericInput(session, "modal_current_season", value = 0)
      updateNumericInput(session, "modal_episodes_current_season", value = 1)
      updateNumericInput(session, "modal_current_episode", value = 0)
      updateNumericInput(session, "modal_total_episodes", value = 1)
      updateNumericInput(session, "modal_total_episodes_watched", value = 0)
      
    }, error = function(e) {
      showNotification(paste("Failed to add item:", e$message), type = "error", duration = 5)
      if (!is.null(con)) dbDisconnect(con)
    })
  })
  
  # ==============================================================================
  # UPDATE MOVIE HANDLER - Edit Modal
  # ==============================================================================
  
  observeEvent(input$update_movie, {
    # CRITICAL: Only validate if the edit modal is actually shown
    if (!rv$show_edit_modal || is.null(rv$editing_item)) {
      return()
    }
    
    item_id <- rv$editing_item$id
    
    # Validation
    if (is.null(input$edit_modal_title) || input$edit_modal_title == "") {
      showNotification("Title is required!", type = "error")
      return()
    }
    if (is.null(input$edit_modal_media_type) || input$edit_modal_media_type == "") {
      showNotification("Media type is required!", type = "error")
      return()
    }
    if (length(input$edit_modal_genre) == 0) {
      showNotification("Please select at least one genre!", type = "error")
      return()
    }
    if (is.null(input$edit_modal_status) || input$edit_modal_status == "") {
      showNotification("Watch status is required!", type = "error")
      return()
    }
    
    # Get rating value from hidden input
    rating_value <- if (!is.null(input$edit_modal_status) && input$edit_modal_status == "Watched" && !is.null(input$edit_modal_rating_value)) {
      as.numeric(input$edit_modal_rating_value)
    } else {
      0  # Default to 0 for Unwatched and Watching
    }
    
    cat("EDIT - Watch Status:", input$edit_modal_status, "| Rating Value:", rating_value, "\n")
    
    # Get TMDB ID if available
    tmdb_id <- rv$edit_current_tmdb_id
    
    # Initialize variables for both movie and TV series
    total_duration <- 0
    watched_duration <- 0
    total_seasons <- 0
    current_season <- 0
    episodes_current_season <- 0
    current_episode <- 0
    total_episodes <- 0
    total_episodes_watched <- 0
    
    # Validate based on media type
    if (input$edit_modal_media_type == "Movie") {
      # Movie validation
      if (is.null(input$edit_modal_total_duration) || input$edit_modal_total_duration < 1) {
        showNotification("Movie duration is required and must be at least 1 minute!", type = "error")
        return()
      }
      if (is.null(input$edit_modal_watched_duration) || input$edit_modal_watched_duration < 0) {
        showNotification("Watched duration is required and cannot be negative!", type = "error")
        return()
      }
      
      total_duration <- if (!is.null(input$edit_modal_total_duration)) as.numeric(input$edit_modal_total_duration) else 0
      watched_duration <- if (!is.null(input$edit_modal_watched_duration)) as.numeric(input$edit_modal_watched_duration) else 0
      
      if (watched_duration > total_duration) {
        showNotification("Watched duration cannot exceed total duration!", type = "error")
        return()
      }
      
      # For movies, set TV series fields to 0 or NULL values
      total_seasons <- 0
      current_season <- 0
      episodes_current_season <- 0
      current_episode <- 0
      total_episodes <- 0
      total_episodes_watched <- 0
      
    } else if (input$edit_modal_media_type == "TV Series") {
      # TV Series validation
      if (is.null(input$edit_modal_total_seasons) || input$edit_modal_total_seasons < 1) {
        showNotification("Total Seasons is required and must be at least 1!", type = "error")
        return()
      }
      if (is.null(input$edit_modal_current_season) || input$edit_modal_current_season < 0) {
        showNotification("Current Season is required!", type = "error")
        return()
      }
      if (is.null(input$edit_modal_episodes_current_season) || input$edit_modal_episodes_current_season < 1) {
        showNotification("Episodes in Current Season is required and must be at least 1!", type = "error")
        return()
      }
      if (is.null(input$edit_modal_current_episode) || input$edit_modal_current_episode < 0) {
        showNotification("Current Episode is required!", type = "error")
        return()
      }
      if (is.null(input$edit_modal_total_episodes) || input$edit_modal_total_episodes < 1) {
        showNotification("Total Episodes is required and must be at least 1!", type = "error")
        return()
      }
      
      # Get all values from inputs properly
      total_seasons <- as.numeric(input$edit_modal_total_seasons)
      episodes_current_season <- as.numeric(input$edit_modal_episodes_current_season)
      total_episodes <- as.numeric(input$edit_modal_total_episodes)
      
      # Handle current_season and current_episode based on status
      if (input$edit_modal_status == "Unwatched") {
        current_season <- 0
        current_episode <- 0
        total_episodes_watched <- 0
        
      } else if (input$edit_modal_status == "Watched") {
        # For "Watched", set current_season and current_episode to final values
        current_season <- total_seasons
        current_episode <- episodes_current_season
        total_episodes_watched <- total_episodes
        
        cat("EDIT - WATCHED STATUS - Setting values:\n")
        cat("  current_season =", current_season, "(should be total_seasons =", total_seasons, ")\n")
        cat("  current_episode =", current_episode, "(should be episodes_current_season =", episodes_current_season, ")\n")
        cat("  total_episodes_watched =", total_episodes_watched, "\n")
        
      } else if (input$edit_modal_status == "Watching") {
        # For "Watching", read values directly from inputs
        current_season <- as.numeric(input$edit_modal_current_season)
        current_episode <- as.numeric(input$edit_modal_current_episode)
        
        # Calculate total_episodes_watched
        if (current_season > 1) {
          total_episodes_watched <- (current_season - 1) * episodes_current_season
        } else {
          total_episodes_watched <- 0
        }
        total_episodes_watched <- total_episodes_watched + current_episode
        total_episodes_watched <- min(total_episodes_watched, total_episodes)
        
        cat("EDIT - WATCHING STATUS - Reading values:\n")
        cat("  current_season =", current_season, "(from input)\n")
        cat("  current_episode =", current_episode, "(from input)\n")
        cat("  total_episodes_watched =", total_episodes_watched, "(calculated)\n")
      }
      
      # Validation
      if (current_season < 0 || current_season > total_seasons) {
        showNotification("Current Season must be between 0 and Total Seasons!", type = "error")
        return()
      }
      if (current_episode < 0 || current_episode > episodes_current_season) {
        showNotification("Current Episode must be between 0 and Episodes in Current Season!", type = "error")
        return()
      }
      if (total_episodes_watched < 0 || total_episodes_watched > total_episodes) {
        showNotification("Total Episodes Watched must be between 0 and Total Episodes!", type = "error")
        return()
      }
      
      # For TV series, set movie duration fields to 0
      total_duration <- 0
      watched_duration <- 0
    }
    
    con <- get_db_connection()
    if (is.null(con)) return()
    
    tryCatch({
      escape_sql <- function(x) {
        if (is.null(x) || is.na(x) || x == "") return("")
        gsub("'", "''", as.character(x))
      }
      
      genre_str <- paste(input$edit_modal_genre, collapse = ", ")
      date_watched <- if (input$edit_modal_status == "Watched") {
        if (rv$editing_item$watch_status != "Watched") {
          # If status changed to Watched, set current date
          Sys.Date()
        } else {
          # Keep existing date_watched if already watched
          rv$editing_item$date_watched
        }
      } else {
        NULL
      }
      
      # SQL UPDATE query with all fields
      query <- sprintf(
        "UPDATE movies_series SET
        title = '%s',
        media_type = '%s',
        genre = '%s',
        year = %s,
        watch_status = '%s',
        rating = %d,
        review = '%s',
        director = '%s',
        cast_members = '%s',
        plot_summary = '%s',
        poster_url = '%s',
        total_duration = %d,
        watched_duration = %d,
        total_seasons = %d,
        current_season = %d,
        episodes_current_season = %d,
        current_episode = %d,
        total_episodes = %d,
        total_episodes_watched = %d,
        date_watched = %s,
        tmdb_id = %s,
        date_modified = NOW()
        WHERE id = %d",
        escape_sql(input$edit_modal_title),
        input$edit_modal_media_type,
        escape_sql(genre_str),
        ifelse(is.na(input$edit_modal_year) || is.null(input$edit_modal_year), "NULL", input$edit_modal_year),
        input$edit_modal_status,
        rating_value,
        escape_sql(input$edit_modal_review),
        escape_sql(input$edit_modal_director),
        escape_sql(input$edit_modal_cast),
        escape_sql(input$edit_modal_plot_summary),
        escape_sql(input$edit_modal_poster),
        total_duration,
        watched_duration,
        total_seasons,
        current_season,
        episodes_current_season,
        current_episode,
        total_episodes,
        total_episodes_watched,
        ifelse(is.null(date_watched), "NULL", paste0("'", date_watched, "'")),
        ifelse(is.null(tmdb_id), "NULL", tmdb_id),
        item_id
      )
      
      cat("\n=== EDIT FINAL SQL QUERY ===\n")
      cat("Item ID:", item_id, "\n")
      cat("current_season:", current_season, "\n")
      cat("current_episode:", current_episode, "\n")
      cat("total_episodes_watched:", total_episodes_watched, "\n")
      cat("Query:", query, "\n\n")
      
      dbExecute(con, query)
      
      # Update watch history if status changed to Watched
      if (input$edit_modal_status == "Watched" && rv$editing_item$watch_status != "Watched") {
        # Check if already in watch history
        check_query <- sprintf("SELECT COUNT(*) as count FROM watch_history WHERE movie_id = %d", item_id)
        count_result <- dbGetQuery(con, check_query)
        
        if (count_result$count[1] == 0) {
          history_query <- sprintf(
            "INSERT INTO watch_history (movie_id, watch_date) VALUES (%d, '%s')",
            item_id, Sys.Date()
          )
          dbExecute(con, history_query)
        }
      }
      
      dbDisconnect(con)
      
      showNotification("✅ Item updated successfully!", type = "message", duration = 3)
      rv$show_edit_modal <- FALSE
      rv$refresh <- rv$refresh + 1
      rv$editing_item <- NULL
      rv$edit_current_tmdb_id <- NULL
      rv$edit_tv_current_season <- 1
      rv$edit_tv_current_episode <- 1
      rv$edit_is_fetching_season <- FALSE
      rv$edit_calculated_total_episodes_watched <- 0
      
      # Clear edit TMDB results
      rv$edit_tmdb_results <- NULL
      
    }, error = function(e) {
      showNotification(paste("Failed to update item:", e$message), type = "error", duration = 5)
      if (!is.null(con)) dbDisconnect(con)
    })
  })
  
  # Main Content
  output$main_content <- renderUI({
    if (rv$page == "home") render_home()
    else if (rv$page == "stats") render_stats()  # NEW: Stats page
    else if (rv$page == "library") render_library()
    else if (rv$page == "recommendations") render_recommendations_page()
    else if (rv$page == "achievements") render_achievements()
  })
  
  # ==============================================================================
  # FAB BUTTON - Only shows on Library tab
  # ==============================================================================
  
  output$fab_button <- renderUI({
    # Only show FAB on library page
    if (rv$page == "library") {
      tags$button(class = "fab", 
                  onclick = "Shiny.setInputValue('show_add_modal', Math.random());", 
                  "+")
    } else {
      NULL
    }
  })
  
  # ==============================================================================
  # STATS TAB
  # ==============================================================================
  render_stats <- function() {
    items <- get_all_items()
    
    # Filter items based on media type selection
    if (rv$stats_media_type == "movies") {
      items <- items[items$media_type == "Movie", ]
    } else if (rv$stats_media_type == "series") {
      items <- items[items$media_type == "TV Series", ]
    }
    # If "all", keep all items
    
    total <- nrow(items)
    watched <- sum(items$watch_status == "Watched", na.rm = TRUE)
    completion <- if (total > 0) round((watched / total) * 100) else 0
    
    # Calculate cinematic profile stats
    avg_rating <- if (nrow(items[items$rating > 0,]) > 0) 
      round(mean(items[items$rating > 0,]$rating), 1) else "N/A"
    
    favorite_genre <- if (nrow(items) > 0 && any(!is.na(items$genre))) {
      genres <- unlist(strsplit(items$genre, ", "))
      names(sort(table(genres), decreasing = TRUE))[1]
    } else "None"
    
    # Calculate watched this month
    current_month <- format(Sys.Date(), "%Y-%m")
    watched_this_month <- if (nrow(items) > 0) {
      month_watched <- items %>%
        filter(watch_status == "Watched" & 
                 !is.na(date_watched) &
                 format(as.Date(date_watched), "%Y-%m") == current_month)
      nrow(month_watched)
    } else 0
    
    # Calculate watch time or episodes based on media type
    if (rv$stats_media_type == "all") {
      # For "All", show watch time for movies and episodes for series
      movies_watch_time <- round(sum(items$total_duration[items$media_type == "Movie"], na.rm = TRUE) / 60, 1)
      total_episodes_watched <- sum(items$total_episodes_watched[items$media_type == "TV Series"], na.rm = TRUE)
      
      # Show both metrics if we have both types
      if (movies_watch_time > 0 && total_episodes_watched > 0) {
        total_watch_metric <- paste0(movies_watch_time, "h, ", total_episodes_watched, " eps")
      } else if (movies_watch_time > 0) {
        total_watch_metric <- paste0(movies_watch_time, "h")
      } else if (total_episodes_watched > 0) {
        total_watch_metric <- paste0(total_episodes_watched, " eps")
      } else {
        total_watch_metric <- "0h"
      }
    } else if (rv$stats_media_type == "movies") {
      # For movies, show watch time
      total_watch_time <- round(sum(items$total_duration, na.rm = TRUE) / 60, 1)
      total_watch_metric <- paste0(total_watch_time, "h")
    } else if (rv$stats_media_type == "series") {
      # For TV series, show total episodes watched
      total_episodes_watched <- sum(items$total_episodes_watched, na.rm = TRUE)
      total_watch_metric <- paste0(total_episodes_watched, " eps")
    } else {
      total_watch_metric <- "0h"
    }
    
    # Calculate media type distribution for the current filter
    movies_count <- sum(items$media_type == "Movie", na.rm = TRUE)
    series_count <- sum(items$media_type == "TV Series", na.rm = TRUE)
    
    tagList(
      tags$div(class = "stats-page",
               tags$div(class = "section-header",
                        tags$h2(class = "section-title", "Your Statistics"),
                        tags$div(class = "stats-tab-nav",
                                 tags$button(class = paste("stats-tab-btn", if(rv$stats_media_type == "all") "active"),
                                             onclick = "Shiny.setInputValue('stats_all_btn', Math.random());",
                                             "All"),
                                 tags$button(class = paste("stats-tab-btn", if(rv$stats_media_type == "movies") "active"),
                                             onclick = "Shiny.setInputValue('stats_movies_btn', Math.random());",
                                             "Movies"),
                                 tags$button(class = paste("stats-tab-btn", if(rv$stats_media_type == "series") "active"),
                                             onclick = "Shiny.setInputValue('stats_series_btn', Math.random());",
                                             "TV Series")
                        )
               ),
               
               # Stats Grid
               tags$div(class = "stats-grid",
                        # Row 1: First 3 stats
                        tags$div(class = "stat-card",
                                 tags$div(class = "stat-icon", "🎬"),
                                 tags$div(class = "stat-content",
                                          tags$h4("Total Items"),
                                          tags$p(total)
                                 )
                        ),
                        tags$div(class = "stat-card",
                                 tags$div(class = "stat-icon", "📈"),
                                 tags$div(class = "stat-content",
                                          tags$h4("Watched This Month"),
                                          tags$p(watched_this_month)
                                 )
                        ),
                        tags$div(class = "stat-card",
                                 tags$div(class = "stat-icon", "✓"),
                                 tags$div(class = "stat-content",
                                          tags$h4("Completion Rate"),
                                          tags$p(paste0(completion, "%"))
                                 )
                        ),
                        
                        # Row 2: Next 3 stats (cinematic profile)
                        tags$div(class = "stat-card",
                                 tags$div(class = "stat-icon", "⭐"),
                                 tags$div(class = "stat-content",
                                          tags$h4("Average Rating"),
                                          tags$p(avg_rating)
                                 )
                        ),
                        tags$div(class = "stat-card",
                                 tags$div(class = "stat-icon", "🎭"),
                                 tags$div(class = "stat-content",
                                          tags$h4("Favorite Genre"),
                                          tags$p(favorite_genre)
                                 )
                        ),
                        tags$div(class = "stat-card",
                                 tags$div(class = "stat-icon", "📊"),
                                 tags$div(class = "stat-content",
                                          tags$h4(if(rv$stats_media_type == "series") "Total Episodes Watched" 
                                                  else if(rv$stats_media_type == "movies") "Total Watch Time" 
                                                  else "Watch Time/Episodes"),
                                          tags$p(total_watch_metric)
                                 )
                        )
               ),
               
               
               # ==============================================================================
               # RECENTLY ADDED SECTION
               # ==============================================================================
               tags$div(class = "section-header", style = "margin-top: 2.5rem;",
                        tags$h2(class = "section-title", "Recently Added"),
                        tags$a(class = "view-all", "View All →", 
                               onclick = "Shiny.setInputValue('view_all_clicked', Math.random());")
               ),
               
               if (nrow(items) == 0) {
                 empty_msg <- get_empty_state_message()
                 tags$div(class = "empty-state",
                          tags$h3(empty_msg$title),
                          tags$p(empty_msg$message)
                 )
               } else {
                 # Display only 5 items in Recently Added section
                 recent_items <- head(items, 5)
                 if (nrow(recent_items) > 0) {
                   tags$div(class = "carousel-wrapper",
                            tags$div(class = "carousel-track",
                                     # Display exactly 5 cards
                                     lapply(1:min(5, nrow(recent_items)), function(i) {
                                       tags$div(class = "carousel-card",
                                                render_movie_card(recent_items[i, ])
                                       )
                                     })
                            )
                   )
                 }
               }
      )
    )
  }

  render_home <- function() {
    items <- get_all_items()
    
    # Get current featured item
    featured_idx <- rv$featured_index + 1  # R uses 1-based indexing
    if (featured_idx > length(FEATURED_CONTENT)) featured_idx <- 1
    featured <- FEATURED_CONTENT[[featured_idx]]
    
    tagList(
      # ==============================================================================
      # FEATURED SECTION
      # ==============================================================================
      tags$div(class = "featured-section",
               style = sprintf("background-image: linear-gradient(to right, rgba(26, 31, 41, 0.95) 0%%, rgba(26, 31, 41, 0.85) 50%%, rgba(26, 31, 41, 0.3) 100%%), url('%s');", 
                               featured$backdrop_url),
               
               tags$div(class = "featured-container",
                        # Left Side: Content
                        tags$div(class = "featured-content",
                                 tags$img(class = "featured-logo", 
                                          src = featured$logo_url,
                                          alt = featured$title,
                                          onerror = "this.style.display='none'; this.nextElementSibling.style.display='block';"),
                                 tags$h1(class = "featured-title-fallback", 
                                         style = "display: none;",
                                         featured$title),
                                 
                                 tags$div(class = "featured-meta",
                                          tags$span(class = "featured-year", featured$year),
                                          tags$span(class = "featured-dot", "•"),
                                          tags$span(class = "featured-duration", featured$duration),
                                          tags$span(class = "featured-dot", "•"),
                                          tags$span(class = "featured-genre", featured$genre)
                                 ),
                                 
                                 tags$p(class = "featured-plot", featured$plot),
                        ),
                        
                        # Right Side: Layered Carousel
                        tags$div(class = "featured-carousel",
                                 tags$div(class = "featured-carousel-container",
                                          lapply(1:length(FEATURED_CONTENT), function(i) {
                                            item <- FEATURED_CONTENT[[i]]
                                            tags$div(class = "featured-poster-item",
                                                     `data-index` = i - 1,
                                                     `data-backdrop` = item$backdrop_url,
                                                     `data-logo` = item$logo_url,
                                                     `data-title` = item$title,
                                                     `data-year` = item$year,
                                                     `data-duration` = item$duration,
                                                     `data-genre` = item$genre,
                                                     `data-plot` = item$plot,
                                                     tags$img(src = item$poster_url, 
                                                              alt = item$title,
                                                              onerror = "this.src='https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster';")
                                            )
                                          })
                                 )
                        )
               )
      ),
    )
  }
  
  render_pagination <- function(current_page, total_pages, prefix) {
    # Calculate which page numbers to show
    max_visible_pages <- 7
    
    if (total_pages <= max_visible_pages) {
      page_numbers <- 1:total_pages
    } else {
      if (current_page <= 4) {
        page_numbers <- c(1:5, NA, total_pages)
      } else if (current_page >= total_pages - 3) {
        page_numbers <- c(1, NA, (total_pages-4):total_pages)
      } else {
        page_numbers <- c(1, NA, (current_page-1):(current_page+1), NA, total_pages)
      }
    }
    
    tagList(
      tags$div(class = "pagination-wrapper",
               tags$div(class = "pagination-container",
                        # Previous Button
                        tags$button(
                          class = "pagination-nav-btn",
                          onclick = paste0("Shiny.setInputValue('", prefix, "_prev_page', Math.random());"),
                          disabled = if(current_page <= 1) "disabled" else NULL,
                          tags$span("←"),
                          tags$span("Previous")
                        ),
                        
                        # Page Numbers
                        tags$div(class = "page-numbers",
                                 lapply(page_numbers, function(page) {
                                   if (is.na(page)) {
                                     tags$span(class = "page-number ellipsis", "...")
                                   } else {
                                     tags$span(
                                       class = paste("page-number", if(page == current_page) "active"),
                                       onclick = sprintf("Shiny.setInputValue('%s_go_to_page', %d);", prefix, page),
                                       page
                                     )
                                   }
                                 })
                        ),
                        
                        # Next Button
                        tags$button(
                          class = "pagination-nav-btn",
                          onclick = paste0("Shiny.setInputValue('", prefix, "_next_page', Math.random());"),
                          disabled = if(current_page >= total_pages) "disabled" else NULL,
                          tags$span("Next"),
                          tags$span("→")
                        )
               )
      )
    )
  }
  
  # Library with Tabs, Search, Sort, Filter, and Single Bottom Pagination
  render_library <- function() {
    items <- get_library_items()
    
    # Check for empty state
    empty_msg <- get_empty_state_message()
    
    # Calculate stats for the grid 
    total_items <- nrow(items)
    
    if (nrow(items) == 0) {
      return(tagList(
        tags$div(class = "library-page",
                 
                 tags$div(class = "section-header",
                          tags$h2(class = "section-title", "Your Library"),
                          tags$div(class = "tab-nav",
                                   tags$button(class = paste("tab-btn", if(rv$library_view == "all") "active"),
                                               onclick = "Shiny.setInputValue('library_all_btn', Math.random());",
                                               "All"),
                                   tags$button(class = paste("tab-btn", if(rv$library_view == "movies") "active"),
                                               onclick = "Shiny.setInputValue('library_movies_btn', Math.random());",
                                               "Movies"),
                                   tags$button(class = paste("tab-btn", if(rv$library_view == "series") "active"),
                                               onclick = "Shiny.setInputValue('library_series_btn', Math.random());",
                                               "TV Series")
                          )
                 ),
                 
                 # Search, Sort, and Filter Controls
                 tags$div(class = "controls-container",
                          tags$div(class = "controls-wrapper",
                                   # Search bar
                                   tags$div(class = "search-container",
                                            tags$div(class = "search-input-wrapper",
                                                     tags$input(type = "text", 
                                                                class = "search-input",
                                                                id = "library-search-input",
                                                                placeholder = "Search movies, series, directors, genres...",
                                                                value = rv$search_query,
                                                                oninput = "handleSearchInput(event);",
                                                                onkeypress = "handleSearchKeyPress(event);"),
                                                     tags$span(class = "search-icon",
                                                               tags$i(class = "fas fa-search"))
                                            )
                                   ),
                                   
                                   # Sort and Filter buttons
                                   tags$div(class = "sort-filter-container",
                                            # Sort
                                            tags$div(class = "sort-container",
                                                     tags$button(class = paste("sort-btn", if(rv$sort_by != "date_added" || rv$sort_order != "desc") "active"),
                                                                 onclick = "toggleDropdown('sort-dropdown', event);",
                                                                 tags$i(class = "fas fa-sort-amount-down"),
                                                                 "Sort by"),
                                                     tags$div(class = "dropdown-menu", id = "sort-dropdown",
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "date_added" && rv$sort_order == "desc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_date_desc', Math.random());",
                                                                       tags$i(class = "fas fa-calendar-plus"),
                                                                       "Date Added (Newest)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "date_added" && rv$sort_order == "asc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_date_asc', Math.random());",
                                                                       tags$i(class = "fas fa-calendar-minus"),
                                                                       "Date Added (Oldest)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "title" && rv$sort_order == "asc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_title_asc', Math.random());",
                                                                       tags$i(class = "fas fa-sort-alpha-down"),
                                                                       "Title (A-Z)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "title" && rv$sort_order == "desc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_title_desc', Math.random());",
                                                                       tags$i(class = "fas fa-sort-alpha-up"),
                                                                       "Title (Z-A)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "year" && rv$sort_order == "desc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_year_desc', Math.random());",
                                                                       tags$i(class = "fas fa-sort-numeric-down"),
                                                                       "Year (Newest)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "year" && rv$sort_order == "asc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_year_asc', Math.random());",
                                                                       tags$i(class = "fas fa-sort-numeric-up"),
                                                                       "Year (Oldest)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "rating" && rv$sort_order == "desc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_rating_desc', Math.random());",
                                                                       tags$i(class = "fas fa-star"),
                                                                       "Rating (High to Low)"),
                                                              tags$div(class = paste("dropdown-item", if(rv$sort_by == "rating" && rv$sort_order == "asc") "active"),
                                                                       onclick = "Shiny.setInputValue('sort_rating_asc', Math.random());",
                                                                       tags$i(class = "fas fa-star-half-alt"),
                                                                       "Rating (Low to High)")
                                                     )
                                            ),
                                            
                                            # Filter
                                            tags$div(class = "filter-container",
                                                     tags$button(class = paste("filter-btn", if(rv$filter_status != "all" || rv$filter_genre != "all" || rv$filter_rating != "all") "active"),
                                                                 onclick = "toggleDropdown('filter-dropdown', event);",
                                                                 tags$i(class = "fas fa-filter"),
                                                                 "Filter"),
                                                     tags$div(class = "dropdown-menu", id = "filter-dropdown",
                                                              tags$div(class = "filter-dropdown-content",
                                                                       # Column 1: Status and Rating
                                                                       tags$div(class = "filter-section",
                                                                                tags$div(class = "filter-section-title", "Watch Status"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_status == "all") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_status_all', Math.random());",
                                                                                         tags$i(class = "fas fa-layer-group"),
                                                                                         "All Status"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_status == "Watched") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_status_watched', Math.random());",
                                                                                         tags$i(class = "fas fa-check-circle"),
                                                                                         "Watched"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_status == "Watching") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_status_watching', Math.random());",
                                                                                         tags$i(class = "fas fa-clock"),
                                                                                         "Watching"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_status == "Unwatched") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_status_unwatched', Math.random());",
                                                                                         tags$i(class = "far fa-circle"),
                                                                                         "Unwatched")
                                                                       ),
                                                                       
                                                                       # Column 2: Rating Filter
                                                                       tags$div(class = "filter-section",
                                                                                tags$div(class = "filter-section-title", "Rating"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "all") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_all', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "All Ratings"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "5") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_5', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "★★★★★ (5 stars)"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "4") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_4', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "★★★★☆ (4 stars)"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "3") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_3', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "★★★☆☆ (3 stars)"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "2") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_2', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "★★☆☆☆ (2 stars)"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "1") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_1', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "★☆☆☆☆ (1 star)"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_rating == "4-5") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_rating_4_5', Math.random());",
                                                                                         tags$i(class = "fas fa-star"),
                                                                                         "★★★★+ (4-5 stars)")
                                                                       ),
                                                                       
                                                                       # Column 3: Genre Filter
                                                                       tags$div(class = "filter-section",
                                                                                tags$div(class = "filter-section-title", "Genre"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "all") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_all', Math.random());",
                                                                                         tags$i(class = "fas fa-tags"),
                                                                                         "All Genres"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Action") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_action', Math.random());",
                                                                                         tags$i(class = "fas fa-bolt"),
                                                                                         "Action"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Adventure") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_adventure', Math.random());",
                                                                                         tags$i(class = "fas fa-mountain"),
                                                                                         "Adventure"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Comedy") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_comedy', Math.random());",
                                                                                         tags$i(class = "fas fa-laugh"),
                                                                                         "Comedy"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Drama") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_drama', Math.random());",
                                                                                         tags$i(class = "fas fa-theater-masks"),
                                                                                         "Drama"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Horror") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_horror', Math.random());",
                                                                                         tags$i(class = "fas fa-ghost"),
                                                                                         "Horror")
                                                                       ),
                                                                       
                                                                       # Column 4: More Genres
                                                                       tags$div(class = "filter-section",
                                                                                tags$div(class = "filter-section-title", "More Genres"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Science Fiction") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_scifi', Math.random());",
                                                                                         tags$i(class = "fas fa-rocket"),
                                                                                         "Science Fiction"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Fantasy") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_fantasy', Math.random());",
                                                                                         tags$i(class = "fas fa-dragon"),
                                                                                         "Fantasy"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Thriller") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_thriller', Math.random());",
                                                                                         tags$i(class = "fas fa-user-secret"),
                                                                                         "Thriller"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Romance") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_romance', Math.random());",
                                                                                         tags$i(class = "fas fa-heart"),
                                                                                         "Romance"),
                                                                                tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Animation") "active"),
                                                                                         onclick = "Shiny.setInputValue('filter_genre_animation', Math.random());",
                                                                                         tags$i(class = "fas fa-film"),
                                                                                         "Animation")
                                                                       )
                                                              ),
                                                              
                                                              # Year Range and Clear Filters
                                                              tags$div(style = "padding: 1rem; border-top: 1px solid rgba(255,255,255,0.1); margin-top: 0.5rem;",
                                                                       tags$div(style = "font-size: 12px; color: #9BA3B0; text-transform: uppercase; font-weight: 600; margin-bottom: 0.5rem;",
                                                                                "Year Range"),
                                                                       tags$div(style = "display: flex; gap: 0.5rem; align-items: center; margin-bottom: 1rem;",
                                                                                tags$input(type = "number", 
                                                                                           style = "flex: 1; padding: 0.5rem; background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15); border-radius: 8px; color: #FFF;",
                                                                                           value = rv$filter_year_min,
                                                                                           min = 1900,
                                                                                           max = 2025,
                                                                                           onchange = "Shiny.setInputValue('filter_year_min', this.value); Shiny.setInputValue('filter_year_trigger', Math.random());"),
                                                                                tags$span(style = "color: #9BA3B0;", "to"),
                                                                                tags$input(type = "number", 
                                                                                           style = "flex: 1; padding: 0.5rem; background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15); border-radius: 8px; color: #FFF;",
                                                                                           value = rv$filter_year_max,
                                                                                           min = 1900,
                                                                                           max = 2025,
                                                                                           onchange = "Shiny.setInputValue('filter_year_max', this.value); Shiny.setInputValue('filter_year_trigger', Math.random());")
                                                                       ),
                                                                       
                                                                       # Clear Filters
                                                                       tags$div(class = "dropdown-item",
                                                                                onclick = "Shiny.setInputValue('clear_all_filters', Math.random());",
                                                                                tags$i(class = "fas fa-times-circle", style = "color: #EF476F;"),
                                                                                tags$span(style = "color: #EF476F;", "Clear All Filters")
                                                                       )
                                                              )
                                                     )
                                            )
                                   )
                          ),
                          
                          # Active Filter Tags
                          if (rv$filter_status != "all" || rv$filter_genre != "all" || rv$filter_rating != "all" || rv$search_query != "" || rv$filter_year_min != 1900 || rv$filter_year_max != 2025) {
                            tags$div(class = "filter-tags",
                                     if (rv$search_query != "") {
                                       tags$div(class = "filter-tag",
                                                tags$i(class = "fas fa-search"),
                                                tags$span("Search: \"", rv$search_query, "\""),
                                                tags$span(class = "remove",
                                                          onclick = "Shiny.setInputValue('clear_search', Math.random());",
                                                          "×")
                                       )
                                     },
                                     if (rv$filter_status != "all") {
                                       tags$div(class = "filter-tag",
                                                tags$i(class = "fas fa-eye"),
                                                tags$span(rv$filter_status),
                                                tags$span(class = "remove",
                                                          onclick = "Shiny.setInputValue('filter_status_all', Math.random());",
                                                          "×")
                                       )
                                     },
                                     if (rv$filter_genre != "all") {
                                       tags$div(class = "filter-tag",
                                                tags$i(class = "fas fa-tags"),
                                                tags$span("Genre: ", rv$filter_genre),
                                                tags$span(class = "remove",
                                                          onclick = "Shiny.setInputValue('filter_genre_all', Math.random());",
                                                          "×")
                                       )
                                     },
                                     if (rv$filter_rating != "all") {
                                       rating_text <- if (rv$filter_rating == "4-5") "4-5 stars" else paste(rv$filter_rating, "stars")
                                       tags$div(class = "filter-tag",
                                                tags$i(class = "fas fa-star"),
                                                tags$span("Rating: ", rating_text),
                                                tags$span(class = "remove",
                                                          onclick = "Shiny.setInputValue('filter_rating_all', Math.random());",
                                                          "×")
                                       )
                                     },
                                     if (rv$filter_year_min != 1900 || rv$filter_year_max != 2025) {
                                       tags$div(class = "filter-tag",
                                                tags$i(class = "fas fa-calendar"),
                                                tags$span("Years: ", rv$filter_year_min, "-", rv$filter_year_max),
                                                tags$span(class = "remove",
                                                          onclick = "Shiny.setInputValue('clear_year_filter', Math.random());",
                                                          "×")
                                       )
                                     }
                            )
                          },
                          
                          # Results info
                          tags$div(class = "results-info",
                                   tags$span("Found", 
                                             tags$strong(paste0(" ", nrow(items), " ")), 
                                             if (rv$library_view == "all") "items" 
                                             else if (rv$library_view == "movies") "movies" 
                                             else "TV series",
                                             if (rv$search_query != "") paste0(" for \"", rv$search_query, "\""))
                          )
                 ),
                 
                 # Empty state message
                 tags$div(class = "empty-state",
                          tags$h3(empty_msg$title),
                          tags$p(empty_msg$message)
                 )
        )
      ))
    }
    
    # Calculate pagination
    total_items <- nrow(items)
    total_pages <- ceiling(total_items / rv$library_items_per_page)
    start_idx <- (rv$library_page - 1) * rv$library_items_per_page + 1
    end_idx <- min(rv$library_page * rv$library_items_per_page, total_items)
    
    # Get items for current page
    current_page_items <- items[start_idx:end_idx, ]
    
    tagList(
      tags$div(class = "library-page",
               
               tags$div(class = "section-header",
                        tags$h2(class = "section-title", "Your Library"),
                        tags$div(class = "tab-nav",
                                 tags$button(class = paste("tab-btn", if(rv$library_view == "all") "active"),
                                             onclick = "Shiny.setInputValue('library_all_btn', Math.random());",
                                             "All"),
                                 tags$button(class = paste("tab-btn", if(rv$library_view == "movies") "active"),
                                             onclick = "Shiny.setInputValue('library_movies_btn', Math.random());",
                                             "Movies"),
                                 tags$button(class = paste("tab-btn", if(rv$library_view == "series") "active"),
                                             onclick = "Shiny.setInputValue('library_series_btn', Math.random());",
                                             "TV Series")
                        )
               ),
               
               # Search, Sort, and Filter Controls - REORDERED: Search first
               tags$div(class = "controls-container",
                        tags$div(class = "controls-wrapper",
                                 # Search bar FIRST
                                 tags$div(class = "search-container",
                                          tags$div(class = "search-input-wrapper",
                                                   tags$input(type = "text", 
                                                              class = "search-input",
                                                              id = "library-search-input-main",
                                                              placeholder = "Search movies, series, directors, genres...",
                                                              value = rv$search_query,
                                                              oninput = "handleSearchInput(event);",
                                                              onkeypress = "handleSearchKeyPress(event);"),
                                                   tags$span(class = "search-icon",
                                                             tags$i(class = "fas fa-search"))
                                          )
                                 ),
                                 
                                 # Sort and Filter buttons after search
                                 tags$div(class = "sort-filter-container",
                                          # Sort
                                          tags$div(class = "sort-container",
                                                   tags$button(class = paste("sort-btn", if(rv$sort_by != "date_added" || rv$sort_order != "desc") "active"),
                                                               onclick = "toggleDropdown('sort-dropdown', event);",
                                                               tags$i(class = "fas fa-sort-amount-down"),
                                                               "Sort by"),
                                                   tags$div(class = "dropdown-menu", id = "sort-dropdown",
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "date_added" && rv$sort_order == "desc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_date_desc', Math.random());",
                                                                     tags$i(class = "fas fa-calendar-plus"),
                                                                     "Date Added (Newest)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "date_added" && rv$sort_order == "asc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_date_asc', Math.random());",
                                                                     tags$i(class = "fas fa-calendar-minus"),
                                                                     "Date Added (Oldest)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "title" && rv$sort_order == "asc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_title_asc', Math.random());",
                                                                     tags$i(class = "fas fa-sort-alpha-down"),
                                                                     "Title (A-Z)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "title" && rv$sort_order == "desc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_title_desc', Math.random());",
                                                                     tags$i(class = "fas fa-sort-alpha-up"),
                                                                     "Title (Z-A)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "year" && rv$sort_order == "desc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_year_desc', Math.random());",
                                                                     tags$i(class = "fas fa-sort-numeric-down"),
                                                                     "Year (Newest)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "year" && rv$sort_order == "asc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_year_asc', Math.random());",
                                                                     tags$i(class = "fas fa-sort-numeric-up"),
                                                                     "Year (Oldest)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "rating" && rv$sort_order == "desc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_rating_desc', Math.random());",
                                                                     tags$i(class = "fas fa-star"),
                                                                     "Rating (High to Low)"),
                                                            tags$div(class = paste("dropdown-item", if(rv$sort_by == "rating" && rv$sort_order == "asc") "active"),
                                                                     onclick = "Shiny.setInputValue('sort_rating_asc', Math.random());",
                                                                     tags$i(class = "fas fa-star-half-alt"),
                                                                     "Rating (Low to High)")
                                                   )
                                          ),
                                          
                                          # Filter
                                          tags$div(class = "filter-container",
                                                   tags$button(class = paste("filter-btn", if(rv$filter_status != "all" || rv$filter_genre != "all" || rv$filter_rating != "all") "active"),
                                                               onclick = "toggleDropdown('filter-dropdown', event);",
                                                               tags$i(class = "fas fa-filter"),
                                                               "Filter"),
                                                   tags$div(class = "dropdown-menu", id = "filter-dropdown",
                                                            tags$div(class = "filter-dropdown-content",
                                                                     # Column 1: Status and Rating
                                                                     tags$div(class = "filter-section",
                                                                              tags$div(class = "filter-section-title", "Watch Status"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_status == "all") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_status_all', Math.random());",
                                                                                       tags$i(class = "fas fa-layer-group"),
                                                                                       "All Status"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_status == "Watched") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_status_watched', Math.random());",
                                                                                       tags$i(class = "fas fa-check-circle"),
                                                                                       "Watched"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_status == "Watching") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_status_watching', Math.random());",
                                                                                       tags$i(class = "fas fa-clock"),
                                                                                       "Watching"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_status == "Unwatched") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_status_unwatched', Math.random());",
                                                                                       tags$i(class = "far fa-circle"),
                                                                                       "Unwatched")
                                                                     ),
                                                                     
                                                                     # Column 2: Rating Filter
                                                                     tags$div(class = "filter-section",
                                                                              tags$div(class = "filter-section-title", "Rating"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "all") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_all', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "All Ratings"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "5") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_5', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "★★★★★ (5 stars)"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "4") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_4', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "★★★★☆ (4 stars)"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "3") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_3', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "★★★☆☆ (3 stars)"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "2") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_2', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "★★☆☆☆ (2 stars)"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "1") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_1', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "★☆☆☆☆ (1 star)"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_rating == "4-5") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_rating_4_5', Math.random());",
                                                                                       tags$i(class = "fas fa-star"),
                                                                                       "★★★★+ (4-5 stars)")
                                                                     ),
                                                                     
                                                                     # Column 3: Genre Filter
                                                                     tags$div(class = "filter-section",
                                                                              tags$div(class = "filter-section-title", "Genre"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "all") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_all', Math.random());",
                                                                                       tags$i(class = "fas fa-tags"),
                                                                                       "All Genres"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Action") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_action', Math.random());",
                                                                                       tags$i(class = "fas fa-bolt"),
                                                                                       "Action"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Adventure") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_adventure', Math.random());",
                                                                                       tags$i(class = "fas fa-mountain"),
                                                                                       "Adventure"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Comedy") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_comedy', Math.random());",
                                                                                       tags$i(class = "fas fa-laugh"),
                                                                                       "Comedy"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Drama") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_drama', Math.random());",
                                                                                       tags$i(class = "fas fa-theater-masks"),
                                                                                       "Drama"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Horror") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_horror', Math.random());",
                                                                                       tags$i(class = "fas fa-ghost"),
                                                                                       "Horror")
                                                                     ),
                                                                     
                                                                     # Column 4: More Genres
                                                                     tags$div(class = "filter-section",
                                                                              tags$div(class = "filter-section-title", "More Genres"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Science Fiction") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_scifi', Math.random());",
                                                                                       tags$i(class = "fas fa-rocket"),
                                                                                       "Science Fiction"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Fantasy") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_fantasy', Math.random());",
                                                                                       tags$i(class = "fas fa-dragon"),
                                                                                       "Fantasy"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Thriller") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_thriller', Math.random());",
                                                                                       tags$i(class = "fas fa-user-secret"),
                                                                                       "Thriller"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Romance") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_romance', Math.random());",
                                                                                       tags$i(class = "fas fa-heart"),
                                                                                       "Romance"),
                                                                              tags$div(class = paste("dropdown-item", if(rv$filter_genre == "Animation") "active"),
                                                                                       onclick = "Shiny.setInputValue('filter_genre_animation', Math.random());",
                                                                                       tags$i(class = "fas fa-film"),
                                                                                       "Animation")
                                                                     )
                                                            ),
                                                            
                                                            # Year Range and Clear Filters
                                                            tags$div(style = "padding: 1rem; border-top: 1px solid rgba(255,255,255,0.1); margin-top: 0.5rem;",
                                                                     tags$div(style = "font-size: 12px; color: #9BA3B0; text-transform: uppercase; font-weight: 600; margin-bottom: 0.5rem;",
                                                                              "Year Range"),
                                                                     tags$div(style = "display: flex; gap: 0.5rem; align-items: center; margin-bottom: 1rem;",
                                                                              tags$input(type = "number", 
                                                                                         style = "flex: 1; padding: 0.5rem; background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15); border-radius: 8px; color: #FFF;",
                                                                                         value = rv$filter_year_min,
                                                                                         min = 1900,
                                                                                         max = 2025,
                                                                                         onchange = "Shiny.setInputValue('filter_year_min', this.value); Shiny.setInputValue('filter_year_trigger', Math.random());"),
                                                                              tags$span(style = "color: #9BA3B0;", "to"),
                                                                              tags$input(type = "number", 
                                                                                         style = "flex: 1; padding: 0.5rem; background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15); border-radius: 8px; color: #FFF;",
                                                                                         value = rv$filter_year_max,
                                                                                         min = 1900,
                                                                                         max = 2025,
                                                                                         onchange = "Shiny.setInputValue('filter_year_max', this.value); Shiny.setInputValue('filter_year_trigger', Math.random());")
                                                                     ),
                                                                     
                                                                     # Clear Filters
                                                                     tags$div(class = "dropdown-item",
                                                                              onclick = "Shiny.setInputValue('clear_all_filters', Math.random());",
                                                                              tags$i(class = "fas fa-times-circle", style = "color: #EF476F;"),
                                                                              tags$span(style = "color: #EF476F;", "Clear All Filters")
                                                                     )
                                                            )
                                                   )
                                          )
                                 )
                        ),
                        
                        # Active Filter Tags
                        if (rv$filter_status != "all" || rv$filter_genre != "all" || rv$filter_rating != "all" || rv$search_query != "" || rv$filter_year_min != 1900 || rv$filter_year_max != 2025) {
                          tags$div(class = "filter-tags",
                                   if (rv$search_query != "") {
                                     tags$div(class = "filter-tag",
                                              tags$i(class = "fas fa-search"),
                                              tags$span("Search: \"", rv$search_query, "\""),
                                              tags$span(class = "remove",
                                                        onclick = "Shiny.setInputValue('clear_search', Math.random());",
                                                        "×")
                                     )
                                   },
                                   if (rv$filter_status != "all") {
                                     tags$div(class = "filter-tag",
                                              tags$i(class = "fas fa-eye"),
                                              tags$span(rv$filter_status),
                                              tags$span(class = "remove",
                                                        onclick = "Shiny.setInputValue('filter_status_all', Math.random());",
                                                        "×")
                                     )
                                   },
                                   if (rv$filter_genre != "all") {
                                     tags$div(class = "filter-tag",
                                              tags$i(class = "fas fa-tags"),
                                              tags$span("Genre: ", rv$filter_genre),
                                              tags$span(class = "remove",
                                                        onclick = "Shiny.setInputValue('filter_genre_all', Math.random());",
                                                        "×")
                                     )
                                   },
                                   if (rv$filter_rating != "all") {
                                     rating_text <- if (rv$filter_rating == "4-5") "4-5 stars" else paste(rv$filter_rating, "stars")
                                     tags$div(class = "filter-tag",
                                              tags$i(class = "fas fa-star"),
                                              tags$span("Rating: ", rating_text),
                                              tags$span(class = "remove",
                                                        onclick = "Shiny.setInputValue('filter_rating_all', Math.random());",
                                                        "×")
                                     )
                                   },
                                   if (rv$filter_year_min != 1900 || rv$filter_year_max != 2025) {
                                     tags$div(class = "filter-tag",
                                              tags$i(class = "fas fa-calendar"),
                                              tags$span("Years: ", rv$filter_year_min, "-", rv$filter_year_max),
                                              tags$span(class = "remove",
                                                        onclick = "Shiny.setInputValue('clear_year_filter', Math.random());",
                                                        "×")
                                     )
                                   }
                          )
                        },
                        
                        # Results info
                        tags$div(class = "results-info",
                                 tags$span("Found", 
                                           tags$strong(paste0(" ", nrow(items), " ")), 
                                           if (rv$library_view == "all") "items" 
                                           else if (rv$library_view == "movies") "movies" 
                                           else "TV series",
                                           if (rv$search_query != "") paste0(" for \"", rv$search_query, "\""))
                        )
               ),
               
               # Movies Grid
               tags$div(class = "movies-grid", style = "grid-template-columns: repeat(5, 1fr) !important;",
                        lapply(1:nrow(current_page_items), function(i) render_movie_card(current_page_items[i, ]))
               ),
               
               # Bottom Pagination (only show if more than 1 page)
               if (total_pages > 1) {
                 render_pagination(rv$library_page, total_pages, "library")
               }
      )
    )
  }
  
  # Recommendations Page with Tabs and Single Bottom Pagination
  render_recommendations_page <- function() {
    items <- get_all_items()
    
    # Determine which media type is active based on click counts
    media_type <- if (rv$rec_series_clicked > rv$rec_movies_clicked) "tv" else "movie"
    active_tab <- if (media_type == "movie") "Movies" else "TV Series"
    
    tagList(
      tags$div(class = "recommendations-page",
               tags$div(class = "section-header",
                        tags$h2(class = "section-title", "Recommendations"),
                        tags$div(class = "tab-nav",
                                 tags$button(class = paste("tab-btn", if(active_tab == "Movies") "active"),
                                             onclick = "Shiny.setInputValue('rec_movies_btn', Math.random());",
                                             "Movies"),
                                 tags$button(class = paste("tab-btn", if(active_tab == "TV Series") "active"),
                                             onclick = "Shiny.setInputValue('rec_series_btn', Math.random());",
                                             "TV Series")
                        )
               ),
               
               render_recommendations_section(items, media_type)
      )
    )
  }
  
  # Render Recommendations Section with Single Bottom Pagination
  render_recommendations_section <- function(items, media_type) {
    # Get watched items of the specific media type
    if (media_type == "movie") {
      watched <- items[items$watch_status == "Watched" & items$media_type == "Movie", ]
    } else {
      watched <- items[items$watch_status == "Watched" & items$media_type == "TV Series", ]
    }
    
    if (nrow(watched) == 0) {
      return(tags$div(class = "empty-state",
                      tags$h3(if (media_type == "movie") 
                        "You haven't watched any movies yet." 
                        else "You haven't watched any TV series yet."),
                      tags$p(if (media_type == "movie") 
                        "Watch some movies to get personalized recommendations!" 
                        else "Watch some TV series to get personalized recommendations!")
      ))
    }
    
    genres <- unlist(strsplit(watched$genre, ", "))
    
    if (length(genres) == 0) {
      return(tags$div(class = "empty-state",
                      tags$h3("No genre data available."),
                      tags$p("Add more ", if(media_type == "movie") "movies" else "TV series", " with genres to get recommendations.")
      ))
    }
    
    top_genres <- names(sort(table(genres), decreasing = TRUE))[1:min(3, length(table(genres)))]
    
    # Map genre names to TMDB IDs
    genre_ids <- c()
    for (genre in top_genres) {
      if (media_type == "movie") {
        # For movies, use the movie genre mapping
        if (!is.null(GENRE_TO_TMDB_ID[[genre]])) {
          genre_ids <- c(genre_ids, GENRE_TO_TMDB_ID[[genre]])
        }
      } else {
        # For TV series, use TV genre mapping
        if (!is.null(TV_GENRE_TO_TMDB_ID[[genre]])) {
          genre_ids <- c(genre_ids, TV_GENRE_TO_TMDB_ID[[genre]])
        } else {
          # Fallback to movie genre mapping if TV mapping not found
          if (!is.null(GENRE_TO_TMDB_ID[[genre]])) {
            genre_ids <- c(genre_ids, GENRE_TO_TMDB_ID[[genre]])
          }
        }
      }
    }
    
    # Remove any NULL or NA values
    genre_ids <- genre_ids[!is.na(genre_ids) & !is.null(genre_ids)]
    
    if (length(genre_ids) == 0) {
      return(tags$div(class = "empty-state",
                      tags$h3("Could not map your genres to TMDB IDs."),
                      tags$p("Try adding more ", if(media_type == "movie") "movies" else "TV series", " with different genres.")
      ))
    }
    
    # Get existing titles and TMDB IDs to exclude
    existing_data <- get_existing_titles_and_ids()
    exclude_titles <- existing_data$titles
    exclude_tmdb_ids <- existing_data$tmdb_ids
    
    # Get recommendations from TMDB, excluding existing items
    recommendations <- get_movie_recommendations(genre_ids, media_type, rv$rec_page, 
                                                 exclude_titles = exclude_titles,
                                                 exclude_tmdb_ids = exclude_tmdb_ids)
    
    if (nrow(recommendations) == 0) {
      return(tags$div(class = "empty-state",
                      tags$h3(if (media_type == "movie") 
                        "No new movie recommendations found." 
                        else "No new TV series recommendations found."),
                      tags$p(if (media_type == "movie") 
                        "You've already added all recommended movies to your library! Try watching more movies with different genres." 
                        else "You've already added all recommended TV series to your library! Try watching more series with different genres.")
      ))
    }

    end_idx <- min(25, nrow(recommendations))
    
    tagList(
      # Content with genre info
      tags$div(style = "padding: 0 3rem; margin-bottom: 2rem; max-width: 1400px; margin-left: auto; margin-right: auto;",
               tags$p(style = "color: #9BA3B0; margin-bottom: 1rem;",
                      paste0("These are the ", if(media_type == "movie") "movie" else "TV series", 
                             " genres you've watched the most:")),
               tags$div(class = "genre-pills",
                        lapply(top_genres, function(g) tags$div(class = "genre-pill", g))
               ),
               tags$p(style = "color: #9BA3B0; margin-top: 1.5rem;",
                      paste0(if(media_type == "movie") "Movie" else "TV Series", 
                             " recommendations based on your most watched genres (excluding items already in your library):"))
      ),
      
      # Recommendations Grid
      tags$div(class = "movies-grid", style = "grid-template-columns: repeat(5, 1fr) !important;",
               lapply(1:end_idx, function(i) {
                 rec <- recommendations[i, ]
                 tags$div(class = "rec-card", 
                          `data-tmdb-id` = rec$tmdb_id,
                          `data-media-type` = if(media_type == "movie") "Movie" else "TV Series",
                          `data-title` = rec$title,
                          tags$img(class = "movie-poster", src = rec$poster),
                          tags$div(class = "movie-info",
                                   tags$h3(class = "movie-title", rec$title),
                                   tags$div(class = "movie-meta",
                                            if (!is.na(rec$year)) tags$span(rec$year),
                                            tags$span("•"),
                                            tags$span(if(media_type == "movie") "Movie" else "TV Series")
                                   ),
                                   if (rec$rating > 0) {
                                     tags$div(class = "movie-rating",
                                              tags$span("⭐"),
                                              tags$span(rec$rating)
                                     )
                                   },
                                   if (nchar(rec$overview) > 0) {
                                     tags$p(style = "color: #9BA3B0; font-size: 14px; margin-bottom: 1rem; line-height: 1.4;",
                                            paste0(substr(rec$overview, 1, 100), "..."))
                                   }
                          )
                 )
               })
      ),
      
      # Bottom Pagination
      render_pagination(rv$rec_page, rv$rec_total_pages, "rec")
    )
  }
  
  # Achievements Page
  render_achievements <- function() {
    tagList(
      tags$div(class = "achievements-page",
               tags$div(class = "section-header",
                        tags$h2(class = "section-title", "Hall of Fame")
               ),
               
               # Stats Overview
               tags$div(class = "achievement-stats",
                        tags$div(class = "achievement-stat-card",
                                 tags$div(class = "stat-number", length(rv$achievements_unlocked)),
                                 tags$div(class = "stat-label", "Unlocked")
                        ),
                        tags$div(class = "achievement-stat-card",
                                 tags$div(class = "stat-number", 
                                          paste0(round(length(rv$achievements_unlocked) / 20 * 100), "%")),
                                 tags$div(class = "stat-label", "Progress")
                        )
               ),
               
               # Badges Grid
               tags$div(class = "badges-grid-five-col-clean",
                        # Badge 1: First Watch
                        tags$div(class = paste("badge-card-clean", if("first_watch" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "first_watch",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/first_watch.png",
                                                   class = "badge-image-large",
                                                   alt = "First Watch Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "First Watch"),
                                 tags$p(class = "badge-description-large", "Added your first item")
                        ),
                        
                        # Badge 2: Media Mogul
                        tags$div(class = paste("badge-card-clean", if("media_mogul" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "media_mogul",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/media_mogul.png", 
                                                   class = "badge-image-large",
                                                   alt = "Media Mogul Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Media Mogul"),
                                 tags$p(class = "badge-description-large", "Add 100 items to your library")
                        ),
                        
                        # Badge 3: Marathon Master
                        tags$div(class = paste("badge-card-clean", if("marathon_master" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "marathon_master",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/marathon_master.png",
                                                   class = "badge-image-large",
                                                   alt = "Marathon Master Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Marathon Master"),
                                 tags$p(class = "badge-description-large", "Completed a TV series with 50+ episodes")
                        ),
                        
                        # Badge 4: Critic in Training
                        tags$div(class = paste("badge-card-clean", if("critic_training" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "critic_training",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/critic_training.png",
                                                   class = "badge-image-large",
                                                   alt = "Critic in Training Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Critic in Training"),
                                 tags$p(class = "badge-description-large", "Rated 10 items")
                        ),
                        
                        # Badge 5: Harsh Critic
                        tags$div(class = paste("badge-card-clean", if("harsh_critic" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "harsh_critic",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/harsh_critic.png",
                                                   class = "badge-image-large",
                                                   alt = "Harsh Critic Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Harsh Critic"),
                                 tags$p(class = "badge-description-large", "Gave a 1-star rating")
                        ),
                        
                        # Badge 6: Genre Sampler
                        tags$div(class = paste("badge-card-clean", if("genre_sampler" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "genre_sampler",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/genre_sampler.png",
                                                   class = "badge-image-large",
                                                   alt = "Genre Sampler Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Genre Sampler"),
                                 tags$p(class = "badge-description-large", "Watched items from 5+ different genres")
                        ),
                        
                        # Badge 7: Time Traveler
                        tags$div(class = paste("badge-card-clean", if("time_traveler" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "time_traveler",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/time_traveler.png",
                                                   class = "badge-image-large",
                                                   alt = "Time Traveler Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Time Traveler"),
                                 tags$p(class = "badge-description-large", "Watched something from 5 different decades")
                        ),
                        
                        # Badge 8: Classic Connoisseur
                        tags$div(class = paste("badge-card-clean", if("classic_connoisseur" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "classic_connoisseur",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/classic_connoisseur.png",
                                                   class = "badge-image-large",
                                                   alt = "Classic Connoisseur Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Classic Connoisseur"),
                                 tags$p(class = "badge-description-large", "Add item from 1970s or earlier")
                        ),
                        
                        # Badge 9: Streak Keeper
                        tags$div(class = paste("badge-card-clean", if("streak_keeper" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "streak_keeper",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/streak_keeper.png",
                                                   class = "badge-image-large",
                                                   alt = "Streak Keeper Badge")
                                 ),
                                 tags$h3(class = "badge-title-large", "Streak Keeper"),
                                 tags$p(class = "badge-description-large", "Watch 7 days in a row")
                        ),
                        
                        # Badge 10: Essayist
                        tags$div(class = paste("badge-card-clean", if("essayist" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "essayist",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/essayist.png",
                                                   class = "badge-image-large",
                                                   alt = "Essayist")
                                 ),
                                 tags$h3(class = "badge-title-large", "Essayist"),
                                 tags$p(class = "badge-description-large", "Write a 500+ character review")
                        ),
                        
                        # Badge 11: Haiku Reviewer
                        tags$div(class = paste("badge-card-clean", if("haiku_reviewer" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "haiku_reviewer",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/haiku_reviewer.png",
                                                   class = "badge-image-large",
                                                   alt = "Haiku Reviewer")
                                 ),
                                 tags$h3(class = "badge-title-large", "Haiku Reviewer"),
                                 tags$p(class = "badge-description-large", "Write a review under 100 characters")
                        ),
                        
                        # Badge 12: Actor Apprentice
                        tags$div(class = paste("badge-card-clean", if("actor_apprentice" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "actor_apprentice",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/actor_apprentice.png",
                                                   class = "badge-image-large",
                                                   alt = "Actor Apprentice")
                                 ),
                                 tags$h3(class = "badge-title-large", "Actor Apprentice"),
                                 tags$p(class = "badge-description-large", "Watch 5+ works by one actor")
                        ),
                        
                        # Badge 13: Director Devotee
                        tags$div(class = paste("badge-card-clean", if("director_devotee" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "director_devotee",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/director_devotee.png",
                                                   class = "badge-image-large",
                                                   alt = "Director Devotee")
                                 ),
                                 tags$h3(class = "badge-title-large", "Director Devotee"),
                                 tags$p(class = "badge-description-large", "Watch 5+ works by one director")
                        ),
                        
                        # Badge 14: Romance Royalty
                        tags$div(class = paste("badge-card-clean", if("romance_royalty" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "romance_royalty",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/romance_royalty.png",
                                                   class = "badge-image-large",
                                                   alt = "Romance Royalty")
                                 ),
                                 tags$h3(class = "badge-title-large", "Romance Royalty"),
                                 tags$p(class = "badge-description-large", "Watch 10+ romance movies")
                        ),
                        
                        # Badge 15: Mad Scientist
                        tags$div(class = paste("badge-card-clean", if("mad_scientist" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "mad_scientist",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/mad_scientist.png",
                                                   class = "badge-image-large",
                                                   alt = "Mad Scientist")
                                 ),
                                 tags$h3(class = "badge-title-large", "Mad Scientist"),
                                 tags$p(class = "badge-description-large", "Watch 10+ sci-fi movies")
                        ),
                        
                        # Badge 16: Perfectionist
                        tags$div(class = paste("badge-card-clean", if("perfectionist" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "perfectionist",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/perfectionist.png",
                                                   class = "badge-image-large",
                                                   alt = "Perfectionist")
                                 ),
                                 tags$h3(class = "badge-title-large", "Perfectionist"),
                                 tags$p(class = "badge-description-large", "Rate every item in your library")
                        ),
                        
                        # Badge 17: Numerical
                        tags$div(class = paste("badge-card-clean", if("numerical" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "numerical",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/numerical.png",
                                                   class = "badge-image-large",
                                                   alt = "Numerical")
                                 ),
                                 tags$h3(class = "badge-title-large", "Numerical"),
                                 tags$p(class = "badge-description-large", "Watch items with numbers in title")
                        ),
                        
                        # Badge 18: The Developer's Cut (SECRET)
                        tags$div(class = paste("badge-card-clean developers-cut", if("developers_cut" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "developers_cut",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/developers_cut.png",
                                                   class = "badge-image-large",
                                                   alt = "The Developer's Cut")
                                 ),
                                 tags$h3(class = "badge-title-large", "The Developer's Cut"),
                                 if("developers_cut" %in% rv$achievements_unlocked) {
                                   tags$p(class = "badge-description-large", "Watch the developer's favorite movie")
                                 } else {
                                   tags$p(class = "badge-description-large", "???")
                                 }
                        ),
                        
                        # Badge 19: The Developer's Season (SECRET)
                        tags$div(class = paste("badge-card-clean developers-season", if("developers_season" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "developers_season",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/developers_season.png",
                                                   class = "badge-image-large",
                                                   alt = "The Developer's Season")
                                 ),
                                 tags$h3(class = "badge-title-large", "The Developer's Season"),
                                 if("developers_season" %in% rv$achievements_unlocked) {
                                   tags$p(class = "badge-description-large", "Complete the developer's favorite TV series")
                                 } else {
                                   tags$p(class = "badge-description-large", "???")
                                 }
                        ),
                        
                        # Badge 20: THE PLOTWIST BADGE (SECRET)
                        tags$div(class = paste("badge-card-clean plotwist-ultimate", if("plotwist_badge" %in% rv$achievements_unlocked) "unlocked" else "locked"),
                                 `data-badge-id` = "plotwist_badge",
                                 tags$div(class = "badge-icon-large",
                                          tags$img(src = "badges/plotwist_badge.png",
                                                   class = "badge-image-large",
                                                   alt = "THE PLOTWIST BADGE")
                                 ),
                                 tags$h3(class = "badge-title-large", "THE PLOTWIST"),
                                 if("plotwist_badge" %in% rv$achievements_unlocked) {
                                   tags$p(class = "badge-description-large", "Rate 'Plotwist' as a movie/TV series (4+ stars)")
                                 } else {
                                   tags$p(class = "badge-description-large", "???")
                                 }
                        )
               )
      )
    )
  }
  
  # Library tab navigation handlers
  observeEvent(input$library_all_btn, { 
    rv$library_view <- "all" 
    rv$library_page <- 1  # Reset to page 1 when switching tabs
  })
  
  observeEvent(input$library_movies_btn, { 
    rv$library_view <- "movies" 
    rv$library_page <- 1  # Reset to page 1 when switching tabs
  })
  
  observeEvent(input$library_series_btn, { 
    rv$library_view <- "series" 
    rv$library_page <- 1  # Reset to page 1 when switching tabs
  })
  
  # Recommendations tab navigation handlers
  observeEvent(input$rec_movies_btn, { 
    rv$rec_type <- "movie" 
    rv$rec_page <- 1  # Reset to page 1 when switching tabs
    rv$rec_movies_clicked <- rv$rec_movies_clicked + 1
  })
  
  observeEvent(input$rec_series_btn, { 
    rv$rec_type <- "tv"
    rv$rec_page <- 1  # Reset to page 1 when switching tabs
    rv$rec_series_clicked <- rv$rec_series_clicked + 1
  })

  observeEvent(input$stats_all_btn, { 
    rv$stats_media_type <- "all"
  })
  
  observeEvent(input$stats_movies_btn, { 
    rv$stats_media_type <- "movies"
  })
  
  observeEvent(input$stats_series_btn, { 
    rv$stats_media_type <- "series"
  })
  
  # ==============================================================================
  # SEARCH, SORT, AND FILTER HANDLERS
  # ==============================================================================

  observeEvent(input$search_trigger, {
    # Update the search query
    rv$search_query <- input$search_input
    rv$library_page <- 1  # Reset to page 1 when searching
  })
  
  # Sort handlers
  observeEvent(input$sort_date_desc, {
    rv$sort_by <- "date_added"
    rv$sort_order <- "desc"
  })
  
  observeEvent(input$sort_date_asc, {
    rv$sort_by <- "date_added"
    rv$sort_order <- "asc"
  })
  
  observeEvent(input$sort_title_asc, {
    rv$sort_by <- "title"
    rv$sort_order <- "asc"
  })
  
  observeEvent(input$sort_title_desc, {
    rv$sort_by <- "title"
    rv$sort_order <- "desc"
  })
  
  observeEvent(input$sort_year_desc, {
    rv$sort_by <- "year"
    rv$sort_order <- "desc"
  })
  
  observeEvent(input$sort_year_asc, {
    rv$sort_by <- "year"
    rv$sort_order <- "asc"
  })
  
  observeEvent(input$sort_rating_desc, {
    rv$sort_by <- "rating"
    rv$sort_order <- "desc"
  })
  
  observeEvent(input$sort_rating_asc, {
    rv$sort_by <- "rating"
    rv$sort_order <- "asc"
  })
  
  # Filter handlers
  observeEvent(input$filter_status_all, {
    rv$filter_status <- "all"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_status_watched, {
    rv$filter_status <- "Watched"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_status_watching, {
    rv$filter_status <- "Watching"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_status_unwatched, {
    rv$filter_status <- "Unwatched"
    rv$library_page <- 1
  })
  
  # Genre filter handlers
  observeEvent(input$filter_genre_all, {
    rv$filter_genre <- "all"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_action, {
    rv$filter_genre <- "Action"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_adventure, {
    rv$filter_genre <- "Adventure"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_comedy, {
    rv$filter_genre <- "Comedy"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_drama, {
    rv$filter_genre <- "Drama"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_horror, {
    rv$filter_genre <- "Horror"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_scifi, {
    rv$filter_genre <- "Science Fiction"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_fantasy, {
    rv$filter_genre <- "Fantasy"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_thriller, {
    rv$filter_genre <- "Thriller"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_romance, {
    rv$filter_genre <- "Romance"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_genre_animation, {
    rv$filter_genre <- "Animation"
    rv$library_page <- 1
  })
  
  # Rating filter handlers
  observeEvent(input$filter_rating_all, {
    rv$filter_rating <- "all"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_rating_1, {
    rv$filter_rating <- "1"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_rating_2, {
    rv$filter_rating <- "2"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_rating_3, {
    rv$filter_rating <- "3"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_rating_4, {
    rv$filter_rating <- "4"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_rating_5, {
    rv$filter_rating <- "5"
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_rating_4_5, {
    rv$filter_rating <- "4-5"
    rv$library_page <- 1
  })
  
  # Year filter handlers
  observeEvent(input$filter_year_trigger, {
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_year_min, {
    rv$filter_year_min <- as.numeric(input$filter_year_min)
    rv$library_page <- 1
  })
  
  observeEvent(input$filter_year_max, {
    rv$filter_year_max <- as.numeric(input$filter_year_max)
    rv$library_page <- 1
  })
  
  # Clear filters
  observeEvent(input$clear_all_filters, {
    rv$search_query <- ""
    rv$sort_by <- "date_added"
    rv$sort_order <- "desc"
    rv$filter_status <- "all"
    rv$filter_genre <- "all"
    rv$filter_rating <- "all"
    rv$filter_year_min <- 1900
    rv$filter_year_max <- 2025
    rv$library_page <- 1
  })
  
  observeEvent(input$clear_search, {
    rv$search_query <- ""
    rv$library_page <- 1
  })
  
  observeEvent(input$clear_year_filter, {
    rv$filter_year_min <- 1900
    rv$filter_year_max <- 2025
    rv$library_page <- 1
  })
  
  # Handle View All button click to redirect to library
  observeEvent(input$view_all_clicked, {
    rv$page <- "library"
    updateNavButtons("library")
    rv$library_page <- 1
  })
  
  # Recommendations pagination controls
  observeEvent(input$rec_prev_page, {
    if (rv$rec_page > 1) {
      rv$rec_page <- rv$rec_page - 1
    }
  })
  
  observeEvent(input$rec_next_page, {
    if (rv$rec_page < rv$rec_total_pages) {
      rv$rec_page <- rv$rec_page + 1
    }
  })
  
  observeEvent(input$rec_go_to_page, {
    if (!is.null(input$rec_go_to_page) && input$rec_go_to_page >= 1 && input$rec_go_to_page <= rv$rec_total_pages) {
      rv$rec_page <- input$rec_go_to_page
    }
  })
  
  # ==============================================================================
  # LIBRARY PAGINATION CONTROLS
  # ==============================================================================
  
  observeEvent(input$library_prev_page, {
    items <- get_library_items()
    total_pages <- ceiling(nrow(items) / rv$library_items_per_page)
    if (rv$library_page > 1) {
      rv$library_page <- rv$library_page - 1
    }
  })
  
  observeEvent(input$library_next_page, {
    items <- get_library_items()
    total_pages <- ceiling(nrow(items) / rv$library_items_per_page)
    if (rv$library_page < total_pages) {
      rv$library_page <- rv$library_page + 1
    }
  })
  
  observeEvent(input$library_go_to_page, {
    items <- get_library_items()
    total_pages <- ceiling(nrow(items) / rv$library_items_per_page)
    if (!is.null(input$library_go_to_page) && 
        input$library_go_to_page >= 1 && 
        input$library_go_to_page <= total_pages) {
      rv$library_page <- input$library_go_to_page
    }
  })

  render_movie_card <- function(item) {
    status_class <- switch(item$watch_status,
                           "Watched" = "badge-watched",
                           "Watching" = "badge-watching",
                           "Unwatched" = "badge-unwatched")
    
    poster <- if (!is.na(item$poster_url) && item$poster_url != "") {
      item$poster_url
    } else {
      "https://via.placeholder.com/500x750/1A1F29/00A8E8?text=No+Poster"
    }
    
    progress_html <- NULL

    if (item$media_type == "TV Series" && !is.na(item$total_episodes) && !is.null(item$total_episodes) && item$total_episodes > 0) {
      total_episodes_watched <- ifelse(is.na(item$total_episodes_watched) || is.null(item$total_episodes_watched) || item$total_episodes_watched < 0, 
                                       0, item$total_episodes_watched)
      total_episodes <- ifelse(is.na(item$total_episodes) || is.null(item$total_episodes) || item$total_episodes < 1, 
                               1, item$total_episodes)
      
      progress <- round((total_episodes_watched / total_episodes) * 100)
      color <- if (progress < 26) "#EF476F" else if (progress < 76) "#FFB800" else "#06D6A0"
      
      # Format display based on watch status
      if (item$watch_status == "Watching") {
        current_season <- ifelse(is.na(item$current_season) || item$current_season < 1, 1, item$current_season)
        current_episode <- ifelse(is.na(item$current_episode) || item$current_episode < 1, 1, item$current_episode)
        progress_text <- paste0("S", current_season, "E", current_episode, " (", 
                                total_episodes_watched, "/", total_episodes, " eps)")
      } else if (item$watch_status == "Watched") {
        total_seasons <- ifelse(is.na(item$total_seasons) || item$total_seasons < 1, 1, item$total_seasons)
        progress_text <- paste0("Complete (", total_episodes, " eps, ", total_seasons, " seasons)")
      } else {
        total_seasons <- ifelse(is.na(item$total_seasons) || item$total_seasons < 1, 1, item$total_seasons)
        progress_text <- paste0("Not started (", total_episodes, " eps, ", total_seasons, " seasons)")
      }
      
      progress_html <- tags$div(class = "progress-container",
                                tags$div(class = "progress-label", progress_text),
                                tags$div(class = "progress-bg",
                                         tags$div(class = "progress-fill", style = paste0("width: ", progress, "%; background: ", color, ";"))
                                )
      )
    }

    else if (item$media_type == "Movie" && !is.na(item$total_duration) && !is.null(item$total_duration) && item$total_duration > 0) {
      watched_duration <- ifelse(is.na(item$watched_duration) || is.null(item$watched_duration) || item$watched_duration < 0, 
                                 0, item$watched_duration)
      total_duration <- ifelse(is.na(item$total_duration) || is.null(item$total_duration) || item$total_duration < 1, 
                               1, item$total_duration)
      
      progress <- round((watched_duration / total_duration) * 100)
      color <- if (progress < 26) "#EF476F" else if (progress < 76) "#FFB800" else "#06D6A0"
      
      # Format duration display
      total_duration_str <- if (total_duration >= 60) {
        hours <- floor(total_duration / 60)
        mins <- total_duration %% 60
        paste0(hours, "h ", mins, "m")
      } else {
        paste0(total_duration, "m")
      }
      
      watched_duration_str <- if (watched_duration >= 60) {
        hours <- floor(watched_duration / 60)
        mins <- watched_duration %% 60
        paste0(hours, "h ", mins, "m")
      } else {
        paste0(watched_duration, "m")
      }
      
      progress_html <- tags$div(class = "progress-container",
                                tags$div(class = "progress-label", 
                                         paste0(watched_duration_str, " / ", total_duration_str)),
                                tags$div(class = "progress-bg",
                                         tags$div(class = "progress-fill", style = paste0("width: ", progress, "%; background: ", color, ";"))
                                )
      )
    }
    
    # ALWAYS show rating stars, even if rating is 0
    rating_value <- ifelse(is.na(item$rating), 0, item$rating)
    filled_stars <- if (rating_value > 0) paste(rep("★", rating_value), collapse = "") else ""
    empty_stars <- if (rating_value < 5) paste(rep("☆", 5 - rating_value), collapse = "") else ""
    
    rating_html <- tags$div(class = "movie-rating",
                            tags$span(style = if(rating_value > 0) "color: #FFB800;" else "color: rgba(155, 163, 176, 0.4);", 
                                      filled_stars),
                            tags$span(style = "color: rgba(155, 163, 176, 0.4);", empty_stars),
                            if (rating_value > 0) tags$span(style = "margin-left: 4px; color: #FFB800; font-weight: 600;", rating_value)
    )
    
    tags$div(class = "movie-card", `data-item-id` = item$id,
             tags$img(class = "movie-poster", src = poster),
             tags$div(class = paste("movie-badge", status_class), toupper(item$watch_status)),
             tags$div(class = "movie-info",
                      tags$h3(class = "movie-title", item$title),
                      tags$div(class = "movie-meta",
                               if (!is.na(item$year)) tags$span(item$year),
                               tags$span("•"),
                               tags$span(item$media_type)
                      ),
                      rating_html,
                      if (!is.null(progress_html)) {
                        tags$div(style = "margin-bottom: 0.5rem;", progress_html)
                      }
             )
    )
  }

  observeEvent(input$hero_button_click, {
    rv$page <- "recommendations"
    updateNavButtons("recommendations")
  })
}

shinyApp(ui = ui, server = server)