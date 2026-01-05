-- PostgreSQL Schema for Plotwist
-- Converted from MySQL for Render deployment

-- Drop tables if they exist (for clean migration)
DROP TABLE IF EXISTS watch_history CASCADE;
DROP TABLE IF EXISTS movies_series CASCADE;
DROP TABLE IF EXISTS users CASCADE;

-- Users Table
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    last_login TIMESTAMP DEFAULT NULL
);

-- Movies/Series Table
CREATE TABLE movies_series (
    id SERIAL PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    media_type VARCHAR(20) NOT NULL DEFAULT 'Movie' CHECK (media_type IN ('Movie', 'TV Series')),
    genre TEXT DEFAULT NULL,
    year INTEGER DEFAULT NULL,
    watch_status VARCHAR(20) NOT NULL DEFAULT 'Unwatched' CHECK (watch_status IN ('Watched', 'Watching', 'Unwatched')),
    rating INTEGER DEFAULT 0 CHECK (rating >= 0 AND rating <= 5),
    review TEXT DEFAULT NULL,
    director VARCHAR(255) DEFAULT NULL,
    "cast" TEXT DEFAULT NULL,
    plot_summary TEXT DEFAULT NULL,
    poster_url TEXT DEFAULT NULL,
    total_episodes INTEGER DEFAULT 0,
    date_added TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    date_watched DATE DEFAULT NULL,
    date_modified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    total_duration INTEGER DEFAULT 0,
    watched_duration INTEGER DEFAULT 0,
    total_seasons INTEGER DEFAULT 0,
    current_season INTEGER DEFAULT 0,
    episodes_current_season INTEGER DEFAULT 0,
    current_episode INTEGER DEFAULT 0,
    total_episodes_watched INTEGER DEFAULT 0,
    tmdb_id INTEGER DEFAULT NULL
);

-- Add comment to total_duration
COMMENT ON COLUMN movies_series.total_duration IS 'Total duration in minutes (for movies)';
COMMENT ON COLUMN movies_series.watched_duration IS 'Watched duration in minutes (for movies)';

-- Watch History Table
CREATE TABLE watch_history (
    id SERIAL PRIMARY KEY,
    movie_id INTEGER NOT NULL,
    watch_date DATE NOT NULL,
    notes TEXT DEFAULT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_movie_id FOREIGN KEY (movie_id) REFERENCES movies_series(id) ON DELETE CASCADE
);

-- Create Indexes for Performance
CREATE INDEX idx_watch_status ON movies_series(watch_status);
CREATE INDEX idx_media_type ON movies_series(media_type);
CREATE INDEX idx_date_added ON movies_series(date_added);
CREATE INDEX idx_date_watched ON movies_series(date_watched);
CREATE INDEX idx_movie_id ON watch_history(movie_id);
CREATE INDEX idx_watch_date ON watch_history(watch_date);
CREATE INDEX idx_email ON users(email);

-- Function to automatically update date_modified
CREATE OR REPLACE FUNCTION update_modified_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.date_modified = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to call the function
CREATE TRIGGER update_movies_series_modtime
    BEFORE UPDATE ON movies_series
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();