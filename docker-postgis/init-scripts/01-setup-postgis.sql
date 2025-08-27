-- docker-postgis/init-scripts/01-setup-postgis.sql
-- Automatic PostGIS setup for GPS2 spatial analysis
-- This script runs ONCE when the database container first starts

-- Enable all PostGIS extensions for comprehensive spatial operations
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;

-- Create dedicated schema for GPS2 research data
CREATE SCHEMA IF NOT EXISTS gps2;

-- Grant comprehensive permissions to our research user
GRANT ALL ON SCHEMA gps2 TO gps2_researcher;
GRANT ALL ON ALL TABLES IN SCHEMA gps2 TO gps2_researcher;
GRANT ALL ON ALL SEQUENCES IN SCHEMA gps2 TO gps2_researcher;
ALTER DEFAULT PRIVILEGES IN SCHEMA gps2 GRANT ALL ON TABLES TO gps2_researcher;
ALTER DEFAULT PRIVILEGES IN SCHEMA gps2 GRANT ALL ON SEQUENCES TO gps2_researcher;

-- Core GPS data table
CREATE TABLE gps2.gps_stationary_points (
    id SERIAL PRIMARY KEY,
    subid INTEGER NOT NULL,
    location GEOMETRY(POINT, 4326) NOT NULL,
    lat NUMERIC(10, 7) NOT NULL,
    lon NUMERIC(10, 7) NOT NULL,
    dttm_obs TIMESTAMP WITH TIME ZONE NOT NULL,
    dist NUMERIC(10, 4),
    duration NUMERIC(10, 4),
    speed NUMERIC(10, 4),
    transit VARCHAR(10),
    movement_state VARCHAR(20),
    date_observed DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Constraints
    CONSTRAINT valid_coordinates CHECK (lat BETWEEN -90 AND 90 AND lon BETWEEN -180 AND 180),
    CONSTRAINT valid_movement_state CHECK (movement_state IN ('stationary', 'transition'))
);

-- Location clusters table
CREATE TABLE gps2.location_clusters (
    id SERIAL PRIMARY KEY,
    cluster_id INTEGER NOT NULL,
    subid INTEGER NOT NULL,
    location GEOMETRY(POINT, 4326) NOT NULL,
    lat NUMERIC(10, 7) NOT NULL,
    lon NUMERIC(10, 7) NOT NULL,
    n_points INTEGER NOT NULL CHECK (n_points > 0),
    first_visit TIMESTAMP WITH TIME ZONE,
    last_visit TIMESTAMP WITH TIME ZONE,
    total_visits INTEGER CHECK (total_visits > 0),
    total_duration_hours NUMERIC(10, 2) CHECK (total_duration_hours >= 0),
    unique_days INTEGER CHECK (unique_days > 0),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Unique constraint to prevent duplicate clusters
    UNIQUE(subid, cluster_id)
);

-- Raw GPS data table (unprocessed)
CREATE TABLE gps2.gps_raw_points (
    id SERIAL PRIMARY KEY,
    subid INTEGER NOT NULL,
    location GEOMETRY(POINT, 4326) NOT NULL,
    lat NUMERIC(10, 7) NOT NULL,
    lon NUMERIC(10, 7) NOT NULL,
    time TIMESTAMP WITH TIME ZONE NOT NULL,
    sgmnt_type VARCHAR(50),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Constraints
    CONSTRAINT valid_raw_coordinates CHECK (lat BETWEEN -90 AND 90 AND lon BETWEEN -180 AND 180),
    UNIQUE(subid, time, lat, lon)
);

-- Geocoding results table
CREATE TABLE gps2.cluster_geocoding (
    id SERIAL PRIMARY KEY,
    cluster_id INTEGER NOT NULL,
    subid INTEGER NOT NULL,
    display_name TEXT,
    house_number VARCHAR(20),
    road VARCHAR(200),
    neighbourhood VARCHAR(100),
    city VARCHAR(100),
    county VARCHAR(100),
    state VARCHAR(50),
    postcode VARCHAR(20),
    country VARCHAR(50),
    place_type VARCHAR(50),
    osm_type VARCHAR(10),
    osm_id BIGINT,
    geocoding_confidence NUMERIC(3,2) CHECK (geocoding_confidence BETWEEN 0 AND 1),
    geocoding_method VARCHAR(50),
    processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Reference to cluster (foreign key)
    FOREIGN KEY (subid, cluster_id) REFERENCES gps2.location_clusters(subid, cluster_id) ON DELETE CASCADE,
    UNIQUE(subid, cluster_id)
);

-- Performance indexes
CREATE INDEX idx_gps_stationary_location ON gps2.gps_stationary_points USING GIST (location);
CREATE INDEX idx_gps_stationary_subid_date ON gps2.gps_stationary_points (subid, date_observed);
CREATE INDEX idx_gps_stationary_dttm ON gps2.gps_stationary_points (dttm_obs);
CREATE INDEX idx_gps_stationary_subid ON gps2.gps_stationary_points (subid);

CREATE INDEX idx_gps_raw_location ON gps2.gps_raw_points USING GIST (location);
CREATE INDEX idx_gps_raw_subid ON gps2.gps_raw_points (subid);
CREATE INDEX idx_gps_raw_time ON gps2.gps_raw_points (time);
CREATE INDEX idx_gps_raw_sgmnt_type ON gps2.gps_raw_points (sgmnt_type);

CREATE INDEX idx_clusters_location ON gps2.location_clusters USING GIST (location);
CREATE INDEX idx_clusters_subid ON gps2.location_clusters (subid);
CREATE INDEX idx_clusters_visits ON gps2.location_clusters (total_visits DESC);
CREATE INDEX idx_clusters_duration ON gps2.location_clusters (total_duration_hours DESC);

CREATE INDEX idx_geocoding_subid_cluster ON gps2.cluster_geocoding (subid, cluster_id);
CREATE INDEX idx_geocoding_place_type ON gps2.cluster_geocoding (place_type);
CREATE INDEX idx_geocoding_method ON gps2.cluster_geocoding (geocoding_method);

-- Test data for connection verification (Madison, WI locations)
INSERT INTO gps2.gps_stationary_points 
    (subid, location, lat, lon, dttm_obs, movement_state, date_observed)
VALUES
    (999, ST_SetSRID(ST_MakePoint(-89.384373, 43.074713), 4326), 
     43.074713, -89.384373, NOW(), 'stationary', CURRENT_DATE),
    (999, ST_SetSRID(ST_MakePoint(-89.4125, 43.0766), 4326), 
     43.0766, -89.4125, NOW(), 'stationary', CURRENT_DATE),
    (999, ST_SetSRID(ST_MakePoint(-89.384444, 43.074722), 4326), 
     43.074722, -89.384444, NOW(), 'stationary', CURRENT_DATE);

-- Utility function to test spatial calculations
CREATE OR REPLACE FUNCTION gps2.test_spatial_functions()
RETURNS TABLE(
    from_location VARCHAR(100),
    to_location VARCHAR(100), 
    distance_meters NUMERIC
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        CASE 
            WHEN a.id = (SELECT MIN(id) FROM gps2.gps_stationary_points WHERE subid = 999) THEN 'Madison Downtown'
            WHEN a.id = (SELECT MIN(id) + 1 FROM gps2.gps_stationary_points WHERE subid = 999) THEN 'UW-Madison Campus'
            ELSE 'Capitol Square'
        END as from_location,
        CASE 
            WHEN b.id = (SELECT MIN(id) FROM gps2.gps_stationary_points WHERE subid = 999) THEN 'Madison Downtown'
            WHEN b.id = (SELECT MIN(id) + 1 FROM gps2.gps_stationary_points WHERE subid = 999) THEN 'UW-Madison Campus'
            ELSE 'Capitol Square'
        END as to_location,
        ROUND(ST_Distance(a.location::geography, b.location::geography)) as distance_meters
    FROM gps2.gps_stationary_points a
    CROSS JOIN gps2.gps_stationary_points b
    WHERE a.id != b.id AND a.subid = 999 AND b.subid = 999
    ORDER BY a.id, b.id;
END;
$$ LANGUAGE plpgsql;

-- View for easy data exploration
CREATE VIEW gps2.participant_summary AS
SELECT 
    subid,
    COUNT(*) as total_points,
    MIN(dttm_obs) as first_observation,
    MAX(dttm_obs) as last_observation,
    COUNT(DISTINCT DATE(dttm_obs)) as unique_days,
    ROUND(ST_X(ST_Centroid(ST_Collect(location)))::numeric, 4) as center_lon,
    ROUND(ST_Y(ST_Centroid(ST_Collect(location)))::numeric, 4) as center_lat,
    ROUND(ST_Distance(
        ST_SetSRID(ST_MakePoint(MIN(ST_X(location)), MIN(ST_Y(location))), 4326)::geography,
        ST_SetSRID(ST_MakePoint(MAX(ST_X(location)), MAX(ST_Y(location))), 4326)::geography
    )) as max_distance_meters
FROM gps2.gps_stationary_points
WHERE subid != 999  -- Exclude test data
GROUP BY subid
ORDER BY total_points DESC;