-- GPS22 Project: Database Schema
-- Description: Core tables for GPS clustering and zoning analysis

-- 1. Subjects table: Normalized subject registry
CREATE TABLE subjects (
    id SERIAL PRIMARY KEY,
    subid TEXT UNIQUE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 2. Raw GPS points table: Normalized with foreign key reference
CREATE TABLE raw_gps_points (
    point_id BIGSERIAL PRIMARY KEY,
    subject_id INTEGER NOT NULL REFERENCES subjects(id),
    lat DOUBLE PRECISION NOT NULL,
    lon DOUBLE PRECISION NOT NULL,
    time TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    sgmnt_type TEXT CHECK (sgmnt_type IN ('place', 'move', 'off') OR sgmnt_type IS NULL),
    
    -- PostGIS geometry column (SRID 4326 = WGS84)
    geom GEOMETRY(POINT, 4326),

    -- Movement classification (populated by R analysis)
    is_stationary BOOLEAN DEFAULT NULL,

    -- Import tracking
    imported_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT valid_coordinates CHECK (
        lat BETWEEN -90 AND 90 AND 
        lon BETWEEN -180 AND 180
    )
);

-- 3. Processed GPS points table: Results from R processing pipeline
CREATE TABLE processed_gps_points (
    processed_id BIGSERIAL PRIMARY KEY,
    raw_point_id BIGINT REFERENCES raw_gps_points(point_id),
    subject_id INTEGER NOT NULL REFERENCES subjects(id),
    
    -- Original coordinates
    lat DOUBLE PRECISION NOT NULL,
    lon DOUBLE PRECISION NOT NULL,
    time TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    
    -- Processed timestamps and movement metrics
    dttm_obs TIMESTAMP WITHOUT TIME ZONE NOT NULL,  -- timezone-converted
    dist_miles DOUBLE PRECISION DEFAULT 0,
    duration_mins DOUBLE PRECISION DEFAULT 0,
    speed_mph DOUBLE PRECISION DEFAULT 0,
    
    -- Movement classification
    movement_state TEXT NOT NULL CHECK (movement_state IN ('stationary', 'transition')),
    is_stationary BOOLEAN NOT NULL,
    
    -- Clustering results (NULL for transition points)
    cluster_id BIGINT DEFAULT NULL,
    
    -- PostGIS geometry column
    geom GEOMETRY(POINT, 4326),
    
    -- Processing metadata
    processed_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT valid_processed_coords CHECK (
        lat BETWEEN -90 AND 90 AND 
        lon BETWEEN -180 AND 180
    ),
    CONSTRAINT valid_speed CHECK (speed_mph >= 0),
    CONSTRAINT valid_duration CHECK (duration_mins >= 0)
);

-- 4. GPS clusters table: Results from R clustering algorithm
CREATE TABLE gps_clusters (
    cluster_id BIGSERIAL PRIMARY KEY,
    subject_id INTEGER NOT NULL REFERENCES subjects(id),
    
    -- Representative cluster location
    lat DOUBLE PRECISION NOT NULL,
    lon DOUBLE PRECISION NOT NULL,
    geom GEOMETRY(POINT, 4326),
    
    -- Cluster statistics
    n_points INTEGER NOT NULL,
    first_visit TIMESTAMP WITHOUT TIME ZONE,
    last_visit TIMESTAMP WITHOUT TIME ZONE,
    total_visits INTEGER DEFAULT 1,
    total_duration_hours DOUBLE PRECISION,
    
    -- Analysis metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT valid_cluster_coords CHECK (
        lat BETWEEN -90 AND 90 AND 
        lon BETWEEN -180 AND 180
    ),
    CONSTRAINT positive_stats CHECK (
        n_points > 0 AND 
        total_visits > 0 AND 
        total_duration_hours >= 0
    )
);

-- 5. Madison Zoning Districts table: Madison-specific zoning data
CREATE TABLE madison_zoning_districts (
    id SERIAL PRIMARY KEY,
    objectid INTEGER,
    zone_code TEXT NOT NULL,
    zone_category TEXT NOT NULL,
    area_sqm NUMERIC,
    geom GEOMETRY(MULTIPOLYGON, 4326),
    
    -- Metadata
    imported_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT valid_zone_category CHECK (
        zone_category IN ('Residential', 'Mixed-Use', 'Commercial', 'Downtown', 'Employment', 'Industrial', 'Special', 'Other')
    )
);

-- 6. Reverse geocode results table: Nominatim API cache
CREATE TABLE reverse_geocode_results (
    cluster_id BIGINT PRIMARY KEY REFERENCES gps_clusters(cluster_id),
    
    -- Address components
    address TEXT,
    city TEXT,
    state TEXT,
    country TEXT DEFAULT 'United States',
    postcode TEXT,
    
    -- Raw Nominatim response
    nominatim_json JSONB,
    
    -- API metadata
    geocoded_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    nominatim_place_id BIGINT
);

-- 7. Final analysis table: Pre-joined summary for fast queries
CREATE TABLE final_analysis (
    analysis_id BIGSERIAL PRIMARY KEY,
    subject_id INTEGER NOT NULL REFERENCES subjects(id),
    cluster_id BIGINT REFERENCES gps_clusters(cluster_id),
    zone_id INTEGER REFERENCES madison_zoning_districts(id),

    -- Location info
    cluster_lat DOUBLE PRECISION,
    cluster_lon DOUBLE PRECISION,
    address TEXT,
    zone_name TEXT,
    zone_type TEXT,

    -- Time metrics
    time_spent_hours DOUBLE PRECISION,
    visit_count INTEGER,
    first_visit TIMESTAMP WITHOUT TIME ZONE,
    last_visit TIMESTAMP WITHOUT TIME ZONE,

    -- Analysis timestamp
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 8. Daily location entropy table: Shannon's entropy for location patterns
CREATE TABLE daily_location_entropy (
    subject_id INTEGER REFERENCES subjects(id),
    date DATE NOT NULL,
    location_entropy DOUBLE PRECISION,
    location_predictability DOUBLE PRECISION,
    unique_locations INTEGER,
    total_time_hours DOUBLE PRECISION,
    total_points INTEGER,
    calculated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    PRIMARY KEY (subject_id, date)
);

-- 9. ADI Block Groups table: Area Deprivation Index for Wisconsin
-- Privacy-compliant: All spatial joins performed locally in PostGIS, no external API calls
CREATE TABLE adi_block_groups (
    fips_2020 TEXT PRIMARY KEY,
    state_postal TEXT NOT NULL,

    -- NaNDA Standardized ADI metrics
    adi_national_percentile INTEGER CHECK (adi_national_percentile BETWEEN 1 AND 100),
    adi_state_decile INTEGER CHECK (adi_state_decile BETWEEN 1 AND 10),

    -- Derived FIPS components (computed from fips_2020)
    state_fips TEXT GENERATED ALWAYS AS (SUBSTRING(fips_2020, 1, 2)) STORED,
    county_fips TEXT GENERATED ALWAYS AS (SUBSTRING(fips_2020, 3, 3)) STORED,
    tract_fips TEXT GENERATED ALWAYS AS (SUBSTRING(fips_2020, 6, 6)) STORED,
    block_group TEXT GENERATED ALWAYS AS (SUBSTRING(fips_2020, 12, 1)) STORED,

    -- Spatial geometry (Census block group boundaries)
    geom GEOMETRY(MULTIPOLYGON, 4326),
    area_sqm NUMERIC,

    -- Metadata
    adi_year INTEGER DEFAULT 2020,
    data_source TEXT DEFAULT 'NaNDA Standardized ADI 2020',
    data_coverage TEXT DEFAULT 'Wisconsin',
    imported_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT valid_fips_length CHECK (LENGTH(fips_2020) = 12)
);

COMMENT ON TABLE adi_block_groups IS
'Area Deprivation Index (ADI) by Census block group. Privacy-compliant: All spatial processing performed locally.';

COMMENT ON COLUMN adi_block_groups.adi_national_percentile IS
'National percentile rank (1-100, higher = more deprived). Source: NaNDA Standardized ADI.';

COMMENT ON COLUMN adi_block_groups.adi_state_decile IS
'State decile rank (1-10, higher = more deprived within state).';

-- Create trigger to automatically populate geometry columns
CREATE OR REPLACE FUNCTION update_geom_from_coords()
RETURNS TRIGGER AS $$
BEGIN
    -- Update geometry column from lat/lon coordinates
    NEW.geom = ST_SetSRID(ST_MakePoint(NEW.lon, NEW.lat), 4326);
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Apply geometry trigger to relevant tables
CREATE TRIGGER raw_gps_points_geom_trigger
    BEFORE INSERT OR UPDATE ON raw_gps_points
    FOR EACH ROW
    EXECUTE FUNCTION update_geom_from_coords();

CREATE TRIGGER processed_gps_points_geom_trigger
    BEFORE INSERT OR UPDATE ON processed_gps_points
    FOR EACH ROW
    EXECUTE FUNCTION update_geom_from_coords();

CREATE TRIGGER gps_clusters_geom_trigger
    BEFORE INSERT OR UPDATE ON gps_clusters
    FOR EACH ROW
    EXECUTE FUNCTION update_geom_from_coords();