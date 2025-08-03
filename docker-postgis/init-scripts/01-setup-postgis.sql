-- init-scripts/01-setup-postgis.sql
-- automatic PostGIS setup for GPS2 spatial analysis
-- This script is designed to run once when the database container first starts

-- enable all PostGIS extensions for comprehensive spatial operations
CREATE EXTENSION IF NOT EXISTS postgis;           -- core spatial functions
CREATE EXTENSION IF NOT EXISTS postgis_topology;  -- topological analysis
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;     -- string matching for geocoding
CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder; -- US Census geocoding

-- create dedicated schema for GPS2 research data
CREATE SCHEMA IF NOT EXISTS gps2;

-- grant comprehensive permissions to our research user
GRANT ALL ON SCHEMA gps2 TO gps2_researcher;
GRANT ALL ON ALL TABLES IN SCHEMA gps2 TO gps2_researcher;
GRANT ALL ON ALL SEQUENCES IN SCHEMA gps2 TO gps2_researcher;
-- ensure future tables also have proper permissions
ALTER DEFAULT PRIVILEGES IN SCHEMA gps2 GRANT ALL ON TABLES TO gps2_researcher;
ALTER DEFAULT PRIVILEGES IN SCHEMA gps2 GRANT ALL ON SEQUENCES TO gps2_researcher;

-- create a test table to verify spatial functionality is working
-- this uses Madison, WI coordinates
CREATE TABLE IF NOT EXISTS gps2.connection_test (
    id SERIAL PRIMARY KEY,
    location_name VARCHAR(100),
    test_point GEOMETRY(POINT, 4326), -- 4326 is the standard GPS coordinate system
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- insert test points representing key Madison locations for verification
INSERT INTO gps2.connection_test (location_name, test_point) VALUES
    ('Madison Downtown', ST_SetSRID(ST_MakePoint(-89.384373, 43.074713), 4326)),
    ('UW-Madison Campus', ST_SetSRID(ST_MakePoint(-89.4125, 43.0766), 4326)),
    ('Capitol Square', ST_SetSRID(ST_MakePoint(-89.384444, 43.074722), 4326));

-- create spatial indexes for fast geographic queries
-- these indexes make distance calculations and spatial searches much faster
CREATE INDEX IF NOT EXISTS idx_connection_test_geom 
ON gps2.connection_test USING GIST (test_point);

-- function to test that spatial calculations work correctly
-- (verify that PostGIS is functioning properly)
CREATE OR REPLACE FUNCTION gps2.test_spatial_functions()
RETURNS TABLE(
    from_location VARCHAR(100),
    to_location VARCHAR(100), 
    distance_meters NUMERIC
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        a.location_name as from_location,
        b.location_name as to_location,
        ROUND(ST_Distance(a.test_point::geography, b.test_point::geography)) as distance_meters
    FROM gps2.connection_test a
    CROSS JOIN gps2.connection_test b
    WHERE a.id != b.id
    ORDER BY a.id, b.id;
END;
$$ LANGUAGE plpgsql;