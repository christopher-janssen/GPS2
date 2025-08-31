-- GPS22 Project: PostGIS Extensions Setup
-- Description: Enable PostGIS extensions for spatial GPS analysis

-- Enable PostGIS extension for spatial operations
CREATE EXTENSION IF NOT EXISTS postgis;

-- Enable PostGIS topology extension (useful for complex spatial operations)
CREATE EXTENSION IF NOT EXISTS postgis_topology;

-- Enable PostGIS SFCGAL extension for advanced 3D operations
-- (Optional but useful for altitude-based GPS analysis)
CREATE EXTENSION IF NOT EXISTS postgis_sfcgal;

-- Enable UUID extension for generating unique identifiers
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Enable hstore extension for flexible key-value storage
-- (Useful for storing variable GPS metadata)
CREATE EXTENSION IF NOT EXISTS hstore;

-- Show enabled extensions for verification
SELECT name, default_version, installed_version 
FROM pg_available_extensions 
WHERE name IN ('postgis', 'postgis_topology', 'postgis_sfcgal', 'uuid-ossp', 'hstore')
ORDER BY name;