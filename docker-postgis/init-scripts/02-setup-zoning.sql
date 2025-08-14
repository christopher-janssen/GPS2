-- docker-postgis/init-scripts/02-setup-zoning.sql
-- Setup for Madison zoning data in GPS2 spatial analysis
-- Run after the main PostGIS setup

-- Create zoning data table for Madison districts
CREATE TABLE IF NOT EXISTS gps2.zoning_districts (
    id SERIAL PRIMARY KEY,
    zone_code VARCHAR(10) NOT NULL,
    zone_name VARCHAR(100) NOT NULL,
    zone_category VARCHAR(50) NOT NULL,
    zone_description TEXT,
    geometry GEOMETRY(MULTIPOLYGON, 4326),
    area_sqm NUMERIC,
    area_acres NUMERIC,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Ensure unique zone codes
    UNIQUE(zone_code)
);

-- Create spatial index for fast intersection queries
CREATE INDEX idx_zoning_districts_geometry ON gps2.zoning_districts USING GIST (geometry);
CREATE INDEX idx_zoning_districts_code ON gps2.zoning_districts (zone_code);
CREATE INDEX idx_zoning_districts_category ON gps2.zoning_districts (zone_category);

-- Create function to update area calculations when geometry changes
CREATE OR REPLACE FUNCTION gps2.update_zoning_area()
RETURNS TRIGGER AS $$
BEGIN
    NEW.area_sqm = ST_Area(NEW.geometry::geography);
    NEW.area_acres = NEW.area_sqm * 0.000247105; -- Convert sq meters to acres
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger to automatically calculate areas
CREATE TRIGGER trigger_update_zoning_area
    BEFORE INSERT OR UPDATE OF geometry ON gps2.zoning_districts
    FOR EACH ROW
    EXECUTE FUNCTION gps2.update_zoning_area();

-- Insert Madison zoning district definitions (without geometry for now)
INSERT INTO gps2.zoning_districts (zone_code, zone_name, zone_category, zone_description) VALUES
-- Residential Districts
('SR-C1', 'Suburban Residential - Consistent 1', 'Residential', 'Low density suburban residential'),
('SR-C2', 'Suburban Residential - Consistent 2', 'Residential', 'Low density suburban residential'),
('SR-C3', 'Suburban Residential - Consistent 3', 'Residential', 'Low density suburban residential'),
('SR-V1', 'Suburban Residential - Varied 1', 'Residential', 'Varied suburban residential'),
('SR-V2', 'Suburban Residential - Varied 2', 'Residential', 'Varied suburban residential'),
('TR-C1', 'Traditional Residential - Consistent 1', 'Residential', 'Traditional neighborhood residential'),
('TR-C2', 'Traditional Residential - Consistent 2', 'Residential', 'Traditional neighborhood residential'),
('TR-C3', 'Traditional Residential - Consistent 3', 'Residential', 'Traditional neighborhood residential'),
('TR-C4', 'Traditional Residential - Consistent 4', 'Residential', 'Traditional neighborhood residential'),
('TR-V1', 'Traditional Residential - Varied 1', 'Residential', 'Varied traditional residential'),
('TR-V2', 'Traditional Residential - Varied 2', 'Residential', 'Varied traditional residential'),
('TR-U1', 'Traditional Residential - Urban 1', 'Residential', 'Urban traditional residential'),
('TR-U2', 'Traditional Residential - Urban 2', 'Residential', 'Urban traditional residential'),
('TR-R', 'Traditional Residential - Rustic', 'Residential', 'Rustic residential'),
('TR-P', 'Traditional Residential - Planned', 'Residential', 'Planned residential development'),
('DR1', 'Downtown Residential 1', 'Downtown', 'Downtown residential district'),
('DR2', 'Downtown Residential 2', 'Downtown', 'Downtown residential district'),

-- Mixed-Use and Commercial Districts
('LMX', 'Limited Mixed-Use', 'Mixed-Use', 'Limited mixed-use development'),
('NMX', 'Neighborhood Mixed-Use', 'Mixed-Use', 'Neighborhood-scale mixed-use'),
('TSS', 'Traditional Shopping Street', 'Commercial', 'Traditional shopping street'),
('MXC', 'Mixed-Use Center', 'Mixed-Use', 'Mixed-use center'),
('CC-T', 'Commercial Corridor - Transitional', 'Commercial', 'Transitional commercial corridor'),
('CC', 'Commercial Center', 'Commercial', 'Commercial center'),
('RMX', 'Regional Mixed-Use', 'Mixed-Use', 'Regional mixed-use'),
('THV', 'Tiny House Village', 'Residential', 'Tiny house village'),

-- Downtown and Urban Districts
('DC', 'Downtown Core', 'Downtown', 'Downtown core district'),
('UOR', 'Urban Office Residential', 'Downtown', 'Urban office residential'),
('UMX', 'Urban Mixed-Use', 'Downtown', 'Urban mixed-use'),

-- Employment Districts
('TE', 'Traditional Employment', 'Employment', 'Traditional employment district'),
('SE', 'Suburban Employment', 'Employment', 'Suburban employment'),
('SEC', 'Suburban Employment Center', 'Employment', 'Suburban employment center'),
('EC', 'Employment Campus', 'Employment', 'Employment campus'),
('IL', 'Industrial - Limited', 'Industrial', 'Limited industrial'),
('IG', 'Industrial - General', 'Industrial', 'General industrial'),

-- Special Districts
('A', 'Agricultural', 'Special', 'Agricultural district'),
('UA', 'Urban Agricultural', 'Special', 'Urban agricultural'),
('CN', 'Conservancy', 'Special', 'Conservancy district'),
('PR', 'Parks and Recreation', 'Special', 'Parks and recreation'),
('AP', 'Airport', 'Special', 'Airport district'),
('CI', 'Campus Institutional', 'Special', 'Campus institutional'),
('PD', 'Planned Development', 'Special', 'Planned development'),
('PMHP', 'Planned Mobile Home Park', 'Special', 'Planned mobile home park'),
('ME', 'Nonmetallic Mineral Extraction', 'Special', 'Nonmetallic mineral extraction'),
('MC', 'Mission Camp District', 'Special', 'Mission camp district'),

-- Historic Districts
('HIST-L', 'Designated Landmark', 'Historic', 'Historic landmark'),
('HIST-MH', 'Mansion Hill Historic District', 'Historic', 'Mansion Hill historic district'),
('HIST-TL', 'Third Lake Ridge Historic District', 'Historic', 'Third Lake Ridge historic district'),
('HIST-UH', 'University Heights Historic District', 'Historic', 'University Heights historic district'),
('HIST-MB', 'Marquette Bungalows Historic District', 'Historic', 'Marquette Bungalows historic district'),
('HIST-FS', 'First Settlement Historic District', 'Historic', 'First Settlement historic district')

ON CONFLICT (zone_code) DO UPDATE SET
    zone_name = EXCLUDED.zone_name,
    zone_category = EXCLUDED.zone_category,
    zone_description = EXCLUDED.zone_description,
    updated_at = CURRENT_TIMESTAMP;

-- Create view for easy zoning queries
CREATE OR REPLACE VIEW gps2.zoning_summary AS
SELECT 
    zone_category,
    COUNT(*) as district_count,
    SUM(area_acres) as total_acres,
    ARRAY_AGG(zone_code ORDER BY zone_code) as zone_codes
FROM gps2.zoning_districts
WHERE geometry IS NOT NULL
GROUP BY zone_category
ORDER BY zone_category;

-- Function to find zoning for a GPS point
CREATE OR REPLACE FUNCTION gps2.get_zoning_for_point(
    input_lat NUMERIC,
    input_lon NUMERIC
)
RETURNS TABLE(
    zone_code VARCHAR(10),
    zone_name VARCHAR(100),
    zone_category VARCHAR(50),
    zone_description TEXT
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        zd.zone_code,
        zd.zone_name,
        zd.zone_category,
        zd.zone_description
    FROM gps2.zoning_districts zd
    WHERE ST_Contains(
        zd.geometry,
        ST_SetSRID(ST_MakePoint(input_lon, input_lat), 4326)
    )
    LIMIT 1;
END;
$$ LANGUAGE plpgsql;

-- Function to find zoning for multiple GPS points (for clusters)
CREATE OR REPLACE FUNCTION gps2.get_zoning_for_clusters()
RETURNS TABLE(
    subid INTEGER,
    cluster_id INTEGER,
    zone_code VARCHAR(10),
    zone_name VARCHAR(100),
    zone_category VARCHAR(50)
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        lc.subid,
        lc.cluster_id,
        zd.zone_code,
        zd.zone_name,
        zd.zone_category
    FROM gps2.location_clusters lc
    LEFT JOIN gps2.zoning_districts zd ON ST_Contains(
        zd.geometry,
        ST_SetSRID(ST_MakePoint(lc.lon, lc.lat), 4326)
    )
    ORDER BY lc.subid, lc.cluster_id;
END;
$$ LANGUAGE plpgsql;

-- Grant permissions
GRANT ALL ON gps2.zoning_districts TO gps2_researcher;
GRANT ALL ON SEQUENCE gps2.zoning_districts_id_seq TO gps2_researcher;
GRANT SELECT ON gps2.zoning_summary TO gps2_researcher;

-- Create some test/reference data
INSERT INTO gps2.zoning_districts (zone_code, zone_name, zone_category, zone_description, geometry) VALUES
('TEST-DOWNTOWN', 'Test Downtown Area', 'Test', 'Test downtown Madison area', 
 ST_SetSRID(
   ST_MakePolygon(
     ST_MakeLine(ARRAY[
       ST_MakePoint(-89.420, 43.070),
       ST_MakePoint(-89.380, 43.070), 
       ST_MakePoint(-89.380, 43.080),
       ST_MakePoint(-89.420, 43.080),
       ST_MakePoint(-89.420, 43.070)
     ])
   ), 4326)
)
ON CONFLICT (zone_code) DO NOTHING;