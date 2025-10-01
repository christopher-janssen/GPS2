-- GPS22 Project: Performance Indexes
-- Description: Spatial and performance indexes for GPS analysis queries

-- ============================================================================
-- SPATIAL INDEXES (PostGIS)
-- ============================================================================

-- Spatial index on raw GPS points geometry
CREATE INDEX idx_raw_gps_points_geom ON raw_gps_points USING GIST (geom);

-- Spatial index on GPS clusters geometry  
CREATE INDEX idx_gps_clusters_geom ON gps_clusters USING GIST (geom);

-- Spatial index on Madison zoning districts geometry
CREATE INDEX idx_madison_zoning_geom ON madison_zoning_districts USING GIST (geom);

-- ============================================================================
-- PERFORMANCE INDEXES (B-tree)
-- ============================================================================

-- Subject-based queries (most common filter)
CREATE INDEX idx_raw_gps_points_subject ON raw_gps_points (subject_id);
CREATE INDEX idx_gps_clusters_subject ON gps_clusters (subject_id);

-- Time-based queries for temporal analysis
CREATE INDEX idx_raw_gps_points_time ON raw_gps_points (time);
CREATE INDEX idx_raw_gps_points_subject_time ON raw_gps_points (subject_id, time);

-- Clustering workflow indexes
CREATE INDEX idx_raw_gps_points_stationary ON raw_gps_points (is_stationary);
CREATE INDEX idx_raw_gps_points_sgmnt_type ON raw_gps_points (sgmnt_type);

-- Cluster analysis indexes
CREATE INDEX idx_gps_clusters_visits ON gps_clusters (total_visits DESC);
CREATE INDEX idx_gps_clusters_duration ON gps_clusters (total_duration_hours DESC);
CREATE INDEX idx_gps_clusters_first_visit ON gps_clusters (first_visit);

-- Madison zoning analysis indexes
CREATE INDEX idx_madison_zoning_code ON madison_zoning_districts (zone_code);
CREATE INDEX idx_madison_zoning_category ON madison_zoning_districts (zone_category);
CREATE INDEX idx_final_analysis_subid ON final_analysis (subject_id);
CREATE INDEX idx_final_analysis_zone_type ON final_analysis (zone_type);

-- Reverse geocoding lookup index
CREATE INDEX idx_reverse_geocode_city ON reverse_geocode_results (city);

-- ============================================================================
-- COMPOSITE INDEXES (Multi-column)
-- ============================================================================

-- Common query patterns
CREATE INDEX idx_raw_gps_subject_time_sgmnt ON raw_gps_points (subject_id, time, sgmnt_type);
CREATE INDEX idx_clusters_subject_visits ON gps_clusters (subject_id, total_visits DESC);
CREATE INDEX idx_final_analysis_subject_zone ON final_analysis (subject_id, zone_type);

-- ============================================================================
-- PARTIAL INDEXES (Filtered)
-- ============================================================================

-- Processed GPS points clustering indexes
CREATE INDEX idx_processed_gps_cluster_id ON processed_gps_points (cluster_id)
WHERE cluster_id IS NOT NULL;

-- Only index stationary points
CREATE INDEX idx_raw_gps_stationary_geom ON raw_gps_points USING GIST (geom)
WHERE is_stationary = true;

-- Only index "place" segments for clustering analysis
CREATE INDEX idx_raw_gps_place_segments ON raw_gps_points (subject_id, time)
WHERE sgmnt_type = 'place';

-- Daily location entropy indexes
CREATE INDEX idx_daily_entropy_subject ON daily_location_entropy(subject_id);
CREATE INDEX idx_daily_entropy_date ON daily_location_entropy(date);

-- ============================================================================
-- ANALYZE STATISTICS
-- ============================================================================

-- Update table statistics for query planning
ANALYZE subjects;
ANALYZE raw_gps_points;
ANALYZE processed_gps_points;
ANALYZE gps_clusters;
ANALYZE madison_zoning_districts;
ANALYZE reverse_geocode_results;
ANALYZE final_analysis;
ANALYZE daily_location_entropy;