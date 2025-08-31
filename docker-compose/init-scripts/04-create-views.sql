-- GPS22 Project: Analysis Views
-- Description: Pre-built views for visualizations and analysis queries

-- ============================================================================
-- RAW DATA VIEWS
-- ============================================================================

-- View: All GPS points with geographic context
CREATE OR REPLACE VIEW v_raw_gps_with_context AS
SELECT 
    rg.point_id,
    rg.subid,
    rg.lat,
    rg.lon,
    rg.time,
    rg.sgmnt_type,
    rg.is_stationary,
    rg.cluster_id,
    rg.geom,
    -- Add day-of-week and hour for temporal analysis
    EXTRACT(DOW FROM rg.time) as day_of_week,
    EXTRACT(HOUR FROM rg.time) as hour_of_day,
    DATE(rg.time) as date_only
FROM raw_gps_points rg;

-- View: Subject summary statistics
CREATE OR REPLACE VIEW v_subject_summaries AS
SELECT 
    s.subid,
    COUNT(rg.point_id) as total_gps_points,
    COUNT(CASE WHEN rg.sgmnt_type = 'place' THEN 1 END) as place_points,
    COUNT(CASE WHEN rg.sgmnt_type = 'move' THEN 1 END) as move_points,
    COUNT(CASE WHEN rg.is_stationary = true THEN 1 END) as stationary_points,
    COUNT(DISTINCT rg.cluster_id) FILTER (WHERE rg.cluster_id IS NOT NULL) as total_clusters,
    MIN(rg.time) as first_gps_timestamp,
    MAX(rg.time) as last_gps_timestamp,
    MAX(rg.time) - MIN(rg.time) as study_duration,
    COUNT(DISTINCT DATE(rg.time)) as days_with_data
FROM subjects s
LEFT JOIN raw_gps_points rg ON s.subid = rg.subid
GROUP BY s.subid;

-- ============================================================================
-- CLUSTERING VIEWS  
-- ============================================================================

-- View: Clusters with detailed statistics
CREATE OR REPLACE VIEW v_cluster_details AS
SELECT 
    gc.cluster_id,
    gc.subid,
    gc.lat,
    gc.lon,
    gc.geom,
    gc.n_points,
    gc.total_visits,
    gc.total_duration_hours,
    gc.first_visit,
    gc.last_visit,
    -- Add reverse geocoding info if available
    rgr.address,
    rgr.city,
    rgr.state,
    -- Calculate days between first and last visit
    CASE 
        WHEN gc.last_visit > gc.first_visit 
        THEN EXTRACT(DAYS FROM gc.last_visit - gc.first_visit) + 1
        ELSE 1 
    END as days_span,
    -- Average duration per visit
    CASE 
        WHEN gc.total_visits > 0 
        THEN gc.total_duration_hours / gc.total_visits 
        ELSE 0 
    END as avg_duration_per_visit
FROM gps_clusters gc
LEFT JOIN reverse_geocode_results rgr ON gc.cluster_id = rgr.cluster_id;

-- View: Top clusters by visit frequency
CREATE OR REPLACE VIEW v_top_clusters_by_visits AS
SELECT 
    ROW_NUMBER() OVER (PARTITION BY subid ORDER BY total_visits DESC) as visit_rank,
    subid,
    cluster_id,
    lat,
    lon,
    total_visits,
    total_duration_hours,
    address,
    city
FROM v_cluster_details
ORDER BY subid, total_visits DESC;

-- ============================================================================
-- SPATIAL ANALYSIS VIEWS
-- ============================================================================

-- View: Clusters with zoning information
CREATE OR REPLACE VIEW v_clusters_with_zoning AS
SELECT 
    cd.cluster_id,
    cd.subid,
    cd.lat,
    cd.lon,
    cd.geom,
    cd.n_points,
    cd.total_visits,
    cd.total_duration_hours,
    cd.address,
    cd.city,
    -- Zoning information (from spatial join)
    za.zone_id,
    za.zone_name,
    za.zone_type
FROM v_cluster_details cd
LEFT JOIN zoning_areas za ON ST_Within(cd.geom, za.geom);

-- View: Subject time spent by zone type
CREATE OR REPLACE VIEW v_time_by_zone_type AS
SELECT 
    fa.subid,
    fa.zone_type,
    COUNT(fa.cluster_id) as clusters_in_zone,
    SUM(fa.time_spent_hours) as total_hours_in_zone,
    AVG(fa.time_spent_hours) as avg_hours_per_cluster,
    COUNT(fa.visit_count) as total_visits
FROM final_analysis fa
WHERE fa.zone_type IS NOT NULL
GROUP BY fa.subid, fa.zone_type
ORDER BY fa.subid, total_hours_in_zone DESC;

-- ============================================================================
-- TEMPORAL ANALYSIS VIEWS
-- ============================================================================

-- View: Daily GPS activity summary
CREATE OR REPLACE VIEW v_daily_activity AS
SELECT 
    subid,
    DATE(time) as activity_date,
    COUNT(*) as total_points,
    COUNT(CASE WHEN sgmnt_type = 'place' THEN 1 END) as place_points,
    COUNT(CASE WHEN sgmnt_type = 'move' THEN 1 END) as move_points,
    MIN(time) as first_point_time,
    MAX(time) as last_point_time,
    MAX(time) - MIN(time) as active_duration
FROM raw_gps_points
GROUP BY subid, DATE(time)
ORDER BY subid, activity_date;

-- View: Hourly activity patterns
CREATE OR REPLACE VIEW v_hourly_patterns AS
SELECT 
    subid,
    EXTRACT(HOUR FROM time) as hour_of_day,
    COUNT(*) as point_count,
    COUNT(DISTINCT DATE(time)) as days_active_in_hour,
    AVG(COUNT(*)) OVER (PARTITION BY subid) as avg_points_per_hour
FROM raw_gps_points
GROUP BY subid, EXTRACT(HOUR FROM time)
ORDER BY subid, hour_of_day;

-- ============================================================================
-- VISUALIZATION HELPER VIEWS
-- ============================================================================

-- View: GeoJSON-ready cluster data for mapping
CREATE OR REPLACE VIEW v_clusters_geojson AS
SELECT 
    cluster_id,
    subid,
    ST_AsGeoJSON(geom)::json as geometry,
    json_build_object(
        'cluster_id', cluster_id,
        'subid', subid,
        'visits', total_visits,
        'duration_hours', total_duration_hours,
        'address', COALESCE(address, 'Unknown'),
        'city', COALESCE(city, 'Unknown')
    ) as properties
FROM v_cluster_details;

-- View: Movement trajectories (connecting consecutive GPS points)
CREATE OR REPLACE VIEW v_movement_trajectories AS
SELECT 
    subid,
    DATE(time) as trajectory_date,
    ST_MakeLine(geom ORDER BY time) as trajectory_geom,
    COUNT(*) as point_count,
    MIN(time) as start_time,
    MAX(time) as end_time
FROM raw_gps_points 
WHERE sgmnt_type = 'move'
GROUP BY subid, DATE(time)
HAVING COUNT(*) >= 2  -- Need at least 2 points to make a line
ORDER BY subid, trajectory_date;

-- ============================================================================
-- DATA QUALITY VIEWS
-- ============================================================================

-- View: Data quality summary
CREATE OR REPLACE VIEW v_data_quality AS
SELECT 
    subid,
    COUNT(*) as total_points,
    -- Temporal gaps (points more than 1 hour apart)
    COUNT(CASE 
        WHEN LAG(time) OVER (PARTITION BY subid ORDER BY time) IS NOT NULL 
        AND time - LAG(time) OVER (PARTITION BY subid ORDER BY time) > INTERVAL '1 hour'
        THEN 1 
    END) as temporal_gaps,
    -- Potential outliers (far from Wisconsin)
    COUNT(CASE 
        WHEN lat NOT BETWEEN 42.0 AND 47.5 OR lon NOT BETWEEN -92.9 AND -86.2 
        THEN 1 
    END) as potential_outliers,
    -- Missing cluster assignments
    COUNT(CASE WHEN cluster_id IS NULL AND sgmnt_type = 'place' THEN 1 END) as unclustered_places
FROM raw_gps_points
GROUP BY subid
ORDER BY subid;