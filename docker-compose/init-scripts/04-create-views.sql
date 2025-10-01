-- GPS22 Project: Analysis Views
-- Description: Pre-built views for visualizations and analysis queries

-- ============================================================================
-- RAW DATA VIEWS
-- ============================================================================

-- View: All GPS points with geographic context
CREATE OR REPLACE VIEW v_raw_gps_with_context AS
SELECT
    rg.point_id,
    s.subid,
    rg.subject_id,
    rg.lat,
    rg.lon,
    rg.time,
    rg.sgmnt_type,
    rg.is_stationary,
    rg.geom,
    -- Add day-of-week and hour for temporal analysis
    EXTRACT(DOW FROM rg.time) as day_of_week,
    EXTRACT(HOUR FROM rg.time) as hour_of_day,
    DATE(rg.time) as date_only
FROM raw_gps_points rg
JOIN subjects s ON rg.subject_id = s.id;

-- View: Subject summary statistics
CREATE OR REPLACE VIEW v_subject_summaries AS
SELECT
    s.subid,
    s.id as subject_id,
    COUNT(rg.point_id) as total_gps_points,
    COUNT(CASE WHEN rg.sgmnt_type = 'place' THEN 1 END) as place_points,
    COUNT(CASE WHEN rg.sgmnt_type = 'move' THEN 1 END) as move_points,
    COUNT(CASE WHEN rg.is_stationary = true THEN 1 END) as stationary_points,
    COUNT(DISTINCT gc.cluster_id) FILTER (WHERE gc.cluster_id IS NOT NULL) as total_clusters,
    MIN(rg.time) as first_gps_timestamp,
    MAX(rg.time) as last_gps_timestamp,
    MAX(rg.time) - MIN(rg.time) as study_duration,
    COUNT(DISTINCT DATE(rg.time)) as days_with_data
FROM subjects s
LEFT JOIN raw_gps_points rg ON s.id = rg.subject_id
LEFT JOIN gps_clusters gc ON s.id = gc.subject_id
GROUP BY s.id, s.subid;

-- ============================================================================
-- CLUSTERING VIEWS  
-- ============================================================================

-- View: Clusters with detailed statistics
CREATE OR REPLACE VIEW v_cluster_details AS
SELECT
    gc.cluster_id,
    s.subid,
    gc.subject_id,
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
JOIN subjects s ON gc.subject_id = s.id
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
    cd.subject_id,
    cd.lat,
    cd.lon,
    cd.geom,
    cd.n_points,
    cd.total_visits,
    cd.total_duration_hours,
    cd.address,
    cd.city,
    -- Zoning information (from spatial join)
    mz.id as zone_id,
    mz.zone_code as zone_name,
    mz.zone_category as zone_type
FROM v_cluster_details cd
LEFT JOIN madison_zoning_districts mz ON ST_Within(cd.geom, mz.geom);

-- View: Subject time spent by zone type
CREATE OR REPLACE VIEW v_time_by_zone_type AS
SELECT
    s.subid,
    fa.subject_id,
    fa.zone_type,
    COUNT(fa.cluster_id) as clusters_in_zone,
    SUM(fa.time_spent_hours) as total_hours_in_zone,
    AVG(fa.time_spent_hours) as avg_hours_per_cluster,
    SUM(fa.visit_count) as total_visits
FROM final_analysis fa
JOIN subjects s ON fa.subject_id = s.id
WHERE fa.zone_type IS NOT NULL
GROUP BY s.subid, fa.subject_id, fa.zone_type
ORDER BY s.subid, total_hours_in_zone DESC;

-- ============================================================================
-- TEMPORAL ANALYSIS VIEWS
-- ============================================================================

-- View: Daily GPS activity summary
CREATE OR REPLACE VIEW v_daily_activity AS
SELECT
    s.subid,
    rg.subject_id,
    DATE(rg.time) as activity_date,
    COUNT(*) as total_points,
    COUNT(CASE WHEN rg.sgmnt_type = 'place' THEN 1 END) as place_points,
    COUNT(CASE WHEN rg.sgmnt_type = 'move' THEN 1 END) as move_points,
    MIN(rg.time) as first_point_time,
    MAX(rg.time) as last_point_time,
    MAX(rg.time) - MIN(rg.time) as active_duration
FROM raw_gps_points rg
JOIN subjects s ON rg.subject_id = s.id
GROUP BY s.subid, rg.subject_id, DATE(rg.time)
ORDER BY s.subid, activity_date;

-- View: Hourly activity patterns
CREATE OR REPLACE VIEW v_hourly_patterns AS
WITH hourly_counts AS (
    SELECT
        s.subid,
        rg.subject_id,
        EXTRACT(HOUR FROM rg.time) as hour_of_day,
        COUNT(*) as point_count,
        COUNT(DISTINCT DATE(rg.time)) as days_active_in_hour
    FROM raw_gps_points rg
    JOIN subjects s ON rg.subject_id = s.id
    GROUP BY s.subid, rg.subject_id, EXTRACT(HOUR FROM rg.time)
)
SELECT
    subid,
    subject_id,
    hour_of_day,
    point_count,
    days_active_in_hour,
    AVG(point_count) OVER (PARTITION BY subid) as avg_points_per_hour
FROM hourly_counts
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
    s.subid,
    rg.subject_id,
    DATE(rg.time) as trajectory_date,
    ST_MakeLine(rg.geom ORDER BY rg.time) as trajectory_geom,
    COUNT(*) as point_count,
    MIN(rg.time) as start_time,
    MAX(rg.time) as end_time
FROM raw_gps_points rg
JOIN subjects s ON rg.subject_id = s.id
WHERE rg.sgmnt_type = 'move'
GROUP BY s.subid, rg.subject_id, DATE(rg.time)
HAVING COUNT(*) >= 2  -- Need at least 2 points to make a line
ORDER BY s.subid, trajectory_date;

-- ============================================================================
-- DATA QUALITY VIEWS
-- ============================================================================

-- View: Data quality summary
CREATE OR REPLACE VIEW v_data_quality AS
WITH time_gaps AS (
    SELECT
        rg.subject_id,
        rg.time,
        LAG(rg.time) OVER (PARTITION BY rg.subject_id ORDER BY rg.time) as prev_time,
        rg.lat,
        rg.lon,
        rg.is_stationary
    FROM raw_gps_points rg
)
SELECT
    s.subid,
    tg.subject_id,
    COUNT(*) as total_points,
    -- Temporal gaps (points more than 1 hour apart)
    COUNT(CASE
        WHEN tg.prev_time IS NOT NULL
        AND tg.time - tg.prev_time > INTERVAL '1 hour'
        THEN 1
    END) as temporal_gaps,
    -- Potential outliers (far from Wisconsin)
    COUNT(CASE
        WHEN tg.lat NOT BETWEEN 42.0 AND 47.5 OR tg.lon NOT BETWEEN -92.9 AND -86.2
        THEN 1
    END) as potential_outliers,
    -- Missing stationary point classifications
    COUNT(CASE WHEN tg.is_stationary IS NULL THEN 1 END) as unclassified_points
FROM time_gaps tg
JOIN subjects s ON tg.subject_id = s.id
GROUP BY s.subid, tg.subject_id
ORDER BY s.subid;