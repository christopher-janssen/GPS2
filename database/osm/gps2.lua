-- gps2.lua — osm2pgsql Flex config for GPS2 study
--
-- Imports OpenStreetMap POI and landuse data into the geolocation database.
-- Designed for Geofabrik Wisconsin PBF snapshots from the 2017-2018 study period.
--
-- Tables written:
--   public_data.osm_poi     — point features (nodes; centroids of closed ways/relations)
--   public_data.osm_landuse — polygon features (ways/relations with landuse tag)
--
-- Usage:
--   osm2pgsql --create --slim --output=flex --style=database/osm/gps2.lua \
--             --database geolocation <wi-snapshot.pbf>

-- ---------------------------------------------------------------------------
-- Tag classes captured as POI features
-- ---------------------------------------------------------------------------
local poi_classes = {
    amenity   = true,
    shop      = true,
    leisure   = true,
    tourism   = true,
    healthcare = true,
    craft     = true,
    office    = true,
    historic  = true,
    emergency = true,
    natural   = true,
    man_made  = true,
    military  = true,
}

-- ---------------------------------------------------------------------------
-- Table definitions
-- ---------------------------------------------------------------------------
local osm_poi = osm2pgsql.define_table({
    name   = 'osm_poi',
    schema = 'public_data',
    ids    = { type = 'any', id_column = 'osm_id', type_column = 'osm_type' },
    columns = {
        { column = 'class',       type = 'text', not_null = true },
        { column = 'type',        type = 'text', not_null = true },
        { column = 'name',        type = 'text' },
        { column = 'housenumber', type = 'text' },
        { column = 'street',      type = 'text' },
        { column = 'city',        type = 'text' },
        { column = 'postcode',    type = 'text' },
        { column = 'lon',         type = 'real' },
        { column = 'lat',         type = 'real' },
        { column = 'geom',        type = 'point', projection = 4326 },
    },
})

local osm_landuse = osm2pgsql.define_table({
    name   = 'osm_landuse',
    schema = 'public_data',
    ids    = { type = 'any', id_column = 'osm_id', type_column = 'osm_type' },
    columns = {
        { column = 'class',      type = 'text', not_null = true },
        { column = 'type',       type = 'text', not_null = true },
        { column = 'name',       type = 'text' },
        { column = 'city',       type = 'text' },
        { column = 'area_sqkm',  type = 'real' },
        { column = 'geom',       type = 'multipolygon', projection = 4326 },
    },
})

-- ---------------------------------------------------------------------------
-- Helper: resolve first matching POI class from a tag set
-- Returns class_key, class_value or nil if none found
-- ---------------------------------------------------------------------------
local function get_poi_class(tags)
    for key, _ in pairs(poi_classes) do
        if tags[key] then
            return key, tags[key]
        end
    end
    return nil, nil
end

-- ---------------------------------------------------------------------------
-- Helper: build shared address/name columns from tags
-- ---------------------------------------------------------------------------
local function poi_row(tags, class_key, class_val, lon, lat, geom)
    return {
        class       = class_key,
        type        = class_val,
        name        = tags['name'],
        housenumber = tags['addr:housenumber'],
        street      = tags['addr:street'],
        city        = tags['addr:city'],
        postcode    = tags['addr:postcode'],
        lon         = lon,
        lat         = lat,
        geom        = geom,
    }
end

-- ---------------------------------------------------------------------------
-- process_node: nodes with a POI class tag → osm_poi (point)
-- ---------------------------------------------------------------------------
function osm2pgsql.process_node(object)
    local class_key, class_val = get_poi_class(object.tags)
    if not class_key then return end

    local geom = object:as_point()
    osm_poi:insert(poi_row(
        object.tags, class_key, class_val,
        geom:lon(), geom:lat(), geom
    ))
end

-- ---------------------------------------------------------------------------
-- process_way: closed ways
--   • landuse tag → osm_landuse (polygon)
--   • POI class tag → osm_poi (centroid)
-- ---------------------------------------------------------------------------
function osm2pgsql.process_way(object)
    if not object.is_closed then return end

    -- Landuse polygons
    if object.tags['landuse'] then
        local geom = object:as_polygon()
        if geom then
            local area = geom:transform(3857):area() / 1e6
            osm_landuse:insert({
                class     = 'landuse',
                type      = object.tags['landuse'],
                name      = object.tags['name'],
                city      = object.tags['addr:city'],
                area_sqkm = area,
                geom      = geom:as_multipolygon(),
            })
        end
        return
    end

    -- POI centroids
    local class_key, class_val = get_poi_class(object.tags)
    if not class_key then return end

    local centroid = object:as_polygon():centroid()
    if not centroid then return end

    osm_poi:insert(poi_row(
        object.tags, class_key, class_val,
        centroid:lon(), centroid:lat(), centroid
    ))
end

-- ---------------------------------------------------------------------------
-- process_relation: multipolygon relations
--   • type=multipolygon + landuse → osm_landuse
--   • type=multipolygon + POI class → osm_poi (centroid)
-- ---------------------------------------------------------------------------
function osm2pgsql.process_relation(object)
    if object.tags['type'] ~= 'multipolygon' then return end

    -- Landuse multipolygons
    if object.tags['landuse'] then
        local geom = object:as_multipolygon()
        if geom then
            local area = geom:transform(3857):area() / 1e6
            osm_landuse:insert({
                class     = 'landuse',
                type      = object.tags['landuse'],
                name      = object.tags['name'],
                city      = object.tags['addr:city'],
                area_sqkm = area,
                geom      = geom,
            })
        end
        return
    end

    -- POI centroids
    local class_key, class_val = get_poi_class(object.tags)
    if not class_key then return end

    local geom = object:as_multipolygon()
    if not geom then return end

    local centroid = geom:centroid()
    if not centroid then return end

    osm_poi:insert(poi_row(
        object.tags, class_key, class_val,
        centroid:lon(), centroid:lat(), centroid
    ))
end
