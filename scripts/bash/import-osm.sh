#!/usr/bin/env bash
# import-osm.sh — Import OSM PBF into geolocation database via osm2pgsql Flex
#
# Usage:
#   ./scripts/bash/import-osm.sh <path-to-pbf> [--filter-wisconsin]
#
# Options:
#   <path-to-pbf>          Path to a Geofabrik Wisconsin (or Midwest) PBF file
#   --filter-wisconsin     Pre-filter a larger extract to Wisconsin bbox before import
#                          Requires osmium-tool; bbox: -92.889,42.492,-86.249,47.309
#
# Dependencies:
#   osm2pgsql >= 1.6  (flex output support)
#   osmium-tool       (only needed with --filter-wisconsin)
#   PostgreSQL with PostGIS (geolocation database must exist)
#
# The Lua style file at database/osm/gps2.lua controls the import schema.
# Run database/01b-public_data-schema.qmd first to create the target tables.

set -euo pipefail

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
PBF_IN=""
FILTER_WI=false

for arg in "$@"; do
    case "$arg" in
        --filter-wisconsin) FILTER_WI=true ;;
        --help|-h)
            sed -n '2,20p' "$0" | sed 's/^# //'
            exit 0
            ;;
        -*) echo "Unknown option: $arg" >&2; exit 1 ;;
        *)  PBF_IN="$arg" ;;
    esac
done

if [[ -z "$PBF_IN" ]]; then
    echo "Error: PBF path required." >&2
    echo "Usage: $0 <path-to-pbf> [--filter-wisconsin]" >&2
    exit 1
fi

if [[ ! -f "$PBF_IN" ]]; then
    echo "Error: File not found: $PBF_IN" >&2
    exit 1
fi

# Resolve repo root (script lives at scripts/bash/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LUA_STYLE="$REPO_ROOT/database/osm/gps2.lua"

if [[ ! -f "$LUA_STYLE" ]]; then
    echo "Error: Lua style not found at $LUA_STYLE" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Step 1 (optional): filter to Wisconsin bounding box
# ---------------------------------------------------------------------------
PBF_IMPORT="$PBF_IN"

if [[ "$FILTER_WI" == true ]]; then
    if ! command -v osmium &>/dev/null; then
        echo "Error: osmium-tool is required for --filter-wisconsin" >&2
        exit 1
    fi
    WI_BBOX="-92.889,42.492,-86.249,47.309"
    PBF_FILTERED="${PBF_IN%.pbf}-wisconsin-filtered.pbf"
    echo "==> Filtering to Wisconsin bbox ($WI_BBOX)..."
    osmium extract \
        --bbox "$WI_BBOX" \
        --output "$PBF_FILTERED" \
        --overwrite \
        "$PBF_IN"
    PBF_IMPORT="$PBF_FILTERED"
    echo "    Filtered file: $PBF_FILTERED"
fi

# ---------------------------------------------------------------------------
# Step 2: Run osm2pgsql
# ---------------------------------------------------------------------------
echo "==> Running osm2pgsql..."
echo "    PBF:   $PBF_IMPORT"
echo "    Style: $LUA_STYLE"
echo "    DB:    geolocation"

osm2pgsql \
    --create \
    --slim \
    --drop \
    --output=flex \
    --style="$LUA_STYLE" \
    --database=geolocation \
    --middle-schema=public_data \
    "$PBF_IMPORT"

# ---------------------------------------------------------------------------
# Step 3: Populate lon/lat from geometry
# ---------------------------------------------------------------------------
echo "==> Populating lon/lat columns..."
psql --dbname=geolocation --no-psqlrc --command="
ALTER TABLE public_data.osm_poi
    ADD COLUMN IF NOT EXISTS lon DOUBLE PRECISION,
    ADD COLUMN IF NOT EXISTS lat DOUBLE PRECISION;

UPDATE public_data.osm_poi
SET lon = ST_X(geom),
    lat = ST_Y(geom);
"

# ---------------------------------------------------------------------------
# Step 4: Row counts
# ---------------------------------------------------------------------------
echo ""
echo "==> Import complete. Row counts:"
psql --dbname=geolocation --no-psqlrc --tuples-only --command="
SELECT
    'osm_poi'     AS table_name,
    COUNT(*)      AS row_count
FROM public_data.osm_poi
UNION ALL
SELECT
    'osm_landuse' AS table_name,
    COUNT(*)      AS row_count
FROM public_data.osm_landuse
ORDER BY table_name;
"

echo ""
echo "==> Next steps:"
echo "    1. Re-run views/poi-visitable-view.qmd to refresh the materialized view"
echo "    2. Re-run database/99-create-indexes.qmd to rebuild indexes"
echo "    3. Run database/02b-import-osm.qmd for full verification"
