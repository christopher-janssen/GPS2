# GPS22 - Privacy-First GPS Analysis for AUD Recovery Research

## Overview

UW-Madison ARC lab project for analyzing GPS data from addiction recovery studies. This system identifies meaningful stops, performs address matching, and categorizes recovery-relevant locations with complete local processing to ensure privacy compliance for sensitive research data.

## Key Features

- **Privacy-First Architecture**: All GPS processing occurs locally with no external API calls
- **Spatial Clustering**: Advanced GPS point clustering with movement state classification
- **Local Geocoding**: Wisconsin-only Nominatim instance for address resolution
- **Interactive Visualizations**: Leaflet-based mapping with comprehensive analysis tools
- **Research-Grade Analysis**: Built specifically for addiction recovery location analysis

## Tech Stack

- **R** - Primary analysis language with spatial packages (sf, DBI, skimr)
- **PostGIS** - Spatial database for GPS clustering and zoning analysis
- **Nominatim** - Local geocoding service (Wisconsin-only for privacy)
- **Docker Compose** - Container orchestration for databases
- **Quarto** - Notebook system for reproducible analysis pipeline

## Quick Start

### Prerequisites
- Docker and Docker Compose
- R with required packages (sf, DBI, RPostgres, leaflet)
- Access to research drive at `/Volumes/jjcurtin/studydata/risk`

### Setup Infrastructure
```bash
# Start PostGIS and Nominatim containers
cd docker-compose && docker-compose up -d

# Check container health
docker-compose ps

# Verify PostGIS connection
# Port: 5433, Database: gps_analysis, User: postgres
```

## Architecture

### Infrastructure Design

The project uses Docker containerization for privacy-compliant local processing:

```text
docker-compose/
├── docker-compose.yml          # Nominatim + PostGIS containers, research drive mount
├── init-scripts/
│   ├── 01-create-extensions.sql    # PostGIS extensions
│   ├── 02-create-tables.sql        # Database schema
│   ├── 03-create-indexes.sql       # Spatial indexes
│   └── 04-create-views.sql         # Views for visualizations
└── volumes/
    └── research-data/
```

### Analysis Workflow

The project follows a structured notebook-based workflow:

1. **`01-setup-infrastructure.qmd`** (COMPLETED) - Docker verification & connection testing
2. **`02-data-import.qmd`** (COMPLETED) - Research drive → PostGIS pipeline  
3. **`03-gps-processing-clustering.qmd`** (COMPLETED) - Movement classification & spatial clustering
4. **`04-reverse-geocoding.qmd`** (COMPLETED) - Nominatim API with rate limiting
5. **`05-spatial-zoning-analysis.qmd`** (IN PROGRESS) - Zoning data & spatial joins
6. **`06-visualizations.qmd`** (COMPLETED) - Interactive Leaflet maps

## Database Schema

### Design Philosophy

**Normalized Structure for Efficiency** - Stores participant metadata once instead of repeating across GPS points, using integer foreign keys for optimal performance and storage.

### Core Tables

#### `subjects` 
```sql
subjects (
  id INTEGER PRIMARY KEY,           -- Auto-increment primary key
  subid TEXT UNIQUE NOT NULL,       -- Original participant identifier  
  created_at TIMESTAMP DEFAULT NOW()
)
```

#### `raw_gps_points`   
```sql
raw_gps_points (
  point_id BIGINT PRIMARY KEY,
  subject_id INTEGER REFERENCES subjects(id),
  lat DOUBLE PRECISION NOT NULL,
  lon DOUBLE PRECISION NOT NULL, 
  time TIMESTAMP NOT NULL,
  sgmnt_type TEXT CHECK (sgmnt_type IN ('place', 'move', 'off')),
  geom GEOMETRY(POINT, 4326),      -- PostGIS spatial column
  is_stationary BOOLEAN,
  cluster_id BIGINT REFERENCES gps_clusters(cluster_id),
  imported_at TIMESTAMP DEFAULT NOW()
)
```

#### `processed_gps_points` 
```sql
processed_gps_points (
  processed_id BIGINT PRIMARY KEY,
  raw_point_id BIGINT REFERENCES raw_gps_points(point_id),
  subject_id INTEGER REFERENCES subjects(id),
  lat DOUBLE PRECISION NOT NULL,
  lon DOUBLE PRECISION NOT NULL,
  time TIMESTAMP NOT NULL,
  dttm_obs TIMESTAMP NOT NULL,
  dist_miles DOUBLE PRECISION DEFAULT 0,
  duration_mins DOUBLE PRECISION DEFAULT 0, 
  speed_mph DOUBLE PRECISION DEFAULT 0,
  movement_state TEXT CHECK (movement_state IN ('stationary', 'transition')),
  is_stationary BOOLEAN NOT NULL,
  cluster_id INTEGER,
  geom GEOMETRY(POINT, 4326),
  processed_at TIMESTAMP DEFAULT NOW()
)
```

#### `gps_clusters` 
```sql
gps_clusters (
  cluster_id BIGINT PRIMARY KEY,
  subject_id INTEGER REFERENCES subjects(id),
  lat DOUBLE PRECISION NOT NULL,
  lon DOUBLE PRECISION NOT NULL,
  geom GEOMETRY(POINT, 4326),
  n_points INTEGER NOT NULL CHECK (n_points > 0),
  first_visit TIMESTAMP,
  last_visit TIMESTAMP,
  total_visits INTEGER DEFAULT 1,
  total_duration_hours DOUBLE PRECISION CHECK (total_duration_hours >= 0),
  created_at TIMESTAMP DEFAULT NOW(),
  address TEXT                     -- Populated from geocoding
)
```

#### `madison_zoning_districts` 
```sql
madison_zoning_districts (
  id INTEGER PRIMARY KEY,
  objectid INTEGER,
  zone_code TEXT NOT NULL,
  zone_category TEXT CHECK (zone_category IN (
    'Residential', 'Mixed-Use', 'Commercial', 'Downtown', 
    'Employment', 'Industrial', 'Special', 'Other'
  )),
  area_sqm NUMERIC,
  geom GEOMETRY(MULTIPOLYGON, 4326),
  imported_at TIMESTAMP DEFAULT NOW()
)
```

#### `reverse_geocode_results` 
```sql
reverse_geocode_results (
  cluster_id BIGINT PRIMARY KEY REFERENCES gps_clusters(cluster_id),
  address TEXT,
  city TEXT,
  state TEXT,
  country TEXT DEFAULT 'United States',
  postcode TEXT,
  nominatim_json JSONB,            -- Full API response
  nominatim_place_id BIGINT,
  geocoded_at TIMESTAMP DEFAULT NOW()
)
```

### Database Performance Features

#### Spatial Indexes (PostGIS GIST)
- `idx_raw_gps_points_geom` - Fast spatial queries on raw GPS data
- `idx_processed_gps_geom` - Spatial operations on processed points
- `idx_gps_clusters_geom` - Location cluster proximity queries
- `idx_madison_zoning_geom` - Zoning polygon intersections

#### Lookup Indexes (B-tree)
- `idx_processed_gps_subject` - Individual participant queries
- `idx_processed_gps_stationary` - Movement state filtering  
- `idx_processed_gps_cluster_id` - Cluster-based analysis
- `idx_gps_clusters_subject` - Participant location patterns
- `idx_madison_zoning_category` - Zoning type filtering
- `idx_madison_zoning_code` - Zone code lookups

**Performance Impact**: These indexes enable O(log n) lookups instead of O(n) table scans, critical for ML feature extraction and spatial analysis on multi-million row datasets.

## Privacy & Compliance

- All GPS processing occurs on local infrastructure
- No external API calls during analysis
- Research data remains on secure university drives
- Geocoding limited to Wisconsin geography only
- Docker containers isolate processing environment

## Research Applications

This system enables analysis of:
- GPS-based movement patterns in addiction recovery studies
- Location-based risk factor identification
- Spatial clustering of meaningful stops
- Recovery-relevant location categorization
- Temporal analysis of location visits
- Geographic distribution of participant activities

## Visualization Capabilities

- Interactive Leaflet maps with toggleable layers
- GPS cluster visualization with detailed popups
- Movement pattern analysis and heatmaps
- Geocoding coverage analysis
- Madison zoning district integration
