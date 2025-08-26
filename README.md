# GPS2: Privacy-Compliant GPS Analysis System

A comprehensive spatial analysis system for addiction recovery research that processes GPS coordinates from study participants to automatically identify venue types and behavioral patterns while maintaining strict data privacy through local infrastructure.

## Key Features

- **Privacy-by-Design**: All sensitive GPS data remains within controlled local infrastructure
- **Automated Location Clustering**: Identifies meaningful locations based on duration and proximity patterns
- **Local Reverse Geocoding**: Converts coordinates to addresses using local OpenStreetMap data (no external API calls)
- **Interactive Visualization**: Creates detailed maps of participant movement patterns with behavioral insights
- **Scalable Processing**: Efficiently handles large datasets through PostGIS spatial database

## Quick Start

### Prerequisites
- Docker Desktop installed and running
- R (4.0+) with Tidyverse packages
- 8GB RAM minimum (16GB recommended)

### Installation

```bash
# First-time setup: Copy and configure environment file
cp .env.example .env
# Edit .env to set RESEARCH_DATA_PATH for your operating system
```

```r
# Bootstrap the entire GPS2 environment (installs packages + sets up containers)
source("scripts/r/bootstrap.R")
bootstrap_gps2_environment()
```

### Docker Database Management

```bash
# Start PostGIS and Nominatim containers
cd docker-postgis && docker-compose up -d

# Connect to database directly
./scripts/bash/connect_db.sh  # Mac/Linux
# Windows: Use Docker Desktop GUI or docker exec

# Stop containers when finished
cd docker-postgis && docker-compose down
```

### Core Workflow

```r
# Load GPS data
source("scripts/r/data_operations.R")
load_gps_data_to_postgis("path/to/tracks.csv")

# Run clustering analysis
source("scripts/r/analysis.R")
clusters <- cluster_all_participants_db(eps = 50)
# Alternative: Use data operations workflow
source("scripts/r/data_operations.R")
cluster_all_participants(eps = 50)

# Create visualizations
source("scripts/r/visualization.R")
map <- map_participant_clusters(participant_id = 19)
save_map(map, "participant_clusters")
```

## Complete Documentation

**[Full Setup and Usage Guide](documentation/gps2_guide.qmd)** - Comprehensive tutorial covering:
- Detailed installation steps
- Complete workflow examples
- Advanced usage patterns
- Troubleshooting guide
- Privacy and security considerations

## System Architecture

```
GPS Data → PostGIS Database → Clustering Analysis → Geocoding → Interactive Maps
    ↓              ↓                    ↓             ↓            ↓
  Privacy     Spatial Indexing    Duration-Based   Local OSM   Behavioral
Compliant    Fast Queries        Location ID      Data Only   Classification
```

## Core Components

### Database Layer (`scripts/r/database.R`)
- Single connection point: `connect_gps2_db()` and `query_gps2_db()`
- Transaction management with `with_gps2_transaction()`
- Connection credentials: host=localhost:5432, user=gps2_researcher, db=gps2_geocoding

### Data Processing (`scripts/r/data_operations.R`, `scripts/r/gps_processing.R`) 
- Main entry point: `load_gps_data_to_postgis()` for CSV import
- Core function: `process_gps()` for noise filtering and movement classification
- Batch processing: `insert_gps_batch()` and `insert_cluster_data()`

### Analysis Engine (`scripts/r/analysis.R`)
- Duration-based clustering: `cluster_stationary_gps_env()` and `cluster_stationary_gps_db()`
- Cross-day aggregation: `aggregate_daily_clusters()` and `assign_final_clusters()`
- Batch geocoding: `reverse_geocode_clusters_db()` and `batch_process_geocoding()`

### Visualization Suite (`scripts/r/visualization.R`)
- Main functions: `visualize_gps_db()` and `visualize_gps_env()` for database and environment data
- Convenience functions: `map_participant_clusters()`, `map_participant_gps()`, `map_participant_geocoded()`
- Batch processing: `generate_participant_maps()` and `save_map()`

## Sample Output

The system generates interactive maps that classify participant locations:
- **🔴 Routine** (5+ days, 8+ visits): Home, work locations
- **🟠 Frequent** (3+ days, 5+ visits): Regular destinations  
- **🔵 Occasional** (2+ days): Sometimes visited places
- **⚪ Rare** (< 2 days): One-time or infrequent visits

## Privacy Compliance

GPS2 addresses privacy challenges identified in spatial epidemiology research by implementing architectural privacy protection:

- **Local Processing**: No GPS coordinates transmitted to external services
- **Container Isolation**: Docker provides process and data isolation
- **Volume Mount Security**: Data accessed via `/research_data` mount (configured via RESEARCH_DATA_PATH in .env)
- **Wisconsin-Only OSM Data**: Local Nominatim contains only Wisconsin geographic data

## 🗂️ Project Structure

```
GPS2/
├── docker-postgis/          # Database container configuration
│   ├── docker-compose.yml   
│   └── init-scripts/        # Automated schema setup
├── scripts/r/               # Core R modules
│   ├── bootstrap.R          # Environment setup
│   ├── database.R           # Connection management
│   ├── data_operations.R    # Data insertion/updates
│   ├── analysis.R           # Clustering & geocoding
│   ├── visualization.R      # Mapping functions
│   └── gps_processing.R     # GPS filtering
├── scripts/bash/            # Shell utilities
│   └── connect_db.sh        # Database connection script
├── utils/                   # Modular utility functions
├── config/                  # Configuration files
│   └── gps2_config.R        # Centralized settings
├── GPS/data/               # Sample and input data
├── documentation/          # Comprehensive guides
└── maps/                   # Generated visualizations
```

## Research Applications

Originally developed for addiction recovery research to:
- Identify high-risk venue exposure patterns
- Quantify routine location stability over time  
- Analyze spatial-temporal behavioral changes
- Support relapse prediction modeling

The system is adaptable for other spatial epidemiology and behavioral research contexts.

## Performance

**Tested Scale:**
- 588,658 GPS points across 167 participants
- Geographic coverage: Continental United States
- Processing time: ~30 seconds per participant for clustering
- Memory usage: ~4GB for large datasets

## Contributing

For lab members:
1. Follow the [setup guide](documentation/gps2_guide.qmd)
2. Use existing structure for new features
3. All GPS data remains local - never commit processed data!

For external collaborators:
- Clone repository and run independent setup
- Work with your own GPS datasets
- Contact team for research collaboration discussions

**Getting Started**: Read the [complete guide](documentation/gps2_guide.qmd) for detailed setup instructions and workflow examples.
