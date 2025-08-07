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

```r
# Install required R packages
install.packages(c("tidyverse", "DBI", "RPostgres", "leaflet", "geosphere", "lubridate"))

# Bootstrap the GPS2 environment
source("scripts/r/bootstrap.R")
bootstrap_gps2_environment()
```

### Basic Usage

```r
# Load and process GPS data
source("scripts/r/data_operations.R")
load_gps_data_to_postgis("GPS/data/tracks.csv")

# Run clustering analysis
source("scripts/r/analysis.R")
clusters <- analyze_all_participants(eps = 50)

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
- PostGIS spatial database with Docker containerization
- Automated connection management and health checks
- Optimized spatial indexes for fast geographic queries

### Data Processing (`scripts/r/gps_processing.R`, `scripts/r/data_operations.R`)
- GPS noise filtering and movement classification
- Batch data insertion with transaction integrity
- Stationary point identification using speed thresholds

### Analysis Engine (`scripts/r/analysis.R`)
- Duration-based clustering algorithm for meaningful location identification
- Local Nominatim reverse geocoding for address resolution
- Scalable processing for large participant cohorts

### Visualization Suite (`scripts/r/visualization.R`)
- Interactive Leaflet maps with behavioral pattern classification
- Location type categorization (routine, frequent, occasional, rare)
- Geocoded address overlays with confidence metrics

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
- **Business Associate Agreement (BAA) Compliance**: Eliminates need for external API agreements
- **Audit Trail**: All operations logged within controlled environment

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
