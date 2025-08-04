# GPS2 Project - Privacy-Compliant Location Analysis for Addiction Recovery Research

## Project Overview

The GPS2 project develops privacy-preserving methods for analyzing GPS location data from addiction recovery participants. Instead of asking participants to manually log every location, we automatically identify venue types and behavioral patterns from GPS coordinates while maintaining strict data privacy and HIPAA compliance.

## Research Context

Participants in a 17-week addiction recovery study have their phones track GPS locations alongside daily mood and craving surveys. Understanding *what types of places* people visit (not just *where* they go) enables researchers to:

- Better predict relapse risk based on environmental exposure
- Provide personalized, location-aware support interventions  
- Identify protective vs. high-risk location patterns
- Reduce participant burden by eliminating manual location logging

## Privacy-First Architecture

Following privacy best practices outlined in recent spatial epidemiology research, GPS2 implements a **local-first** approach that keeps sensitive location data within secured research infrastructure:

### Core Privacy Protections
- **Local PostGIS Processing**: All GPS analysis happens in Docker containers on research systems
- **No External API Transmission**: Participant coordinates never leave your infrastructure
- **Zone-Based Analysis**: Commercial district analysis only, residential areas excluded
- **Spatial Aggregation**: H3 hexagonal grids prevent individual location inference
- **Comprehensive Caching**: Build local venue databases to minimize external dependencies

### Technical Implementation
- **PostGIS + Docker**: Containerized spatial database with full offline capability
- **R-based Analysis Pipeline**: Reproducible workflows for clustering and feature extraction
- **Duration-Based Clustering**: Identifies meaningful locations based on time spent, not just proximity
- **Multi-API Integration**: Strategic use of Census, OpenStreetMap, and commercial APIs when needed

## Current Capabilities

### 1. GPS Data Processing
- **Speed-based filtering**: Distinguishes stationary vs. transition points (4 mph threshold)
- **Movement classification**: Identifies meaningful stops vs. brief pauses
- **Quality control**: Removes GPS drift, outliers, and invalid readings

### 2. Location Clustering  
- **Duration-weighted clustering**: Groups nearby points based on time spent, not just distance
- **Cross-day aggregation**: Identifies recurring locations across multiple days
- **Behavioral classification**: 
  - **Routine locations** (≥5 days, ≥8 visits): Home, work, regular establishments
  - **Frequent locations** (≥3 days, ≥5 visits): Gym, regular shopping, social spots  
  - **Occasional locations** (≥2 days): Sometimes-visited places
  - **Rare locations**: One-off visits

### 3. Interactive Visualization
- **Leaflet-based mapping**: Web-interactive maps for data exploration
- **Multi-participant overviews**: Compare movement patterns across participants
- **Individual deep-dives**: Day-by-day movement analysis with layer controls
- **Cluster representatives**: Meaningful location summaries with visit statistics

### 4. Database Infrastructure
- **PostGIS spatial database**: Full offline geocoding and spatial analysis
- **Docker containerization**: Portable, reproducible development environment
- **R integration**: Seamless data transfer between R analysis and PostGIS storage
- **Batch processing**: Efficient handling of large GPS datasets

## Planned Enhancements

### Venue Identification Pipeline
- **Risk-relevant categorization**: Automatic identification of bars, liquor stores, treatment centers
- **Support location detection**: Healthcare facilities, AA meeting locations, gyms
- **Environmental context**: Neighborhood characteristics, accessibility, hours of operation

### Advanced Analytics
- **Temporal pattern analysis**: Time-of-day and day-of-week behavioral patterns
- **Risk prediction features**: Location-based variables for machine learning models  
- **Social environment mapping**: Co-occurrence patterns and peer influence indicators

## File Structure

```
GPS2/
├── qmd/                          # Analysis notebooks
│   ├── postgis_setup.qmd         # Database environment setup
│   ├── duration_clustering.qmd   # Location clustering workflows  
│   ├── generate_maps.qmd         # Interactive visualization creation
│   └── api_comparison.qmd        # External API evaluation guide
├── scripts/
│   ├── r/
│   │   ├── postgis_connection.R  # Database connectivity functions
│   │   ├── gps_filtering.R       # Data processing and quality control
│   │   ├── duration_cluster.R    # Clustering algorithms
│   │   └── map_gps.R            # Leaflet mapping functions
│   └── bash/
│       └── connect_db.sh         # Quick database connection utility
├── docker-postgis/               # Containerized spatial database
│   ├── docker-compose.yml       # PostGIS service configuration
│   └── init-scripts/            # Database initialization
├── documentation/               # Project planning and progress notes
└── data/                       # Sample GPS data (anonymized)
```

## Getting Started

### Prerequisites
- Docker and Docker Compose
- R (≥4.0) with spatial analysis packages
- 8GB+ RAM recommended for large GPS datasets

### Quick Setup
1. **Start PostGIS Database**:
   ```bash
   cd docker-postgis
   docker-compose up -d
   ```

2. **Initialize R Environment**:
   ```r
   # Run the setup notebook
   quarto::quarto_render("qmd/postgis_setup.qmd")
   ```

3. **Load Sample Data**:
   ```r
   source("scripts/r/gps_filtering.R")
   gps_data <- read_csv("data/tracks.csv")
   processed_data <- process_gps(gps_data) |> get_stationary()
   ```

4. **Create Visualizations**:
   ```r
   source("scripts/r/map_gps.R")
   map_gps(processed_data)  # Overview map
   map_gps(processed_data, participant_id = 19)  # Individual analysis
   ```