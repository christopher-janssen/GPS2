# GPS22 Development Notes & Planning

## Work in Progress

### Fix

- [ ] **Quality Filtering Validation**: Test ">30min & >0.31mi" rule - may remove valid long visits with GPS drift
- [ ] **Daily Clustering Boundary**: Fix midnight splits for 24-hour locations (hospitals, night shifts) 
- [ ] **Minimum Points Requirement**: Validate 2-point cluster minimum vs single-point long visits with sparse GPS

### Build

- [ ] **Spatial Clustering Validation**: Implement silhouette analysis to optimize 20m radius
- [ ] **Duration Calculation**: Replace time span (max-min) with actual presence time estimation
- [ ] **Cross-Day Location Merging**: Add temporal patterns to distance-based aggregation logic


## Development Log

### 7/1/2025

#### Initial Setup & Stationarity Detection

- ✅ Made robust data access script 
- ✅ Established traveling vs stationary distinction (time derivative ref)
- ~~Abandoned k-means clustering approach~~

### 7/3/2025

#### Clustering Algorithm Research

- Investigated alternative clustering over k-means: DBSCAN and `fpc` package in R
- Read Claire's papers and DBSCAN research

### 7/6/2025

#### Stationary Point Validation

- Created script to view stationary points for validation
- Prepared visualizations for team discussion on stationarity accuracy

### 7/7/2025

#### Visualization Development

- Created downloadable Leaflet maps for team meeting

### 7/8/2025

#### Clustering Strategy Pivot

- **Decision**: Use more liberal stationary definitions (patterns split across days)
- **New Approach**: Create clusters across DAYS rather than total datasets 
- **Methods**: Both DBSCAN and K-MEANS with defined radius thresholds
- Focus on creating maps that demonstrate this approach

### 7/11/2025

#### Daily Visualization Success

- ✅ Successfully implemented daily visualizations with improved readability
- ✅ Consolidated scripts for cleanliness and workflow efficiency
- **Next**: Move towards clustering implementation
- Considered visualizing both own data and Claire's data for accuracy validation

### 7/13/2025

#### Workflow Consolidation

- **Major Documentation Win**: Consolidated workflows into clean QMD scripts
- ✅ Created first-attempt clustering script
- ✅ Fixed time_derivative calculations and mapping demos
- **Focus**: Need to implement day-to-day clustering for better real-world use cases

### 7/15/2025

#### Time-Duration Modified DBSCAN Research

- **Goal**: Better identify USEFUL points, filter out transit stops (bus stops, traffic lights)
- **Research**: Paper sent to Claire on time-duration DBSCAN modifications
- **Caching Strategy**: Researching sustainable caching for large-scale POI queries across multiple participants
- **Performance**: Considering Julia/C integration for computational efficiency

### 7/16/2025

#### Algorithm Pivot: Abandon DBSCAN

- ❌ **DBSCAN abandoned**: Doesn't work well with our GPS data collection techniques
- ✅ **New Approach**: Time/duration based clustering methods showing promising results
- **Performance Research**: GPS POI caching proposal using grid-based hierarchical methods
- **Tech Exploration**: Considering Julia implementation for performance

### 7/17/2025

#### Caching Strategy Decision

- **Meeting Outcome**: Even un-optimized caching system sufficient for OSM daily rate limits
- **Research**: Function decorators (Adam's suggestion) for caching implementation
- **Next**: Develop POI querying system linking with known location information

### 8/3/2025

#### Infrastructure Breakthrough: PostGIS + Docker

- 🎉 **Major Infrastructure Win**: Privacy-compliant GPS analysis system operational
- ✅ Built Docker container with PostgreSQL + PostGIS spatial extensions
- ✅ Created `postgis_connection.R` with comprehensive database connection/testing
- ✅ Verified all spatial operations: distance calculations, coordinate extraction, indexing
- ✅ Successfully tested with Madison WI locations (downtown, campus, capitol)
- ✅ Established robust R ↔ PostGIS bridge for GPS data integration
- **Next**: Migrate CSV-based workflows into secure local database for API caching

### 8/7/2025

#### Major Project Organization Overhaul

- 🚀 **Organization Win**: Comprehensive function organization and workflow setup
- ✅ Added detailed usage and setup guide for project onboarding
- ✅ Updated/cleaned sample data for tutorials
- ✅ Deleted obsolete QMD files, moved API documentation to proper folder structure
- ✅ Updated file paths in `gps2_guide.qmd` for new organization

### 8/13/2025

#### Zoning Analysis Integration

- ✅ Added `zoning_operations.R` for spatial analysis capabilities
- ✅ Implemented zoning visualizations for geographic context

### 8/19/2025

#### Visualization & Analysis Improvements

- ✅ Enhanced visualization functions with better display capabilities
- ✅ Improved analysis script performance and readability

### 8/22/2025

#### Code Organization & Utils

- ✅ Added `/utils` folder to modularize functions and clean up main scripts
- **Impact**: Significantly shortened main analysis scripts, improved maintainability

### 8/25/2025

#### Tidyverse Compliance Update

- ✅ Updated entire codebase for tidyverse compliance and best practices
- **Impact**: Improved code readability and R community standard adherence

### 8/26/2025

#### Security, Testing & Documentation

- Comprehensive function validity and security testing
- Created testing scripts and documentation (moved to archive after completion)
- Updated Docker YAML with environment path configuration
- Created `.env` file for robust, OS-based filepath handling
- Updated all documentation to reflect code changes
- Improved package loading to lab/tidyverse best practices
- Updated function documentation to Roxygen standards (preparing for eventual package creation)

### 8/27/2025

#### Repository Overhaul & QMD Workflow Architecture

- Abandoned script-based workflow for structured Quarto notebook system
- Started sequential notebook development: `01-setup-infrastructure.qmd`, `02-data-import.qmd`
    - Better documentation, step-by-step validation, easier collaboration with lab
- Built notebooks incrementally in numerical order to establish reproducible pipeline
- much cleaner development process, eliminates ad-hoc script management issues

### 8/28/2025

#### Production Database Implementation

- Improved Docker Compose for robust multi-container deployment (PostGIS + Nominatim)
- Implemented normalized schema with proper foreign key relationships
- Established comprehensive spatial indexing strategy for million+ row performance
- Deferred indexing until after clustering for speed improvement
- Added database constraints and validation triggers for data integrity

### 8/29/2025

#### Complete Data Pipeline Success

- Successfully processed raw GPS points from research drive
- Implemented movement classification pipeline with stationary vs. transition detection
- Generated location clusters across 668 participants
- 89.6% success rate (11,244 addresses) with local Nominatim deployment
- Built a more comprehensive interactive visualization script, less cluttered
- All QMD notebooks operational with real production data

### 8/30/2025

#### Documentation Architecture Overhaul

- Restructured entire project documentation for clarity
- Created comprehensive `documentation/README.md` with database schema and architecture details
- merged development history in `documentation/notes.md` to preserve progress-tracking
- Added 03a-movement-classification.qmd to address deeprooted clustering/processing issues


### 10/27

- Add ADI information for EACH lat/lon from the `gps_enriched.csv` in RD, into a new CSV `gps_enriched_all.csv`
- Add Nominatim placex information (match to the nearest centroid + distance)
- Nearest shop `class` and `type`, with `distance` to centroid (accuracy metric)
- Explore: can locations have multiple shop "types"?

- Try to make the code readable (with assistance / walkthrough)
