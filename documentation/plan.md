Project Summary:
Goal: Develop a methodology for analyzing GPS data from AUD recovery participants to create features for relapse prediction models, directly addressing privacy concerns about transmitting patient location data to external services.

## PostGIS + Docker

PostGIS is a spatial database extension for PostgreSQL, which includes "TIGER" geocoder functionality. TIGER (Topologically Integrated Geographic Encoding and References) uses the US Census Bureau data to convert addresses into geographic coordinates (it also has some reverse geocoding capabilities). 

Ideally, the Docker container setup would create an isolated, portable environment that packages the entire geocoding system (postgreSQL database, PostGIS extensions, TIGER data, and geocoding functions) into a single container, which streamlines the local installation and deployment. Additionally, the container would ideally keep all data and geocoding information isolated from anything else on the computer, but realistically Docker would be used primarily for *convenience*.

## ArcGIS

ArcGIS is a *commercial* geographic information system (GIS) software suite developed that includes geocoding capabilities. ArcGIS uses multiple reference datasets including street networks, postal codes, and administrative boundaries to convert addresses into geographic coordinates. Reverse geocoding, batch processing, and address standardization features are also available.

Ideally, the desktop installation would create a complete, self-contained geocoding environment that includes the ArcGIS software, licensed reference data, and geocoding engines all stored locally on the user's machine. This setup would ensure all patient address data remains within direct control and never transmits to external servers. Additionally, ArcGIS would provide enterprise-grade geocoding accuracy and performance, but notably the primary benefit is data security compliance rather than technical superiority.

## QGIS

QGIS is a free, open-source geographic information system that provides geocoding functionality through various plugins and service connections. By default, QGIS geocoding relies on external services like OpenStreetMap's Nominatim, which would (according to the paper) violate compliance by transmitting patient addresses to third-party servers. However, QGIS can be configured to use local geocoding solutions.

Ideally, the local setup would involve configuring QGIS to connect to locally-hosted geocoding services (such as a local PostGIS/TIGER installation or offline geocoding databases) rather than external web services. This configuration creates a compliant geocoding environment where all address processing occurs within the our infrastructure. Additionally, QGIS has cost-effective geocoding capabilities with full source code transparency, but notably the setup supposedly requires a decent amount of technical work to properly configure local-only geocoding.

## The Elephant in the Room

The project aims to contextualize GPS coordinates from study participants by identifying venue type and information (bars, healthcare facilities, parks, etc.) to enhance relapse-risk prediction. This hypothetical approach aims to reduce participant burden by eliminating manual location logging while still providing richer data for understanding recovery patterns.

This creates potential privacy challenges similar to those identified by Rundle et al. regarding the transmission of location data to third-party services. While Rundle et al. focus specifically on patient residential addresses, the same fundamental privacy principles would likely apply to GPS coordinate data. 

According to the best practices outlined in their work, patient location information should not be transmitted to external commercial APIs (like Google Maps or OpenStreetMap) without Business Associate Agreements (BAAs), which we do not have. In the perspective of the authors, GPS coordinates from participants' daily movements would likely be considered personally identifiable information, and transmitting them to third-party services for venue identification could violate HIPAA compliance protocols that require keeping patient data within secured local systems.

## Solution?

Given this difficulty, I have thought about the way that we can best work with these limitations:

Core Architecture:

Local PostGIS processing: All GPS data processing happens in a Docker container on local research systems - no sensitive coordinates ever leave your infrastructure
Zone-based exclusion: In terms of external API calls, complete blackout of residential areas, analysis only in commercial and mixed-use zones where alcohol venue exposure is relevant for relapse prediction
Uniform spatial resolution: Use consistent H3 hexagonal grid (resolution 9) with area-weighted majority zone classification - simple and computationally doable in PostGIS
Cache: Build comprehensive venue cache in PostGIS to reduce external API dependency over time

IF NEEDED:
Query obfuscation: When pulling POI data from external APIs, query visited commercial districts rather than just participant-visited locations to prevent inference of actual movement patterns

Benefits:

Eliminates the privacy vulnerability identified in Rundle et al. by keeping all location processing local
Maintains analytical utility for detecting behavioral patterns relevant to relapse risk
Uses established spatial analysis methods (PostGIS, H3) rather than experimental privacy techniques
Creates reproducible methodology for other researchers

Implementation: PostGIS handles spatial operations (zoning, hexagonal aggregation), R handles machine learning feature engineering and model integration.
Bottom line: This is architectural privacy protection - solving the privacy problem by keeping sensitive data local, not by adding complex mathematical privacy mechanisms on top.