# Location Metadata Decision Framework
**Meeting Date:** [To be filled]
**Prepared by:** Christopher Janssen
**Purpose:** Guide discussion on GPS enrichment metadata selection

---

## Quick Reference

### Current Enrichment Status
- ✅ **Implemented:** ADI scores, nearest POI (20m radius)
- ⚠️ **Partially working:** POI matching has noise issues (bicycle_parking, benches dominating)
- ❌ **Not implemented:** Landuse polygons, multi-distance buffers, POI density

---

## Decision 1: POI Noise Filtering

### Problem
Infrastructure/utility POI dominate matches with no research value.

### Current Exclusions (Insufficient)
**Classes:** man_made, natural, military, emergency, healthcare
**Types:** parking, bench, bicycle_parking, shelter, waste_basket, toilets

### Proposed Additional Exclusions
**Add these types:**
- post_box, telephone, vending_machine
- drinking_water, recycling
- car_sharing, charging_station, motorcycle_parking
- streetlight, fire_hydrant, traffic_signals

### Decision Questions
1. Should we exclude ALL transportation infrastructure except bus_stop?
2. Should generic retail shops be excluded or kept?
3. Any other noise categories to add?

---

## Decision 2: POI Selection Framework

### HIGH RELEVANCE (Keep - Core Research Value)

| Category | POI Filter | Research Rationale |
|----------|------------|-------------------|
| **Alcohol (On-Premise)** | amenity IN (bar, pub, nightclub, biergarten) | Core exposure variable |
| **Alcohol (Off-Premise)** | shop IN (alcohol, wine, beverages) | Availability/access measurement |
| **Food/Dining** | amenity IN (restaurant, cafe, fast_food, food_court) | Social context, co-occurrence |
| **Healthcare** | amenity IN (hospital, clinic, pharmacy, doctors, dentist) | Service access, health behavior |
| **Education** | amenity IN (school, university, college, kindergarten, library) | Activity validation, time-use |
| **Financial** | amenity IN (bank, atm, post_office) | Essential services access |

### MEDIUM RELEVANCE (Discuss with Team)

| Category | POI Filter | Considerations |
|----------|------------|----------------|
| **Retail (General)** | class='shop' (excl. alcohol) | High frequency, may dominate matches |
| **Recreation** | leisure IN (park, playground, sports_centre, pool) | Moderate value; focus on point features only? |
| **Transportation** | amenity IN (bus_stop, parking_entrance) | Very high frequency, mostly noise |
| **Social Venues** | amenity IN (community_centre, theatre, cinema) | Low frequency, unclear value |

### LOW RELEVANCE / NOISE (Exclude)

**Infrastructure:** bicycle_parking, bench, waste_basket, shelter, toilets
**Utility:** post_box, telephone, vending_machine, drinking_water, recycling
**Transportation:** parking, motorcycle_parking, charging_station, car_sharing

### Decision Questions
1. Keep all HIGH relevance categories?
2. Which MEDIUM categories are worth keeping?
3. Any additions/modifications to NOISE list?

---

## Decision 3: Landuse Polygon Enrichment

### What Is It?
Point-in-polygon matching to add landuse context (parks, residential zones, natural areas).

### Current Status
❌ **Not implemented** - OSM landuse data is loaded but not used in enrichment

### What It Provides

**Greenspace Metrics:**
- Time in parks (leisure=park)
- Time in natural areas (forest, water, grassland)
- % greenspace exposure per subject

**Built Environment Context:**
- Residential vs. commercial vs. industrial zones
- Land use mixing (multiple types in activity space)

**Park Accessibility:**
- Distance to nearest park from each GPS point
- Urban greenspace access metrics

### Implementation Effort
- **Complexity:** LOW - similar to ADI enrichment (point-in-polygon)
- **Performance:** FAST - spatial indexes handle 1.24M points efficiently
- **Match Rate:** ~40-60% (depends on polygon coverage)

### Research Value Examples
- Alcohol venue exposure in areas with/without parks
- Greenspace access + ADI + alcohol outlets (environmental justice)
- Physical activity opportunities context
- Time allocation: natural vs. built environment

### Decision Questions
1. Should we add landuse enrichment to the pipeline?
2. Which landuse categories matter most?
   - Parks (leisure=park)?
   - All greenspace (parks + forest + water)?
   - Built environment zones (residential/commercial)?
3. Calculate accessibility metrics (distance to nearest park)?

---

## Decision 4: Multi-Distance Buffers

### Current Approach
- Single 20m radius
- "Nearest POI" only (not all POI in range)

### Alternative: Multiple Distance Thresholds

| Distance | Interpretation | Use Case |
|----------|---------------|----------|
| **20m** | "I'm at this place" | Direct presence/exposure |
| **100m** | "Visible/accessible" | Immediate environment |
| **500m** | "Walking distance" | Neighborhood context |

### Alternative: POI Density (Count ALL in radius)

Instead of "nearest X", calculate:
- Count of alcohol venues within 500m
- Count of food venues within 500m
- Density (POI per km²) in buffer

### Implementation Effort
- **Complexity:** MEDIUM - requires additional queries per distance
- **Performance:** SLOWER - more computationally intensive
- **Match Rate:** Increases with larger radius

### Decision Questions
1. Is 20m sufficient or do we need broader context?
2. Should we count ALL POI in radius vs. just nearest?
3. If multi-distance, which thresholds? (100m? 500m? 1km?)
4. Priority: more distance thresholds OR landuse enrichment?

---

## Decision 5: Enrichment Strategy

### Option A: Focused (Current + Minor Improvements)
- ✅ Keep current approach
- ➕ Add expanded noise filters
- ➕ Focus only on high-relevance POI categories
- ⏱️ Fastest to implement

### Option B: Landuse Addition (Recommended)
- ✅ Keep current POI enrichment (with noise fixes)
- ➕ Add landuse polygon enrichment
- ➕ Calculate greenspace metrics
- ⏱️ Moderate implementation time
- 🎯 High research value, novel contribution

### Option C: Multi-Scale Comprehensive
- ✅ Keep current POI enrichment (with noise fixes)
- ➕ Add landuse polygon enrichment
- ➕ Add multi-distance buffers (20m, 100m, 500m)
- ➕ Calculate POI density metrics
- ⏱️ Longest implementation time
- 📊 Maximum environmental context

### Decision Questions
1. Which option aligns with research timeline?
2. What are the priority research questions?
3. Can we phase implementation (Option B now, Option C later)?

---

## Summary: Key Decisions Needed

1. **POI Noise:** Approve expanded exclusion list?
2. **POI Categories:** Which to keep/exclude from MEDIUM relevance?
3. **Landuse Enrichment:** Add to pipeline? Which categories?
4. **Distance Thresholds:** Stick with 20m or add 100m/500m?
5. **Implementation Priority:** Focused (A), Landuse (B), or Comprehensive (C)?

---

## Technical Notes

### Database Performance
- All enrichment queries run in seconds-minutes (not hours)
- Spatial indexes optimized for 1.24M GPS points
- Can handle complex multi-join enrichment

### Privacy
- All processing done locally on VM
- No external geocoding APIs
- OSM data already in local database

### Extensibility
- Easy to add more POI categories later
- Can retroactively re-enrich with new metadata
- Future studies can use same infrastructure

---

## Exploration Materials Available

1. **explore_poi_noise.qmd** - POI frequency analysis and decision framework
2. **explore_greenspace.qmd** - Landuse enrichment demo with metrics
3. **explore_nominatim.qmd** - Full OSM data catalog

**Run these notebooks before the meeting to see actual data patterns!**
