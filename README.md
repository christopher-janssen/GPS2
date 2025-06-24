# The Panopticon Project - Location Contextualization for Recovery Monitoring

## Project Goal

This project automatically figures out what kind of places people visit based on their GPS coordinates. Instead of asking study participants to manually log every location they go to, we use mapping APIs to automatically identify whether a GPS point is a bar, park, grocery store, healthcare facility, etc.

## Relevance

People in addiction recovery are participating in a 17-week study where their phones track GPS locations along with daily mood and craving surveys. Understanding *what* these locations are (not just where they are) helps researchers better predict risk and provide personalized support messages. 

For example:
- GPS shows someone at coordinates (43.0731, -89.4012)
- Our system identifies this as "Merchant Public House" - a bar/restaurant
- This context helps the risk prediction algorithm understand potential triggers

## The Problem

Raw GPS coordinates like `(43.0731, -89.4012)` don't tell researchers much. Asking participants to manually categorize every location they visit creates survey fatigue and reduces study participation.

## The Solution

We're building a system that takes GPS coordinates and automatically extracts useful information by:

1. **Querying mapping APIs** (Google Maps, OpenStreetMap) to identify what's at each location
2. **Categorizing venues** into recovery-relevant types (bars, healthcare, parks, routine locations)
3. **Comparing different data sources** to find the best balance of accuracy and cost

## Expected Outcome

A system that can take any GPS coordinate from the study and automatically return contextual information like:
- Venue type and name
- Risk category (high-risk venue, neutral location, supportive environment)
- Environmental characteristics
- Cost and reliability metrics for different data sources

This reduces participant burden while giving researchers richer data for understanding recovery patterns.
