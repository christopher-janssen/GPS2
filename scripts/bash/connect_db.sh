#!/bin/bash
# scripts/connect_db.sh
# quick script to connect to GPS2 PostGIS database

echo "🔌 Connecting to GPS2 PostGIS Database..."
echo "==========================================="

# check if container is running
if ! docker ps | grep -q "gps2_geocoding"; then
    echo "❌ GPS2 container not running!"
    echo "Starting container..."
    cd docker-postgis
    docker-compose up -d
    echo "Waiting for database to initialize..."
    sleep 10
    cd ..
fi

echo "✅ Container running"
echo "Password: secure_research_password"
echo "Useful commands once connected:"
echo "   \\l          - list databases"
echo "   \\dt gps2.*  - list tables in gps2 schema"  
echo "   \\d gps2.table_name - describe table structure"
echo "   \\q          - quit"
echo ""
echo "Connecting..."

# connect to database
docker exec -it gps2_geocoding psql -U gps2_researcher -d gps2_geocoding