#!/bin/bash
# scripts/connect_db.sh
# quick script to connect to GPS2 PostGIS database

echo "Connecting to GPS2 PostGIS Database..."
echo "==========================================="

# check if container is running
if ! docker ps | grep -q "gps-postgis"; then
    echo "❌ GPS2 container not running!"
    echo "Starting container..."
    cd docker-compose
    docker-compose up -d
    echo "Waiting for database to initialize..."
    sleep 10
    cd ..
fi

echo "✅ Container running"
echo "Password: postgres"
echo "Useful commands once connected:"
echo "   \\l                    - list databases"
echo "   \\dt                   - list tables in current database"
echo "   \\d table_name         - describe table structure"
echo "   \\di                   - list indexes"
echo "   \\dn                   - list schemas"
echo "   SELECT version();      - check PostGIS version"
echo "   \\q                    - quit"
echo ""
echo "Connecting..."

# connect to database
docker exec -it gps-postgis psql -U postgres -d gps_analysis