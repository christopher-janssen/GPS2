#!/bin/bash
# Add a PostgreSQL user for a VM user
# Usage: sudo add-postgres-user <username>

if [ "$EUID" -ne 0 ]; then
  echo "Error: This script must be run as root (use sudo)"
  exit 1
fi

if [ -z "$1" ]; then
  echo "Usage: sudo add-postgres-user <username>"
  echo "Example: sudo add-postgres-user cjanssen3"
  exit 1
fi

USERNAME=$1

echo "Creating PostgreSQL role for user: $USERNAME"
sudo -u postgres createuser "$USERNAME"

echo "Granting database access..."
sudo -u postgres psql -c "GRANT ALL ON DATABASE geolocation TO $USERNAME;"

echo ""
echo "Success! User '$USERNAME' can now access PostgreSQL."
echo "They can connect with: psql geolocation"
