#!/bin/bash
set -e

# Perform any custom initialization tasks here
# For example, you can run SQL scripts during container startup

# Check if PostgreSQL data directory is empty (indicating first-time setup)
if [ ! "$(ls -A /var/lib/postgresql/data)" ]; then
  # Initialize the database with custom SQL scripts
  echo "Initializing database..."
  
  # Example: Run SQL script
#   psql -U your_user -d your_db -a -f /docker-entrypoint-initdb.d/init.sql
  
  echo "Database initialization complete."
fi

# Start PostgreSQL
exec "$@"
