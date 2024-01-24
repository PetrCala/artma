#!/bin/bash
set -e

# Before PostgreSQL can function correctly, the database cluster must be initialized:
initdb -D /var/lib/postgres/data

# internal start of server in order to allow set-up using psql-client
# does not listen on external TCP/IP and waits until start finishes
pg_ctl -D "/var/lib/postgres/data" -o "-c listen_addresses=''" -w start

# create a user or role
psql -d postgres -c "CREATE USER postgres WITH PASSWORD 'password';" 

# create database 
psql -v ON_ERROR_STOP=1 -d postgres -c "CREATE DATABASE artma OWNER 'postgres';"

# stop internal postgres server
pg_ctl -v ON_ERROR_STOP=1 -D "/var/lib/postgres/data" -m fast -w stop

# Check if PostgreSQL data directory is empty (indicating first-time setup)
# if [ ! "$(ls -A /var/lib/postgresql/data)" ]; then
#   # Initialize the database with custom SQL scripts
#   echo "Initializing database..."
  
#   # Example: Run SQL script
#   psql -U ${POSTGRES_USER} -d ${POSTGRES_DB} -a -f /docker-entrypoint-initdb.d/init.sql
  
#   echo "Database initialization complete."
# fi

# Start PostgreSQL
exec "$@"
