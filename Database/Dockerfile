# Use the official PostgreSQL image as a base
FROM postgres:latest

# Copy the entrypoint script into the container
COPY ./docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

# Set the entrypoint
# ENTRYPOINT ["docker-entrypoint.sh", "-c", "'shared_buffers=256MB'", "-c", "'max_connections=200'"]
ENTRYPOINT ["docker-entrypoint.sh"]

EXPOSE 5432

CMD ["postgres"]
