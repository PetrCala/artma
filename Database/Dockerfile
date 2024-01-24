# Use the official PostgreSQL image as a base
FROM postgres:latest

# Copy the entrypoint script into the container
COPY ./Database/docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

# Set the entrypoint
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["postgres"]
