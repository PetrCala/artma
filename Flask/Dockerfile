#
# ---- Base ----
FROM python:3.12 AS base

#
# ---- Release ----
FROM base AS release

# Set the working directory in the container to /app
WORKDIR /usr/src/app

# Copy the current directory contents (Flask-app folder) into the container at /app
COPY . /usr/src/app/

# Install any needed packages specified in setup.py
RUN pip install --no-cache-dir -e .

# Make port 8080 available to the world outside this container
EXPOSE 8080

# Define environment variable
# ENV NAME World

#
# ---- Dev ----
FROM release AS dev
CMD ["python", "app.py"]

#
# ---- Prod ----
FROM release AS prod
CMD ["python", "app.py"]
# TBA