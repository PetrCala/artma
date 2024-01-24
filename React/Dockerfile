# Use a Node base image
FROM node:21-alpine

# Set the working directory in the container
WORKDIR /client

# Copy package.json and package-lock.json
COPY client/package*.json /client

# Install project dependencies
RUN npm install

# Copy the client directory contents into the container at /client
COPY client/ /client

# Build the app
RUN npm run build

# Install serve to serve the static files
RUN npm install -g serve

# Make port 3000 available to the world outside this container
EXPOSE 3000

# Run the app in dev mode
CMD ["npm", "run", "dev"]
# CMD ["serve", "-s", "build"] # for production
# CMD ["npm", "run", "start"] # for production
