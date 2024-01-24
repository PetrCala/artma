# ---- Base Node ----
FROM node:21-alpine AS base

#
# ---- Dependencies ----
FROM base AS dependencies

# Copy package.json and package-lock.json
COPY client/package*.json /tmp

# Install project dependencies only if package.json has changed
RUN cd /tmp && npm install


#
# ---- Test ----
# run linters, setup and tests
FROM dependencies AS test
COPY . .
# RUN  npm run lint && npm run setup && npm run test
RUN npm run lint

#
# ---- Release ----
FROM base AS release

# Set directory inside the container
WORKDIR /usr/src/app

# Copy the client directory contents into the container at /client
COPY client/ /usr/src/app

# Copy dependencies
COPY --from=dependencies /tmp/node_modules ./node_modules

# Build the app
RUN npm run build

# Install serve to serve the static files
RUN npm install -g serve

# Make port 3000 available to the world outside this container
EXPOSE 3000

# Run the app in dev mode
CMD ["npm", "run", "start"]
# CMD ["serve", "-s", "build"] # for production
# CMD ["npm", "run", "dev"] # for hot-reloading


# ---- Other ----
# Copy files as a non-root user. The `node` user is built in the Node image.
# RUN chown node:node ./
# USER node

# Separate builder stage to compile SASS, so we can copy just the resulting CSS files.
# FROM rubygem/compass AS builder
# COPY ./src/public /dist
# WORKDIR /dist
# RUN compass compile
# Output: css/app.css

