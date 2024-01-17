<div align="center">
    <h1>
        <!-- <a href="https://link-here"> -->
        Automatic Replication Tools for Meta-Analysis
        <!-- </a> -->
    </h1>
</div>


#### Table of Contents
- [How to run](#how-to-run)
  - [Prerequisites](#prerequisites)
  - [Steps](#steps)
  - [Notes](#notes)
- [Handling images containers](#handling-images-containers)
  - [Scripts](#scripts)
- [Technical choices](#technical-choices)
    - [Front-end](#front-end)
    - [Back end](#back-end)
    - [CI/CD](#cicd)
- [Useful notes](#useful-notes)
- [Useful resources](#useful-resources)

# How to run

## Prerequisites

- Install `node`, `Python`, `R`, `Podman` (or `Docker`),
- [Set up podman](https://podman.io/docs/installation) or [Set up Docker](https://docs.docker.com/engine/install/)
- Install `podman-compose` using `brew install podman-compose`

## Steps

- Initialize the podman using `podman machine start`.
- Run `npm run start:dev`. This will build all necessary images, and start up relevant containers in a development environment. For production environment, execute `npm run start:prod` instead.
- Individual parts of the application can be accessed from your browser or from the terminal under these domains:
  - **React application:** `127.0.0.1:3000`
  - **Flask API endpoint:** `127.0.0.1:8080`
  - **R API endpoint:** `127.0.0.1:8787`
  
## Notes

- You can check that the containers are up and running by calling `podman ps -a` from another terminal instance.
- Upon pressing Ctrl+C in the terminal instance where the `start:dev` command was executed, all of the running containers will be gracefully shut down and deleted.
- Any images built during the process will remain present. You can check the list of these images by running `podman images`. Remove these using `podman rmi [image-name]`, or `podman rmi -a`, which will remove all images.
- You can also access any of the containers by using `localhost` as the host name, such as `localhost:3000` for the React application.

# Handling images containers

## Scripts

There are several pre-defined node scripts which should help you work with images and containers. All of these can be executed using `npm run [script-name]`Here is a list of some of them:

- `images:build`: Builds the Docker images only if they don't already exist.
- `images:rebuild`: Forces the rebuild of all Docker images.
- `images:rebuild-<image-tag>`: Force the rebuild of a single Docker image (specified by `<image-tag>`). E.g., `bun run images:rebuild-react` rebuilds the React image.
- `images:rename`: Renames all existing images to match the current project version.
- `start:dev`: Builds any missing images and starts up all containers in a development environment. Upon Ctrl+C, all of the containers will be gracefully shut down and deleted.
- `start:prod`: Very similar to `start:dev`, but builds and starts the containers in a production environment.
- `stop`: Stops and removes all existing containers.

# Technical choices

### Front-end

- React with Next.js

### Back end

- Flask
- R API endpoint through [Plumber](https://www.rplumber.io)

### CI/CD

- Containers using Podman (or Docker)

# Useful notes

- Remove empty images

  ```bash
  podman images --format "{{.ID}}: {{.Repository}}:{{.Tag}}" | grep '<none>' | awk -F: '{print $1}' | xargs -I {} podman rmi {}
  ```

- Get logs of a container that is running in a detached mode

  ```bash
  podman logs -f <container-name>
  ```

# Useful resources

- [Retro-board GH repo](https://github.com/antoinejaussoin/retro-board/blob/develop/docker-compose.yml) - For integrating PostgreSQL into the project