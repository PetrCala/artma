#!/bin/bash
# composeUp.sh
set -e

SCRIPTS_DIR=$(dirname "${BASH_SOURCE[0]}")
PROJECT_ROOT=$(dirname "$SCRIPTS_DIR")
source "$SCRIPTS_DIR/shellUtils.sh"

# Set the environment. Pass 'dev' or 'prod' as an argument to this script.
ENVIRONMENT=${1:-dev} # Default to 'dev' if no argument is provided

if [[ "$ENVIRONMENT" != "prod" && "$ENVIRONMENT" != "dev" ]]; then
    error "Invalid environment. Please provide either 'prod' or 'dev' as an argument."
    exit 1
fi

"$SCRIPTS_DIR/setenv.sh" $ENVIRONMENT # Set the .env file to the correct environment

if [ -f "$PROJECT_ROOT/.env" ]; then
    source "$PROJECT_ROOT/.env"
else
    error "Error: .env file not found."
    exit 1
fi

# Static
package_version=$(get_package_version)
repository_name=${REPOSITORY_NAME:-"localhost"}
image_name=${IMAGE_NAME:-"artma"}

# Set the application environment variables for docker compose
export FLASK_IMAGE_NAME="$repository_name/$image_name/flask:v$package_version"
export REACT_IMAGE_NAME="$repository_name/$image_name/react:v$package_version"
export R_IMAGE_NAME="$repository_name/$image_name/r:v$package_version"

export FLASK_ENV=${FLASK_ENV}
export FLASK_RUN_HOST=${FLASK_RUN_HOST}
export FLASK_RUN_PORT=${FLASK_RUN_PORT}
export R_HOST=${R_HOST}
export R_PORT=${R_PORT}

image_names=("$FLASK_IMAGE_NAME" "$REACT_IMAGE_NAME" "$R_IMAGE_NAME")
prod_services=("flask_prod" "react_prod" "r_prod")
dev_services=("flask_dev" "react_dev" "r_dev")

if [[ "$ENVIRONMENT" == "prod" ]]; then
    services=("${prod_services[@]}")
else
    services=("${dev_services[@]}")
fi

# Check if images exist
missing_images=()
build_required=false

for image_name in "${image_names[@]}"; do
    if ! image_exists "$image_name" | grep -q "true" >/dev/null; then
        missing_images+=("$image_name")
        build_required=true
    fi
done

if [ "$build_required" = true ]; then
    if [["$ENVIRONMENT" == "prod"]]; then
        # Always build images in production
        info "Building missing images as per BUILD_ variables..."
        npm run images:build
    else
        read -p "Some of the local images are missing: ${missing_images[*]}. Do you want to build them now? (y/N) " response
        case "$response" in
        [yY][eE][sS] | [yY])
            info "Building missing images..."
            npm run images:build
            ;;
        *)
            error "Required images are missing. Exiting..."
            exit 1
            ;;
        esac
    fi
fi

# Function to clean up containers on Ctrl+C
cleanup() {
    info "Stopping containers..."
    podman-compose down
    info "Successfully stopped and removed all containers."
    success "Done."
    exit 0
}

# Trap Ctrl+C and call the cleanup function
trap cleanup SIGINT

info "Running all containers for version $package_version in $ENVIRONMENT environment..."

podman-compose up "${services[@]}" # Only the relevant services

exit 0
