#!/bin/bash
# composeUp.sh
set -e

SCRIPTS_DIR=$(dirname "${BASH_SOURCE[0]}")
PROJECT_ROOT=$(dirname "$SCRIPTS_DIR")
source "$SCRIPTS_DIR/shellUtils.sh";

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

repository_name=${REPOSITORY_NAME:-"localhost"}
image_name=${IMAGE_NAME:-"artma"}
package_version=$(get_package_version)

# Static
export FLASK_IMAGE_NAME="$repository_name/$image_name/flask:v$package_version"
export REACT_IMAGE_NAME="$repository_name/$image_name/react:v$package_version"
export R_IMAGE_NAME="$repository_name/$image_name/r:v$package_version"

# Set the application environments
if [ "$ENVIRONMENT" = "prod" ]; then
    export FLASK_ENV="production"
    export BUILD_ENV="production" # For react
elif [ "$ENVIRONMENT" = "dev" ]; then
    export FLASK_ENV="development"
    export BUILD_ENV="development" # For react
else
    error "Invalid application environment. Exiting..."
    exit 1
fi

# Check if images exist
image_names=("$FLASK_IMAGE_NAME" "$REACT_IMAGE_NAME" "$R_IMAGE_NAME")
missing_images=()
build_required=false

for image_name in "${image_names[@]}"; do
    if ! image_exists "$image_name" | grep -q "true" >/dev/null; then
        missing_images+=("$image_name")
        build_required=true
    fi
done

# Handling BUILD_ variables
export BUILD_FLASK_IMAGE=${BUILD_FLASK_IMAGE:-"none"}
export BUILD_REACT_IMAGE=${BUILD_REACT_IMAGE:-"none"}
export BUILD_R_IMAGE=${BUILD_R_IMAGE:-"none"}

if [ "$build_required" = true ]; then
    if [[ "$BUILD_FLASK_IMAGE" != "none" || "$BUILD_REACT_IMAGE" != "none" || "$BUILD_R_IMAGE" != "none" ]]; then
        info "Building missing images as per BUILD_ variables..."
        npm run images:build
    else
        read -p "Some of the local images are missing: ${missing_images[*]}. Do you want to build them now? (y/N) " response
        case "$response" in
            [yY][eE][sS]|[yY])
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
    exit 0;
}

# Trap Ctrl+C and call the cleanup function
trap cleanup SIGINT

info "Running all containers for version $package_version in $ENVIRONMENT environment..."

podman-compose up

exit 0;