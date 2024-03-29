#!/bin/bash
set -e

SCRIPTS_DIR=$(dirname "${BASH_SOURCE[0]}")
source "$SCRIPTS_DIR/shellUtils.sh"

if [[ -z "$1" ]]; then
    error "You must provide either an image name to rebuild, or 'all' to rebuild all images."
    exit 1
fi

# Static
build_key="$1"
repository_name="localhost"
image_name="artma"
dockerfile_tags=("flask:Flask/" "react:React/" "r:R/") # tag:project-folder

# Call the function to get the package version
version=$(get_package_version)

function buildImage() {
    image_key=$1
    image_folder=$2
    other_arg=$3

    new_image_tag="$image_name/$image_key:v$version" # e.g. artma/flask:v1

    if [ "$other_arg" == "force-rebuild" ]; then
        info "Deleting $new_image_tag"
        podman rmi "$repository_name/$new_image_tag" || true
    fi

    # Check if the image already exists
    if ! image_exists "$repository_name/$new_image_tag" | grep -q "true" >/dev/null; then
        info "Building $new_image_tag"
        podman build -t "$repository_name/$new_image_tag" "$value"
    else
        info "Image $new_image_tag already exists. Skipping build."
    fi
}

if [[ "$build_key" == "all" ]]; then
    info "Building up all images for version $version"
fi

image_built=false

for entry in "${dockerfile_tags[@]}"; do
    IFS=':' read -r key value <<<"$entry"
    if [[ "$key" == "$build_key" || "$build_key" == "all" ]]; then
        buildImage "$key" "$value" $2
        image_built=true
    fi
done

if [ $image_built == false ]; then
    error "Could not find the image $image_key"
    exit 1
fi

success "Done building images!"