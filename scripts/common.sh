#!/bin/bash

set -e

SCRIPTS_DIR_REL=$(dirname "${BASH_SOURCE[0]}")
. "$SCRIPTS_DIR_REL/shellUtils.sh"
SCRIPTS_DIR=$(get_abs_path "$SCRIPTS_DIR_REL")

PKG_NAME="artma"

# Direcotry names - _DIR_NAME
CACHE_DIR_NAME="_cache"
R_DIR_NAME="R"
OUTPUT_DIR_NAME="output"
CONFIG_DIR_NAME="config"
CONFIG_SRC_DIR_NAME="$R_DIR_NAME/libs/config"
LINT_DIR_NAME="R" # Name of the directory to lint
TEST_DIR_NAME="tests/testthat"
E2E_TEST_DIR_NAME="tests/E2E"
SCRIPTS_SRC_DIR_NAME="src"

# File names - _FILE_NAME
RUN_FILE_NAME="run.R"
METADATA_FILE_NAME="metadata.yaml" # Metadata in the R directory
CONFIG_FILE_NAME="config.yaml"
CONFIG_SRC_FILE_NAME="config_src.yaml" # Config template
SENTINEL_FILE_NAME="last_poetry_install.txt"
SETUP_ENV_FILE_NAME="base/setup_env.R"

# Full folder paths - _DIR
ROJECT_ROOT_REL=$(dirname "$SCRIPTS_DIR")
PROJECT_ROOT=$(get_abs_path "$ROJECT_ROOT_REL")
CACHE_DIR="$PROJECT_ROOT/$CACHE_DIR_NAME"
R_DIR="$PROJECT_ROOT/$R_DIR_NAME"
OUTPUT_DIR="$PROJECT_ROOT/$OUTPUT_DIR_NAME"
CONFIG_DIR="$PROJECT_ROOT/$CONFIG_DIR_NAME"
CONFIG_SRC_DIR="$PROJECT_ROOT/$CONFIG_SRC_DIR_NAME"
LINT_DIR="$PROJECT_ROOT/$LINT_DIR_NAME"
SCRIPTS_SRC_DIR="$SCRIPTS_DIR/$SCRIPTS_SRC_DIR_NAME"

# Full paths to individual files - _FILE_PATH
METADATA_FILE_PATH="$R_DIR/$METADATA_FILE_NAME"
SETUP_ENV_FILE_PATH="$R_DIR/$SETUP_ENV_FILE_NAME"
RUN_FILE_PATH="$R_DIR/$RUN_FILE_NAME"
CONFIG_FILE_PATH="$R_DIR/$CONFIG_FILE_NAME"
CONFIG_SRC_FILE_PATH="$CONFIG_SRC_DIR/$CONFIG_SRC_FILE_NAME"
