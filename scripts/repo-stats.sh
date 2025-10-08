#!/bin/bash
# Repository statistics for artma package

set -e

# Colors for output
BOLD="\033[1m"
GREEN="\033[32m"
BLUE="\033[34m"
YELLOW="\033[33m"
RESET="\033[0m"

echo -e "${BOLD}${BLUE}=== artma Repository Statistics ===${RESET}\n"

# Package info
echo -e "${BOLD}Package Information:${RESET}"
PACKAGE_VERSION=$(grep "^Version:" DESCRIPTION | awk '{print $2}')
PACKAGE_NAME=$(grep "^Package:" DESCRIPTION | awk '{print $2}')
echo "  Name: $PACKAGE_NAME"
echo "  Version: $PACKAGE_VERSION"
echo ""

# Git info
echo -e "${BOLD}Git Information:${RESET}"
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "N/A")
LAST_COMMIT=$(git log -1 --format="%h - %s" 2>/dev/null || echo "N/A")
TOTAL_COMMITS=$(git rev-list --count HEAD 2>/dev/null || echo "N/A")
echo "  Current branch: $CURRENT_BRANCH"
echo "  Last commit: $LAST_COMMIT"
echo "  Total commits: $TOTAL_COMMITS"
echo ""

# Code statistics
echo -e "${BOLD}Code Statistics:${RESET}"

# Count R files and lines (excluding local/ directory and build artifacts)
# Primary locations: R/, inst/, tests/, scripts/R/, vignettes/
R_FILES=$(find . -type f -name "*.R" \
  -not -path "./local/*" \
  -not -path "./.Rcheck/*" \
  -not -path "./..Rcheck/*" \
  -not -path "./_cache/*" \
  -not -path "./revdep/*" \
  -not -path "./Meta/*" \
  -not -path "./doc/*" | wc -l | xargs)

R_LINES=$(find . -type f -name "*.R" \
  -not -path "./local/*" \
  -not -path "./.Rcheck/*" \
  -not -path "./..Rcheck/*" \
  -not -path "./_cache/*" \
  -not -path "./revdep/*" \
  -not -path "./Meta/*" \
  -not -path "./doc/*" \
  -exec wc -l {} + | tail -n 1 | awk '{print $1}')

# Count by directory
R_DIR_LINES=$(find ./R -type f -name "*.R" -exec wc -l {} + 2>/dev/null | tail -n 1 | awk '{print $1}' || echo "0")
INST_DIR_LINES=$(find ./inst -type f -name "*.R" -exec wc -l {} + 2>/dev/null | tail -n 1 | awk '{print $1}' || echo "0")
TESTS_DIR_LINES=$(find ./tests -type f -name "*.R" -exec wc -l {} + 2>/dev/null | tail -n 1 | awk '{print $1}' || echo "0")
SCRIPTS_DIR_LINES=$(find ./scripts -type f -name "*.R" -exec wc -l {} + 2>/dev/null | tail -n 1 | awk '{print $1}' || echo "0")

echo -e "  Total R files: ${GREEN}${R_FILES}${RESET}"
echo -e "  Total lines of R code: ${GREEN}${R_LINES}${RESET}"
echo ""
echo "  Breakdown by directory:"
echo "    R/: $R_DIR_LINES lines"
echo "    inst/: $INST_DIR_LINES lines"
echo "    tests/: $TESTS_DIR_LINES lines"
echo "    scripts/: $SCRIPTS_DIR_LINES lines"
echo ""

# Count methods
METHODS_COUNT=$(find ./inst/artma/methods -type f -name "*.R" 2>/dev/null | wc -l | xargs || echo "0")
echo "  Runtime methods: $METHODS_COUNT"

# Count test files
TEST_FILES=$(find ./tests/testthat -type f -name "test-*.R" 2>/dev/null | wc -l | xargs || echo "0")
echo "  Test files: $TEST_FILES"
echo ""

# YAML/config files
echo -e "${BOLD}Configuration Files:${RESET}"
YAML_FILES=$(find . -type f -name "*.yaml" -o -name "*.yml" | wc -l | xargs)
echo "  YAML files: $YAML_FILES"
echo ""

# Dependencies
echo -e "${BOLD}Dependencies:${RESET}"
IMPORTS=$(grep "^Imports:" DESCRIPTION -A 100 | sed -n '/^Imports:/,/^[A-Z]/p' | grep -v "^[A-Z]" | grep -c "," || echo "0")
IMPORTS=$((IMPORTS + 1))  # Add 1 for the last package (no comma)
echo "  Imported packages: ~$IMPORTS"
echo ""

# Repository size
echo -e "${BOLD}Repository Size:${RESET}"
REPO_SIZE=$(du -sh . 2>/dev/null | awk '{print $1}')
CODE_SIZE=$(du -sh ./R ./inst ./tests 2>/dev/null | awk '{sum+=$1} END {print sum "K"}')
echo "  Total size: $REPO_SIZE"
echo ""

echo -e "${BOLD}${GREEN}Statistics generated successfully!${RESET}"
