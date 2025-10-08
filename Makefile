# Makefile for artma R package
#
# Usage:
#   make install    - Install package dependencies
#   make test       - Run tests
#   make check      - Run R CMD check
#   make document   - Generate documentation
#   make build      - Build package tarball
#   make clean      - Clean build artifacts

.PHONY: help install test test-file test-filter test-e2e check check-fast lint document build clean coverage coverage-report style desc-normalize all dev quick setup vignettes fix-options clear-cache stats

# Default target
help:
	@echo "artma package - Available targets:"
	@echo ""
	@echo "  make install          Install package locally"
	@echo "  make deps             Install package dependencies"
	@echo "  make setup            Setup the development environment"
	@echo "  make test             Run test suite"
	@echo "  make test-file        Run specific test file (FILE=test-name.R)"
	@echo "  make test-filter      Run tests matching pattern (FILTER=pattern)"
	@echo "  make test-e2e         Run end-to-end tests"
	@echo "  make check            Run R CMD check"
	@echo "  make check-fast       Run R CMD check (without installation)"
	@echo "  make lint             Lint package code"
	@echo "  make document         Generate documentation with roxygen2"
	@echo "  make build            Build source package"
	@echo "  make vignettes        Build vignettes"
	@echo "  make clean            Remove build artifacts"
	@echo "  make coverage         Generate test coverage report"
	@echo "  make coverage-report  Open interactive coverage report"
	@echo "  make style            Auto-format code with styler"
	@echo "  make desc-normalize   Normalize DESCRIPTION file"
	@echo "  make fix-options      Validate option templates and default files"
	@echo "  make clear-cache      Clear package cache"
	@echo "  make stats            Display repository statistics"
	@echo "  make all              Run document, test, lint, and check"
	@echo "  make quick            Quick dev cycle (document + test)"
	@echo "  make dev              Load package for interactive development"
	@echo ""

# Install package dependencies
deps:
	@echo "Installing package dependencies..."
	@Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	@Rscript -e "devtools::install_deps(dependencies = TRUE, upgrade = 'never')"

# Setup development environment
setup:
	@echo "Setting up development environment..."
	@bash scripts/setup.sh

# Install package locally
install: deps
	@echo "Installing artma package..."
	@Rscript -e "devtools::install()"

# Run all tests
test:
	@echo "Running tests..."
	@bash scripts/test.sh

# Run specific test file
test-file:
ifndef FILE
	@echo "Error: FILE not specified. Usage: make test-file FILE=test-name.R"
	@exit 1
endif
	@echo "Running tests in $(FILE)..."
	@bash scripts/test.sh --file $(FILE)

# Run tests matching a filter pattern
test-filter:
ifndef FILTER
	@echo "Error: FILTER not specified. Usage: make test-filter FILTER=pattern"
	@exit 1
endif
	@echo "Running tests matching pattern: $(FILTER)..."
	@bash scripts/test.sh --filter $(FILTER)

# Run end-to-end tests
test-e2e:
	@echo "Running end-to-end tests..."
	@bash scripts/testE2E.sh

# Run R CMD check
check:
	@echo "Running R CMD check..."
	@Rscript -e "devtools::check()"

# Check without installation (faster for iterative development)
check-fast:
	@echo "Running R CMD check (without installation)..."
	@Rscript -e "devtools::check(args = c('--no-install'))"

# Lint package
lint:
	@echo "Linting package..."
	@Rscript -e "devtools::load_all(); lintr::lint_package()"

# Generate documentation
document:
	@echo "Generating documentation..."
	@Rscript -e "devtools::document()"

# Build package tarball
build: document
	@echo "Building package..."
	@Rscript -e "devtools::build()"

# Build vignettes
vignettes:
	@echo "Building vignettes..."
	@Rscript -e "devtools::build_vignettes()"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf *.tar.gz
	@rm -rf .Rcheck
	@rm -rf man/*.Rd
	@rm -rf doc
	@rm -rf Meta
	@find . -name ".Rhistory" -delete
	@find . -name ".DS_Store" -delete

# Generate test coverage report
coverage:
	@echo "Generating test coverage report..."
	@Rscript -e "covr::package_coverage()"

# Interactive coverage report
coverage-report:
	@echo "Opening interactive coverage report..."
	@Rscript -e "covr::report(covr::package_coverage())"

# Auto-format code with styler
style:
	@echo "Styling R code..."
	@Rscript -e "styler::style_pkg()"

# Normalize DESCRIPTION file
desc-normalize:
	@echo "Normalizing DESCRIPTION file..."
	@Rscript -e "desc::desc_normalize()"

# Fix options templates and default files
fix-options:
	@echo "Validating option templates..."
	@Rscript -e "source('scripts/R/fix_options.R')"

# Clear package cache
clear-cache:
	@echo "Clearing package cache..."
	@Rscript -e "source('scripts/R/clear_cache.R')"

# Display repository statistics
stats:
	@bash scripts/repo-stats.sh

# Run all quality checks
all: document test lint check
	@echo "All checks passed!"

# Development mode - load package for interactive use
dev:
	@echo "Loading package for development..."
	@R -e "devtools::load_all(); library(artma)"

# Quick development cycle
quick: document test
	@echo "Quick dev cycle complete!"
