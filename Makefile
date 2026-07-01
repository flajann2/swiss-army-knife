# =============================================================================
# Makefile for swiss-army-knife
# Author: Fred Mitchell
# =============================================================================
# Usage:
#   make sdist            # regenerate changelog + build source tarball
#   make upload-candidate # upload as Hackage candidate (for testing)
#   make publish          # publish the release on Hackage
#   make clean            # clean build artifacts + generated CHANGELOG.md
#   make version          # show the version detected from .cabal
# =============================================================================

PKG_NAME   := swiss-army-knife
CABAL_FILE := $(PKG_NAME).cabal

# Dynamically extract version from the .cabal file
VERSION := $(shell grep -m1 '^version:' $(CABAL_FILE) | cut -d: -f2 | tr -d ' \t')

TARBALL := dist-newstyle/sdist/$(PKG_NAME)-$(VERSION).tar.gz

.PHONY: help changelog sdist upload-candidate publish clean version check

help:
	@echo "Available targets:"
	@echo "  make sdist            - Regenerate CHANGELOG.md and build source tarball"
	@echo "  make upload-candidate - Upload as Hackage candidate (testing only)"
	@echo "  make publish          - Publish release on Hackage"
	@echo "  make clean            - Clean build artifacts and generated files"
	@echo "  make version          - Show detected package version"
	@echo "  make check            - Run cabal check"

# -----------------------------------------------------------------------------
# Convert OrgMode changelog to GitHub-flavored Markdown (what Hackage prefers)
# -----------------------------------------------------------------------------
changelog:
	@echo "→ Converting CHANGELOG.org → CHANGELOG.md"
	pandoc CHANGELOG.org -o CHANGELOG.md -f org -t gfm --wrap=none

# -----------------------------------------------------------------------------
# Always regenerate changelog before creating the source distribution
# -----------------------------------------------------------------------------
sdist: changelog
	@echo "→ Building source tarball for version $(VERSION)"
	cabal clean
	cabal sdist

# -----------------------------------------------------------------------------
# Upload as a candidate (safe for testing, does not publish yet)
# -----------------------------------------------------------------------------
upload-candidate: sdist
	@echo "→ Uploading candidate $(PKG_NAME)-$(VERSION) to Hackage..."
	cabal upload $(TARBALL)

# -----------------------------------------------------------------------------
# Publish the release (permanent — use with care)
# -----------------------------------------------------------------------------
publish: sdist
	@echo "→ Publishing $(PKG_NAME)-$(VERSION) to Hackage..."
	cabal upload --publish $(TARBALL)

# -----------------------------------------------------------------------------
# Quick sanity check
# -----------------------------------------------------------------------------
check:
	cabal check

# -----------------------------------------------------------------------------
# Show the version that was auto-detected from the .cabal file
# -----------------------------------------------------------------------------
version:
	@echo "$(PKG_NAME) version: $(VERSION)"

# -----------------------------------------------------------------------------
# Clean everything
# -----------------------------------------------------------------------------
clean:
	cabal clean
	rm -f CHANGELOG.md
