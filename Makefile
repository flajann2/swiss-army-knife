# =============================================================================
# Makefile for swiss-army-knife
# =============================================================================
# Usage examples:
#   make sdist                 # Regenerate changelog + build source tarball
#   make upload-candidate      # Upload as Hackage candidate
#   make publish               # Publish release on Hackage
#   make aur-prepare           # Prepare Hackage tarball + update AUR files
#   make aur-publish           # Prepare + commit + push to AUR (with confirmation)
#   make clean                 # Clean everything
# =============================================================================

PKG_NAME   := swiss-army-knife
CABAL_FILE := $(PKG_NAME).cabal

# Dynamically extract version from the .cabal file
VERSION := $(shell grep -m1 '^version:' $(CABAL_FILE) | cut -d: -f2 | tr -d ' \t')

TARBALL := dist-newstyle/sdist/$(PKG_NAME)-$(VERSION).tar.gz

# AUR destination directory (override with AUR_DEST=... if needed)
AUR_DEST ?= /development/aur/swiss-army-knife

.PHONY: help changelog sdist upload-candidate publish clean version check \
        aur-prepare aur-update-pkgbuild aur-generate-srcinfo aur-publish

help:
	@echo "Available targets:"
	@echo "  make sdist              - Regenerate CHANGELOG.md and build source tarball"
	@echo "  make upload-candidate   - Upload as Hackage candidate (testing only)"
	@echo "  make publish            - Publish release on Hackage"
	@echo "  make aur-prepare        - Full prep: sdist + update PKGBUILD + copy to AUR dir"
	@echo "  make aur-publish        - Prepare + commit + push to AUR (with confirmation)"
	@echo "  make clean              - Clean build artifacts and generated files"
	@echo "  make version            - Show detected package version"
	@echo "  make check              - Run cabal check"

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

# -----------------------------------------------------------------------------
# AUR targets
# -----------------------------------------------------------------------------

# Update version and sha256sums in PKGBUILD from the freshly built tarball
aur-update-pkgbuild: sdist
	@echo "→ Updating PKGBUILD to version $(VERSION)"
	@# Reset pkgrel to 1 when version changes
	@sed -i 's/^pkgver=.*/pkgver=$(VERSION)/' PKGBUILD
	@sed -i 's/^pkgrel=.*/pkgrel=1/' PKGBUILD
	@# Compute and set correct sha256sum
	@SHA=$$(sha256sum $(TARBALL) | cut -d' ' -f1); \
	sed -i "s/^sha256sums=.*/sha256sums=('$$SHA')/" PKGBUILD
	@echo "→ PKGBUILD updated for version $(VERSION)"

# Regenerate .SRCINFO from the updated PKGBUILD
aur-generate-srcinfo:
	@echo "→ Regenerating .SRCINFO"
	@makepkg --printsrcinfo > .SRCINFO

# Full AUR preparation: build tarball, update PKGBUILD, regenerate .SRCINFO,
# and automatically copy files to ~/aur/swiss-army-knife
aur-prepare: aur-update-pkgbuild aur-generate-srcinfo
	@echo "→ Copying updated files to $(AUR_DEST)"
	@mkdir -p $(AUR_DEST)
	@cp PKGBUILD $(AUR_DEST)/
	@cp .SRCINFO $(AUR_DEST)/
	@echo ""
	@echo "✅ AUR files ready in $(AUR_DEST)"
	@echo "   You can now review and push them."

# -----------------------------------------------------------------------------
# aur-publish: Prepare everything, commit with intelligent message,
# ask for confirmation, then push to AUR.
# -----------------------------------------------------------------------------
aur-publish: aur-prepare
	@echo ""
	@echo "→ Publishing to AUR ($(AUR_DEST))..."
	@cd $(AUR_DEST) && \
	if [ ! -d .git ]; then \
		echo "ERROR: $(AUR_DEST) is not a git repository!"; \
		exit 1; \
	fi && \
	git add PKGBUILD .SRCINFO && \
	if git diff --cached --quiet; then \
		echo "No changes to commit."; \
	else \
		git commit -m "Update to $(VERSION)"; \
		echo "✅ Committed: Update to $(VERSION)"; \
		echo ""; \
		read -p "Push to AUR now? [y/N] " confirm; \
		if [ "$$confirm" = "y" ] || [ "$$confirm" = "Y" ]; then \
			git push && echo "✅ Successfully pushed to AUR."; \
		else \
			echo "Push cancelled. You can push manually later with: git push"; \
		fi \
	fi
