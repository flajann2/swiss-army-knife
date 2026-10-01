# =============================================================================
# Makefile for swiss-army-knife
# =============================================================================
# Usage examples:
#   make build                 # Dynamic build
#   make install               # Build and install sak to ~/.local/bin
#   make sdist                 # Regenerate changelog + build source tarball
#   make upload-candidate      # Upload as Hackage candidate
#   make publish               # Publish release on Hackage
#   make aur-prepare           # Prepare Hackage tarball + update AUR files
#   make aur-publish           # Prepare + commit + push to AUR (with confirmation)
#   make clean                 # Clean everything
# =============================================================================

PKG_NAME   := swiss-army-knife
EXE_NAME   := sak
CABAL_FILE := $(PKG_NAME).cabal

# Dynamically extract version from the .cabal file
VERSION := $(shell grep -m1 '^version:' $(CABAL_FILE) | cut -d: -f2 | tr -d ' \t')

TARBALL := dist-newstyle/sdist/$(PKG_NAME)-$(VERSION).tar.gz

# Official Hackage tarball URL (this is what AUR users will actually download)
HACKAGE_TARBALL_URL := https://hackage.haskell.org/package/$(PKG_NAME)-$(VERSION)/$(PKG_NAME)-$(VERSION).tar.gz
HACKAGE_TARBALL     := /tmp/$(PKG_NAME)-$(VERSION).tar.gz

# AUR destination directory (override with AUR_DEST=... if needed)
AUR_DEST ?= /development/aur/swiss-army-knife

# Local install location (override with PREFIX=... or INSTALL_DIR=...)
PREFIX      ?= $(HOME)/.local
INSTALL_DIR ?= $(PREFIX)/bin

# Common flags for cabal install
INSTALL_FLAGS := --installdir=$(INSTALL_DIR) \
                 --install-method=copy \
                 --overwrite-policy=always

.PHONY: help changelog sdist upload-candidate publish clean version check \
        aur-prepare aur-update-pkgbuild aur-generate-srcinfo aur-publish \
        build build-static check-static install install-static uninstall

help:
	@echo "Available targets:"
	@echo "  make build              - Verify dynamic build succeeds"
	@echo "  make build-static       - Verify static build succeeds"
	@echo "  make install            - Build and install $(EXE_NAME) to $(INSTALL_DIR)"
	@echo "  make install-static     - Same, but statically linked"
	@echo "  make uninstall          - Remove $(INSTALL_DIR)/$(EXE_NAME)"
	@echo "  make sdist              - Regenerate CHANGELOG.md and build source tarball"
	@echo "  make upload-candidate   - Upload as Hackage candidate (testing only)"
	@echo "  make publish            - Publish release on Hackage"
	@echo "  make aur-prepare        - Full prep: sdist + update PKGBUILD (official Hackage hash) + copy to AUR"
	@echo "  make aur-publish        - Prepare + commit + push to AUR (with confirmation)"
	@echo "  make clean              - Clean build artifacts and generated files"
	@echo "  make version            - Show detected package version"
	@echo "  make check              - Run cabal check"
	@echo "  make check-static       - Static build, then cabal check"

# -----------------------------------------------------------------------------
# Ensure a static build
# -----------------------------------------------------------------------------
build-static:
	@echo "→ Verifying static build succeeds"
	cabal build --enable-executable-static

# -----------------------------------------------------------------------------
# Ensure a dynamic build
# -----------------------------------------------------------------------------
build:
	@echo "→ Verifying dynamic build succeeds"
	cabal build

check:
	cabal check

check-static: build-static
	cabal check

# -----------------------------------------------------------------------------
# Local install of the executable
# -----------------------------------------------------------------------------
install:
	@echo "→ Installing $(PKG_NAME)-$(VERSION) to $(INSTALL_DIR)"
	@mkdir -p $(INSTALL_DIR)
	cabal install exe:$(EXE_NAME) $(INSTALL_FLAGS)
	@echo "✅ Installed $(INSTALL_DIR)/$(EXE_NAME)"

install-static:
	@echo "→ Installing static $(PKG_NAME)-$(VERSION) to $(INSTALL_DIR)"
	@mkdir -p $(INSTALL_DIR)
	cabal install exe:$(EXE_NAME) --enable-executable-static $(INSTALL_FLAGS)
	@echo "✅ Installed $(INSTALL_DIR)/$(EXE_NAME) (static)"

uninstall:
	@echo "→ Removing $(INSTALL_DIR)/$(EXE_NAME)"
	rm -f $(INSTALL_DIR)/$(EXE_NAME)

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

# Update version and sha256sums in PKGBUILD using the *official* Hackage tarball
aur-update-pkgbuild: sdist
	@echo "→ Updating PKGBUILD to version $(VERSION)"
	@# Reset pkgrel to 1 when version changes
	@sed -i 's/^pkgver=.*/pkgver=$(VERSION)/' PKGBUILD
	@sed -i 's/^pkgrel=.*/pkgrel=1/' PKGBUILD

	@echo "→ Downloading official tarball from Hackage to get correct hash..."
	@wget -q -O $(HACKAGE_TARBALL) $(HACKAGE_TARBALL_URL) || \
		(echo "ERROR: Failed to download from Hackage. Is the version published?"; exit 1)

	@# Compute sha256 from the *official* Hackage tarball, update PKGBUILD,
	@# and warn if it differs from the locally built tarball.
	@SHA=$$(sha256sum $(HACKAGE_TARBALL) | cut -d' ' -f1); \
	sed -i "s/^sha256sums=.*/sha256sums=('$$SHA')/" PKGBUILD; \
	echo "→ PKGBUILD updated with official Hackage hash for version $(VERSION)"; \
	LOCAL_SHA=$$(sha256sum $(TARBALL) | cut -d' ' -f1); \
	if [ "$$LOCAL_SHA" != "$$SHA" ]; then \
		echo "⚠️  WARNING: Local tarball hash differs from Hackage hash!"; \
		echo "   This can happen if you uploaded a different tarball than the one just built."; \
	fi
	@rm -f $(HACKAGE_TARBALL)

# Regenerate .SRCINFO from the updated PKGBUILD
aur-generate-srcinfo:
	@echo "→ Regenerating .SRCINFO"
	@makepkg --printsrcinfo > .SRCINFO

# Full AUR preparation: build tarball, update PKGBUILD, regenerate .SRCINFO,
# and copy the files to $(AUR_DEST)
aur-prepare: aur-update-pkgbuild aur-generate-srcinfo
	@echo "→ Copying updated files to $(AUR_DEST)"
	@mkdir -p $(AUR_DEST)
	@cp PKGBUILD $(AUR_DEST)/
	@cp .SRCINFO $(AUR_DEST)/
	@echo ""
	@echo "✅ AUR files ready in $(AUR_DEST)"
	@echo "   You can now review and push them."

# -----------------------------------------------------------------------------
# aur-publish: Prepare everything, commit, ask for confirmation, then push.
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
		echo "✅ No changes detected — AUR package is already up to date with version $(VERSION)."; \
	else \
		git commit -m "Update to $(VERSION)" && \
		echo "✅ Committed: Update to $(VERSION)" && \
		echo "" && \
		read -p "Push to AUR now? [y/N] " confirm && \
		if [ "$$confirm" = "y" ] || [ "$$confirm" = "Y" ]; then \
			git push && echo "✅ Successfully pushed to AUR."; \
		else \
			echo "ℹ️  Push skipped. You can push manually later with:"; \
			echo "     cd $(AUR_DEST) && git push"; \
		fi \
	fi
