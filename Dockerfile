# syntax=docker/dockerfile:1
ARG R_VERSION=4.6.1
FROM rocker/r-ver:${R_VERSION}

ARG QUARTO_VERSION=1.10.18
ARG RENV_VERSION=1.2.4
ARG TARGETARCH

# promote to an env var so it survives into the RUN below regardless of shell quoting
ENV RENV_VERSION=${RENV_VERSION}

# System libraries: TempleCBE deps (pdftools/r2rtf/graphics) + doc rendering toolchain
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    pkg-config \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1-dev \
    libuv1 \
    libpoppler-cpp-dev \
    poppler-data \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libbz2-dev \
    zlib1g-dev \
    liblzma-dev \
    libicu-dev \
    libv8-dev \
    libglpk-dev \
    libx11-dev \
    libgomp1 \
    ca-certificates \
    git \
    curl \
    xz-utils \
  && rm -rf /var/lib/apt/lists/*

# Quarto CLI (bundles its own pandoc) — arch picked up from buildx's TARGETARCH
RUN QUARTO_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "arm64" || echo "amd64") \
  && curl -fLo /tmp/quarto.deb "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${QUARTO_ARCH}.deb" \
  && dpkg -i /tmp/quarto.deb \
  && rm /tmp/quarto.deb

# Install TeX live packages for PDF rendering (Quarto -> PDF / r2rtf's LaTeX-adjacent output)
RUN apt-get update && apt-get install -y --no-install-recommends \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-xetex \
    texlive-pictures \
    texlive-science \
  && rm -rf /var/lib/apt/lists/*

# pandoc for rmarkdown::render() (Quarto's bundled pandoc is not on R's path). Its own small
# layer after the TeX layer, so adding or updating it does not rebuild the layers above. Note
# this is the distribution's pandoc, not the one Quarto bundles: R Markdown renders use this
# one, Quarto renders use theirs.
RUN apt-get update && apt-get install -y --no-install-recommends pandoc \
  && rm -rf /var/lib/apt/lists/*

# renv itself, pinned to the exact version recorded in renv/activate.R (keep the two
# in sync -- renv::activate()/renv::upgrade() rewrite activate.R's embedded version
# whenever the project's renv version changes). Installed before the lockfile/source
# are copied in so this layer only invalidates on a renv upgrade, not on every commit.
ENV RENV_PATHS_CACHE=/usr/local/renv/cache
RUN mkdir -p ${RENV_PATHS_CACHE} && chown root:root ${RENV_PATHS_CACHE}
RUN Rscript -e 'install.packages("remotes", repos = "https://packagemanager.posit.co/cran/latest"); \
    remotes::install_version("renv", version = Sys.getenv("RENV_VERSION"), repos = "https://packagemanager.posit.co/cran/latest")'

WORKDIR /pkg

# One project library for every renv call below and for every R session started in /pkg at run time.
# Without this, the sessions that ran before `COPY . .` installed into /pkg/renv/library while a
# session started afterwards put ~/.cache/R/renv/library/<project> on .libPaths() instead, so the
# packages installed below were on disk but invisible (found by a DGX Spark run, 2026-10-23, and by
# the check at the end of this file).
ENV RENV_PATHS_LIBRARY=/pkg/renv/library

# Restore the exact package versions pinned in renv.lock -- the same lockfile
# renv::restore() uses on a Windows dev machine -- instead of remotes::install_deps()
# resolving whatever the latest CRAN/r-universe versions happen to be at build time.
# Copied ahead of the rest of the source tree (like DESCRIPTION was before) so this
# layer still caches independently of R/**, docs, etc.
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json ./renv/

# Use the specified cache directory for renv to speed restores across builds
RUN Rscript -e 'Sys.setenv(RENV_PATHS_CACHE = Sys.getenv("RENV_PATHS_CACHE")); renv::restore(prompt = FALSE)'

# The Suggests that TempleCBE needs at run time (lme4, furrr, glmnet, rsample, openxlsx, zip, yardstick,
# probably, quarto, baguette, missRanger, missForest, FactoMineR, finetune and their dependencies) are
# recorded in renv.lock, so the restore above installs them: one install phase, and the same versions
# on amd64 and arm64.

# Fail the build if the installed packages differ from renv.lock (a restore that fell back to another
# version), so renv.lock keeps describing the image.
RUN Rscript -e 'lock <- renv::lockfile_read("renv.lock")$Packages; ip <- installed.packages(); bad <- vapply(lock, function(p) !(p$Package %in% rownames(ip)) || ip[p$Package, "Version"] != p$Version, logical(1)); if (any(bad)) stop("Packages differ from renv.lock after the restore: ", paste(names(lock)[bad], collapse = ", "), call. = FALSE)'

COPY . .

# Plain `R CMD INSTALL` doesn't source .Rprofile, so it can't see the renv-managed
# project library renv::restore() just populated and fails on TempleCBE's Imports.
# renv::install() runs inside the same renv-activated session restore() used, so the
# library it installs into is guaranteed consistent -- no separate path plumbing needed.
RUN Rscript -e 'renv::install(".", prompt = FALSE, INSTALL_opts = c("--no-multiarch", "--with-keep.source"))'

# A fresh R session started in /pkg (as `docker run IMAGE Rscript ...` does) must find TempleCBE and
# every package installed above. A DGX Spark run reported that the extra packages were installed but
# masked at run time (renv put another library on .libPaths()); this fails the build, with the
# library paths printed, instead of shipping an image in which about 30 tests error and 400 skip.
RUN Rscript -e 'need <- c("TempleCBE", "lme4", "furrr", "glmnet", "rsample", "openxlsx", "zip", "yardstick", "probably", "quarto", "baguette", "missRanger", "missForest", "FactoMineR", "finetune"); have <- vapply(need, requireNamespace, logical(1), quietly = TRUE); if (!all(have)) { print(.libPaths()); stop("not found in a fresh R session in /pkg: ", paste(need[!have], collapse = ", "), call. = FALSE) }'

CMD ["R"]
