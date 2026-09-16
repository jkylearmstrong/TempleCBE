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
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1-dev \
    libpoppler-cpp-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libbz2-dev \
    zlib1g-dev \
    liblzma-dev \
    libicu-dev \
    libv8-dev \
    libgomp1 \
    libuv1 libuv1-dev \
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

# Ensure libuv runtime is present for packages like fs that load libuv.so.1
RUN apt-get update && apt-get install -y --no-install-recommends libuv1 || true
# If libuv.so.1 is not available (Ubuntu may ship libuv v2), build libuv v1 from source as a fallback
RUN apt-get update && apt-get install -y --no-install-recommends autoconf automake libtool pkg-config make ca-certificates curl && \
    LIBUV_VER=1.44.2 && \
    curl -fsSL https://dist.libuv.org/dist/v${LIBUV_VER}/libuv-v${LIBUV_VER}.tar.gz -o /tmp/libuv.tar.gz && \
    mkdir -p /tmp/libuv-src && tar -xzf /tmp/libuv.tar.gz -C /tmp/libuv-src --strip-components=1 && \
    cd /tmp/libuv-src && sh autogen.sh && ./configure && make -j"$(nproc)" && make install && ldconfig && \
    rm -rf /tmp/libuv.* /tmp/libuv-src && apt-get purge -y --auto-remove autoconf automake libtool make curl pkg-config || true
RUN rm -rf /var/lib/apt/lists/*

# renv itself, pinned to the exact version recorded in renv/activate.R (keep the two
# in sync -- renv::activate()/renv::upgrade() rewrite activate.R's embedded version
# whenever the project's renv version changes). Installed before the lockfile/source
# are copied in so this layer only invalidates on a renv upgrade, not on every commit.
ENV RENV_PATHS_CACHE=/usr/local/renv/cache
RUN mkdir -p ${RENV_PATHS_CACHE} && chown root:root ${RENV_PATHS_CACHE}
RUN Rscript -e 'install.packages("remotes", repos = "https://packagemanager.posit.co/cran/latest"); \
    remotes::install_version("renv", version = Sys.getenv("RENV_VERSION"), repos = "https://packagemanager.posit.co/cran/latest")'

WORKDIR /pkg

# Restore the exact package versions pinned in renv.lock -- the same lockfile
# renv::restore() uses on a Windows dev machine -- instead of remotes::install_deps()
# resolving whatever the latest CRAN/r-universe versions happen to be at build time.
# Copied ahead of the rest of the source tree (like DESCRIPTION was before) so this
# layer still caches independently of R/**, docs, etc.
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json ./renv/

# Additional system packages required by renv-managed R packages (fs, igraph,
# clipr, haven, pdftools) not covered by the toolchain installed above
RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake libglpk-dev libx11-dev poppler-data \
  && rm -rf /var/lib/apt/lists/*

# Use the specified cache directory for renv to speed restores across builds
RUN Rscript -e 'Sys.setenv(RENV_PATHS_CACHE = Sys.getenv("RENV_PATHS_CACHE")); renv::restore(prompt = FALSE)'

COPY . .

# Plain `R CMD INSTALL` doesn't source .Rprofile, so it can't see the renv-managed
# project library renv::restore() just populated and fails on TempleCBE's Imports.
# renv::install() runs inside the same renv-activated session restore() used, so the
# library it installs into is guaranteed consistent -- no separate path plumbing needed.
RUN Rscript -e 'renv::install(".", prompt = FALSE, INSTALL_opts = c("--no-multiarch", "--with-keep.source"))'

CMD ["R"]
