# syntax=docker/dockerfile:1
ARG R_VERSION=4.6.1
FROM rocker/r-ver:${R_VERSION}

ARG QUARTO_VERSION=1.10.18
ARG TARGETARCH

# System libraries: TempleCBE deps (pdftools/r2rtf/graphics) + doc rendering toolchain
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpoppler-cpp-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    git \
    curl \
    xz-utils \
  && rm -rf /var/lib/apt/lists/*

# Quarto CLI (bundles its own pandoc) — arch picked up from buildx's TARGETARCH
RUN QUARTO_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "arm64" || echo "amd64") \
  && curl -fLo /tmp/quarto.deb "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${QUARTO_ARCH}.deb" \
  && dpkg -i /tmp/quarto.deb \
  && rm /tmp/quarto.deb

# TinyTeX for PDF rendering (Quarto -> PDF / r2rtf's LaTeX-adjacent output)
RUN quarto install tinytex --no-prompt

WORKDIR /pkg

# Install TempleCBE's declared deps first so this layer caches independently of source changes
COPY DESCRIPTION .
RUN Rscript -e 'install.packages("remotes", repos = "https://packagemanager.posit.co/cran/latest"); \
    remotes::install_deps(dependencies = TRUE, repos = c( \
      jkylearmstrong = "https://jkylearmstrong.r-universe.dev", \
      RSPM = "https://packagemanager.posit.co/cran/latest" \
    ))'

COPY . .
RUN R CMD INSTALL --no-multiarch --with-keep.source .

CMD ["R"]
