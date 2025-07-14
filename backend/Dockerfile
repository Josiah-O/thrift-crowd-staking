# Stage 1: Build stage with proper Haskell environment
FROM haskell:9.4 as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    pkg-config \
    build-essential \
    curl \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /app

# Copy Stack files first for caching
COPY backend/stack.yaml backend/package.yaml ./

# Copy smart-contracts directory
COPY smart-contracts/ ../smart-contracts/

# Copy source code
COPY backend/src/ src/

# Install GHC and dependencies
RUN stack setup --install-ghc
RUN stack build --dependencies-only

# Build the application
RUN stack build

# Stage 2: Runtime stage using IOG's official image
FROM ghcr.io/intersectmbo/cardano-node:10.3.1

# Install runtime dependencies (PostgreSQL client libraries)
# Note: This image is Nix-based, so we need to use nix-env or similar
# For now, we'll copy the binary and use a minimal runtime approach

# Copy the built binary from the builder stage
COPY --from=builder /app/.stack-work/install/x86_64-linux/*/*/bin/thrift-crowd-staking-exe /usr/local/bin/

# Create a simple entrypoint script
RUN echo '#!/bin/sh' > /usr/local/bin/start-app && \
    echo 'exec thrift-crowd-staking-exe' >> /usr/local/bin/start-app && \
    chmod +x /usr/local/bin/start-app

# Create non-root user
RUN adduser --disabled-password --gecos '' appuser
USER appuser

WORKDIR /app
EXPOSE 8080

# Override the default entrypoint to run our app instead of cardano-node
ENTRYPOINT ["/usr/local/bin/start-app"]
