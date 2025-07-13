# Multi-stage build for better caching and smaller final image
FROM haskell:8.10.7 as builder

# Fix Debian Buster archived repositories
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's/security.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i '/stretch-updates/d' /etc/apt/sources.list

# Install system dependencies
RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy Stack configuration first for better caching
COPY stack.yaml package.yaml ./

# Install GHC and setup Stack
RUN stack setup --install-ghc

# Build dependencies in separate layer - this will cache!
RUN stack build --only-dependencies

# Copy source code
COPY src/ src/

# Build the application
RUN stack build --copy-bins

# Runtime stage - much smaller final image
FROM ubuntu:20.04

# Install runtime dependencies including cardano-cli
RUN apt-get update && \
    apt-get install -y libpq5 ca-certificates curl gnupg && \
    # Add IOG signing key and download cardano-node 9.1.0
    curl -fsSL https://github.com/IntersectMBO/cardano-node/releases/download/9.1.0/cardano-node-9.1.0-linux.tar.gz | tar -xz -C /usr/local/bin && \
    # Clean up
    rm -rf /var/lib/apt/lists/*

# Copy the built binary from builder stage
COPY --from=builder /root/.local/bin/thrift-crowd-staking-exe /usr/local/bin/

# Create app directory and copy source for development
WORKDIR /app
COPY src/ src/

EXPOSE 8080

CMD ["thrift-crowd-staking-exe"]
