# Stage 1: Use IOG's pre-built image with all crypto dependencies
FROM inputoutput/cardano-node:8.7.3 as builder

# Install additional system dependencies (if needed)
RUN apt-get update && apt-get install -y \
    libpq-dev \
    pkg-config \
    build-essential \
    curl \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 18.x (if needed)
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs

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

# Stage 2: Minimal runtime image
FROM ubuntu:20.04

# Install runtime dependencies (PostgreSQL, etc.)
RUN apt-get update && apt-get install -y \
    libpq5 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy the built binary from the builder stage
COPY --from=builder /app/.stack-work/install/x86_64-linux/*/*/bin/thrift-crowd-staking-exe /usr/local/bin/

# Create non-root user
RUN adduser --disabled-password --gecos '' appuser
USER appuser

WORKDIR /app
EXPOSE 8080
CMD ["thrift-crowd-staking-exe"]
