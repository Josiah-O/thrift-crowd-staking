# Multi-stage build for better caching and smaller final image
FROM haskell:9.2.8 as builder

# Fix Debian Buster archived repositories
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's/security.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i '/stretch-updates/d' /etc/apt/sources.list

# Install system dependencies
RUN apt-get update && \
    apt-get install -y libpq-dev libsodium-dev pkg-config git build-essential && \
    rm -rf /var/lib/apt/lists/*

# Build and install BLST from source (required for cardano-crypto-class)
RUN cd /tmp && \
    git clone https://github.com/supranational/blst.git && \
    cd blst && \
    ./build.sh && \
    cp libblst.a /usr/local/lib/ && \
    cp bindings/blst.h bindings/blst_aux.h /usr/local/include/ && \
    echo "prefix=/usr/local" > /usr/local/lib/pkgconfig/libblst.pc && \
    echo "exec_prefix=\${prefix}" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "libdir=\${exec_prefix}/lib" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "includedir=\${prefix}/include" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "Name: libblst" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "Description: BLS12-381 signature library" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "Version: 0.3.11" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "Libs: -L\${libdir} -lblst" >> /usr/local/lib/pkgconfig/libblst.pc && \
    echo "Cflags: -I\${includedir}" >> /usr/local/lib/pkgconfig/libblst.pc && \
    mkdir -p /usr/local/lib/pkgconfig && \
    ldconfig && \
    cd / && rm -rf /tmp/blst

WORKDIR /app

# Copy Stack configuration first for better caching
COPY backend/stack.yaml backend/package.yaml ./

# Copy smart-contracts directory for Stack to resolve
COPY smart-contracts/ ../smart-contracts/

# Copy source code - needed for Stack setup
COPY backend/src/ src/

# Install GHC and setup Stack
RUN stack setup --install-ghc

# Build dependencies in separate layer - this will cache!
RUN stack build --dependencies-only

# Build the application
RUN stack build

# Runtime stage
FROM ubuntu:20.04

# Install system dependencies for runtime
RUN apt-get update && \
    apt-get install -y libpq5 ca-certificates curl gnupg && \
    rm -rf /var/lib/apt/lists/*

# Install Node.js 18.x
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs

# Copy the built binary from builder stage
COPY --from=builder /app/.stack-work/install/x86_64-linux/*/*/bin/thrift-crowd-staking-exe /usr/local/bin/thrift-crowd-staking-exe

# Create non-root user
RUN adduser --disabled-password --gecos '' appuser
USER appuser

WORKDIR /app

EXPOSE 8080

CMD ["thrift-crowd-staking-exe"]
