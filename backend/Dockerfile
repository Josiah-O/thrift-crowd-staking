# Stage 1: Build stage with proper Haskell environment
FROM haskell:9.6 as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    pkg-config \
    build-essential \
    curl \
    gnupg \
    libsodium-dev \
    git \
    cmake \
    automake \
    libtool \
    && rm -rf /var/lib/apt/lists/*

# Install BLST from source (required for cardano-crypto-class)
RUN git clone https://github.com/supranational/blst.git /tmp/blst \
    && cd /tmp/blst \
    && ./build.sh \
    && cp libblst.a /usr/local/lib/ \
    && cp bindings/blst.h /usr/local/include/ \
    && cp bindings/blst_aux.h /usr/local/include/ \
    && mkdir -p /usr/local/lib/pkgconfig \
    && echo "prefix=/usr/local" > /usr/local/lib/pkgconfig/libblst.pc \
    && echo "exec_prefix=\${prefix}" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "libdir=\${exec_prefix}/lib" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "includedir=\${prefix}/include" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "Name: libblst" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "Description: Multilingual BLS12-381 signature library" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "URL: https://github.com/supranational/blst" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "Version: 0.3.11" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "Cflags: -I\${includedir}" >> /usr/local/lib/pkgconfig/libblst.pc \
    && echo "Libs: -L\${libdir} -lblst" >> /usr/local/lib/pkgconfig/libblst.pc \
    && rm -rf /tmp/blst

# Install libsecp256k1 from source (required for cardano-crypto-class)
RUN git clone https://github.com/bitcoin-core/secp256k1 /tmp/secp256k1 \
    && cd /tmp/secp256k1 \
    && git checkout ac83be33 \
    && ./autogen.sh \
    && ./configure --enable-module-recovery --enable-experimental --enable-module-schnorrsig \
    && make \
    && make install \
    && ldconfig \
    && rm -rf /tmp/secp256k1

WORKDIR /app

# Copy Stack files first for caching
COPY backend/stack.yaml backend/package.yaml ./

# Copy smart-contracts directory
COPY smart-contracts/ ../smart-contracts/

# Copy source code
COPY backend/src/ src/

# Set GHC memory optimizations to reduce memory usage
ENV GHC_OPTS="-j1 +RTS -M2G -A64m -RTS"
ENV STACK_OPTS="--jobs=1"

# Install GHC and dependencies
RUN stack setup --install-ghc
RUN stack build --dependencies-only $STACK_OPTS

# Build the application
RUN stack build $STACK_OPTS

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
