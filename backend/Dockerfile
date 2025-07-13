# Multi-stage build for better caching and smaller final image
FROM haskell:9.2.8 as builder

# Fix Debian Buster archived repositories
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's/security.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i '/stretch-updates/d' /etc/apt/sources.list

# Install system dependencies
RUN apt-get update && \
    apt-get install -y libpq-dev libsodium-dev pkg-config build-essential git curl libtool autoconf automake && \
    rm -rf /var/lib/apt/lists/*

# Build IOG-specified versions of crypto libraries
RUN git clone https://github.com/input-output-hk/libsodium.git && \
    cd libsodium && \
    git checkout 66f017f1 && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install && \
    cd .. && \
    rm -rf libsodium

RUN git clone https://github.com/bitcoin-core/secp256k1.git && \
    cd secp256k1 && \
    git checkout ac83be33 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install && \
    cd .. && \
    rm -rf secp256k1

# Build BLST from source
RUN git clone https://github.com/supranational/blst.git && \
    cd blst && \
    ./build.sh && \
    cp libblst.a /usr/local/lib/ && \
    cp bindings/blst.h /usr/local/include/ && \
    echo "prefix=/usr/local\nlibdir=/usr/local/lib\nincludedir=/usr/local/include\n\nName: blst\nDescription: BLS12-381 signature library\nVersion: 0.3.10\nLibs: -L\${libdir} -lblst\nCflags: -I\${includedir}" > /usr/local/lib/pkgconfig/blst.pc && \
    echo "/usr/local/lib" > /etc/ld.so.conf.d/blst.conf && \
    ldconfig && \
    cd .. && \
    rm -rf blst

# Update library cache
RUN ldconfig

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
