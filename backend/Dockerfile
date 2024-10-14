FROM haskell:8.10.7

WORKDIR /app

# Copy only the files needed for dependency resolution
COPY package.yaml stack.yaml ./

# Install GHC for the specific resolver
RUN stack setup

# Install system libraries that your project might depend on
RUN apt-get update && apt-get install -y libpq-dev

# Install dependencies in multiple steps to leverage Docker caching
RUN stack build --only-dependencies --test --no-run-tests
RUN stack build --only-dependencies --test --no-run-tests --copy-bins

# Now copy the rest of the code
COPY . .

# Build the project
RUN stack build

EXPOSE 8080

CMD ["stack", "run"]
