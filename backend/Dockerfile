FROM haskell:8.10.7

WORKDIR /app

COPY . .
RUN stack build --system-ghc

EXPOSE 8080

CMD ["stack", "exec", "thrift-crowd-staking-exe"]