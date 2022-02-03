ARG BUILDER_IMAGE=arm64v8/erlang:24-alpine
ARG RUNNER_IMAGE=arm64v8/alpine:3.15
FROM arm64v8/erlang:24-alpine as builder

ARG REBAR_DIAGNOSTIC=0
ENV DIAGNOSTIC=${REBAR_DIAGNOSTIC}

RUN apk add --no-cache --update git tar build-base linux-headers autoconf automake libtool pkgconfig dbus-dev bzip2 bison flex gmp-dev cmake lz4 libsodium-dev openssl-dev sed wget curl

# Install Rust toolchain
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

WORKDIR /usr/src/miner

ENV CC=gcc CXX=g++ CFLAGS="-U__sun__" \
    ERLANG_ROCKSDB_OPTS="-DWITH_BUNDLE_SNAPPY=ON -DWITH_BUNDLE_LZ4=ON" \
    ERL_COMPILER_OPTIONS="[deterministic]" \
    PATH="/root/.cargo/bin:$PATH" \
    RUSTFLAGS="-C target-feature=-crt-static"

# Add and compile the dependencies to cache
COPY ./rebar* ./
RUN ./rebar3 compile

ARG VERSION=2022.01.29.0_GA

ARG BUILD_NET=mainnet
ENV DIAGNOSTIC=${REBAR_DIAGNOSTIC}

ARG TAR_PATH=_build/docker/rel/*/*.tar.gz

# Now add our code
COPY . .

RUN ./rebar3 as docker tar -n miner 

RUN mkdir -p /opt/docker/update
RUN tar -zxvf ${TAR_PATH} -C /opt/docker
RUN wget -O /opt/docker/update/genesis https://snapshots.helium.wtf/genesis.${BUILD_NET}

FROM ${RUNNER_IMAGE} as runner

ARG VERSION= 2022.01.29.0_GA

RUN apk add --no-cache --update ncurses dbus libsodium libstdc++
RUN ulimit -n 128000

WORKDIR /opt/miner

ENV COOKIE=miner \
    # Write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # add miner to path, for easy interactions
    PATH=$PATH:/opt/miner/bin

COPY --from=builder /opt/docker /opt/miner

COPY start-miner.sh /opt/miner

RUN ln -sf /opt/miner/releases/${VERSION} /config

VOLUME ["/opt/miner/hotfix", "/var/data"]

ENTRYPOINT ["/opt/miner/start_miner.sh"]
