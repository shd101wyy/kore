FROM ubuntu:bionic

ENV TZ=America/Chicago
RUN    ln --symbolic --no-dereference --force /usr/share/zoneinfo/$TZ /etc/localtime \
    && echo $TZ > /etc/timezone

RUN apt update && apt upgrade -y

RUN apt install --yes                                                             \
        autoconf bison clang-6.0 cmake curl flex gcc git jq libboost-test-dev     \
        libffi-dev libgmp-dev libjemalloc-dev libmpfr-dev libtool libyaml-cpp-dev \
        libz3-dev make maven opam openjdk-8-jdk pkg-config z3 zlib1g-dev

RUN update-alternatives --set java /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/java

RUN curl -sSL https://get.haskellstack.org/ | sh

ARG USER_ID=1000
ARG GROUP_ID=1000
RUN    groupadd --gid $GROUP_ID user                                        \
    && useradd --create-home --uid $USER_ID --shell /bin/sh --gid user user

USER $USER_ID:$GROUP_ID

ENV LC_ALL=C.UTF-8
ADD --chown=user:user stack.yaml /home/user/.tmp-haskell/
ADD --chown=user:user src/main/haskell/kore/package.yaml /home/user/.tmp-haskell/src/main/haskell/kore/
RUN    cd /home/user/.tmp-haskell \
    && stack build --only-dependencies --test --bench --haddock --library-profiling
