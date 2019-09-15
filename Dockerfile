FROM ubuntu:18.04 AS build-env
MAINTAINER Ryan Butler <ryan@rbutler.io>

RUN apt-get update
RUN apt-get install -y postgresql-client postgresql-contrib libpq-dev curl
RUN curl -sSL https://get.haskellstack.org/ | sh

COPY stack.yaml /mnt
COPY *.cabal /mnt
WORKDIR /mnt
RUN rm -rf ~/.stack &&  \
stack config set system-ghc --global true && \
stack setup && \
stack install --split-objs --ghc-options="-fPIC -fllvm" --only-dependencies

COPY . /mnt
RUN stack install

FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -y postgresql-client postgresql-contrib libpq-dev curl

WORKDIR /mnt
COPY --from=build-env /mnt .
COPY --from=build-env /root/.local/bin/hearts /bin/
