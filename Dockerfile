FROM ubuntu:18.04
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
