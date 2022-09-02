## -*- docker-image-name: "connect-devenv-base" -*-

FROM ubuntu:rolling
#FROM ubuntu:20.04
#FROM ubuntu:21.10

# Install packages
ENV DEBIAN_FRONTEND noninteractive

COPY base-environment.sh extra-packages.lst /tmp/
RUN cd /tmp && \
   ./base-environment.sh && \
   rm base-environment.sh extra-packages.lst && \
   rm -rf /var/lib/apt/lists/*

ENTRYPOINT /bin/bash -l
