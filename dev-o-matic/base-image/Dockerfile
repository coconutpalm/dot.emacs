## -*- docker-image-name: "connect-devenv-base" -*-

#FROM ubuntu:rolling   # Docker isn't (yet) supported on 20.10 so locking to 20.04 for now
#FROM ubuntu:20.04
FROM ubuntu:21.10

# Install packages
ENV DEBIAN_FRONTEND noninteractive

COPY base-environment.sh extra-packages.lst /tmp/
RUN cd /tmp && \
   ./base-environment.sh && \
   rm base-environment.sh extra-packages.lst && \
   rm -rf /var/lib/apt/lists/*

ENTRYPOINT /bin/bash -l
