FROM mrcide/naomi-base:latest

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://inla.r-inla-download.org/R/stable \
        INLA

RUN install_remote \
        mrc-ide/naomi

ARG GIT_SHA='UNKNOWN'
ARG GIT_BRANCH='UNKNOWN'
ARG NAOMI.EXTENSIONS_VERSION='UNKNOWN'
ENV NAOMI.EXTENSIONS_GIT_HASH $GIT_SHA
ENV NAOMI.EXTENSIONS_GIT_BRANCH $GIT_BRANCH
ENV NAOMI.EXTENSIONS_VERSION $NAOMI_VERSION

COPY . /src
