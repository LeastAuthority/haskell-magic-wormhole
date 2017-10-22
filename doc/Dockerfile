# Minimal environment for Python magic-wormhole client.
FROM ubuntu:16.04

RUN apt-get --quiet update && apt-get --quiet install -y python-pip
ENV PATH=$HOME/.local/bin:$PATH
RUN pip install --user --upgrade pip
RUN pip install --user magic-wormhole
