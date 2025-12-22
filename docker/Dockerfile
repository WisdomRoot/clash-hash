# clash-hash STA Docker Environment
# Provides OpenSTA for static timing analysis

FROM ubuntu:20.04

ENV THE_PREFIX=/usr/
ENV HOME=/workspace/
ENV NPROC=8

# Install basic dependencies
RUN export DEBIAN_FRONTEND=noninteractive && \
    apt update && \
    apt install -y --no-install-recommends tzdata && \
    apt install -y vim g++ gcc cmake swig tcl-dev flex bison make git python3 \
                   gdb wget python3-pip libc6-dev libffi-dev libgmp-dev xz-utils \
                   zlib1g-dev gnupg netbase m4 tcsh tcl-dev tk-dev libeigen3-dev \
                   autoconf automake libtool && \
    apt-get autoclean && apt-get clean

# Install yq for YAML processing (optional, but useful)
RUN wget -qO /usr/local/bin/yq https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 && \
    chmod a+x /usr/local/bin/yq

# Install OSS CAD Suite (includes Yosys, abc, etc.)
RUN wget https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2023-07-19/oss-cad-suite-linux-x64-20230719.tgz -O oss-cad-suite.tgz && \
    tar zxvf oss-cad-suite.tgz && \
    rm oss-cad-suite.tgz

ENV PATH="$PATH:/oss-cad-suite/bin"

# Build CUDD library (required by OpenSTA)
RUN git clone https://github.com/ivmai/cudd.git && \
    cd cudd && \
    autoreconf -fiv && \
    ./configure --enable-shared --enable-obj && \
    make && \
    make install && \
    ldconfig

# Build OpenSTA from source (using OpenROAD version)
RUN git clone https://github.com/The-OpenROAD-Project/OpenSTA.git && \
    cd OpenSTA && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make

ENV PATH="$PATH:/OpenSTA/build"

# Install Python packages for analysis
RUN pip3 install pyyaml numpy

# Set working directory
WORKDIR /workspace/clash-hash

# Default command
CMD ["/bin/bash"]
