# ML-DSA STA Docker Environment
# Provides OpenSTA for static timing analysis

FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV THE_PREFIX=/usr/
ENV HOME=/workspace/
ENV NPROC=8

# Install build dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        tzdata \
        vim \
        gcc \
        g++ \
        make \
        cmake \
        git \
        wget \
        python3 \
        python3-pip \
        swig \
        flex \
        libfl-dev \
        bison \
        tcl-dev \
        tk-dev \
        libeigen3-dev \
        libgtest-dev \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        zlib1g-dev \
        xz-utils \
        gnupg \
        netbase \
        m4 \
        tcsh \
        gdb \
        autoconf \
        automake \
        libtool && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Verify compiler installation
RUN command -v gcc && gcc --version && \
    command -v g++ && g++ --version

# Install yq
RUN wget -qO /usr/local/bin/yq \
        https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 && \
    chmod +x /usr/local/bin/yq

# Install OSS CAD Suite
RUN wget -q \
        https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2023-07-19/oss-cad-suite-linux-x64-20230719.tgz \
        -O /tmp/oss-cad-suite.tgz && \
    tar -xzf /tmp/oss-cad-suite.tgz -C / && \
    rm /tmp/oss-cad-suite.tgz

ENV PATH="/oss-cad-suite/bin:${PATH}"

# Build and install CUDD
RUN git clone --depth 1 --branch 3.0.0 \
        https://github.com/cuddorg/cudd.git /opt/cudd && \
    cd /opt/cudd && \
    autoreconf -fiv && \
    ./configure \
        --prefix=/usr/local \
        --enable-shared \
        --enable-obj && \
    make -j"${NPROC}" && \
    make install && \
    ldconfig

# Build OpenSTA with C++20
RUN git clone --depth 1 \
        https://github.com/The-OpenROAD-Project/OpenSTA.git /opt/OpenSTA && \
    cmake -S /opt/OpenSTA -B /opt/OpenSTA/build \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_C_COMPILER=/usr/bin/gcc \
        -DCMAKE_CXX_COMPILER=/usr/bin/g++ \
        -DCMAKE_CXX_STANDARD=20 \
        -DCMAKE_CXX_STANDARD_REQUIRED=ON \
        -DCUDD_DIR=/usr/local && \
    cmake --build /opt/OpenSTA/build --parallel "${NPROC}"

ENV PATH="/opt/OpenSTA/build:${PATH}"

# Install Python dependencies
RUN pip3 install --no-cache-dir pyyaml numpy

WORKDIR /workspace/ML-DSA

CMD ["/bin/bash"]