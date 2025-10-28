# clash-hash development container
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# Base toolchain: Yosys and Python
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        git \
        python3 \
        python3-venv \
        yosys \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace
ENV PYTHONPATH=/workspace

CMD ["bash"]
