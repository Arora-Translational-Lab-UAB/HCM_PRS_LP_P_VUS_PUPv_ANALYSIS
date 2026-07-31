# hash:sha256:1b1d690388aba58455c9659fb58ebed8e4a00fc58e4ee22d2b8df722fc7f00c8
#FROM registry.codeocean.com/codeocean/ubuntu:24.04
# hash:sha256:1e0d907ed5234de13428daac9f46f1877daf01cda446dec70c363a01ff074ec6
#FROM registry.codeocean.com/codeocean/miniconda3:4.7.10-python3.7-ubuntu18.04
# hash:sha256:531d4ebd5e1e48f77ba9bbeb7f36093f7505398d97c518b8bacd14850558d047
#FROM registry.codeocean.com/codeocean/r-base:3.6.0-ubuntu18.04

#ARG DEBIAN_FRONTEND=noninteractive
 
#COPY environment.yml /tmp
 
#RUN conda env update --name base --file /tmp/environment.yml && \
#  conda clean -ya

#RUN apt-get update \
#    && apt-get install -y --no-install-recommends \
#        python3=3.12.3-0ubuntu2.1 \
#        python3-pip=24.0+dfsg-1ubuntu1.3 \
#        python3-setuptools=68.1.2-2ubuntu1.2 \
#        python3-wheel=0.42.0-2 \
#        r-base-dev=4.3.3-2build2 \
#    && rm -rf /var/lib/apt/lists/*



FROM registry.codeocean.com/codeocean/ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

# Install system dependencies, Python, and R
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        python3=3.12.3-0ubuntu2.1 \
        python3-pip=24.0+dfsg-1ubuntu1.3 \
        python3-setuptools=68.1.2-2ubuntu1.2 \
        python3-wheel=0.42.0-2 \
        r-base=4.3.3-2build2 \
        r-base-dev=4.3.3-2build2 \
    && rm -rf /var/lib/apt/lists/*

COPY environment.yml /tmp

# If you need conda, ensure it is installed or use pip/R package installation directly
# RUN conda env update --name base --file /tmp/environment.yml && conda clean -ya