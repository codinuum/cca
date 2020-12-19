FROM ubuntu:20.04

MAINTAINER codinuum

RUN set -x && \
    useradd -r -s /bin/nologin cca && \
    mkdir -p /opt/cca/modules && \
    mkdir -p /var/lib/cca && \
    mkdir /root/src

COPY LICENSE /opt/cca/
COPY cca /opt/cca/

RUN set -x && \
    cd /root && \
    apt-get update && \
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
            sudo \
            vim \
            opam \
            net-tools \
            m4 flex bison automake autoconf \
            libtool pkg-config swig \
            libgmp-dev libssl-dev libz-dev libreadline-dev librdf0-dev libpcre3-dev unixodbc-dev \
            gawk gperf \
            sloccount \
            unixodbc \
            openjdk-8-jdk \
            python3 python3-dev \
            python3-distutils \
            python3-psutil \
            python3-pygit2 \
            python3-distutils \
            wget ca-certificates \
            git rsync && \
    wget https://bootstrap.pypa.io/get-pip.py && \
    python3 get-pip.py && \
    pip3 install pyodbc simplejson && \
    rm get-pip.py

RUN set -x && \
    cd /root && \
    git clone https://github.com/dajobe/redland-bindings && \
    cd redland-bindings && \
    ./autogen.sh --with-python=python3 && \
    make install && \
    cd /root && \
    rm -r redland-bindings

RUN set -x && \
    cd /root && \
    git clone https://github.com/openlink/virtuoso-opensource && \
    cd virtuoso-opensource && \
    ./autogen.sh && \
    env CFLAGS='-O2 -m64' ./configure --prefix=/opt/virtuoso --with-layout=opt --with-readline=/usr --program-transform-name="s/isql/isql-v/" --disable-dbpedia-vad --disable-demo-vad --enable-fct-vad --enable-ods-vad --disable-sparqldemo-vad --disable-tutorial-vad --enable-isparql-vad --enable-rdfmappers-vad && \
    make && make install && \
    cd /root && \
    rm -r virtuoso-opensource

COPY src /root/src/

RUN set -x && \
    cd /root && \
    opam init -y --disable-sandboxing && \
    eval $(opam config env) && \
    opam install -y camlzip cryptokit csv git-unix menhir ocamlnet pxp ulex uuidm pcre && \
    git clone https://github.com/codinuum/volt && \
    cd volt && sh configure && make all && make install && \
    cd /root && \
    rm -r volt && \
    cd src && \
    make && \
    cd ast/analyzing && \
    cp -r bin etc /opt/cca/ && \
    cp modules/Mverilog*.cmxs /opt/cca/modules/ && \
    cp modules/Mpython*.cmxs /opt/cca/modules/ && \
    cp modules/Mjava*.cmxs /opt/cca/modules/ && \
    cp modules/Mfortran*.cmxs /opt/cca/modules/ && \
    cp modules/Mcpp*.cmxs /opt/cca/modules/ && \
    cd /root && \
    rm -r src

RUN set -x && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

CMD ["/bin/bash"]
