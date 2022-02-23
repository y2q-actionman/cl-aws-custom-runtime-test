FROM amazonlinux:2022.0.20220202.0
ARG SBCL_VERSION=2.2.1
ARG ARCH=arm64
ARG SBCL_BOOTSTRAP_VERSION=1.4.2
ARG SBCL_BOOTSTRAP_BIN=sbcl-${SBCL_BOOTSTRAP_VERSION}-${ARCH}-linux-binary.tar.bz2
ARG SBCL_BOOTSTRAP_DIR=sbcl-${SBCL_BOOTSTRAP_VERSION}-${ARCH}-linux
ARG SBCL_SRC=http://prdownloads.sourceforge.net/sbcl/sbcl-${SBCL_VERSION}-source.tar.bz2?download
ARG BUILD_TOOLS="bzip2 tar make clang openssl-devel findutils"
RUN mkdir /work
WORKDIR /work

RUN yum install -y zip $BUILD_TOOLS \
  && rm -rf /var/cache/yum/* \
  && yum clean all    

# Snapshot external resources
RUN curl -s -f -L --output sbcl-bs.tar.bz2 https://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_BOOTSTRAP_VERSION}/sbcl-${SBCL_BOOTSTRAP_VERSION}-${ARCH}-linux-binary.tar.bz2?download 
RUN curl -s -f -L --output sbcl-${SBCL_VERSION}.tar.bz2 https://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-source.tar.bz2?download 
RUN curl -s -f -O "https://beta.quicklisp.org/quicklisp.lisp"

## Get SBCL and install it to /usr/local/bin/sbcl
RUN ls -l \
    && (mkdir sbcl-bs && cd sbcl-bs && tar -xvf ../sbcl-bs.tar.bz2 --strip-components=1 && sh install.sh ) \
    && rm -r sbcl-bs \
    && tar -xvf sbcl-${SBCL_VERSION}.tar.bz2 \
    && (cd sbcl-${SBCL_VERSION}; sh make.sh && sh install.sh) \
    && rm -rf sbcl-${SBCL_VERSION}

# Get the quicklisp bootstrap and install it.
RUN /usr/local/bin/sbcl --non-interactive \
	--load "quicklisp.lisp" \
	--eval "(quicklisp-quickstart:install)" \
	--eval "(ql-util:without-prompting (ql:add-to-init-file))" \
	&& rm quicklisp.lisp

## Install some libs into the local quicklisp repository.
## To get quicklisp libs at build time, I use 'ql:quickload'.

# Assign WORKDIR as a local repository
RUN echo "(push #P\"/work/\" ql:*local-project-directories*)" >>$HOME/.sbclrc

# 'aws-lambda-function-util'
COPY aws-lambda-function-util /work/aws-lambda-function-util/
RUN ls -l && /usr/local/bin/sbcl --non-interactive \
	--eval "(ql:quickload '#:aws-lambda-function-util)" \
	--eval "(mapc #'ql-dist:clean (ql-dist:all-dists))"

# 'aws-lambda-runtime'
COPY aws-lambda-runtime /work/aws-lambda-runtime/
RUN /usr/local/bin/sbcl --non-interactive \
	--eval "(ql:quickload '#:aws-lambda-runtime)" \
	--eval "(mapc #'ql-dist:clean (ql-dist:all-dists))"

# install some additional libs
COPY aws-lambda-runtime-additional-libraries /work/aws-lambda-runtime-additional-libraries/
RUN /usr/local/bin/sbcl --non-interactive \
	--eval "(ql:quickload '#:aws-lambda-runtime-additional-libraries)" \
	--eval "(mapc #'ql-dist:clean (ql-dist:all-dists))"
