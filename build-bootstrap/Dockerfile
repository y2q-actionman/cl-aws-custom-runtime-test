FROM amazonlinux:2017.03.1.20170812

RUN yum install -y zip bzip2 zlib

RUN mkdir /work
WORKDIR /work

## Get SBCL and install it to /usr/local/bin/sbcl
ARG SBCL_BIN=sbcl-1.4.16-x86-64-linux
RUN curl -s -f -O -L http://prdownloads.sourceforge.net/sbcl/$SBCL_BIN-binary.tar.bz2 \
	&& bzip2 -cd $SBCL_BIN-binary.tar.bz2 | tar xvf - \
	&& cd $SBCL_BIN \
	&& sh install.sh

# Get the quicklisp bootstrap and install it.
RUN curl -s -f -O "https://beta.quicklisp.org/quicklisp.lisp" \
	&& /usr/local/bin/sbcl --non-interactive \
	--load "quicklisp.lisp" \
	--eval "(quicklisp-quickstart:install)" \
	--eval "(ql-util:without-prompting (ql:add-to-init-file))"

## Install some libs into the local quicklisp repository.
## To get quicklisp libs at build time, I use 'ql:quickload'.

# Assign WORKDIR as a local repository
RUN echo "(push #P\"/work/\" ql:*local-project-directories*)" >>$HOME/.sbclrc

# Roswell runtime.
# (What I want is its Lisp code ('ros' package) only, but I must fetch
# its Lisp-implementation-management feature together.)
ARG ROSWELL_VER=19.1.10.96
RUN curl -s -f -O -L https://github.com/roswell/roswell/releases/download/v$ROSWELL_VER/roswell_$ROSWELL_VER.orig.tar.gz \
	&& tar xvf roswell_$ROSWELL_VER.orig.tar.gz \
	&& /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload #:roswell)"

# 'aws-lambda-runtime'
COPY aws-lambda-runtime /work/aws-lambda-runtime/
RUN /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload #:aws-lambda-runtime)"

# 'aws-lambda-function-util'
COPY aws-lambda-function-util /work/aws-lambda-function-util/
RUN /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload #:aws-lambda-function-util)"

# # install some libs for building bootstrap.
# COPY build-bootstrap/ql_libs_at_docker_build.lisp /work/
# RUN /usr/local/bin/sbcl --non-interactive --load "ql_libs_at_docker_build.lisp"

# TODO: move to the top directory, fix build scripts.
