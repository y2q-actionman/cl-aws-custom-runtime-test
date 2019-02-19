FROM amazonlinux:2017.03.1.20170812

RUN mkdir /work
WORKDIR /work

RUN yum install -y zip bzip2

## Get SBCL and install it to /usr/local/bin/sbcl
ARG SBCL_BIN=sbcl-1.4.16-x86-64-linux
RUN curl -s -f -O -L http://prdownloads.sourceforge.net/sbcl/$SBCL_BIN-binary.tar.bz2 \
	&& tar xvf $SBCL_BIN-binary.tar.bz2 \
	&& rm $SBCL_BIN-binary.tar.bz2 \
	&& (cd $SBCL_BIN; sh install.sh) \
	&& rm -r $SBCL_BIN

# Get the quicklisp bootstrap and install it.
RUN curl -s -f -O "https://beta.quicklisp.org/quicklisp.lisp" \
	&& /usr/local/bin/sbcl --non-interactive \
	--load "quicklisp.lisp" \
	--eval "(quicklisp-quickstart:install)" \
	--eval "(ql-util:without-prompting (ql:add-to-init-file))" \
	&& rm quicklisp.lisp

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
	&& rm roswell_$ROSWELL_VER.orig.tar.gz \
	&& /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload '#:roswell)"

# 'aws-lambda-function-util'
COPY aws-lambda-function-util /work/aws-lambda-function-util/
RUN /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload '#:aws-lambda-function-util)"

# 'aws-lambda-runtime'
COPY aws-lambda-runtime /work/aws-lambda-runtime/
RUN /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload '#:aws-lambda-runtime)"

# install some additional libs
COPY aws-lambda-runtime-builtin-libraries /work/aws-lambda-runtime-builtin-libraries/
RUN /usr/local/bin/sbcl --non-interactive --eval "(ql:quickload '#:aws-lambda-runtime-builtin-libraries)"
