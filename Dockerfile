FROM amazonlinux:2017.03.1.20170812

RUN mkdir /work
WORKDIR /work

RUN yum install -y zip bzip2 \
  && rm -rf /var/cache/yum/* \
  && yum clean all    

## Get SBCL and install it to /usr/local/bin/sbcl
ARG SBCL_BIN=sbcl-1.5.0-x86-64-linux
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

# 'aws-lambda-function-util'
COPY aws-lambda-function-util /work/aws-lambda-function-util/
RUN /usr/local/bin/sbcl --non-interactive \
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
