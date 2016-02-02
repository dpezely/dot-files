# Makefile - fetch & install SBCL, Quicklisp & SLIME

all: help

help:
	@echo 'Usage: "make setup" or "make update"'

update: quicklisp-update

setup: sbcl quicklisp slime library-dependencies


# LISP_LIB path must match what's in ~/.sbclrc
LISP_LIB=/usr/local/lisp

#CACHE_DIR=~/.cache
CACHE_DIR=/var/cache

SLIME_CACHE=${CACHE_DIR}/slime

SBCL_VERSION=1.3.1
BASE_URL=http://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_VERSION}
ARCH=$(shell uname -s | perl -nwe 'tr/[A-Z]/[a-z]/; print')
SBCL_BIN=sbcl-${SBCL_VERSION}-x86-64-${ARCH}-binary.tar.bz2
SBCL_DOC=sbcl-${SBCL_VERSION}-documentation-html.tar.bz2

SLIME_GIT=https://github.com/slime/slime.git

SBCL=sbcl --noinform
SPEED=--load "speed.lisp"
TESTING=--eval '(push :testing *features*)'
DEBUG=--eval '(push :debug *features*)'
END=--eval '(sb-ext:quit)'

LIBRARIES="hunchentoot" "cl-utilities" "cl-ppcre" "fare-csv" "linedit"

#FASL_DIR=~/.cache/common-lisp/sbcl-*/*/*/PROJECT-NAME

urls:
	@echo ${BASE_URL}/${SBCL_BIN}
	@echo ${BASE_URL}/${SBCL_DOC}

sbcl: ${SBCL_BIN} ${SBCL_DOC}
	tar jxf ${SBCL_BIN}
	mv sbcl-${SBCL_VERSION}*-${ARCH} sbcl-${SBCL_VERSION}
	tar jxf ${SBCL_DOC}
	@echo "Running sudo to finish installation:"
	(cd sbcl-${SBCL_VERSION}/ && sudo sh install.sh)
	rm -rf sbcl-${SBCL_VERSION}

# Intentionally abusing Makefile to download only once
${SBCL_BIN}:
	[ -f ${SBCL_BIN} ] || wget ${BASE_URL}/${SBCL_BIN}

${SBCL_DOC}:
	[ -f ${SBCL_DOC} ] || wget ${BASE_URL}/${SBCL_DOC}

quicklisp: ${LISP_LIB}/quicklisp ${HOME}/quicklisp ${LISP_LIB}/quicklisp.lisp
	${SBCL} ${SPEED} --load ${LISP_LIB}/quicklisp.lisp --eval '(quicklisp-quickstart:install)' ${END}

${LISP_LIB}/quicklisp:
	sudo mkdir -p ${LISP_LIB}/quicklisp
	sudo chown -R ${USER} ${LISP_LIB}

${HOME}/quicklisp:
	ln -s ${LISP_LIB}/quicklisp ${HOME}/quicklisp

${LISP_LIB}/quicklisp.lisp:
	(cd ${LISP_LIB} && wget https://beta.quicklisp.org/quicklisp.lisp)
	(cd ${LISP_LIB} && wget https://beta.quicklisp.org/quicklisp.lisp.asc)
	(cd ${LISP_LIB} && wget https://beta.quicklisp.org/release-key.txt)
	gpg --import ${LISP_LIB}/release-key.txt
	gpg --verify ${LISP_LIB}/quicklisp.lisp.asc ${LISP_LIB}/quicklisp.lisp

slime: slime-cache slime-from-quicklisp

slime-cache: ${SLIME_CACHE}
	sudo mkdir -p ${SLIME_CACHE}
	sudo chown ${USER} ${SLIME_CACHE}

slime-from-quicklisp: quicklisp
	${SBCL} ${SPEED} --eval '(ql:quickload "quicklisp-slime-helper")' ${END}

slime-from-git:
	(cd ${LISP_LIB} && git clone ${SLIME_GIT})
	(cd ${LISP_LIB} && ln -s slime/swank.asd .)
	${SBCL} --eval "(require 'swank)" ${END}

deps:   library-dependencies

depend: library-dependencies

library-dependencies:
	${SBCL} ${SPEED} --eval "(ql:quickload '(${LIBRARIES}))" ${END}

quicklisp-update:
	${SBCL} ${SPEED} --eval '(ql:update-dist "quicklisp")' ${END}
