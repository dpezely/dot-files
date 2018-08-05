# Makefile - fetch & install SBCL, Quicklisp & SLIME

.PHONY: help
all: help

.PHONY: help
help:
	@echo 'Usage: "make setup" or "make update"'

.PHONY: update
update: quicklisp-update

.PHONY: setup
setup: sbcl quicklisp slime deps


# LISP_LIB path must match what's in ~/.sbclrc
LISP_LIB=/usr/local/lisp

#CACHE_DIR=~/.cache
CACHE_DIR=/var/cache

SLIME_CACHE=${CACHE_DIR}/slime

SBCL_VERSION=1.4.10
BASE_URL=http://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_VERSION}
ARCH=$(shell uname -s | perl -nwe 'tr/[A-Z]/[a-z]/; print')
SBCL_BIN=sbcl-${SBCL_VERSION}-x86-64-${ARCH}-binary.tar.bz2
SBCL_SRC=sbcl-${SBCL_VERSION}-source.tar.bz2
SBCL_DOC=sbcl-${SBCL_VERSION}-documentation-html.tar.bz2

SBCL_PATH=$(shell which sbcl || echo /usr/local/bin/sbcl)
SBCL=sbcl --noinform
SPEED=--load "speed.lisp"
TESTING=--eval '(push :testing *features*)'
DEBUG=--eval '(push :debug *features*)'
END=--eval '(sb-ext:quit)'

LIBRARIES=hunchentoot split-sequence cl-ppcre fare-csv

LIB_CACHE_DIR=~/.cache/common-lisp
FASL_DIR=${LIB_CACHE_DIR}/sbcl-*/*/*/${PROJECT_NAME}

.PHONY: urls
urls:
	@echo ${BASE_URL}/${SBCL_BIN}
	@echo ${BASE_URL}/${SBCL_DOC}

tags: TAGS
	find . -name '*.{c,erl,js,go,lisp,pl,py,rb,rs,sh}' -print | etags -

.PHONY: ref
ref: references

.PHONY: references
# http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html
# http://www.lispworks.com/documentation/common-lisp.html
references:
	wget -N http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz
	wget -N http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
	@echo "Running sudo to finish installation:"
	sudo tar xfg -C ${LISP_LIB} cltl_ht.tgz
	sudo tar xfg -C ${LISP_LIB} HyperSpec-7-0.tar.gz

sbcl: ${SBCL_PATH}

${SBCL_PATH}: ${SBCL_BIN} ${SBCL_DOC}
	tar jxf ${SBCL_BIN}
	mv sbcl-${SBCL_VERSION}*-${ARCH} sbcl-${SBCL_VERSION}
	tar jxf ${SBCL_DOC}
	@echo "Running sudo to finish installation:"
	(cd sbcl-${SBCL_VERSION}/ && sudo sh install.sh)
	rm -rf sbcl-${SBCL_VERSION}

sbcl-from-src: ${SBCL_SRC} ${SBCL_DOC}
	tar jxf ${SBCL_SRC}
	tar jxf ${SBCL_DOC}
	(cd sbcl-${SBCL_VERSION}/ ; sh make.sh)
	@echo "Running sudo to finish installation:"
	(cd sbcl-${SBCL_VERSION}/ ; sudo sh install.sh)
	@echo "Preserve sbcl-${SBCL_VERSION} subdirectory for M-."

${SBCL_SRC}:
	wget -N ${BASE_URL}/${SBCL_SRC}

${SBCL_BIN}:
	wget -N ${BASE_URL}/${SBCL_BIN}

${SBCL_DOC}:
	wget -N ${BASE_URL}/${SBCL_DOC}

.PHONY: quicklisp-update
quicklisp-update:
	${SBCL} ${SPEED} --eval '(ql:update-client)' ${END}
	${SBCL} ${SPEED} --eval '(ql:update-dist "quicklisp")' ${END}
	${SBCL} ${SPEED} --eval '(ql:update-all-dists)' ${END}

quicklisp: ${LISP_LIB}/quicklisp ${HOME}/quicklisp ${LISP_LIB}/quicklisp.lisp
	${SBCL} ${SPEED} --load ${LISP_LIB}/quicklisp.lisp \
	   --eval '(quicklisp-quickstart:install)' ${END}

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

.PHONY: slime
slime: slime-cache slime-from-quicklisp

slime-cache: ${SLIME_CACHE}

${SLIME_CACHE}:
	sudo mkdir -p ${SLIME_CACHE}
	sudo chown ${USER} ${SLIME_CACHE}

slime-from-quicklisp: ${LISP_LIB}/quicklisp ${SLIME_CACHE}
	${SBCL} ${SPEED} --eval '(ql:quickload "quicklisp-slime-helper")' ${END}

.PHONY: deps
deps:   library-dependencies

.PHONY: depend
depend: library-dependencies

.PHONY: library-dependencies
library-dependencies:
	${SBCL} ${SPEED} --eval "(ql:quickload '(${LIBRARIES}))" ${END}

.PHONY: clean
clean:
	rm -rf ${FASL_DIR}

.PHONY: dist-clean
dist-clean: clean
	rm -rf ${LIB_CACHE_DIR}
	find . -name '*~' -delete
