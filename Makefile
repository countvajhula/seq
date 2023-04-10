# Adapted from: http://www.greghendershott.com/2017/04/racket-makefiles.html
SHELL=/bin/bash

PACKAGE-NAME=seq

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

help:
	@echo "install-req - install the req dependency manager for Racket (used to simplify installation)"
	@echo "install - install package along with dependencies"
	@echo "remove - remove package"
	@echo "build - Compile libraries"
	@echo "build-docs - Build docs"
	@echo "build-all - Compile libraries, build docs, and check dependencies"
	@echo "clean - remove all build artifacts"
	@echo "check-deps - check dependencies"
	@echo "test - run tests"
	@echo "test-with-errortrace - run tests with error tracing"
	@echo "errortrace - alias for test-with-errortrace"
	@echo "cover - Run test coverage checker and view report"
	@echo "cover-coveralls - Run test coverage and upload to Coveralls"
	@echo "coverage-check - Run test coverage checker"
	@echo "coverage-report - View test coverage report"
	@echo "docs - view docs in a browser"

install-req:
	raco pkg install --auto --skip-installed req

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install: install-req
	raco req -A

remove:
	raco req -R

# Primarily for day-to-day dev.
# Build libraries from source.
build:
	raco setup --no-docs --pkgs $(PACKAGE-NAME)-lib

# Primarily for day-to-day dev.
# Build docs (if any).
build-docs:
	raco setup --no-launcher --no-foreign-libs --no-info-domain --no-pkg-deps \
	--no-install --no-post-install --pkgs $(PACKAGE-NAME)-doc

# Primarily for day-to-day dev.
# Build libraries from source, build docs (if any), and check dependencies.
build-all:
	raco setup $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)-{lib,test,doc} $(PACKAGE-NAME)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)-{lib,test,doc}

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

# Suitable for both day-to-day dev and CI
test:
	raco test -exp $(PACKAGE-NAME)-{lib,test,doc}

test-base:
	raco test -x $(PACKAGE-NAME)-test/tests/base.rkt

test-api:
	raco test -x $(PACKAGE-NAME)-test/tests/api.rkt

test-iso:
	raco test -x $(PACKAGE-NAME)-test/tests/iso.rkt

build+test: build test

errortrace-base:
	racket -l errortrace -l racket -e '(require (submod "$(PACKAGE-NAME)-test/tests/base.rkt" test))'

errortrace-api:
	racket -l errortrace -l racket -e '(require (submod "$(PACKAGE-NAME)-test/tests/api.rkt" test))'

errortrace-iso:
	racket -l errortrace -l racket -e '(require (submod "$(PACKAGE-NAME)-test/tests/iso.rkt" test))'

test-with-errortrace: errortrace-base errortrace-api errortrace-iso

errortrace: test-with-errortrace

docs:
	raco docs $(PACKAGE-NAME)

coverage-check:
	raco cover -b -d ./coverage -p $(PACKAGE-NAME)-{lib,test}

coverage-report:
	open coverage/index.html

cover: coverage-check coverage-report

cover-coveralls:
	raco cover -b -f coveralls -p $(PACKAGE-NAME)-{lib,test}

.PHONY:	help install remove build build-docs build-all test clean check-deps test test-base test-api test-iso build+test errortrace-base errortrace-api errortrace-iso test-with-errortrace errortrace docs coverage-check coverage-report cover cover-coveralls
